---
title: "Course Project Report"
author: "Eric Donnelly, 920672479"
date: "2024-03-10"
output: html_document
---

```{r setup, include = FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
```

# Libraries
```{r}
library(dplyr)
library(ggplot2)
library(tidyverse)
library(stats)
library(caret)
library(glmnet)
library(car)
library(MASS)
library(class)
library(pROC)
library(xgboost)
```

# Reference
Steinmetz, N.A., Zatka-Haas, P., Carandini, M. et al. Distributed coding of choice, action and engagement across the mouse brain. Nature 576, 266–273 (2019). https://doi.org/10.1038/s41586-019-1787-x
ChatGPT. Used to inquire about data visualizations and code processing.

# Obtaining Data Structure
## Storing Sessions
```{r}
session = list()
for(i in 1:18){
  session[[i]] = readRDS(paste('~/david tbh/wq 2024/141a/STA141AProject/Data/session',i,'.rds',sep = ''))
  print(session[[i]]$mouse_name)
  print(session[[i]]$date_exp)
}
```

## Variable Summary Statistics + Dimensions
```{r}
# exploring session 1 as an example/introduction
summary(session[[1]])

# getting sizes of columns
dim(session[[1]]$spks[[1]])
```
There are 40 bins for each trial.
The summary statistics for the first session are not very conclusive or helpful, indicating a need to process the sessions.
Something interesting to note is that the "length" of the contrasts and the subsequent feedback is only listed as 114 despite there being 912 observations, something that shows how the contrasts being listed as 0.00 to denote no difference is not accounted for. This indicates that there are 114 instances of contrast being applied in trials.

```{r}
# showing an example of a trial
print(session[[1]]$spks[[1]][1, ])

# summing all session 1 spikes
x = 1
s1s = 0
while (x < 115) {
  s1s = s1s + sum(session[[1]]$spks[[1]][x, ])
  x = x + 1
}
s1s
```
There are 88 spikes in session 1, trial 1.

```{r}
# example of connecting neuron spikes with corresponding brain region
session[[1]]$spks[[1]][6, 6] 
session[[1]]$brain_area[6]
session[[1]]$spks[[1]][6, 3] 
session[[1]]$brain_area[6]
```
This indicates as an example that there is an ACA neuron with a spike at time bin 3 but not 6 in session 1, trial 1.

# Data Processing
## Feature Engineering for Trial Averages
```{r}
# creating function for averages of spikes by regions for trials
treg <- function(x, t) {
  # adding sums of spikes + brain areas
  tibblify <- tibble("neuronspikes" = rowSums(session[[x]]$spks[[t]])) %>%
    add_column("brain_area" = session[[x]]$brain_area) %>%
    
    # creating rows for spikes by region
    group_by(brain_area) %>%
    summarize(regionspikestotal = sum(neuronspikes), regionsum = n(), regionspikesavg = mean(neuronspikes)) %>%
    
    # adding over previous columns by specified session, trial number
    add_column("trial" = t) %>%
    add_column("feedback_type" = session[[x]]$feedback_type[t]) %>%
    add_column("contrast_left" = session[[x]]$contrast_left[t]) %>%
    add_column("contrast_right" = session[[x]]$contrast_right[t])
  tibblify
}

# creating function for averages of neuron spikes by time bins (1 - 40)
bins <- paste(as.character(1:40))
ttime <- function(x, t) {
  
  # feature engineering
  tbavg <- matrix(colMeans(session[[x]]$spks[[t]]), nrow = 1)
  colnames(tbavg) <- bins
  
  # adding over previous columns by specified session, trial number
  tibblify = as_tibble(tbavg) %>%
    add_column("trial" = t) %>%
    add_column("feedback_type" = session[[x]]$feedback_type[t]) %>%
    add_column("contrast_left" = session[[x]]$contrast_left[t]) %>%
    add_column("contrast_right" = session[[x]]$contrast_right[t])
  tibblify
}

# displays session 1, trial 1 averages of spikes by regions as an example
treg(1, 1)
```

## Functions to Read Sessions Easier
```{r}
# creating function to transform sessions for easier reading using trial averages by region
sreg <- function(x) {
  
  # making trial tibble using previous function
  trials <- list()
  for (t in 1:length(session[[x]]$spks)) {
    trialtib <- treg(x, t)
    trials[[t]] <- trialtib
  }
  
  # adding over previous columns specified by session
  tibblify <- do.call(rbind, trials)
  tibblify <- tibblify %>%
    add_column("session" = x) %>%
    add_column("mouse_name" = session[[x]]$mouse_name) %>%
    add_column("date_exp" = session[[x]]$date_exp)
  tibblify
}

# creating function to transform sessions for easier reading using trial averages by time bins (1 - 40)
stime <- function(x) {
  
  # making trial tibble using previous function
  trials <- list()
  for (t in 1:length(session[[x]]$spks)) {
    trialtib <- ttime(x, t)
    trials[[t]] <- trialtib
  }
  
  # adding over previous columns specified by session
  tibblify <- as_tibble(do.call(rbind, trials))
  tibblify <- tibblify %>%
    add_column("session" = x) %>%
    add_column("mouse_name" = session[[x]]$mouse_name) %>%
    add_column("date_exp" = session[[x]]$date_exp)
  tibblify
}

# transforms session 1 with trial averages by region as an example
sreg(1)
summary(sreg(1))
head(sreg(1))
```

## Transforming All 18 Sessions
```{r}
# spikes average by region, adding columns for "successes" (1s) + change in contrast
sessionsreg <- lapply(1:18, sreg) %>%
  bind_rows() %>%
  mutate(success = as.numeric(feedback_type == 1), deltacontrast = abs(contrast_left - contrast_right))

# same thing for average by time bins + factorizing session number
sessionstime <- lapply(1:18, stime) %>%
  bind_rows() %>%
  mutate(session = as.factor(session), success = as.numeric(feedback_type == 1), deltacontrast = abs(contrast_left - contrast_right))

head(sessionsreg)
head(sessionstime)
```
After using these functions, the summaries here provide row information for each trial, whereas the columns contain the average spikes by time (1-40).
These will be utilized further in the exploratory data analysis section.

# Exploratory Data Analysis (EDA)
## Visualizing Average Spike Rate By Brain region
```{r}
# creating density plots of spikes by brain region
spikesbyregion <- function(x) {
  ggplot(sreg(x), aes(x = regionspikesavg, col = brain_area, fill = brain_area)) +
    geom_density(alpha = 0.5) +
    labs(title = paste("distribution of spikes over brain regions: session", x), x = "average spike rate", y = "density (frequency)")
}

s = 1
while (s < 19) {
  print(spikesbyregion(s))
  s = s + 1
}
```
There appear to be no discernible or obvious patterns in these distributions, which is unsurprising given the inconsistency between which brain regions have spikes at all and which of those brain regions will have the highest frequency of spikes. This indicates that further analysis with a more logical form of visualizing is required.

## Visualizing Brain Areas by Sections and Mice
```{r}
# counting brain areas per session
sessionsreg %>%
  group_by(session) %>%
  summarize(uareas = n_distinct(brain_area))

# which brain areas have data for ALL sessions
sessionsreg %>%
  group_by(session) %>%
  summarize(allareas = paste(unique(brain_area), collapse = ", "))
```
Looking at the data here, root comes the closest but it narrowly misses out on just two sessions from being in every session (4 and 16).
However, this still indicates that it can hold some solid predicting power in later models as it very frequently possesses neuron spikes.
Another fairly frequent brain area appears to be CA1.
With this, the next step can also follow a similar logic by measuring which sessions and mice have frequent successes from the "success" column constructed earlier.

```{r}
# success rate by SESSION
sessionsreg %>%
  group_by(session) %>%
  summarize(successreg = mean(success, na.rm = TRUE))

# success rate by MOUSE
sessionsreg %>%
  group_by(mouse_name) %>%
  summarize(successmouse = mean(success, na.rm = TRUE))
```
Initially, the success rates all appear to average somewhere around 0.65 for the first half of the sessions, and tend to increase up to 0.7 and 0.8 later on. The only notable standout for the mice is Lederberg, who from the initial data structure information, notably had the most amount of sessions out of the four mice and the highest success rate (0.76 compared to the average of 0.656). This suggests that the number of sessions a mouse goes through significantly improves their success rates over time, indicating it would make a good predictor for the final model.

## Visualizing Correlations by Trial
```{r}
# plotting histogram of change in contrast
ggplot(sessionsreg, aes(x = deltacontrast)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue") +
  labs(title = "change in contrast by trial", x = "change in contrast", y = "density (frequency)")
```
Most frequently, there is no change in contrasts over the trials, but it is followed behind by a change of 0.5.

```{r}
# plotting change in contrast vs. success rate
ggplot(sessionsreg, aes(x = deltacontrast, y = success)) +
  geom_point() +
  geom_smooth(method = "lm", color = "purple") +
  labs(title = "change in contrast vs. success rate", x = "change in contrast", y = "success rate by trial")

# plotting change in contrast by MOUSE
ggplot(sessionsreg, aes(x = mouse_name, y = deltacontrast, fill = mouse_name)) +
  geom_boxplot() +
  labs(title = "change in contrast by mouse", x = "mouse name", y = "change in contrast")
```
Higher contrasts only slightly indicate a higher success rate by trial, but there is still a positive correlation.
Out of all the mice the only different result from an average of 0.5 is Forssmann, with an average of 0.25.

```{r}
# plotting overall success rate by MOUSE + CONTRAST
ggplot(sessionsreg, aes(x = deltacontrast, fill = mouse_name)) +
  geom_histogram(position = "dodge", binwidth = 0.1) +
  labs(title = "success rate by mouse and contrast levels", x = "change in contrast", y = "density (frequency)") +
  facet_wrap(~mouse_name)
```
These findings that Lederberg has the most amount of successes (density) are consistent with previous findings, once again showing that a higher amount of trials correlates to a higher amount of successes. It can also be said that no contrast might produce more successes, but this may be misleading due to the higher portion of no contrast trials performed.

## Visualizing Sucess Rates by Various Metrics Over Time
```{r}
# calculating success rate by trials
successbytrial <- sessionsreg %>%
  group_by(trial) %>%
  summarize(successrate = mean(success))

# plotting change in success rate by trials (over time)
ggplot(successbytrial, aes(x = trial, y = successrate)) +
  geom_line(col = "skyblue") +
  geom_point() +
  labs(title = "success rate change by trials", x = "trial", y = "success rate")
```
The results here seem to indicate that the success rates for the mice will gradually become more consistent, and then start rising to higher rates after around 250 trials, but doing too many (up until at least 350) starts to produce unpredictable and extreme results.

```{r}
# calculate success rate by individual mouse
successbymouse <- sessionsreg %>%
  group_by(mouse_name, trial) %>%
  summarize(mousesuccessrate = mean(success))

# Line plot of success rate change over time by mouse
ggplot(successbymouse, aes(x = trial, y = mousesuccessrate, col = mouse_name)) +
  geom_line() +
  geom_point(col = "black") +
  facet_wrap(~mouse_name) +
  labs(title = "change in success rate by mouse (over time)", x = "trial", y = "success rate")
```
The individual graphs for this visualization corroborate the unpredictability of doing too many trials per mouse. Additionally, this now gives insight that there is no clear linear pattern to follow when looking at the success rates over time of individual mice, just that the results start to get constantly extreme near the end of their trials.

```{r}
# plotting average spike rate over time by trial + session
ggplot(sessionsreg, aes(x = trial, y = regionspikesavg, color = session)) +
  geom_line() +
  geom_point() +
  labs(title = "change in average spike rate over time", x = "trial", y = "average spike rate")
```
In general for all 18 sessions, there appears to be a decline in neuron spikes as the trials go on (starting at around 250), which would be realistic as these mice are going through the same trials many times and bound to start reacting less as they endure the same conditions repeatedly. The outlying spikes in the data (ex. dark spike at around 150 trials) can hopefully be investigated through looking at the individual mice performance.

```{r}
# plotting average spike rate over time by trial + session for INDIVIDUAL MICE
ggplot(sessionsreg, aes(x = trial, y = regionspikesavg, color = mouse_name)) +
  geom_line() +
  geom_point() +
  facet_wrap(~mouse_name) +
  labs(title = "change in average spike rate over time by mouse", x = "trial", y = "average spike rate")
```
The outliers can seemingly be explained as natural occurrences due to the sheer number of trials that Hench and Lederberg went through compared to Cori and Forssmann, as outliers are to be expected at higher quantities of data.

## Dimension Reduction Using Principal Component Analysis (PCA)
```{r}
# perform initial PCA + plotting initial variance
features = sessionstime[, 1:40]
pca <- prcomp(features, scale. = TRUE)
head(features)
summary(pca)
plot(pca, type = "l")

# transforming to data frame for plotting results
pcadf <- as.data.frame(pca$x)
pcadf$session <- sessionstime$session
pcadf$mouse_name <- sessionstime$mouse_name

# plotting results by session
ggplot(pcadf, aes(x = PC1, y = PC2, color = session)) +
  geom_point() +
  labs(title = "pc1 vs. pc2 by session")

# plotting results by mouse
ggplot(pcadf, aes(x = PC1, y = PC2, color = mouse_name)) +
  geom_point() +
  labs(title = "pc1 vs. pc2 by mouse")
```
A lot of the variance shown to be captured by the first principal component looks good initially, as it does indicate that there are definitive factors that structure a lot of the data how it is (correlation) but it still requires more exploration.
Visually, there also appears to be a lot of higher variance in the later sessions (14 - 18) that corroborate with Lederberg and Hench (the mice with the most sessions) having higher variance in their principal components.

```{r}
# gather reduced features
rfeatures <- as.data.frame(predict(pca, newdata = features))
rpca <- prcomp(rfeatures, scale. = TRUE)
head(rfeatures)
summary(rpca)
plot(rpca, type = "l")

# transforming to data frame for plotting results
rpcadf <- as.data.frame(rpca$x)
rpcadf$session <- sessionstime$session
rpcadf$mouse_name <- sessionstime$mouse_name

# plotting reduced results by session
ggplot(rpcadf, aes(x = PC1, y = PC2, color = session)) +
  geom_point() +
  labs(title = "pc1 vs. pc2 by session")

# plotting reduced results by mouse
ggplot(rpcadf, aes(x = PC1, y = PC2, color = mouse_name)) +
  geom_point() +
  labs(title = "pc1 vs. pc2 by mouse")
```

# Data Integration for Final Model
## Selecting Features
```{r}
# constructing predictors set
preds <- c("session", "trial", "deltacontrast", "contrast_right","contrast_left", bins)
predset <- sessionstime[preds]
predset$trial <- as.numeric(predset$trial)
label <- as.numeric(sessionstime$success)
data <- model.matrix(~., predset)
head(sessionstime[preds])
```
Based on the findings in the previous steps, session number, trial number (and subsequently the bin numbers 1 - 40), the change in contrast, and the left and right contrasts in themselves seem to be the most logical choice for initial predictors when using all 18 sessions.

## Constructing Initial Test Data and Training Data + Validating Model
```{r}
set.seed(1022)

# splitting the data + labels into test set + training set
trainer <- createDataPartition(label, p = 0.8, list = FALSE, times = 1)
traindf <- predset[trainer, ]
traindata <- data[trainer, ]

testdf <- predset[-trainer, ]
testdata <- data[-trainer, ]

trainlabel <- label[trainer]
testlabel <- label[-trainer]

# function to assess model performance after each step
testthatmodel <- function() {
  
  # observing prediction results
  xgb <- xgboost(data = traindata, label = trainlabel, objective = "binary:logistic", nrounds = 10)
  predict <- predict(xgb, newdata = testdata)
  predictlabels <- as.numeric(ifelse(predict > 0.5, 1, 0))

  # evaluating model performance
  accuracy <- mean(predictlabels == testlabel)
  confmat <- confusionMatrix(as.factor(predictlabels), as.factor(testlabel))
  roc <- roc(testlabel, predict)
  
  list(accuracy, confmat$table, roc)
}

testthatmodel()
```

## Model Performance on Session 1
```{r}
set.seed(1022)

# splitting the data + labels into test set + training set based on 50 random trials from session 1
s1 <- which(sessionstime$session == 1)
tester <- sample(s1, 50, replace = FALSE)
trainer <- 1:nrow(sessionstime)
trainer <- trainer[!(trainer %in% tester)]

traindf <- predset[trainer, ]
traindata <- data[trainer, ]

testdf <- predset[-trainer, ]
testdata <- data[-trainer, ]

trainlabel <- label[trainer]
testlabel <- label[-trainer]

testthatmodel()
```

## Model Performance on Session 18
```{r}
set.seed(1022)

# splitting the data + labels into test set + training set based on 50 random trials from session 18
s18 <- which(sessionstime$session == 18)
tester <- sample(s18, 50, replace = FALSE)
trainer <- 1:nrow(sessionstime)
trainer <- trainer[!(trainer %in% tester)]

traindf <- predset[trainer, ]
traindata <- data[trainer, ]

testdf <- predset[-trainer, ]
testdata <- data[-trainer, ]

trainlabel <- label[trainer]
testlabel <- label[-trainer]

testthatmodel()
```
The model performance averages to around a 70% with its given predictors when tested on the actual sessions, so it indicates that it would be a good fit for the final model.

# Assessing Model Performance
```{r}
# performing step-wise selection
steppers <- step(lm(trainlabel ~ ., data = traindf), direction = "both", trace = FALSE)
summary(steppers)

# training model using selected features from step-wise selection
traindata <- traindata[, colnames(traindata) %in% names(steppers$model)]
testdata <- testdata[, colnames(testdata) %in% names(steppers$model)]

testthatmodel()
```

```{r}
# performing LASSO regression + identifying optimal lambda
yeehaw <- cv.glmnet(traindata, trainlabel, alpha = 1)
bestlam <- yeehaw$lambda.min

# fitting optimal model
lasso <- glmnet(traindata, trainlabel, alpha = 1, lambda = bestlam)

# observing prediction results
predict <- predict(lasso, s = bestlam, newx = testdata, type = "response")
predictlabels <- as.numeric(ifelse(predict > 0.5, 1, 0))

# evaluating model performance
accuracy <- mean(predictlabels == testlabel)
confmat <- confusionMatrix(as.factor(predictlabels), as.factor(testlabel))
roc <- roc(testlabel, predict)

list(accuracy, confmat$table, roc)
```
Overall, it appears that step-wise selection had the best performance out of all the models at 0.8555.

## Testing Collinearity in Final Step-wise Model
```{r}
cormat <- cor(traindata)
cormat

vif <- diag(solve(cormat))
vif
```
The model appears to have no outstanding correlation with itself, and appropriately low multicollinearity values.
