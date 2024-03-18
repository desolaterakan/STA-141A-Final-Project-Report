# STA-141A-Final-Project-Report
The RStudio code of a final research project for Shizhe Chen at UC Davis, Winter Quarter 2024.

Objective
  The goal of this project was to study and generalize the neural activity of mice as detailed by Steinmetz et al. (2019). The dataset, consisting of 18 sessions of numerous trials run on four mice, will be named as the list vector session. The exploration of this dataset will mainly revolve around the neural responses, denoted as spikes, of each of the mice as they make decisions and respond to cognitive contrasts that are presented to them.
  The insights and observations gathered from this exploratory data analysis will be used to infer the desired predictors for a final model that will predict the likelihood of a mouse engaging in behaviors that will be deemed as a “success”. These will be denoted through the various processes of variable exploration, model selection, and validation techniques in order to ensure the most reliable performance of the final model.

Introduction
The key objectives of this project are:

1. Exploratory Data Analysis (EDA)
- An overview of the dataset structure across all 18 sessions will be provided to familiarize all variables.
- Any notable patterns or statistical inferences observed will be explained.
- Changes in neural spikes or feedback will be explained, as well as examined across sessions and time bins.
- Feature engineering will be performed as deemed appropriate.

2. Data Integration
- Based on the previously observed insights from the EDA, a potential approach toward constructing the final model will be explained, taking into account patterns and behaviors that are noted in the data.
- The data chosen and features selected will be justified and explained accordingly.

3. Predictive Modeling
- The predictors and predictive models chosen for the training data and test data will be explained.
- The evaluation of the predictive models throughout each step along with what performance metrics will be considered will be presented and assessed on the models.

session contains the following information for 18 individual sessions (denoted [[1]] - [[18]]):
Variable/List
Description
contrast_left
A double that indicates the level {0, 0.25, 0.5, 1} of how much contrast was applied as a stimulus on a screen to the left of the mouse.
contrast_right
A double that indicates the level {0, 0.25, 0.5, 1} of how much contrast was applied as a stimulus on a screen to the right of the mouse.
feedback_type
A double that indicates a level {-1, 1} of what feedback was administered to the mouse based on its decision after a stimulus (or lack of one) was presented.
mouse_name
A character to denote which mouse {Cori, Forssmann, Hench, Lederberg} was being tested.
brain_area
A character to denote which area of the brain was activated during a trial.
date_exp
A character to denote the date the session took place on.
spks
A list to denote how many spikes were recorded over the session.
time
A list of 40 time bins to base each observation in.

Initial Exploration
  During the initial exploration of the dataset, its structure is thoroughly examined through the use of summary statistics on individual sessions, sets of trials, and summing spikes and connecting them to brain regions. This included identifying the total number of neurons recorded and showing examples of individual data lists.
  As mentioned, there are 40 bins for each trial. The summary statistics for the first session were not very conclusive or helpful, which indicated a need to process the sessions into forms that were easier to read. Something interesting to note is that the “length” of the contrasts and the subsequent feedback is only listed as 114 despite there being 912 observations, something that shows how the contrasts being listed as 0.00 to denote no difference is not accounted for. This indicates that there are 114 instances of contrast being applied in trials.

Data Processing
	In order to better visualize the data, functions to add columns to individual sessions that took the averages of spikes across regions by trial were created. These utilized tibbles to enhance the data visualization for each session and make accessing individual trials easier. The same was done in order to transform sessions for easier reading, using trial averages by brain region in order to engineer new features. Most notably, these functions were utilized to apply these transformations to all 18 sessions, by region and time bins, and also to mutate a new column called “deltacontrast” onto each session, representing the change in contrast by taking the absolute value of the left contrast subtracted by the right contrast. This allowed for more explorative methods of analyzing the changes in stimuli across sessions. Additionally, a “success” column was mutated on to indicate when positive feedback (1) was applied to a decision that a mouse made during a trial. This will be utilized much more heavily later in the model construction process.

Data Visualization
	After all 18 sessions were transformed for easier reading, they were plotted by the average of spikes across brain regions on a density plot to assess any patterns among where in the mice brains any stimuli were noted.
Figure 1: Density Plots of Spikes by Brain Region (Session 18)

  In Figure 1, which displays specifically session 18 out of all the sessions plotted against each other, it can be observed that there appear to be no discernible or obvious patterns in these distributions, which is unsurprising given the inconsistency between which brain regions have spikes at all and which of those brain regions will have the highest frequency of spikes. This indicates that further analysis with a more logical form of visualizing is required in order to assess any potential shared patterns between the 18 sessions.
  Subsequently, the number of unique brain areas per session and the number of brain areas which had data for the most sessions were assessed through code. Looking at the data, it appeared that root came the closest to possessing spikes across all brain regions, but it narrowly missed out on just two sessions from being in every session (4 and 16). However, this still indicates that it can hold some solid predicting power in later models as it very frequently possesses neuron spikes. Another fairly frequent brain area appears to be CA1.
With this, the next step can also follow a similar logic by measuring which sessions and mice have frequent successes from the "success" column constructed earlier.
Figure 2: Success Rates by Session (Left) and by Mouse (Right)

	In Figure 2, the success rates initially all appear to average somewhere around 0.65 for the first half of the sessions, and tend to increase up to 0.7 and 0.8 later on. The only notable standout for the mice is Lederberg, who from the initial data structure information, notably had the highest number of sessions out of the four mice and the highest success rate (0.76 compared to the average of 0.656). This suggests that the number of sessions a mouse goes through significantly improves their success rates over time, indicating it would make a good predictor for the final model.
Figure 3: Change in Contrast by Trial (Left) and by Mouse (Right)

	In Figure 3, it can be observed that the most frequent trials had no change in contrasts, but the most frequent amount being 0.00 is followed closely behind by a change of 0.5. The change in contrast over trials versus success rate was also analyzed in a similar manner, and a simple regression model to investigate their potential correlation showed that higher contrasts only slightly indicated a higher success rate by trial. This still showed a positive correlation.
The mice were also plotted by their change in contrast over the trials, but out of all the mice, the only different result from an average of 0.5 was Forssmann, with an average of 0.25.
	Plotting the individual mice as well as their change in contrast against their success rate additionally revealed that Lederberg having the most amount of successes (a higher level of density) was consistent with previous findings, once again showing that a higher amount of trials correlates to a higher amount of successes. It can also be said that no contrast might produce more successes, but this may be misleading due to the higher portion of no contrast trials performed throughout the sessions.
Figure 4: Change in Success Rate and Average Spike Rate by Mouse Over Time

	In Figure 4, these investigations were based on results that seemed to indicate that the success rates for the mice will gradually become more consistent over time, and then start rising to higher rates after around 250 trials, but doing too many (up until at least 350) starts to produce unpredictable and extreme results. Holistically, the previous graph produced had pictured these conclusions, but once the graph was split into individual mice, all four of them corroborated the unpredictability of doing too many trials per mouse. Additionally, this now gives insight that there is no clear linear pattern to follow when looking at the success rates over time of individual mice, just that the results start to get constantly extreme near the end of their trials.
	In general for all 18 sessions, when plotted, there appeared to be a decline in neuron spikes as the trials went on (starting at around 250), which would be realistic to conclude, as these mice are going through the same trials many times meaning that they are bound to start reacting less as they endure the same conditions repeatedly. The outlying spikes in the data (ex. dark spike at around 150 trials) were investigated through looking at the individual mice performance, and when looking at the graphs, the outliers could seemingly be explained as natural occurrences due to the sheer number of trials that Hench and Lederberg went through compared to Cori and Forssmann, as outliers are to be expected at higher quantities of data.

Data Integration
Figure 5: Dimension Reduction Using Principal Component Analysis (PCA)

  After thorough analysis of various observations made in the initial exploration process, principal component analysis (PCA) was performed in order to assess the dimensionality of this data, since due to its massive size, it would be appropriate to analyze this quality. In Figure 5, A lot of the variance shown to be captured by the first principal component looks good initially, as it does indicate that there are definitive factors that structure a lot of the data how it is (correlation) but it still requires more exploration.
  Visually, there also appears to be a lot of higher variance in the later sessions (14 - 18) that corroborate with Lederberg and Hench (the mice with the most sessions) having higher variance in their principal components.
  With these insights, the last steps of integrating the data before constructing the final model would now be viable.
	Based on the findings in the previous steps, session number, trial number (and subsequently the bin numbers 1 - 40), the change in contrast, and the left and right contrasts in themselves seem to be the most logical choice for initial predictors when using all 18 sessions. These were placed in a predictor vector, along with setting the label and the model matrix. This was done with the goal of effectively capturing all the most correlated predictors while minimizing as much variance and noise as possible that was introduced by specific factors across sessions in mind.

Initial Prediction Modeling
  When constructing the initial test data and training data, a subset of 80% was used to create the training set, and this was what the rest of the testing sets and prediction labels were based off of. A function was created in order to properly recreate appropriate model evaluation steps for each step of this iterative process: the evaluation metrics of XGBoost for quality of the data splits, accuracy, precision, confusion matrices, and area under the curve to measure the true positive rate against the false positive rate (AUC, or ROC) are utilized in order to assess the performance of the constructed models and determine their effectiveness in predicting successful responses from the mice when presented with their visual stimuli.
Figure 6: Initial Predictive Model and Validation

	In Figure 6, we can see that this initial model appears to have relatively good performance on its first run, with the proportion of its accurate predictions for success being around 72.14%, indicating a decent fit. Now that the model had been constructed, it was time to test it on actual data from the sessions.

Prediction Performance on Test Sets
	Specifically, and for the purpose of testing, the model performance was assessed on subsets of test data from sessions 1 and 18. The training and test data was appropriately set up, as well as the prediction labels. When tested on the first and last sessions, the model performed at a proportion of approximately 65.1% and 76.08%, respectively. These indicated fits for the data that were still decent, and realistic sets of the data it was being tested on were showing promising results, especially when considering the factors of accuracy, confusion matrices, and areas under the curve (AUROC) that were being reported.

Assessing Final Model Performance
	In order to assess the quality of the model, final tests were performed. Stepwise selection was first chosen to determine which factors of the 18 sessions were most helpful and contributed the best performance to the final model. After an exhaustive search, the model concluded its final best factors.
Figure 7: Stepwise Selection Model Performance

	In Figure 7, it can be seen that the stepwise selection model had chosen its final set of predictors, and looking at its very low p-value of around 2.2-16, it can be concluded that these results can very easily be observed in the real data, indicating its good fit for the model. Additionally, its area under the curve indicated it performed with a rate of 85.55% for its successful predictions, indicating its standing as a very good candidate for the final model.
	LASSO regression was then considered to be used on the model. Its performance with finding the best lambda was optimal, and it ended up with a very acceptable accuracy rate of 86%, but a success rate that fell just short of the stepwise selection model of 82.72%. This indicated that the best model to be selected as the final model would be the stepwise selection model, and it would be tested on going forward.
	Lastly, the collinearity of the final model would need to be assessed.
Figure 8: Correlation and Variance Inflation Factor (VIF) of Final Stepwise Model

The model appears to have no outstanding correlation with itself, and appropriately low multicollinearity values, indicating its goodness-of-fit and appropriate selection as the final model to be used on the real data.

Conclusion and Discussion
	Overall, the exploratory analysis revealed several important insights for this kind of research. Notable neural activities in mice during repeated trials and sessions were taken into account during the model constructing process, and their indications were invaluable to the feature selection and final build. The features used for prediction, such as neural activity based on presented stimuli, were scrutinized under evaluation metrics in order to assess the final model prediction performance. Significant implications for future research on the understanding of mammal behavior can be gleaned from this data.
  While valuable information was obtained from this investigation, its present limitations such as the obvious complexity of neural activity patterns themselves can hinder any solid research. As well as potential confounding variables or data, there could be many potential fallacies in the holistic research process on these mice. More diverse trial settings with clearer constraints could also help validate the data processing and integration a lot further.
  These conclusions set a potential foundation for any further investigations into neural spike activity patterns, and successfully predicting mammal behavior with high accuracy.

References
Steinmetz, N.A., Zatka-Haas, P., Carandini, M. et al. Distributed coding of choice, action and engagement across the mouse brain. Nature 576, 266–273 (2019). https://doi.org/10.1038/s41586-019-1787-x
ChatGPT. Used to inquire about data visualizations and code processing.
