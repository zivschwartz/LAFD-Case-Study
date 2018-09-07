# LAFD-Case-Study
### This is the final report accompanying the code that was submitted towards a Kaggle Competition for a Final Project in my class, Stats 101C. Each group was given a large dataset from the Los Angeles Fire Department. With these data we were tasked to predict the elapsed time it takes for an emergency responder to arrive after being contacted. Along with this we were given a testing dataset, omitting the variable being predicted -- elapsed time. We fit several models using the training dataset and checked daily how well each model was performing by submitting our predictions for the testing dataset on Kaggle. 

### To clean and make the testing dataset more presentable, all NA's in a particular column were replaced by the mean value of that variable given the data that was there. To counter this, in the training dataset, instead of using the mean to replace NA's, the NA's were predicted with the use of the R package GBM (Gradient Boosting Model) and the other predictors of the testing data. 

### Our best model consists of averaging our predictions from a Generalized Boosted Regression Model and a XGBoost Model. Some predictors were omitted from the final model since they either contained the same value throughout each ID or were entirely unique themselves (i.e. ID). Looking at the distribution of the predicted values, it was not as skewed as the values within the training dataset. Our best Mean Squared Error with our Scaled GBM + XGB model was 1,367,555.05046 -- which then ranked us as the #2 team from over 90 other UCLA teams.
