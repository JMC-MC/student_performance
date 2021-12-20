##################################################

# Main code for Student Performance Project EDX 

##################################################
#Setup
##################################################

# Check for and install required packages

list_of_packages <- c("ggplot2", "tidyverse","corrplot","caret","GGally","Hmisc","glmnet","kernlab","caretEnsemble")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

#Attach required packages

lapply(list_of_packages,library,character.only = TRUE)

# Load data from local file using relative file path

df_math <- read.csv("data/student-mat.csv",sep=";",header=TRUE)
df_port <- read.csv("data/student-por.csv",sep=";",header=TRUE)

##################################################
# Cleaning
##################################################
 
#Check for NAs and blanks
na_math <- df_math[rowSums(is.na(df_math)) > 0,]
na_math
# No NAs or blanks in the maths class data. 
na_port <- df_port[rowSums(is.na(df_port)) > 0,]
# No NAs or blanks in the port class data.

# Divide data into training and validation set
set.seed(2017)
train_index <- createDataPartition(y=df_port$G3, p=0.9, list=FALSE)

train_set <- df_port[train_index,]
valid_set <- df_port[-train_index,]

##################################################
# Explore
##################################################
# Variable types
str(train_set)

# Majority of variable are categorical
des<-describe(train_set)
# The following categorical variables have more than 80% of students in a single category. Pstatus, Failures, Schoolsup, Paid, nursery, higher
des$Mjob
des$Fjob
# The majority of response for Mjob and Fjob are "other". This provides minimal insight

# G3 distribution
ggplot(data = train_set,aes(G3)) + geom_histogram(binwidth = 1)

# How many 0 G3 scores are there in the train_set
zero_G3 <- train_set %>% filter(G3==0) %>% nrow()
zero_G3
# 15 Scored 0 
# Of the zero scores how many scored 0 in other test
zero_G3_G2_G1 <- train_set %>% filter(G3==0 | G2==0 | G1==0)
zero_G3_G2_G1
# There are no students that scored 0 at G2 and scored higher than 0 at G3
# This suggests that it is likely that a score of 0 was given to students who did not complete the test
G3_min <- train_set %>% filter(G3>0 & G3<5)
G3_min
# Only 1 student scored less than 5. 


##################################################
# Preprocessing
##################################################

# Remove outliers, G3=<1 
# It is assumed that these students did not complete the test.
ts_p0 <- train_set %>% filter(G3>1)

# Remove variables with low variability.
ts_p1 <- ts_p0 %>% select(-c(Pstatus, failures, schoolsup, paid, nursery, higher))

# Remove Mjob and Fjob, most responses in the other category and therefore provide very little information. 
ts_p2 <- ts_p1 %>% select(-c(Mjob,Fjob))

#Convert all chr variables to factors
ts_p3 <- ts_p2 %>% mutate_if(is.character,as.factor)
#Convert all chr variables to numeric
ts_p4 <- ts_p3 %>% mutate_if(is.factor,as.numeric)

# Finding correlation
crs <- ts_p4 %>% cor()
corrplot(crs,method = 'square')

# Correlation between Mother and Father education levels.
cor(ts_p4$Medu,ts_p4$Fedu)
# This is likely to be a good predictor of a students performance, so we want to preserve the predictor. 
# Take an the mean of Medu and Fedu and drop both variables. 
ts_p5 <- ts_p4 %>% mutate(Pedu = (Medu+Fedu)/2) %>% select(-c(Medu,Fedu))

# Weekday Alcohol consumption (Dalc) and Weekend Alcohol consumption (Walc) have a positive correlation.
cor(ts_p4$Dalc,ts_p4$Walc)
# Both variables are also negatively corr with G3 and therefore may be good predictors.
# Mean of Dalc and Walc as Ave Alc (Aalc) drop both variables
ts_p6 <- ts_p5 %>% mutate(Aalc = (Dalc+Walc)/2) %>% select(-c(Dalc,Walc))

# Free time and going out correlation
cor(ts_p4$freetime,ts_p4$goout)
# Go out and Aalc correlation
cor(ts_p6$goout,ts_p6$Aalc)

# Go out is correlated with two other variables we will therefore drop it from our predictors. 
ts_p7 <- ts_p6%>% select(-goout)

# address and travel time correlation
cor(ts_p4$address,ts_p4$traveltime)

# school and travel time correlation
cor(ts_p4$school,ts_p4$traveltime)

# school and travel time correlation
cor(ts_p4$school,ts_p4$address)

# It appears that there is a relationship between school, travel time and address.
# We know that school is correlated with G3 and will therefore keep this predictor and remove the others
ts_p8 <- ts_p7%>% select(-c(address,traveltime))

#We are not going to use the variable G2 & G1 as we are trying to make predictions early in the education process. 
ts_p9 <- ts_p8%>% select(-c(G1,G2))

# Normalize excluding G3 the data for multiple regression with the exception of the value being predicted. 
# store in variable mul_reg_pp
processed <- preProcess(ts_p9[,-16],method = 'range')
mul_reg_pp <- predict(processed,as.data.frame(ts_p9))

#Standardize the data excluding G3 and store in variable stan_pp

processed <- preProcess(ts_p9[,-16],method = c("center", "scale"))
stan_pp <- predict(processed,as.data.frame(ts_p9))


##################################################
# Develop models
##################################################
# Calculate RMSE
RMSE <- function(true_score, predicted_score){
  sqrt(mean((true_score - predicted_score)^2))
}

# Naive Model 

  # Use the mean training_set G3 score as the prediction for all G3 scores. 
  naive_pred <- mean(ts_p0$G3)
  # Calculate RMSE for this model
  naive_RMSE <- RMSE(test_set$G3,naive_pred)
  
# Multivariate Linear Regression
  
  # Fit the model using all predictor
  set.seed(2009)
  mul_reg_fit <- train(G3~., data = mul_reg_pp,method="lm",trControl = trainControl(method = "repeatedcv",number = 10,repeats = 3))
  mul_reg_fit$results
  summary(mul_reg_fit)
  # Remove insignificant predictors 
  set.seed(2009)
  mul_reg_fit <- train(G3~school+sex+studytime+reason+guardian+famsup+Pedu+Aalc+health+absences , data = mul_reg_pp,method="lm",trControl = trainControl(method = "repeatedcv",number = 10,repeats = 3))
  mul_reg_fit$results
  #Set graphical parameter to plot all final model graphs together.
  par(mfrow=c(4,1))
  # Plot residual graphs
  plot(mul_reg_fit$finalModel)
  #Reset parameters ready for next plot
  par(mfrow=c(1,1))
  
# Observation from the residuals vs fit plot
    # The residuals are partially randomly distributed around 0.
    # The relationship appears close to linear.
    # However the data is not equally spread across the score range so there is some uncertainty.
    # 3 outliers identified 309,373,562
# Observation from the residuals vs fit plot
  d<-density(mul_reg_fit$finalModel$residuals)
  plot(d,main='ResidualPlot',xlab='Residual value')
  # Some slight negative skewness to the residuals
# Observation of Residuals Vs Leverage 
  # No observations are outside cook's distance.
  
  # Eliminate extreme values
  mul_reg_pp <- mul_reg_pp[-which(rownames(mul_reg_pp)%in% c("562","373","309")),]

  # Correct with skew with log
  set.seed(2009)
  mul_reg_fit_log <- train(log10(G3)~school+sex+studytime+reason+guardian+famsup+Pedu+Aalc+health+absences, data = mul_reg_pp,method="lm",trControl = trainControl(method = "repeatedcv",number = 10,repeats = 3))
  # Plot with log
  d_l<-density(mul_reg_fit_log$finalModel$residuals)
  plot(d_l,main='ResidualPlot',xlab='Residual value, log(G3)')
  # Inverse log to get RMSE
  skew_adu_results <- RMSE(mul_reg_pp$G3,10^mul_reg_fit_log$finalModel$fitted.values)
  skew_adu_results
  #RMSE improved from 2.39 to 2.31
  
# Penalized Linear Regression 
  # Use standardized data to avoid the model becoming affected by the scale of the predictors. 
  # Train the model glmnet (ridge&lasso)
  glmnet_fit = train(G3 ~ ., 
                  data=stan_pp, 
                  method="glmnet",
                  metric = "RMSE",
                  trControl = trainControl(method="repeatedcv", number=10, repeats=3))
  min(ridge_fit$results$RMSE)
  # Tune using grid method 
  lambda <- seq(0.0001, 1, length = 100)
  glmnet_fit_tune = train(G3 ~ ., 
                    data=stan_pp, 
                    method="glmnet",
                    metric = "RMSE",
                    trControl = trainControl(method="repeatedcv", number=10, repeats=3),
                    # alpha = 0:1 try lasso (1) and ridge (0)
                    tuneGrid = expand.grid(alpha = 0:1,lambda = lambda)
                    )
  plot(glmnet_fit_tune)
  # Ridge model achieves lower RMSE
  glmnet_fit_tune$bestTune
  min(glmnet_fit_tune$results$RMSE)
  
# Regression Tree

  # Train the model using ts_p9 df, as normalization is not required for this model
  r_tree = train(G3 ~ ., 
                  data=ts_p9, 
                  method="rpart", 
                 trControl = trainControl(method = "repeatedcv",number = 10,repeats = 3))
  # Plot the tree
  plot(r_tree$finalModel)
  text(r_tree$finalModel)
  min(r_tree$results$RMSE)
  
# Random Forest
  # Train the model using ts_p9 df, as normalization is not required for this model
  rf_fit = train(G3 ~ ., 
                 data=ts_p9, 
                 method="rf",
                 metric = "RMSE",
                 trControl = trainControl(method = "repeatedcv",number = 10,repeats = 3))
  min(rf_fit$results$RMSE)
  # Tune RF using random search for mtry
  # This element of the code takes approx 3 mins to run
  control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
  set.seed(2017)
  rf_fit_tunes = train(G3 ~ ., 
                 data=ts_p9, 
                 method="rf",
                 metric = "RMSE",
                 tuneLength=15,
                 trControl = control)
  plot(rf_fit_tunes)
  # optimal mtry = 4
  min(rf_fit_tunes$results$RMSE)
  # Take a look at variable importance
  varImp(rf_fit)
  
# KNN 
  # Train the model
  # Using standardized data. 
  knn_fit = train(G3 ~ ., 
                 data=stan_pp, 
                 method="knn",
                 metric = "RMSE",
                 tuneLenth=15,
                 trControl = trainControl(method="repeatedcv", number=10, repeats=3))
  plot(knn_fit)
  # k = 9 gives the lowest RMSE however intergers above 9 were not tested. 
  # Tune using grid method
  set.seed(2017)
  k_list <- seq(1,60,2)
  knn_fit_tuned = train(G3 ~ ., 
                  data=ts_p9, 
                  method="knn",
                  metric = "RMSE",
                  preProcess = c("center", "scale"),
                  savePredictions= TRUE,
                  trControl = trainControl(method="repeatedcv", number=10, repeats=3),
                  tuneGrid = expand.grid(k = k_list))
  plot(knn_fit_tuned)
  # optimal k=31
  min(knn_fit_tuned$results$RMSE)

  
  #SVM 
  set.seed(2017)
  svm_fit = train(G3 ~ ., 
                  data=stan_pp, 
                  method="svmLinear",
                  metric = "RMSE",
                  trControl = trainControl(method="repeatedcv", number=10, repeats=3))
  svm_fit
  # RMSE 2.42 without tuning 
  # Tune for C parameter
  set.seed(2017)
  c_list <- seq(0.001,0.02,0.001)
  svm_fit_tuned = train(G3 ~ ., 
                  data=stan_pp, 
                  method="svmLinear",
                  metric = "RMSE",tuneGrid = expand.grid(C = c_list),
                  trControl = trainControl(method="repeatedcv", number=10, repeats=3)
                  )
  plot(svm_fit_tuned)
  svm_fit_tuned$bestTune
  #The final value used for the model was C = 0.013
  min(svm_fit_tuned$results$RMSE)
  
  
# Ensemble
  # Using caret list I am unable to tune each model individual so this will be left to be done automatically
  # The multivariate regression cannot be used due to the adjustments that have been made outside of the model
  set.seed(2017)  
  model_list <- caretList(G3 ~ .,
    data=stan_pp,
    trControl = trainControl(method="repeatedcv", number=10, repeats=3, savePredictions = "final"),
    methodList = c("lm","glmnet", "rf", "knn","svmLinear"),
    tuneList = NULL,
    continue_on_fail = FALSE)
  set.seed(2017)
  # Create stack using rf
  rf_ensemble <- caretStack(
    model_list,
    method="rf",
    metric="RMSE",
    trControl=trainControl(method="repeatedcv", number=10, repeats=3, savePredictions = "final")
  )
  rf_ensemble$ens_model
  varImp(rf_ensemble$ens_model)
  min(rf_ensemble$error$RMSE)
 #RMSE 2.35
 
##################################################
# Compare models using RMSE
##################################################
 
# Create comparison Data Frame
model_comp <- tibble(Model=c("Naive","Multivariate Regression","Penalized Linear Regression","Regression Tree","Random Forest","KNN","SVM","Ensemble"),
       RMSE=c(naive_RMSE,skew_adu_results,min(glmnet_fit_tune$results$RMSE),min(r_tree$results$RMSE),min(rf_fit_tunes$results$RMSE),min(knn_fit_tuned$results$RMSE),min(svm_fit_tuned$results$RMSE),min(rf_ensemble$error$RMSE)))
model_comp %>% arrange(by=RMSE)

# The top performing models are Ensemble & Multivariate Regression.

##################################################
# Validate selected models
##################################################
# Adjust validation set to include Pedu & Aalc
valid_set_adj <- valid_set %>% mutate(Pedu = (Medu+Fedu)/2,Aalc = (Dalc+Walc)/2) %>% select(-c(Medu,Fedu,Dalc,Walc))
#Convert all chr variables to factors & then to numeric
valid_set_adj_num <- valid_set_adj %>% mutate_if(is.character,as.factor) %>% mutate_if(is.factor,as.numeric)

# Validate Ensemble model
ens_pred <- predict(rf_ensemble,valid_set_adj_num)
final_ens_RMSE <-  RMSE(valid_set_adj_num$G3,ens_pred)
final_ens_RMSE

# Create results data frame
results <- data.frame(predictions=ens_pred,scores=valid_set_adj_num$G3,errors=valid_set_adj_num$G3-ens_pred)
ggplot(data = results,aes(errors)) + geom_histogram(binwidth = 1)

# Summarize errors
summary(results$errors)
sd(results$errors)

ggplot(data = results,aes(predictions)) + geom_histogram(binwidth = 1)
ggplot(data = results,aes(scores)) + geom_histogram(binwidth = 1)



# Validate Multi Variate Regression model
mul_reg_pred <- predict(mul_reg_fit_log,valid_set_adj_num)
final_multi_reg_RSME <- RMSE(valid_set_adj_num$G3,10^mul_reg_pred)

# The high RMSE for the Multi Variate Regression model compared to the RMSE during training suggests that the model has been over fitted


