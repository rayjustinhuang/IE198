# Import libraries
library(dplyr)
library(RWeka)
library(ggplot2)
library(ROSE)

# Import data
train <- read.csv("ChurnTrain.csv")
test <- read.csv("ChurnTest.csv")

# View summary of data
summary(train)

# Convert area code to categorical variable
train$Area.Code <- as.factor(train$Area.Code)
test$Area.Code <- as.factor(test$Area.Code)

# Model with a decision tree via the J48 algorithm to get a baseline
J48Model <- J48(Churn. ~ ., data = train)
J48Model
summary(J48Model)
evaluate_Weka_classifier(J48Model, numFolds = 10, class = TRUE, seed = 4747)
roc.curve(train$Churn., predict(J48Model))

# AUC of 0.900; True TPR of 0.710; Accuracy of 94.0699%

#############################

# For better model evaluation, we should split train data into its own train and test sets 

# Split shuffled train data into further train and test sets
trainsize <- floor((nrow(test)/(nrow(train)+nrow(test)) * nrow(train)))
set.seed(4747)
traintestrows <- sample(seq_len(nrow(train)), size = trainsize)
traintrain <- train[-traintestrows, ]
traintest <- train[traintestrows, ]

summary(traintrain$Churn.)
summary(traintest$Churn.)

#############################

# Check J48 Model with traintrain data for comparison
J48Modeltraintraindata <- J48(Churn. ~ ., data = traintrain)
J48Modeltraintraindata
summary(J48Modeltraintraindata)
evaluate_Weka_classifier(J48Modeltraintraindata, numFolds = 10, class = TRUE, seed = 4747)
roc.curve(traintrain$Churn., predict(J48Modeltraintraindata))

# AUC of 0.902; True TPR of 0.728; Accuracy of 94.6866%

# Check J48 model learned on traintrain data with unseen traintest data
evaluate_Weka_classifier(J48Modeltraintraindata, newdata = traintest, numFolds = 10, class=TRUE,
                         seed = 4747)
roc.curve(traintest$Churn., predict(J48Modeltraintraindata, newdata = traintest))

# AUC of 0.785; True TPR of 0.538; Accuracy of 89.6226%

# Model with an AdaBoosted Random Forest
RF <- make_Weka_classifier("weka/classifiers/trees/RandomForest")
AdaBoostedRF <- AdaBoostM1(Churn. ~ ., data = traintrain,
                            control = Weka_control(W=list(RF)))
summary(AdaBoostedRF)
roc.curve(traintrain$Churn., predict(AdaBoostedRF))
evaluate_Weka_classifier(AdaBoostedRF,
                         numFolds = 10, class=TRUE, seed = 4747)

# AUC of 1.000; TPR of 0.745; Accuracy of 95.4753%

# Test the model on our unseen traintest dataset
evaluate_Weka_classifier(AdaBoostedRF, newdata = traintest,
                         numFolds = 10, class=TRUE, seed = 4747)
Firstpredictions <- predict(AdaBoostedRF, newdata = traintest)
traintestpredictions <- data.frame(Firstpredictions)
roc.curve(traintest$Churn., traintestpredictions$Firstpredictions)

# AUC of 0.805; True TPR of 0.354; Accuracy of 88.6792%
# True TPR went way down
# Class imbalance could be affecting results

#############################

# Generate synthetic data via Randomly Over Sampling Examples to correct class imbalance
traintrainROSE <- ROSE(Churn. ~ ., data = traintrain[ , 1:19], seed = 4747)$data
summary(traintrainROSE$Churn.)

# NOTE: For churn data, "True" TPR is most important; "False" TPR and accuracy do not need to be super high

# It is cheaper to retain customers than to have them switch -> better to be safe (predict more 
# people are going to churn to ensure we cover all people that might churn) than sorry

#############################

# Check J48 Model with ROSE-ed data for comparison
J48ModelROSEdata <- J48(Churn. ~ ., data = traintrainROSE)
J48ModelROSEdata
summary(J48ModelROSEdata)
evaluate_Weka_classifier(J48ModelROSEdata, numFolds = 10, class = TRUE, seed = 4747)
roc.curve(traintrainROSE$Churn., predict(J48ModelROSEdata))

# AUC of 0.952; True TPR of 0.746; Accuracy of 76.6293%

# Check J48 model learned on ROSE-ed data with unseen traintest data
evaluate_Weka_classifier(J48ModelROSEdata, newdata = traintest, numFolds = 10, class=TRUE,
                         seed = 4747)
roc.curve(traintest$Churn., predict(J48ModelROSEdata, newdata = traintest))

# AUC of 0.747; True TPR of 0.538; Accuracy of 89.6226%

#############################

# Model ROSE-ed data with an AdaBoosted Random Forest
AdaBoostedRFROSE <- AdaBoostM1(Churn. ~ ., data = traintrainROSE,
                               control = Weka_control(W=list(RF)))
summary(AdaBoostedRFROSE)
evaluate_Weka_classifier(AdaBoostedRFROSE,
                         numFolds = 10, class=TRUE, seed = 4747)
roc.curve(traintrainROSE$Churn., predict(AdaBoostedRFROSE))

# AUC of 1.000; True TPR of 0.800; Accuracy of 83.105%

# Test the Boosted RF model on unseen traintest data
evaluate_Weka_classifier(AdaBoostedRFROSE, newdata = traintest,
                         numFolds = 10, class=TRUE, seed = 4747)
traintestpredictions$BoostedRFROSEpredictions <- predict(AdaBoostedRFROSE, newdata = traintest)
roc.curve(traintest$Churn., traintestpredictions$BoostedRFROSEpredictions)

# AUC of 0.841; True TPR of 0.354; Accuracy of 88.6792%
# AUC shows a significant improvement over non-ROSE-ed data

# We will use the ROSE-ed data moving forward

############################

# Model ROSE-ed data with an AdaBoosted Support Vector Classifier
AdaBoostedSVM <- AdaBoostM1(Churn. ~ ., data = traintrainROSE,
                            control = Weka_control(W=list(SMO)))
summary(AdaBoostedSVM)
evaluate_Weka_classifier(AdaBoostedSVM,
                         numFolds = 10, class=TRUE, seed = 4747)
roc.curve(traintrainROSE$Churn., predict(AdaBoostedSVM))

# Test the Boosted SVM model on unseen traintest data
evaluate_Weka_classifier(AdaBoostedSVM, newdata = traintest,
                         numFolds = 10, class=TRUE, seed = 4747)
traintestpredictions$BoostedSVMpredictions <- predict(AdaBoostedSVM, newdata = traintest)
roc.curve(traintest$Churn., traintestpredictions$BoostedSVMpredictions)

# AUC of 0.779; True TPR of 0.231; Accuracy of 82.5472%
# Performance is worse than Boosted RF

############################

# Model ROSE-ed data with an AdaBoosted J48 Decision Tree
AdaBoostedJ48 <- AdaBoostM1(Churn. ~ ., data = traintrainROSE,
                            control = Weka_control(W=list(J48)))
summary(AdaBoostedJ48)
evaluate_Weka_classifier(AdaBoostedJ48,
                         numFolds = 10, class=TRUE, seed = 4747)
roc.curve(traintrainROSE$Churn., predict(AdaBoostedJ48))

# Test the Boosted J48 model on unseen traintest data
evaluate_Weka_classifier(AdaBoostedJ48, newdata = traintest,
                         numFolds = 10, class=TRUE, seed = 4747)
traintestpredictions$BoostedJ48predictions <- predict(AdaBoostedJ48, newdata = traintest)
roc.curve(traintest$Churn., traintestpredictions$BoostedJ48predictions)

# AUC of 0.800; True TPR of 0.523; Accuracy of 89.3868%
# Performance is better than Boosted RF

############################

# Model ROSE-ed data with a Random Forest
RF <- RF(Churn. ~ ., data = traintrainROSE,
         control = Weka_control(K=1))
summary(RF)
evaluate_Weka_classifier(RF, numFolds = 10, class=TRUE, seed=4747)
roc.curve(traintrainROSE$Churn., predict(RF))

# AUC of 1.000; TPR of 0.779; Accuracy of 82.1503%

# Test the Random Forest model on unseen traintest data
evaluate_Weka_classifier(RF, newdata = traintest,
                         numFolds = 10, class=TRUE, seed=4747)
traintestpredictions$RFpredictions <- predict(RF, newdata = traintest)
roc.curve(traintest$Churn., traintestpredictions$RFpredictions)

# AUC of 0.781; True TPR of 0.123; Accuracy of 86.3208%

############################

# Model ROSE-ed data with an ANN
MLP <- make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")
ANN <- MLP(Churn. ~ ., data = traintrainROSE)
summary(ANN)
evaluate_Weka_classifier(ANN, numFolds = 10, class=TRUE, seed=4747)
roc.curve(traintrainROSE$Churn., predict(ANN))

# AUC of 0.901; True TPR of 0.756; Accuracy of 79.3275%

evaluate_Weka_classifier(ANN, newdata = traintest,
                         numFolds = 10, class=TRUE, seed=4747)
traintestpredictions$ANNpredictions <- predict(ANN, newdata = traintest)
roc.curve(traintest$Churn., traintestpredictions$ANNpredictions)

# AUC of 0.759; True TPR of 0.415; Accuracy of 86.0849%