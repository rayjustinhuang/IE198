# Import libraries
library(dplyr)
library(RWeka)
library(ggplot2)
library(ROSE)
library(GGally)

# Import data
origtrain <- read.csv("ChurnTrain.csv")
origtest <- read.csv("ChurnTest.csv")

# View summary of data
summary(train)

# Convert area code to categorical variable
origtrain$Area.Code <- as.factor(origtrain$Area.Code)
origtest$Area.Code <- as.factor(origtest$Area.Code)

# Measure information gain to aid in feature selection
InfoGain <- InfoGainAttributeEval(Churn. ~ ., data = train)
InfoGain

# Attributes with the lowest information gain are Account Length, Day Calls, Eve Calls, Night info (mins, call, charge)
# The most important attributes are Day Charge, Customer Service Calls, and International Plan
# I suspect that Minutes and Charge are collinear. Maybe remove one of them later after more preprocessing
columnstodrop1 <- c("Account.Length","Day.Calls","Eve.Calls","Night.Mins","Night.Calls","Night.Charge")

# We create new datasets with low-gain attributes removed
train1 <- origtrain[ , !(names(origtrain) %in% columnstodrop1)]
test1 <- origtest[ , !(names(origtest) %in% columnstodrop1)]

InfoGain2 <- InfoGainAttributeEval(Churn. ~ ., data = train1)
InfoGain2

# Geography also seems to have a minimal impact; we can remove this from the datasets
train2 <- subset(train1, select = -c(Area.Code))
test2 <- subset(test1, select = -c(Area.Code))

InfoGain3 <- InfoGainAttributeEval(Churn. ~ ., data = train2)
InfoGain3

# Check scatterplot matrix
ggscatmat(test2, columns = 1:ncol(test2)-1)

# As suspected, Charge and Minutes are perfectly correlated. We remove minutes
dropminutes <- c("Day.Mins","Eve.Mins","Intl.Mins")
train3 <- train2[ , !(names(train2) %in% dropminutes)]
test3 <- test2[ , !(names(test2) %in% dropminutes)]

# Some of the data also appears to be normally distributed. We can standardize continuous variables
continuousvars <- c("Day.Charge", "Eve.Charge","Intl.Charge")
stdtrain <- train3 %>% mutate_each_(funs(scale(.) %>% as.vector), 
                             vars = continuousvars)
stdtest <- test3 %>% mutate_each_(funs(scale(.) %>% as.vector), 
                                   vars = continuousvars)

# Set seed
set.seed(4747)

# Model with a decision tree via the J48 algorithm to get a baseline
InitialJ48Model <- J48(Churn. ~ ., data = stdtrain)
InitialJ48Model
summary(InitialJ48Model)

# Use 10-fold cross-validation and plot ROC curve
evaluate_Weka_classifier(InitialJ48Model, numFolds = 10, class = TRUE, seed = 4747, complexity = TRUE)
roc.curve(stdtrain$Churn., predict(InitialJ48Model))

# AUC of 0.889; Recall of 0.720; Accuracy of 94.5994%

# Ready to begin modeling

# Clean up workspace
rm(list = c("columnstodrop1","continuousvars","dropminutes","InfoGain2","InfoGain3","origtest","origtrain",
            "test1","test2","test3","train1","train2","train3"))

#############################

# For better model evaluation, we should split train data into its own train and test sets 

# Split shuffled train data into further train and test sets
trainsize <- floor((nrow(stdtest)/(nrow(stdtrain)+nrow(stdtest)) * nrow(stdtrain)))
subtestrows <- sample(seq_len(nrow(stdtrain)), size = trainsize)
subtraining <- stdtrain[-subtestrows, ]
holdout <- stdtrain[subtestrows, ]

# Check if both training and holdout have the same percentage of churn entries
prop.table(summary(subtraining$Churn.))
prop.table(summary(holdout$Churn.))

#############################

# Check J48 Model with subtrain data for comparison
J48Modelsubtrainingdata <- J48(Churn. ~ ., data = subtraining)
J48Modelsubtrainingdata
summary(J48Modelsubtrainingdata)
evaluate_Weka_classifier(J48Modelsubtrainingdata, numFolds = 10, class = TRUE, seed = 4747)
roc.curve(subtraining$Churn., predict(J48Modelsubtrainingdata))

# AUC of 0.895; Recall of 0.728; Accuracy of 94.8111%

# Check J48 model learned on subtraining data with holdout data
evaluate_Weka_classifier(J48Modelsubtrainingdata, newdata = holdout, numFolds = 10, class=TRUE,
                         seed = 4747)
roc.curve(holdout$Churn., predict(J48Modelsubtrainingdata, newdata = holdout))

# AUC of 0.801; Recall of 0.569; Accuracy of 90.8019%

# Area under the curve and Recall had a fairly significant drop
# Correcting for class imbalance might improve the metrics

#############################

# Generate synthetic data via Randomly Over Sampling Examples to correct class imbalance
subtrainingROSE <- ROSE(Churn. ~ ., data = subtraining, seed = 4747)$data
summary(subtrainingROSE$Churn.)

# NOTE: For churn data, Recall ("True" TPR) is most important; "False" TPR and accuracy do not need to be super high
# AUC is always important (perhaps most important) to determine discriminatory power of the model

# It is cheaper to retain customers than to have them switch -> better to be safe (predict more 
# people are going to churn to ensure we cover all people that might churn) than sorry

#############################

# Check J48 Model with ROSE-ed data for comparison
J48ModelROSEdata <- J48(Churn. ~ ., data = subtrainingROSE)
J48ModelROSEdata
summary(J48ModelROSEdata)
evaluate_Weka_classifier(J48ModelROSEdata, numFolds = 10, class = TRUE, seed = 4747)
roc.curve(subtrainingROSE$Churn., predict(J48ModelROSEdata))

# AUC of 0.856; Recall of 0.782; Accuracy of 79.8257%

# Check J48 model learned on ROSE-ed data with unseen holdout data
evaluate_Weka_classifier(J48ModelROSEdata, newdata = holdout, numFolds = 10, class=TRUE,
                         seed = 4747)
roc.curve(holdout$Churn., predict(J48ModelROSEdata, newdata = holdout))

# AUC of 0.781; Recall of 0.569; Accuracy of 90.8019%

#############################

# Metrics don't seem to have been hurt; but churn now has better representation in the dataset
# Let's proceed with the ROSE-ed data

# Model with a Random Forest
RF <- make_Weka_classifier("weka/classifiers/trees/RandomForest")
RandomForest <- RF(Churn. ~ ., data = subtrainingROSE)
evaluate_Weka_classifier(RandomForest, numFolds = 10, class = TRUE, complexity = TRUE, 
                         seed = 4747)
roc.curve(subtrainingROSE$Churn., predict(RandomForest))

# AUC of 1.000; Recall of 0.779; Accuracy of 82.3163%

# Test on holdout data
evaluate_Weka_classifier(RandomForest, newdata = holdout, numFolds = 10, class = TRUE, 
                         complexity = TRUE, seed = 4747)
roc.curve(holdout$Churn., predict(RandomForest, newdata = holdout))

# AUC of 0.827; Recall of 0.492; Accuracy of 89.6226%
# Poor recall! Decent AUC.

#############################

# Try oversampling instead of ROSE
subtrainingOV <-  ovun.sample(Churn. ~ ., data = subtraining, method = "over")$data
summary(subtrainingOV$Churn.)

# Model with J48 and Random Forest for comparison
J48ModelOVdata <- J48(Churn. ~ ., data = subtrainingOV)
evaluate_Weka_classifier(J48ModelOVdata, numFolds = 10, class = TRUE, seed = 4747)
roc.curve(subtrainingOV$Churn., predict(J48ModelOVdata))

# AUC of 0.992; Recall of 0.993; Accuracy of 96.1687%

# Check J48 model learned on oversampled data with unseen holdout data
evaluate_Weka_classifier(J48ModelOVdata, newdata = holdout, numFolds = 10, class=TRUE,
                         seed = 4747)
roc.curve(holdout$Churn., predict(J48ModelOVdata, newdata = holdout))

# AUC of 0.806; Recall of 0.569; Accuracy of 90.8019%

RandomForestOV <- RF(Churn. ~ ., data = subtrainingOV)
evaluate_Weka_classifier(RandomForest, numFolds = 10, class = TRUE, complexity = TRUE, 
                         seed = 4747)
roc.curve(subtrainingOV$Churn., predict(RandomForestOV))

# AUC of 1.000; Recall of 0.779; Accuracy of 82.3163%

# Test on holdout data
evaluate_Weka_classifier(RandomForestOV, newdata = holdout, numFolds = 10, class = TRUE, 
                         complexity = TRUE, seed = 4747)
roc.curve(holdout$Churn., predict(RandomForestOV, newdata = holdout))

# AUC of 0.835; Recall of 0.492; Accuracy of 89.6226%
# Poor recall again. Try different models

# Decision tree and random forest have almost identical performance with oversampled data
# As expected, rules are largely the same. Try different models
# Use oversampled data from here on out

#############################

# AdaBoosted Random Forest
AdaBoostedRF <- AdaBoostM1(Churn. ~ ., data = subtrainingOV,
                           control = Weka_control(W=list(RF)))
summary(AdaBoostedRF)
roc.curve(subtrainingOV$Churn., predict(AdaBoostedRF))
evaluate_Weka_classifier(AdaBoostedRF, complexity = TRUE,
                         numFolds = 10, class=TRUE, seed = 4747)

# AUC of 1.000; Recall of 0.999; Accuracy of 98.7711%
# Good - but boosting may have overfit to training data

# Test the model on our unseen holdout dataset
evaluate_Weka_classifier(AdaBoostedRF, newdata = holdout,
                         numFolds = 10, class=TRUE, seed = 4747)
FirstPredictions <- predict(AdaBoostedRF, newdata = holdout)
holdoutpredictions <- data.frame(FirstPredictions)
roc.curve(holdout$Churn., holdoutpredictions$FirstPredictions)

# AUC of 0.834; Recall of 0.492; Accuracy of 90.3302%
# Still bad recall...

# Support Vector Machine-based Classifier
SVC <- SMO(Churn. ~ ., data = subtrainingOV)
evaluate_Weka_classifier(SVC, complexity = TRUE, numFolds = 10, class = TRUE,
                         seed = 4747)
roc.curve(subtrainingOV$Churn., predict(SVC))

# AUC of 0.768; Recall of 0.784; Accuracy of 76.8193%

evaluate_Weka_classifier(SVC, newdata = holdout, complexity = TRUE, numFolds = 10, class = TRUE,
                         seed = 4747)
roc.curve(holdout$Churn., predict(SVC, newdata = holdout))

# Recall of 0! Bad

# Artificial Neural Network
MLP <- make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")
ANN <- MLP(Churn. ~ ., data = subtrainingOV)
evaluate_Weka_classifier(ANN, numFolds = 10, class=TRUE, seed=4747)
roc.curve(subtrainingOV$Churn., predict(ANN))

# AUC of 0.897; Recall of 0.843; Accuracy of 88.3373%

evaluate_Weka_classifier(ANN, newdata = holdout, complexity = TRUE,
                         numFolds = 10, class=TRUE, seed=4747)
holdoutpredictions$ANNpredictions <- predict(ANN, newdata = holdout)
roc.curve(holdout$Churn., holdoutpredictions$ANNpredictions)

# AUC of 0.841; True TPR of 0.538; Accuracy of 90.0943%
# Second best model after J48. Try boosting both

# AdaBoosted J48 decision tree
AdaBoostedJ48 <- AdaBoostM1(Churn. ~ ., data = subtrainingOV,
                           control = Weka_control(W=list(J48)))
roc.curve(subtrainingOV$Churn., predict(AdaBoostedJ48))
evaluate_Weka_classifier(AdaBoostedJ48, complexity = TRUE,
                         numFolds = 10, class=TRUE, seed = 4747)

# AUC of 1.000; Recall of 0.999; Accuracy of 98.6506%

# Test the model on our unseen holdout dataset
evaluate_Weka_classifier(AdaBoostedJ48, newdata = holdout,
                         numFolds = 10, class=TRUE, seed = 4747)
holdoutpredictions$BoostedJ48predictions <- predict(AdaBoostedJ48, newdata = holdout)
roc.curve(holdout$Churn., holdoutpredictions$BoostedJ48predictions)

# AUC of 0.802; Recall of 0.600; Accuracy of 91.2736%
# Best recall so far!

# AdaBoosted Artificial Neural Network
AdaBoostedANN <- AdaBoostM1(Churn. ~ ., data = subtrainingOV,
                            control = Weka_control(W=list(MLP)))
roc.curve(subtrainingOV$Churn., predict(AdaBoostedANN))
evaluate_Weka_classifier(AdaBoostedANN, complexity = TRUE,
                         numFolds = 10, class = TRUE, seed = 4747)

# AUC of 0.919; Recall of 0.862; Accuracy of 90.0723%

# Holdout data
evaluate_Weka_classifier(AdaBoostedANN, newdata = holdout,
                         numFolds = 10, class = TRUE, seed = 4747)
holdoutpredictions$BoostedANNpredictions <- predict(AdaBoostedANN, newdata = holdout)
roc.curve(holdout$Churn., holdoutpredictions$BoostedANNpredictions)

# AUC of 0.850; Recall of 0.538; Accuracy of 89.6226%
# Best AUC so far!

# Try kNN with regular subtraining data (not oversampled)
kNN <- IBk(Churn. ~ ., data = subtraining)
roc.curve(subtraining$Churn., predict(kNN))
evaluate_Weka_classifier(kNN, complexity = TRUE, numFolds = 10, class = TRUE,
                         seed = 4747)

# AUC of 1.000; Recall of 0.571; Accuracy of 89.6226%

evaluate_Weka_classifier(kNN, newdata = holdout, numFolds = 10,
                         class = TRUE, seed = 4747)
holdoutpredictions$kNNpredictions <- predict(kNN, newdata = holdout)
roc.curve(holdout$Churn., holdoutpredictions$kNNpredictions)

# AUC of 0.715; Recall of 0.431; Accuracy of 83.9623%

# Try boosted decision stumps
AdaBoostedStumps <- AdaBoostM1(Churn. ~ ., data = subtrainingOV)
roc.curve(subtrainingOV$Churn., predict(AdaBoostedStumps))
evaluate_Weka_classifier(AdaBoostedStumps, complexity = TRUE, numFolds = 10, class = TRUE,
                         seed = 4747)

# AUC of 0.772; Recall of 0.807; Accuracy of 80.6024%
evaluate_Weka_classifier(AdaBoostedStumps, newdata = holdout, numFolds = 10,
                         class = TRUE, seed = 4747)
holdoutpredictions$BoostedStumppredictions <- predict(AdaBoostedStumps, newdata = holdout)
roc.curve(holdout$Churn., holdoutpredictions$BoostedStumppredictions)

# AUC of 0.745; Recall of 0.369; Accuracy of 88.2075%

#############################

# Time to narrow down and fine-tune the final model.

# Results of three best models on holdout data
# Boosted Forest: AUC of 0.834; Recall of 0.492; Accuracy of 90.3302%
# Boosted Tree: AUC of 0.802; Recall of 0.600; Accuracy of 91.2736%
# Boosted ANN: AUC of 0.850; Recall of 0.538; Accuracy of 89.6226%

# The most important features are AUC, Recall, and Accuracy in that order
# Best balance of all is the Boosted ANN - Winner

# Test if using ROSE-ed data improves the model

# AdaBoosted Artificial Neural Network with ROSE-ed data
AdaBoostedANN.ROSE <- AdaBoostM1(Churn. ~ ., data = subtrainingROSE,
                            control = Weka_control(W=list(MLP)))
roc.curve(subtrainingROSE$Churn., predict(AdaBoostedANN.ROSE))
evaluate_Weka_classifier(AdaBoostedANN.ROSE, complexity = TRUE,
                         numFolds = 10, class = TRUE, seed = 4747)

# AUC of 0.837; Recall of 0.791; Accuracy of 81.9012%

# Holdout data
evaluate_Weka_classifier(AdaBoostedANN.ROSE, newdata = holdout,
                         numFolds = 10, class = TRUE, seed = 4747)
holdoutpredictions$BoostedANN.ROSEpredictions <- predict(AdaBoostedANN.ROSE, newdata = holdout)
roc.curve(holdout$Churn., holdoutpredictions$BoostedANN.ROSEpredictions)

# AUC of 0.810; Recall of 0.538; Accuracy of 89.6226%

# Test for the effect of using regular data on the model
AdaBoostedANN.vanilla <- AdaBoostM1(Churn. ~ ., data = subtraining,
                               control = Weka_control(W=list(MLP)))
roc.curve(subtraining$Churn., predict(AdaBoostedANN.vanilla))
evaluate_Weka_classifier(AdaBoostedANN.vanilla, complexity = TRUE,
                         numFolds = 10, class = TRUE, seed = 4747)

# AUC of 0.897; Recall of 0.629; Accuracy of 91.9884%

# Holdout set
evaluate_Weka_classifier(AdaBoostedANN.vanilla, newdata = holdout,
                         numFolds = 10, class = TRUE, seed = 4747)
holdoutpredictions$BoostedANN.vanillapredictions <- predict(AdaBoostedANN.vanilla, newdata = holdout)
roc.curve(holdout$Churn., holdoutpredictions$BoostedANN.vanillapredictions)

# AUC of 0.813; Recall of 0.538; Accuracy of 89.6226%

# The best results happened with oversampled data; we will use oversampled data to train our final model

#############################

# We now tune the parameters of the Boosted ANN

# Artificial Neural Network - tuning for no. of epochs
AdaBoostedANNepochs50 <- AdaBoostM1(Churn. ~ ., data = subtrainingOV,
                            control = Weka_control(W = list(MLP, L = 0.15, N = 50)))
AdaBoostedANNepochs75 <- AdaBoostM1(Churn. ~ ., data = subtrainingOV,
                                    control = Weka_control(W=list(MLP, L = 0.15, N = 75)))
AdaBoostedANNepochs100 <- AdaBoostM1(Churn. ~ ., data = subtrainingOV,
                                    control = Weka_control(W=list(MLP, L = 0.15, N = 100)))
AdaBoostedANNepochs150 <- AdaBoostM1(Churn. ~ ., data = subtrainingOV,
                                    control = Weka_control(W=list(MLP, L = 0.15, N = 150)))
AdaBoostedANNepochs250 <- AdaBoostM1(Churn. ~ ., data = subtrainingOV,
                                    control = Weka_control(W=list(MLP, L = 0.15, N = 250)))
AdaBoostedANNepochs350 <- AdaBoostM1(Churn. ~ ., data = subtrainingOV,
                                     control = Weka_control(W=list(MLP, L = 0.15, N = 350)))
AdaBoostedANNepochs450 <- AdaBoostM1(Churn. ~ ., data = subtrainingOV,
                                     control = Weka_control(W=list(MLP, L = 0.15, N = 450)))
AdaBoostedANNepochs500 <- AdaBoostM1(Churn. ~ ., data = subtrainingOV,
                                     control = Weka_control(W=list(MLP, L = 0.15, N = 500)))
AdaBoostedANNepochs550 <- AdaBoostM1(Churn. ~ ., data = subtrainingOV,
                                     control = Weka_control(W=list(MLP, L = 0.15, N = 550)))
AdaBoostedANNepochs1000 <- AdaBoostM1(Churn. ~ ., data = subtrainingOV,
                                     control = Weka_control(W=list(MLP, L = 0.15, N = 1000)))
AdaBoostedANNepochs750 <- AdaBoostM1(Churn. ~ ., data = subtrainingOV,
                                      control = Weka_control(W=list(MLP, L = 0.15, N = 750)))
AdaBoostedANNepochs650 <- AdaBoostM1(Churn. ~ ., data = subtrainingOV,
                                     control = Weka_control(W=list(MLP, L = 0.15, N = 650)))
AdaBoostedANNepochs850 <- AdaBoostM1(Churn. ~ ., data = subtrainingOV,
                                     control = Weka_control(W=list(MLP, L = 0.15, N = 850)))
AdaBoostedANNepochs950 <- AdaBoostM1(Churn. ~ ., data = subtrainingOV,
                                     control = Weka_control(W=list(MLP, L = 0.15, N = 950)))
AdaBoostedANNepochs975 <- AdaBoostM1(Churn. ~ ., data = subtrainingOV,
                                     control = Weka_control(W=list(MLP, L = 0.15, N = 975)))


summary(AdaBoostedANNepochs50, class = TRUE)   # Recall = 0.858; Accuracy = 88.9157%
summary(AdaBoostedANNepochs75, class = TRUE)   # Recall = 0.859; Accuracy = 89.253%
summary(AdaBoostedANNepochs100, class = TRUE)  # Recall = 0.875; Accuracy = 89.759%
summary(AdaBoostedANNepochs150, class = TRUE)  # Recall = 0.869; Accuracy = 90.3373%
summary(AdaBoostedANNepochs250, class = TRUE)  # Recall = 0.874; Accuracy = 90.6747%
summary(AdaBoostedANNepochs350, class = TRUE)  # Recall = 0.878; Accuracy = 91.2048%
summary(AdaBoostedANNepochs450, class = TRUE)  # Recall = 0.872; Accuracy = 91.3735%
summary(AdaBoostedANNepochs500, class = TRUE)  # Recall = 0.873; Accuracy = 91.7108%
summary(AdaBoostedANNepochs550, class = TRUE)  # Recall = 0.895; Accuracy = 92.6265%
summary(AdaBoostedANNepochs1000, class = TRUE) # Recall = 0.888; Accuracy = 91.8313%
summary(AdaBoostedANNepochs750, class = TRUE)  # Recall = 0.917; Accuracy = 92.8916%
summary(AdaBoostedANNepochs650, class = TRUE)  # Recall = 0.897; Accuracy = 91.8072%
summary(AdaBoostedANNepochs850, class = TRUE)  # Recall = 0.902; Accuracy = 92.5783%
summary(AdaBoostedANNepochs950, class = TRUE)  # Recall = 0.928; Accuracy = 93.9277%
summary(AdaBoostedANNepochs975, class = TRUE)  # Recall = 0.934; Accuracy = 93.4699%

# 975 is a good number of epochs
# Tune for learning rate

AdaBoostedANNl0.1 <- AdaBoostM1(Churn. ~ ., data = subtrainingOV,
                                     control = Weka_control(W=list(MLP, L = 0.10, N = 975)))
AdaBoostedANNl0.2 <- AdaBoostM1(Churn. ~ ., data = subtrainingOV,
                                control = Weka_control(W=list(MLP, L = 0.20, N = 975)))

summary(AdaBoostedANNl0.1, class = TRUE)  # Recall = 0.919; Accuracy = 93.3735%
summary(AdaBoostedANNl0.2, class = TRUE)  # Recall = 0.915; Accuracy = 93.2530%

# 0.15 is a good learning rate. Testing on the holdout data:

evaluate_Weka_classifier(AdaBoostedANNepochs975, newdata = holdout, class = TRUE,
                         seed = 4747, numFolds = 10)
roc.curve(subtrainingOV$Churn.)
