# Import libraries
library(dplyr)
library(RWeka)
library(ggplot2)

# Import data
train <- read.csv("ChurnTrain.csv")
test <- read.csv("ChurnTest.csv")

# View summary of data
summary(train)

# Convert area code to categorical variable
train$Area.Code <- as.factor(train$Area.Code)
test$Area.Code <- as.factor(test$Area.Code)


# Model with a decision tree via the J48 algorithm
J48Model <- J48(Churn. ~ Account.Length + Area.Code + Int.l.Plan +
                  VMail.Plan + VMail.Message + Day.Mins + 
                  Day.Calls + Day.Charge + Eve.Mins + Eve.Calls + 
                  Eve.Charge + Night.Mins + Night.Calls + 
                  Night.Charge + Intl.Mins + Intl.Calls + 
                  Intl.Charge + CustServ.Calls, data = train)
J48Model
plot(J48Model)

# Split shuffled train data into further train and test sets
trainsize <- floor((nrow(test)/(nrow(train)+nrow(test)) * nrow(train)))
set.seed(4747)
traintestrows <- sample(seq_len(nrow(train)), size = trainsize)
traintrain <- train[-traintestrows, ]
traintest <- train[traintestrows, ]

summary(traintrain$Churn.)
summary(traintest$Churn.)

# Model with an AdaBoosted Random Forest
RF <- make_Weka_classifier("weka/classifiers/trees/RandomForest")
AdaBoostedRF <- AdaBoostM1(Churn. ~ Account.Length + Area.Code + Int.l.Plan +
                              VMail.Plan + VMail.Message + Day.Mins + 
                              Day.Calls + Day.Charge + Eve.Mins + Eve.Calls + 
                              Eve.Charge + Night.Mins + Night.Calls + 
                              Night.Charge + Intl.Mins + Intl.Calls + 
                              Intl.Charge + CustServ.Calls, data = traintrain,
                            control = Weka_control(W=list(RF)))
AdaBoostedRFPredictions <- traintrain
AdaBoostedRFPredictions$predictions <- predict(AdaBoostedRF)
summary(AdaBoostedRF)

# Perform 10-fold cross validation
evaluate_Weka_classifier(AdaBoostedRF, newdata = traintrain,
                         numFolds = 10, class=TRUE, seed = 4747)
evaluate_Weka_classifier(AdaBoostedRF, newdata = traintest,
                         numFolds = 10, seed = 4747)

