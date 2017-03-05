##### Classification (continued)

### SVM

# Limitations of perceptrons
# - solution not unique if separable
# - data needs to be separable to converge
# - linear model

# These are solved using support vector machines

# Find hyperplane that maximizes the margin

# If data is not linear, map points to higher-dimensional space
# (kernel programming)

# Fun fact: SVMs and ensembles do well in competitions

library(RWeka)

# Predicting defaults via SVM
creditsetnumericSMO <- read.csv("creditsetnumeric.csv")
creditsetnumericSMO$default10yr <- as.factor(
  creditsetnumericSMO$default10yr)
SVMModel <- SMO(default10yr ~ income + age + loan + LTI, 
                data = creditsetnumericSMO)
creditsettestSMO <- read.csv("creditsettest.csv")
creditsettestSMO$default10yr <- as.factor(
  creditsettestSMO$default10yr)
creditsettestSMO$predictions <- predict(SVMModel,
                                        creditsettestSMO)
creditsettestSMO

### Ensembles

# construct a set of classifiers
# aggregate predictions made by multiple classifiers
# Wisdom of the Crowd
# types of ensembles - serial and parallel ensembles

# Parallel Ensembles
# - combines approximate independent, diverse base learners

# Bagging

# probability of selection = 1/n
# probability of not being selected = 1-1/n
# probability of not being selected = (1-1/n)^n

# 1/e = OOB (out of the bag)
# 0.632 = ITB (in the bag)

# Boosting

# Wrong predictions are more likely to be selected again

# Adaboost
bankdata <- read.csv('bankdata.csv')
AdaboostModel <- AdaBoostM1(pep ~ age + sex + region + income +
                              married + children + car +
                              save_act + current_act + mortgage,
                            data = bankdata,
                            control = Weka_control(W = list(J48
                                                            )))
adaboostpred <- bankdata
adaboostpred$predictions <- predict(AdaboostModel)
adaboostpred[1:10,]

# Random Forest
adult <- read.csv("adult.csv")
RF <- make_Weka_classifier("weka/classifiers/trees/RandomForest")
RFModel <- RF(income ~ age + workclass + finalweight + 
                education + education.num + marital.status + 
                occupation + relationship + race + sex + 
                capitalgain + capitalloss + hoursperweek + 
                country,
              data = adult, control = Weka_control(K=1))
adult$predictions <- predict(RFModel, adult, se.fit=TRUE)
adult[1:10,]

# Model Evaluation

# Metrics for performance evaluation


# Confusion matrix

# Most widely used metric - accuracy

# Re-substitution errors example
J48Model <- J48(pep ~ age + sex + region + income + married + 
                  children + car + save_act + current_act +
                  mortgage, data = bankdata)
summary(J48Model)

# Generalization errors
sample <- floor(0.67 * nrow(bankdata))
set.seed(123)
train_ind <- sample(seq_len(nrow(bankdata)), size = sample)
bankdatatrain <- bankdata[train_ind,]
bankdatatest <- bankdata[-train_ind,]
J48ModelHoldout <- J48(pep ~ age + sex + region + income + 
                         married + children + car + save_act + 
                         current_act + mortgage, 
                       data = bankdatatrain)
evaluate_Weka_classifier(J48ModelHoldout, newdata=bankdatatest)

# Cross-validation
evaluate_Weka_classifier(J48Model, newdata = bankdata,
                         numFolds = 10)
