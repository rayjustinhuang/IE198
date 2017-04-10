##### Regression (continued)

# Extra Sum of Squares (cont.)
# - Coefficient of multiple determination
# - Residual mean square
# - AIC (Akaike Information Criterion)

# Standard deviation - on average, a point is sigma (units) away
# from the mean

# Computational Techniques  for Variable Selection
# - All possible regressions

# - Forward selection regression
# Idea: no variables are in themodel originally, but are added one
# at a time. Selection procedure:
# 1. First regressor selected is that with the highest correlation
#    with the response. If the AIC of the model decreases,
#    the regressor stays.
# 2. The second regressor examined is the onew with the largest 
#    partial correlation with the response. If the AIC of the
#    model decreases, the regressor stays.
# 3. This process continues until all regressors are examined.

carbasefit <- lm(mpg ~ 1, data = cardata)
forward <- step(carbasefit, scope = list(lower=~1, upper=~cyl+
                                           disp+hp+drat+wt+
                                           qsec+vs+am+gear+carb,
                                         direction = "forward",
                                         trace=1))

# Backward elimination
# All variables are in the model originally, examined one at a
# time, and removed if not significant
# 1. Partial F statistic is calculated per variable as if it were
#    the last one added to the model. The regressor with the 
#    smallest F statistic is examined first and will be removed if
#    AIC goes down.
# 2. If this regressor is removed, then the model is refit with
#    the remaining regressor variables and the partial F statistic
#    is calculated again. The regressor with the smallest partial
#    F statistic will be removed if the AIC goes down.
# 3. The process continues until AIC can no longer decrease.

carfullfit <- lm(mpg~., data = cardata)
backward <- step(carefullfit, scope = list(lower=~1, upper=~cyl+
                                             disp+hp+drat+wt+
                                             qsec+vs+am+gear+carb,
                                           direction = "backward",
                                           trace=1))


# F statistic = (t statistic)^2
# If X is normally distributed, X^2 is chi-square distributed

# Stepwise regression
# Modification of forward selection
# 1. The contribution of each regressor variable that is put into
#    the model is reassessed by means of its partial F statistic.

Stepwise <- step(carbasefit, scope = list(lower=~1, upper=~cyl+
                                             disp+hp+drat+wt+
                                             qsec+vs+am+gear+carb,
                                           direction = "both",
                                           trace=1))

carfinalfit <- lm(mpg~wt+cyl+hp, data = cardata)
summary(carfinalfit)

# F-statistic is sometimes called the signal-to-noise ratio

# Cautions
# - No one model may be the best
# - The three stepwise techniques could result in different
#   models
# - Greedy algorithm is used
# - Inexperienced analysts may use the final model simply
#   because the procedure spit it out
# - Needs lots of common sense

# standardized regression coefficients

options(scipen = 100)
scardata <- data.frame(scale(cardata, center = TRUE, scale = TRUE))
scarfinalfit <- lm(mpg~., data = scardata)
summary(scarfinalfit)

# Indicator variables

toollife <- read.csv("toollife.csv")
toollifefit <- lm(hours~rpm+tooltype, data = toollife)
summary(toollifefit)

toollifefit <- lm(hours ~ rpm+tooltype+rpm*tooltype, data = 
                    toollife)
summary(toollifefit)

# Multicollinearity

# VIF
library(car)
wgmdata <- read.csv("wgmdata.csv")
wgmdatafit <- lm(y~., data = wgmdata)
summary(wgmdatafit)
vif(wgmdatafit)

wgmdatafit <- lm(y~x1+x2+x3+x5+x6, data = wgmdata)
summary(wgmdatafit)
vif(wgmdatafit)

# Eigenvalues
# Kappa < 100, no serious problem
# 100 < Kappa < 1000, moderate to strong multicollinearity
# 1000 < Kappa, strong multicollinearity

xtransx <- cbind(subset(wgmdata, select = c(2:7)))
r <- cov(xtransx)
eigen(r)

# Methods for dealing with multicollinearity
# - Collect more data
# - Respecify the model
# - Ridge regression

# Logistic regression
# Predicts the probability of an outcome that can only have two values

# Probability of menarche
library("MASS")
menarchedata <- read.csv("menarchedata.csv")
menarchedata.fit <- glm(cbind(menarche, total-menarche) ~ age,
                        family=binomial(logit), data = menarchedata)
summary(menarchedata.fit)
plot(menarche/total ~ age, data = menarchedata)
lines(menarchedata$age, menarchedata.fit$fitted, type = "l",
      col = "red")

# Probability of defaulting
creditdata <- read.csv("creditsetnumeric.csv")
creditdata.fit <- glm(default10yr ~ income + age + loan +
                        LTI, family=binomial(logit),
                      data = creditdata)
summary(creditdata.fit)

