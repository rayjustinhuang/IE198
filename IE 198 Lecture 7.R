##### Regression (continued)

View(deliverytime)

# DelTime = B0 + B1*Ncases + B2*Distance

# Parameters (B's) are unknowns

deliverytime <- read.csv("deliverytime.csv")

lrfit <- lm(deltime ~ ncases + distance, data = deliverytime)

summary(lrfit)

par(mfrow = c(2,2), mar = c(2,2,2,2))

plot(lrfit)

# "If p is low, let it go (reject the null)" -Prof. Erickson Laguno

# Adjusted R^2 penalizes added terms that are not significant

# Scatter diagrams may not be adequate in detecting 
# interdependencies between regressor variables

# Model Adequacy Checking
# - Error term (sum of errors) has 0 mean
# - Error term has constant variance
# - Errors are uncorrelated
# - Errors are normally distributed (required for tests and 
#   intervals)

# Utilize residual plots to detect violations

# Normal probability plot of residuals - checks normality
# assumption
# Residuals against fitted values - checks for nonconstant 
# variance, nonlinearity, potential outliers
# Residuals versus order - checks for autocorrelation

# If residuals vs. fitted shows anomalies, use variance-
# stabilizing transformations, e.g.:

slrfit <- lm(deltime^0.5 ~ ncases + distance, data = deliverytime)
plot(slrfit)

# Predicting using the original model:

deliverytimenewdata <- read.csv("deliverytimendata.csv")

predict(lrfit, deliverytimenewdata, interval = "confidence")

# The further away your new data is from the original data, 
# i.e. the original range of your data, the less accurate
# your prediction is

# 95% C.I.: If you run the experiment 100 times, 95 of the means
# of the results will fall within this interval

# Detecting outliers

# Leverage point - unusual x-value, very little effect on
# regression coefficients, little effect on slope (fits the line)

# Influence point - unusual in both x and y

# Outlier = a leverage point that is also an influence point

# The leverage statistic
# If a given observation has a leverage statistic that greatly
# exceeds (p+1)/n, it is an outlier

# Cutoff = (p+1)/n = 4/25 = 0.16
plot(hatvalues(lrfit))
abline(h = 4/25, col = "red")

# Studentized residuals
# Big hi and big error => outlier

# +- 3 standard deviations is the standard cutoff
plot(rownames(deliverytime), rstudent(lrfit))
abline(h = 3, col="red")
rstudent(lrfit)

# Selecting variables/features to include
cardata <- read.csv("cars.csv")
rownames(cardata) <- cardata[,1]
cardata <- cardata[, c(2:12)]
mpglrfit <- lm(mpg~., data = cardata)
summary(mpglrfit)

# Extra sum of squares
