# Case Study 3 - Regression Modeling

# Import data
full <- read.csv("tvdataset.csv")
summary(tvdata)
tvdata <- subset(full, select = -c(X, name))
namereference <- subset(full, select = c(X, name))
str(tvdata)

# Reorder (reverse) levels for easier interpretation of coefficients
tvdata$type <- factor(tvdata$type, levels=rev(levels(tvdata$type)))

# Fit linear regression
lm <- lm(cost ~ ., data = tvdata)
summary(lm)

# Create regression plots
par(mfrow=c(2,2), mar=c(2,2,2,2))
plot(lm)

# Detect outliers
plot(hatvalues(lm))
abline(h = 10/69, col="red")
plot(rownames(tvdata), rstudent(lm))
abline(h=3, col='red')
rstudent(lm)

# Remove outliers
tvdata_no <- tvdata[-c(4,11,28,53),]

# Stepwise regression
basemodel <- lm(cost ~ 1, data = tvdata_no)
stepwisem <- step(basemodel, scope = list(lower = ~ 1, upper = ~network+day+length+viewers+d1849rating+facebooklikes+facebooktalkingabout+twitter+age+type, direction = 'both', trace = 1))

# Reduce and standardize dataset
reduced <- subset(tvdata, select = c(cost, twitter, viewers, day, network, length, d1849rating, facebooklikes, facebooktalkingabout))
options(scipen = 100)
stdreduced <- data.frame(scale(reduced[, c("cost", "twitter", "viewers", "length", "d1849rating", "facebooklikes", "facebooktalkingabout")], center = TRUE, scale = TRUE))
stdreduced$day <- reduced$day
stdreduced$network <- reduced$network

# Fit final model
finallm <- lm(cost ~ ., data = stdreduced)
summary(finallm)
