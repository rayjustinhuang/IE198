##### IE 198 Case Study 1: Data Preprocessing

### Part 1: Bank Dataset

# Import dataset
bankdata <- read.csv("bankdata.csv")

# Check summary statistics
summary(bankdata)
bankints <- data.frame(bankdata$age, bankdata$income, bankdata$children)
lapply(bankints, sd)
lapply(bankints, var)
lapply(bankints, sum)
lapply(bankints, range)

# Group by PEP, married, and has car
library(dplyr)
PEPmarriedcar <- group_by(bankdata, pep, married, car)
PEPmarriedcarsummary <- summarise(PEPmarriedcar,
                                  MeanAge = mean(age),
                                  MeanIncome = mean(income),
                                  MeanKids = mean(children))

# Generate a histogram for the income variable
library(ggplot2)
incomehistogram <- ggplot(data = bankdata, aes(x=income)) +
  geom_bar(stat='bin', fill='limegreen', color='black') +
  xlab('Income') + ylab('Frequency') + ggtitle('Histogram of Income')
incomehistogram

# Generate using R's hist() function
hist(bankdata$income, ylab = 'Frequency', xlab = 'Income', col = 'limegreen', main = 'Histogram of Income')

# Generate boxplot of income variable by PEP
incomeboxplot <- ggplot(data = bankdata, aes(x=pep, y=income)) +
  geom_boxplot(aes(fill=pep)) +
  theme(legend.title=element_blank()) +
  xlab('PEP Purchaser') + ylab('Income') +
  ggtitle('Boxplot of Income vs. PEP')
incomeboxplot

# Generate boxplot of income by region
incomebyregion <- ggplot(data = bankdata, aes(x=region, y=income)) +
  geom_boxplot(aes(fill=region)) +
  theme(legend.title=element_blank()) +
  xlab('Region') + ylab('Income') +
  ggtitle('Boxplot of Income vs. Region')
incomebyregion

# Generate scatterplot matrix of income, age, and number of children
# incomeagechildren <- ggplot(data=bankdata, aes(x=age,y=income)) +
#   geom_point(aes(color=factor(children))) +
#   xlab('Age') + ylab('Income') + labs(color='Children') +
#   ggtitle('Income vs. Age and Number of Children')
# incomeagechildren

# Generate scatterplot matrix of income, age, and number of children
incomeagechildren <- bankdata[,c('income','age','children')]
pairs(incomeagechildren)

ggscatmat(data=bankdata, columns=c('income','age','children'))
