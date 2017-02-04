##### R Basics

# set the working directory
setwd("~/UP Files/IE 198/IE 198 Work")

# see documentation on functions
help(solve)
?solve

# this comment is not a hashtag
# expression
2 + 3
# assignment
x <- 2 + 3
# print assignment
x

# this is a group of commands
{
  y <- 2 + 3
  z <- 2 + 4
}
# this is an incomplete command
x <- 1 +
  2

# loading packages
library(reshape2)

# dataframes
x <- c(10.4, 5.6, 3.1, 6.4, 21.7)
y <- c(11.4, 6.6, 4.1, 7.4, 22.7)
z <- data.frame(x,y)
z

# read a CSV file
deliverytime <- read.csv(file="deliverytime.csv", header = TRUE, 
                         sep = ",")
deliverytime

# read from Excel
library(xlsx)
deliveryxlsx <- read.xlsx("deliverytime.xlsx")

##### Univariate Analysis

# display summary statistics using both pastecs and R's built-in
# summary function
library(pastecs)
options(scipen = 100, digits = 2)
stat.desc(deliverytime)
summary(deliverytime)

# create histograms of the delivery time data
hist(deliverytime$deltime, ylab = "Minutes", col = 'green')
hist(deliverytime$ncases, ylab = "Cases", col = 'green')
hist(deliverytime$distance, main = "Histogram for Distance",
     ylab = "Feet", col = 'green')

# create boxplots
boxplot(deliverytime$deltime, ylab = "Minutes", col = 'green')
boxplot(deliverytime$ncases, ylab = "Cases", col = 'green')
boxplot(deliverytime$distance, main = "Boxplot for Distance",
     ylab = "Feet", col = 'green')

##### Bivariate Analysis

# scatterplot array
pairs(deliverytime)

# correlation (manual calculation)
sdeliverytime <- deliverytime
sdeliverytime$deltime <- 
  (deliverytime$deltime-mean(deliverytime$deltime))/
  sd(deliverytime$deltime)
sdeliverytime$ncases <- 
  (deliverytime$ncases-mean(deliverytime$ncases))/
  sd(deliverytime$ncases)
sdeliverytime$distance <- 
  (deliverytime$distance-mean(deliverytime$distance))/
  sd(deliverytime$distance)
correl <- 
  sum(sdeliverytime$deltime*sdeliverytime$ncases)/
  (nrow(sdeliverytime)-1)
correl

# correlation (built-in function)
cor(deliverytime$ncases, deliverytime$deltime)

# important note: the correlation statistic only measures 
#*linear* relationships; there may still be quadratic, 
# cubic, etc. (higher-order) relationships present

# Euclidean distance (manual)
NDelTime <- (20-mean(deliverytime$deltime))/
  sd(deliverytime$deltime)
NCases <- (3-mean(deliverytime$ncases))/
  sd(deliverytime$ncases)
Distances <- ((sdeliverytime$deltime-NDelTime)^2 +
  (sdeliverytime$ncases-NCases)^2)^0.5
which.max(Distances)
which.min(Distances)

# stacked column chart
# combination chart

# reshaping and melting
# melt - transforming multiple measures/columns into a single 
#        column/variable
# cast - summarization of a molten dataset into aggregated data
library(reshape2)
heisenberg <- read.csv("simple.csv")
heisenberg.m <- melt(heisenberg, id = c('trial'),
                     measure = c('mass', 'velocity'))
heisenberg.m
heisenberg.c <- dcast(heisenberg.m, trial ~ variable, mean)
heisenberg.c

##### Visualization

# to be taken up in the future

##### Data Preprocessing

# why? data in the real world is dirty
# dirty: incomplete, noisy, inconsistent
# Sir Rex: 70% to 80% of time spent on preprocessing data
# garbage in, garbage out

# major tasks in data preprocessing:
# - data integration
# - data transformation
# - data cleaning
# - data reduction
# - data discretization

# merging
# first, create new trialcost dataframe to merge with heisenberg
trial <- c("A", "C", "D")
cost <- c(11.4, 3.3, 1.1)
trialcost <- data.frame(trial, cost)
trialcost
# do the actual merge (inner, outer, left, and right joins)
innerjoin <- merge(x = heisenberg, y = trialcost, 
                   by = c("trial"))
outerjoin <- merge(x = heisenberg, y = trialcost, 
                   by = c("trial"), all = TRUE)
leftjoin <- merge(x = heisenberg,  y = trialcost,  
                  by = c("trial"),  all.x = TRUE)
rightjoin <- merge(x = heisenberg,  y = trialcost,  
                   by = c("trial"),  all.y = TRUE)

# data transformations
# data discretization and data encoding

# min-max normalization

# (z-score) standardization -> standard normal dist

# binning
# equal-width (distance) partitioning - equal width bins,
#     best for normally or evenly distributed data
# equal-depth (frequency) partitioning - equal cardinality bins

# data encoding
# binary or class based