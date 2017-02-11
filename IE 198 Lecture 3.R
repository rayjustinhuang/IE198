##### Data Preprocessing (continued)

# data cleaning
# - one of the three biggest problems in data warehousing -Ralph
# Kimball
# tasks: fill in missing values, identify outliers, smooth out
# noisy data, etc.

# missing data may need to be inferred

# handling missing data
# - ignore the tuple: usually done when class label is missing
# - fill in missing value/s manually? tedious, infeasible
# - data imputation: fill it in automatically with: a global 
# constant, the attribute mean, the attribute mean for all 
# samples belonging to the same class

# noisy data - random error or variance in a measured variable
# incorrect attributes: faulty data, data entry, data 
# transmission, tech limitations, 

# handling noisy data: binning, regresssion, clustering, or
# combined computer and human inspection

# outlier identification
# inner fence: Q1 or Q3 +- 1.5*IQR ==> beyond: mild outlier
# outer fence: Q1 or Q3 +- 3*IQR ==> beyond: extreme outlier

# data reduction

# data may not be balanced: e.g. medical data with 9900 negative
# cases and only 100 positive patients

# solved by:
# - upsampling: randomly select tuples from minority class to
#               increase samples (sometimes called bootstrapping)
# - downsampling: randomly select records from majority class to
#                 decrease samples

# aggregation: combine 2 or more attributes/objects into 
# single attributes/objects

# sampling

# curse of dimensionality
# - when dimensionality increases, data becomes increasingly
# sparse in the space that it occupies
# - density becomes smaller

# feature subset selection

# feature creation

# using dplyr
library(dplyr)

# filtering
flights <- read.csv('flights.csv')
filtered <- filter(flights, month == 1, day == 1)
View(filtered)

filtered2 <- filter(flights, month == 1 | month == 7)
View(filtered2)

# slicing
slice(flights, 1:10)

# arranging
sorted <- arrange(flights, year, month, day)
View(sorted)

descsorted <- arrange(flights, desc(arr_delay))
View(descsorted)

# selecting certain columns
selectedcol <- select(flights, year, month, day)
View(selectedcol)

selectedcol2 <- select(flights, -(year:day))
View(selectedcol2)

# distinct rows
distinct <- distinct(select(flights, origin, dest))
View(distinct)

# mutating
delayed <- mutate(flights,
                 gain = arr_delay - dep_delay,
                 speed = distance / air_time * 60,
                 gain_per_hour = gain / (air_time / 60))
View(delayed)

# summarize
meandelay <- summarise(flights,
                      delay = mean(dep_delay, na.rm=TRUE))
View(meandelay)

# grouping
by_tailnum <- group_by(flights, tailnum)
delay <- summarise(by_tailnum,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dist < 2000)
View(delay)

# summarize and aggregate
destinations <- group_by(flights, dest)
destsummary <- summarise(destinations,
                         planes = n_distinct(tailnum),
                         flights = n(),
                         delay = mean(dep_delay, na.rm = TRUE))
View(destsummary)

##### Classification Methodologies

# Zero R - no rule, predict the majority

# One R - one rule, choose rule with smallest total error

# Naive Bayes

