##### IE 198 Case Study 1: Data Preprocessing

### Part 2: Call Center Datasets

callcenter2013 <- read.csv('2013.csv')
callcenter2014 <- read.csv('2014.csv')
callcenter2015 <- read.csv('2015.csv')

library(dplyr)

# Merge the three datasets vertically
complete <- rbind(callcenter2013, callcenter2014, callcenter2015)

# Group by month and year
permonthyear <- group_by(complete, MONTH, YEAR)
callspermonthyear <- summarise(permonthyear,
                               "Number of Calls"=n())
callspermonthyear

# Group by month
permonth <- group_by(complete, MONTH)
callspermonth <- summarise(permonth,
                           MeanCalls = n()/3)
callspermonth

# Count invalid calls
invalid <- filter(complete, TRANSTYPE == "INVALID")
invalidpertype <- group_by(invalid, TRANS)
invalidpertypesummary <- summarise(invalidpertype,
                                   Count=n())
invalidpertypesummary

# Group by subscriber
persub <- group_by(complete, SUBID)
persubsummary <- summarise(persub,
                           Count=n())
mean(persubsummary$Count)

# Group then arrange by transaction and transaction count
pertransaction <- complete %>% 
  group_by(TRANS) %>%
  summarise(TransactionCount = n()) %>%
  arrange(desc(TransactionCount))
pertransaction

# Group by subscriber and by transaction. Given that time
# does not matter, repeat calls are the number of calls per sub
# per transaction minus 1. Then group by subscriber again to get
# the total number of repeat calls per subscriber. Get the mean
# of the repeat calls per subscriber to get the answer.
repeatcall <- complete %>%
  group_by(SUBID, TRANS) %>%
  summarise(RepeatCalls = n()-1) %>%
  group_by(SUBID) %>%
  summarise(SubRepeatCalls = sum(RepeatCalls))
repeatcall
# sum(repeatcall$RepeatCalls)/nlevels(repeatcall$TRANS)
mean(repeatcall$SubRepeatCalls)
