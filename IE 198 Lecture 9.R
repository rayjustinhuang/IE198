##### Unsupervised Learning

# "Fishing expedition"
# Lots of research happening in this area

# In unsupervised learning, you look for patterns in 
# data without making any specific predictions

# In supervised learning, you look for y <- f(x)
# y can either be categorical or numeric

### Association Rule Mining

# Data is in basket format

# Itemset
# - a collection of one or more items
# - k-itemset - an itemset that contains k items

# Support count (sigma)
# - frequency of occurrence of an itemset

# Support
# - fraction of transactions that contain an itemset

# Frequent itemset
# - an itemset whose support is >= a minsup threshold

# Association rule
# - an implication expression of the form X->Y, where
# X and Y are itemsets

# Rule evaluation metrics
# Confidence (c)
# - measures how often items in Y appear in 
# transactions that contain X

# Goal of association rule mining:
# find all rules having
# - support >= minsup threshold
# - confidence >= minconf threshold

# Setting the appropriate minsup threshold
# Using a single minimum support threshold may not
# be appropriate

# Brute-force approach
# - list all possible rules and compute metrics for
# each rule
# - Computationally prohibitive!!!

# Two-step approach
# - frequent itemset generation
# - generate all itemsets whose support >= minsup
# - rule generation
# - still computationally expensive

# Apriori principle
# If an itemset is frequent, then all of its subsets
# must also be frequent
# support of an itemset never exceeds the support of
# its subsets - anti-monotone property of support


# Lift ratio
# - high confidence rules can sometimes be misleading
# because the confidence measure ignores the support

# - the bigger the lift, the better the association
# c(A->B)/s(B)

library(arules)
library(arulesViz)
par(mar=c(2,2,2,2))
groceries <- read.transactions("groceries.csv",
                               format = "basket",
                               sep = ",")
itemFrequencyPlot(groceries, topN=20, type='absolute')

rules <- apriori(groceries, parameter = list(supp = 0.001,
                                             conf = 0.8))
options(digits = 2)
inspect(rules[1:20])
rules <- sort(rules, by='confidence', decreasing = TRUE)
inspect(rules[1:20])

plot(rules[1:20], method='graph', interactive=TRUE, shading = T)

# Sequential Pattern Mining

# Sequential pattern - a frequent subsequence (i.e., a subsequence
# whose support >= minsup)

# In subsequences, order matters but events don't have to be
# consecutive

library(arulesSequences)
SequenceData <- read.csv('hotline.csv', stringsAsFactors = FALSE)
SequenceData$count <- rep(1, nrow(SequenceData))
SequenceData <- subset(SequenceData, select=c("SUBID","TRANSID",
                                              'count','TRANS'))
write.table(SequenceData, 'playdata.txt', sep='\t',
            col.names = F, row.names = F)
Transactions <- read_baskets('playdata.txt', info=c('sequenceID',
                                                    'eventID',
                                                    'size'),
                             sep='\t')
SequenceRules <- cspade(Transactions, parameter=list(support=0.01),
                        control=list(verbose=TRUE))
summary(SequenceRules)
as(SequenceRules, 'data.frame')

# Clustering

# K-Means clustering
# Hierarchical clustering

# Types of clustering methodologies
# Partitional clustering - a divisional data objects into
# non-overlapping subsets (clusters) such that each data object is
# in exactly one subset
# Hierarchical clustering - a set of nested clusters organized as
# a hierarchical tree; can be visualized as a dendogram

# k-means
cars <- read.csv('cars.csv')
rownames(cars) <- cars[,1]
cars <- cars[,c(2:12)]
fit <- kmeans(cars,5)
aggregate(cars, by=list(fit$cluster), FUN=mean)
carswithclusters <- data.frame(cars,fit$cluster)
carswithclusters

# Principal components projection
library(cluster)
clusplot(cars, fit$cluster, color=TRUE, shade=TRUE, labels=2,
         lines=0)

# Hierarchical clustering
# Strengths - any desired number of clusters can be obtained by
# 'cutting the dendogram at the proper level
# - they may correspond to meaningful taxonomies

# Agglomerative algorithm


# Cluster similarity: Ward's method
# Similarity of two clusters is based on the increase in squared
# error

d <- dist(cars, method='euclidean')
fit <- hclust(d, method='ward.D')
plot(fit, main='hierarchical clustering for cars dataset')
groups <- cutree(fit, k=5)
rect.hclust(fit, k=5, border='red')
