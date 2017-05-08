##### Case 4 - Web Mining

download.file(url='http://curl.haxx.se/ca/cacert.pem', destfile="cacert.pem")

library(twitteR)
library(wordcloud)
library(tm)
library(plyr)
library(stringr)

consumer_key = 'O8zHuCez3sKue6zD2AeGClREe'
consumer_secret = 'Hbdx933zDSgMsED2zFG6mJomM0OWCSxhWpLLTwKaHlzGpr7dGu'
access_token = '254104231-AVLEduIxWzSMME9hV9hRVJfoXN72fEGaMHiI72Nh'
access_secret = 'Bj131yz4DZthCsx4jg9DdOQRoQMHmfeV0OLN2vgOvtX0s'

setup_twitter_oauth(consumer_key, consumer_secret, access_token,
                    access_secret)

#### iPhone 6 vs Galaxy S6

iphone.tweets = searchTwitter('#iphone6', lang='en', n=1500)
galaxys6.tweets = searchTwitter('#SamsungGalaxyS6', lang='en', n=1500)

iphone.text = laply(iphone.tweets, function(t) t$getText())
galaxys6.text = laply(galaxys6.tweets, function(t) t$getText())

iphone = str_replace_all(iphone.text,'[^[:graph:]]', ' ')
galaxys6 = str_replace_all(galaxys6.text,'[^[:graph:]]', ' ')

iphone.scores = score.sentiment(iphone, pos.words, neg.words,.progress = 'text')
galaxys6.scores = score.sentiment(galaxys6, pos.words, neg.words,.progress = 'text')

# Histogram of scores
par(mfrow = c(1,2), mar=c(2,2,2,2))
hist(iphone.scores$score)
hist(galaxys6.scores$score)

# Mean scores
AvgiPhoneScore = mean(iphone.scores$score)
Avggalaxys6Score = mean(galaxys6.scores$score)

# Word clouds
set.seed(4363)
par(mfrow = c(1,2), mar=c(2,2,2,2))
wordcloud(iphone, max.words = 30)
wordcloud(galaxys6, max.words = 30)

#### Superman vs Batman
superman.tweets = searchTwitter('#Superman', lang='en', n=2000)
batman.tweets = searchTwitter('#Batman', lang='en', n=2000)

superman.text = laply(superman.tweets, function(t) t$getText())
batman.text = laply(batman.tweets, function(t) t$getText())

superman = str_replace_all(superman.text, '[^[:graph:]]', ' ')
batman = str_replace_all(batman.text, '[^[:graph:]]', ' ')

superman.scores = score.sentiment(superman, pos.words, neg.words,.progress = 'text')
batman.scores = score.sentiment(batman, pos.words, neg.words,.progress = 'text')

hist(superman.scores$score)
hist(batman.scores$score)

avgsupermanscore = mean(superman.scores$score)
avgbatmanscore = mean(batman.scores$score)

library(RColorBrewer)
# supermanpalette <- brewer.pal(5, 'RdBu')
# batmanpalette <- brewer.pal(5, 'PuOr')

supermanpalette <- c('firebrick1', 'firebrick2', 'firebrick3', 'firebrick4')
batmanpalette <- c('dodgerblue1', 'dodgerblue2', 'dodgerblue3', 'dodgerblue4')

wordcloud(superman, max.words = 47, random.order = F, colors = supermanpalette)
wordcloud(batman, max.words = 47, random.order = F, colors = batmanpalette)

### DC vs Marvel
dc.tweets = searchTwitter('#DCComics', lang='en', n=2000)
marvel.tweets = searchTwitter('#Marvel', lang='en', n=2000)

dc.text = laply(dc.tweets, function(t) t$getText())
marvel.text = laply(marvel.tweets, function(t) t$getText())

dc = str_replace_all(dc.text, '[^[:graph:]]', ' ')
marvel = str_replace_all(marvel.text, '[^[:graph:]]', ' ')

dc.scores = score.sentiment(dc, pos.words, neg.words,.progress = 'text')
marvel.scores = score.sentiment(marvel, pos.words, neg.words,.progress = 'text')

hist(dc.scores$score)
hist(marvel.scores$score)

avgdcscore = mean(dc.scores$score)
avgmarvelscore = mean(marvel.scores$score)

dcpalette <- brewer.pal(5, 'Blues')
marvelpalette <- brewer.pal(5, 'Reds')

wordcloud(dc, max.words = 74, random.order = F, colors = batmanpalette)
wordcloud(marvel, max.words = 74, random.order = F, colors = supermanpalette)
