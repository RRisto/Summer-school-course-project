#create dtm-s
source("helpers.R")
library(corpustools)
library(RTextTools)
tweetClean=cleanTweet(blacklivesmatterTwitterDf$text)
tweetClean=gsub("nytim", "", tweetClean)
dtmTweets =  create_matrix(tweetClean, 
                           removeStopwords=T, stemWords=T, language='english', 
                           minWordLength =3 )
#wordcloud
dtm.wordcloud(dtmTweets, pal = brewer.pal(6, "Dark2"))
#topic model
ldaTweets=lda.fit(dtmTweets, K=20)
terms(ldaTweets, 10)[,1:5] 
# blacklivesmatterTwitterDf$date=as.Date(blacklivesmatterTwitterDf$created)
# par(mar = rep(2, 4))
# lda.plot.topic(ldaTweets, topic_nr = 1, 
#                time_var=blacklivesmatterTwitterDf$date, 
#                category_var = blacklivesmatterTwitterDf$date,
#                 date_interval='year')
# 
# lda.plot.time(ldaTweets, 1, blacklivesmatterTwitterDf$date,
#               date_interval='week')
# 
# lda.plot.category(ldaTweets, 1, blacklivesmatterTwitterDf$date)

#how many retweets
table(blacklivesmatterTwitterDf$isRetweet)

library(ggmap)
TweetLocations=blacklivesmatterTwitterDf[!is.na(blacklivesmatterTwitterDf$latitude),]
TweetLocations$longitude=as.numeric(as.character(TweetLocations$longitude))
TweetLocations$latitude=as.numeric(as.character(TweetLocations$latitude))
###PLot twitter tweets on a map
#USA
mp <- NULL
mapWorld <- borders("usa", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld

#Now Layer the cities on top
mp <- mp+ geom_point(data=TweetLocations,aes(x=longitude, y=latitude)
                     ,color="red", size=3, alpha=0.5) 
mp

###subset periods
library(RTextTools)
twitter=blacklivesmatterDf
twitter$date=as.Date(twitter$created)
twitter$period=ifelse(twitter$date<"2016-07-05", 1,NA)
twitter$period=ifelse(twitter$date>="2016-07-05"&twitter$date<"2016-07-07", 
                      2,twitter$period)
twitter$period=ifelse(twitter$date>="2016-07-07", 3,twitter$period)
#check it
table(twitter$date, twitter$period)

#clean tweets, vectorized removes some docs!!! use loop
for (i in 1:nrow(twitter)) {
  if(length(cleanTweet(twitter$text[i]))==0) {
    twitter$textClean[i]=NA
  } else {
    twitter$textClean[i]=cleanTweet(twitter$text[i])
  }
}
#remove NAs
twitterClean=twitter[!is.na(twitter$textClean),]
#make dtm
matTw <- create_matrix(twitterClean$text)
matTw
rownames(matTw) <- twitterClean$id
matTw
View(as.matrix(matTw))
#make topic model
mTw = lda.fit(matTw, K=5, alpha=.2)  # K is cut-off, alpha is internal coherence
terms(mTw,20)

library(LDAvis)
json = ldavis_json(mTw, matTw)
serVis(json)
#make dtm 
tpdTw = as.data.frame(posterior(mTw)$topics, na.rm=T)

# merge back to get colnames for new data
tpdTw = merge(twitterClean, tpdTw, by.y="row.names", by.x="id")

head(tpdTw)
table(tpdTw$period)  # in accord with the time-based fractioning, there are increasing numbers
# of posts as the periods avance. The increase is larger from the first to the second period.

tapply(tpdTw$`1`, tpdTw$period, mean)
tapply(tpdTw$`2`, tpdTw$period, mean)
tapply(tpdTw$`3`, tpdTw$period, mean)
tapply(tpdTw$`4`, tpdTw$period, mean)
tapply(tpdTw$`5`, tpdTw$period, mean)

####NYT articles
NYTarticles=readRDS("./data/NYTarticles.RDS")
library(RTextTools)
NYTarticles$date=as.Date(NYTarticles$dates)
NYTarticles$period=ifelse(NYTarticles$date<"2016-07-05", 1,NA)
NYTarticles$period=ifelse(NYTarticles$date>="2016-07-05"&NYTarticles$date<"2016-07-07", 
                      2,NYTarticles$period)
NYTarticles$period=ifelse(NYTarticles$date>="2016-07-07", 3,NYTarticles$period)
#check it
table(NYTarticles$date, NYTarticles$period)
##clean articles, vectorized removes some docs!!! use loop
for (i in 1:nrow(NYTarticles)) {
  if(length(cleanTweet(NYTarticles$bodyTitle[i]))==0) {
    NYTarticles$textClean[i]=NA
  } else {
    NYTarticles$textClean[i]=cleanTweet(NYTarticles$bodyTitle[i])
  }
}
#remove NAs
NYTartClean=NYTarticles[!is.na(NYTarticles$textClean),]
NYTartClean$id=1:nrow(NYTartClean)
#make dtm
matNYTart <- create_matrix(NYTartClean$textClean)
matNYTart
rownames(matNYTart) <- 1:nrow(matNYTart)
matNYTart
View(as.matrix(matNYTart))
#make topic model
mNYTart = lda.fit(matNYTart, K=5, alpha=.2)  # K is cut-off, alpha is internal coherence
terms(mNYTart,20)

library(LDAvis)
jsonNYT = ldavis_json(mNYTart, matNYTart)
serVis(jsonNYT)
#make dtm 
tpdNYTart = as.data.frame(posterior(mNYTart)$topics, na.rm=T)

# merge back to get colnames for new data
tpdNYTart = merge(NYTartClean, tpdNYTart, by.y="row.names", by.x="id")

head(tpdNYTart)
table(tpdNYTart$period)  # in accord with the time-based fractioning, there are increasing numbers
# of posts as the periods avance. The increase is larger from the first to the second period.

tapply(tpdNYTart$`1`, tpdNYTart$period, mean)
tapply(tpdNYTart$`2`, tpdNYTart$period, mean)
tapply(tpdNYTart$`3`, tpdNYTart$period, mean)
tapply(tpdNYTart$`4`, tpdNYTart$period, mean)
tapply(tpdNYTart$`5`, tpdNYTart$period, mean)

