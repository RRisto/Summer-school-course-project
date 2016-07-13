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