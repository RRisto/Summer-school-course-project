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
TweetLocations=blacklivesmatterDf[!is.na(blacklivesmatterDf$latitude),]
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
#keep original, tweets
#twitter_original=twitter
twitter=twitter_original[twitter_original$isRetweet==FALSE,]
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
#remove retweets
#twitterClean_original=twitterClean
twitterClean=twitterClean[twitterClean$isRetweet==FALSE,]
#make dtm
matTw <- create_matrix(twitterClean$textClean)
#matTw
rownames(matTw) <- twitterClean$id
#matTw
#View(as.matrix(matTw))
#make topic model
mTw = lda.fit(matTw, K=3, alpha=.2)  # K is cut-off, alpha is internal coherence
#terms(mTw,20)

library(LDAvis)
json = ldavis_json(mTw, matTw)
serVis(json)

#make separate folder for vis, this could be uploaded
mTw %>%
  topicmodels2LDAvis() %>%
  LDAvis::serVis(out.dir = 'Twitter', open.browser = F)
#make dtm 
tpdTw = as.data.frame(posterior(mTw)$topics, na.rm=T)

# merge back to get colnames for new data
tpdTw = merge(twitterClean, tpdTw, by.y="row.names", by.x="id")

#head(tpdTw)
table(tpdTw$period)  # in accord with the time-based fractioning, there are increasing numbers
# of posts as the periods avance. The increase is larger from the first to the second period.

tapply(tpdTw$`1`, tpdTw$period, mean)
tapply(tpdTw$`2`, tpdTw$period, mean)
tapply(tpdTw$`3`, tpdTw$period, mean)
# tapply(tpdTw$`4`, tpdTw$period, mean)
# tapply(tpdTw$`5`, tpdTw$period, mean)

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
#keep articles from same period as twitter
#NYTarticles_original=NYTarticles
NYTarticles=NYTarticles_original[NYTarticles_original$date>="2016-07-02",]
sum(!is.na(NYTarticles$body))
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
#matNYTart
rownames(matNYTart) <- 1:nrow(matNYTart)
matNYTart
#View(as.matrix(matNYTart))
#make topic model
mNYTart = lda.fit(matNYTart, K=3, alpha=.2)  # K is cut-off, alpha is internal coherence
terms(mNYTart,20)

library(LDAvis)
jsonNYT = ldavis_json(mNYTart, matNYTart)
serVis(jsonNYT)

mNYTart %>%
  topicmodels2LDAvis() %>%
  LDAvis::serVis(out.dir = 'NYT', open.browser = F)

#make dtm 
tpdNYTart = as.data.frame(posterior(mNYTart)$topics, na.rm=T)

# merge back to get colnames for new data
tpdNYTart = merge(NYTartClean, tpdNYTart, by.y="row.names", by.x="id")

head(tpdNYTart)
table(tpdNYTart$period)  # in accord with the time-based fractioning,
#there are increasing numbers
# of posts as the periods avance. The increase is larger from the first
#to the second period.

tapply(tpdNYTart$`1`, tpdNYTart$period, mean)
tapply(tpdNYTart$`2`, tpdNYTart$period, mean)
tapply(tpdNYTart$`3`, tpdNYTart$period, mean)
tapply(tpdNYTart$`4`, tpdNYTart$period, mean)
tapply(tpdNYTart$`5`, tpdNYTart$period, mean)

###facebook comments
fb=readRDS("./data/fbComments.RDS")
fb$date=as.Date(fb$created_time)
fb$period=ifelse(fb$date<"2016-07-05", 1,NA)
fb$period=ifelse(fb$date>="2016-07-05"&fb$date<"2016-07-07", 
                      2,fb$period)
fb$period=ifelse(fb$date>="2016-07-07", 3,fb$period)

#check it
table(fb$date, fb$period)
#keep same period
#fb_original=fb
fb=fb[fb_original$date>="2016-07-02",]
#clean messages, vectorized removes some docs!!! use loop
for (i in 1:nrow(fb)) {
  if(length(cleanTweet(fb$message[i]))==0) {
    fb$textClean[i]=NA
  } else {
    fb$textClean[i]=cleanTweet(fb$message[i])
  }
}

#remove NAs
fbClean=fb[!is.na(fb$textClean),]
fbClean = fbClean[grepl("\\w{3,}", fbClean$textClean), ]#removes 
#empty strings

fbClean$id=1:nrow(fbClean)
#make dtm
matFb <- create_matrix(fbClean$textClean, minWordLength = 3,
                       removeStopwords = T)
#extra for fb, because apparently LDa makes soe docs with 0 entry
rowTotals <- apply(matFb , 1, sum)#row sums
matFb   <- matFb[rowTotals> 0, ] #keep rows with at least 1 entry
rownames(matFb) <- 1:nrow(matFb)
#matFb
#View(as.matrix(matFb))
#make topic model
mFb= lda.fit(matFb, K=3, alpha=.2)  # K is cut-off, alpha is internal 
#coherence
terms(mFb,20)

library(LDAvis)
jsonFb = ldavis_json(mFb, matFb)
serVis(jsonFb)

#make separate folder for vis, this could be uploaded
mFb %>%
  topicmodels2LDAvis() %>%
  LDAvis::serVis(out.dir = 'Facebook', open.browser = F)
#make dtm 
tpdFb = as.data.frame(posterior(mFb)$topics, na.rm=T)

# merge back to get colnames for new data
tpdFb = merge(fbClean, tpdFb, by.y="row.names", by.x="id")

head(tpdFb)
table(tpdFb$period)  # in accord with the time-based fractioning, 
#there are increasing numbers
# of posts as the periods avance. The increase is larger from 
#the first to the second period.

tapply(tpdFb$`1`, tpdFb$period, mean)
tapply(tpdFb$`2`, tpdFb$period, mean)
tapply(tpdFb$`3`, tpdFb$period, mean)
tapply(tpdFb$`4`, tpdFb$period, mean)
tapply(tpdFb$`5`, tpdFb$period, mean)


####plotting on timescale
tweetsTopicTime=data.frame(list(topic1=c(topic1=tapply(tpdTw$`1`, tpdTw$period, mean),
                           topic2=tapply(tpdTw$`2`, tpdTw$period, mean),
                           topic3=tapply(tpdTw$`3`, tpdTw$period, mean))))

tweetsTopicTime$time=rep(c("02-04.07","05-06.07","07-11.07"),3)
tweetsTopicTime$topic=as.factor(c(rep(1,3), rep(2,3), rep(3,3)))
tweetsTopicTime$dataset="Twitter"
#fb
fbTopicTime=data.frame(list(topic1=c(topic1=tapply(tpdFb$`1`, tpdFb$period, mean),
                                         topic2=tapply(tpdFb$`2`, tpdFb$period, mean),
                                         topic3=tapply(tpdFb$`3`, tpdFb$period, mean))))

fbTopicTime$time=rep(c("02-04.07","05-06.07","07-11.07"),3)
fbTopicTime$topic=as.factor(c(rep(1,3), rep(2,3), rep(3,3)))
fbTopicTime$dataset="Facebook"

#NYT
NYTTopicTime=data.frame(list(topic1=c(topic1=tapply(tpdNYTart$`1`, tpdNYTart$period, mean),
                                     topic2=tapply(tpdNYTart$`2`, tpdNYTart$period, mean),
                                     topic3=tapply(tpdNYTart$`3`, tpdNYTart$period, mean))))

NYTTopicTime$time=rep(c("02-04.07","05-06.07","07-11.07"),3)
NYTTopicTime$topic=as.factor(c(rep(1,3), rep(2,3), rep(3,3)))
NYTTopicTime$dataset="NY Times"

#plot it
library(ggplot2)
ggplot(tweetsTopicTime, aes(x=factor(time), y=topic1, group=topic,
                            colour=topic))+
  geom_line()+
  theme_minimal()

ggplot(NYTTopicTime, aes(x=factor(time), y=topic1, group=topic,
                            colour=topic))+
  geom_line()+
  theme_minimal()

ggplot(fbTopicTime, aes(x=factor(time), y=topic1, group=topic,
                         colour=topic))+
  geom_line()+
  theme_minimal()

#as one plot
topicTime=rbind(tweetsTopicTime,fbTopicTime, NYTTopicTime)

ggplot(topicTime, aes(x=factor(time), y=topic1, group=topic,
                        colour=topic))+
  geom_line(size=1)+
  facet_wrap(~dataset)+
  theme_minimal()+
  xlab("time period")+
  ylab("topic popularity")+
  theme(axis.title.y = element_text(colour="grey20",size=15,face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,face="bold"),
        axis.text.y = element_text(colour="grey20",size=12,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=15,face="bold"),
        strip.text.x = element_text(size=12, face="bold"))
  
  
###network of word dist over topics
wordNet(mFb,faktor = 0.02)
wordNet(mTw,faktor = 0.02)
wordNet(mNYTart, faktor = 0.02)

##calculate topic similarity based on word probs
Fbtopic1=topicmodels::posterior(mFb)[["terms"]][1,]
twtopic1=topicmodels::posterior(mTw)[["terms"]][1,]

myList <- list(Fbtopic1, twtopic1)
#create matrix of two vectors
proov=do.call(rbind, lapply(lapply(myList, unlist), "[",
                      unique(unlist(c(sapply(myList,names))))))
#replace NAs with 0s
proov[is.na(proov)] <- 0
#cosine distance
library(lsa)
cosine(proov[1,], proov[2,])

topicCosine(mFb, mTw, 1,1)
topicCosine(mFb, mTw, 2,2)
topicCosine(mFb, mTw, 3,3)
topicCosine(mNYTart, mTw, 1,1)
topicCosine(mNYTart, mTw, 2,2)
topicCosine(mNYTart, mTw, 3,3)

vec=list(mFb, mTw,mNYTart)
result=data.frame(NULL)
temp=data.frame(NULL)
for(n in 1:3) {
  for(i in 1:length(vec)) {
    for(j in 1:length(vec)) {
      result[i,j]=topicCosine(vec[[i]],vec[[j]], n,n)
    }
  }
  }

vec=list(mFb, mTw,mNYTart)
names=c("mFb", "mTw", "mNYTart")
resultList=list()
temp=data.frame(NULL)
for(n in 1:3) {
  for(i in 1:length(vec)) {
    for(j in 1:length(vec)) {
      temp[i,j]=topicCosine(vec[[i]],vec[[j]], n,n)
    }
  }
  resultList[[n]]=data.frame(temp)
}

resultList <- lapply(resultList,function(DF) {rownames(DF) <- names; DF})
resultList <- lapply(resultList,function(DF) {colnames(DF) <- names; DF})
