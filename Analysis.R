#code for final project, much repetitions and it's so ugly it's 
#got more global variables than the latest report on climate change. 
#But it works (and was made in a hurry)
#other .R files include script for getting data from specific source
source("helpers.R")#my helper functions, make life bit easier
library(corpustools)
library(RTextTools)
#how many retweets
table(blacklivesmatterTwitterDf$isRetweet)
library(ggmap)
TweetLocations=blacklivesmatterDf[!is.na(blacklivesmatterDf$latitude),]
TweetLocations$longitude=as.numeric(as.character(TweetLocations$longitude))
TweetLocations$latitude=as.numeric(as.character(TweetLocations$latitude))
###PLot twitter tweets on a map
#USA
mp <- NULL
# create a layer of borders
mapWorld <- borders("usa", colour="gray50", fill="lightblue", alpha=0.5) 
mp <- ggplot() +   mapWorld

#Now Layer the cities on top
mp <- mp+ theme_minimal()+
  geom_point(data=TweetLocations,aes(x=longitude, y=latitude)
                     ,color="red", size=6, alpha=0.5)+
  xlab("")+
  ylab("")
 
mp
########################Data preparation and topic modelling
############################TWITTER
twitter=blacklivesmatterDf
twitter$date=as.Date(twitter$created)
twitter$period=ifelse(twitter$date<"2016-07-05", 1,NA)
twitter$period=ifelse(twitter$date>="2016-07-05"&twitter$date<"2016-07-07", 
                      2,twitter$period)
twitter$period=ifelse(twitter$date>="2016-07-07", 3,twitter$period)
#check it
table(twitter$date, twitter$period)
#keep original tweets just in case
#twitter_original=twitter
twitter=twitter_original[twitter_original$isRetweet==FALSE,]
#clean tweets, vectorized removes some docs!!! used loop for quick fix
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
mTw = lda.fit(matTw, K=3, alpha=.2)  # K is cut-off, alpha is internal 
#coherence
#terms(mTw,20)
#in RStudio visualisation
# library(LDAvis)
# json = ldavis_json(mTw, matTw)
# serVis(json)

#make separate folder for vis, this could be uploaded to web
mTw %>%
  topicmodels2LDAvis() %>%
  LDAvis::serVis(out.dir = 'Twitter', open.browser = F)
#make dtm 
tpdTw = as.data.frame(posterior(mTw)$topics, na.rm=T)

# merge back to get colnames for new data
tpdTw = merge(twitterClean, tpdTw, by.y="row.names", by.x="id")

#head(tpdTw)
table(tpdTw$period)  # in accord with the time-based fractioning,
#get topic popularity fro each period and each topic
tapply(tpdTw$`1`, tpdTw$period, mean)
tapply(tpdTw$`2`, tpdTw$period, mean)
tapply(tpdTw$`3`, tpdTw$period, mean)

####NYT ARTICLES
NYTarticles=readRDS("./data/NYTarticles.RDS")
library(RTextTools)
NYTarticles$date=as.Date(NYTarticles$dates)
NYTarticles$period=ifelse(NYTarticles$date<"2016-07-05", 1,NA)
NYTarticles$period=ifelse(NYTarticles$date>="2016-07-05"&
                            NYTarticles$date<"2016-07-07", 
                      2,NYTarticles$period)
NYTarticles$period=ifelse(NYTarticles$date>="2016-07-07", 3,
                          NYTarticles$period)
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
#matNYTart
#View(as.matrix(matNYTart))
#make topic model
mNYTart = lda.fit(matNYTart, K=3, alpha=.2)  # K is cut-off, alpha is 
#internal coherence
#terms(mNYTart,20)

# library(LDAvis)
# jsonNYT = ldavis_json(mNYTart, matNYTart)
# serVis(jsonNYT)

mNYTart %>%
  topicmodels2LDAvis() %>%
  LDAvis::serVis(out.dir = 'NYT', open.browser = F)

#make dtm 
# #rearrange topics based on visluaosation, topuic 3 here is other
# #dataset topic 1, topic 1 here , is topic 2 in other datasets
# #and topic 2 ohter dataset topic 3
# tpdNYTart = as.data.frame(posterior(mNYTart)$topics[,c(2,1,3)], na.rm=T)
# names(tpdNYTart)=1:3
# #rearrange, only in NYT!!!
# NyTPOsterior=posterior(mNYTart)[["terms"]][c(2,1,3),]
#make dtm 
tpdNYTart = as.data.frame(posterior(mNYTart)$topics, na.rm=T)

# merge back to get colnames for new data
tpdNYTart = merge(NYTartClean, tpdNYTart, by.y="row.names", by.x="id")

head(tpdNYTart)
table(tpdNYTart$period)  # in accord with the time-based fractioning,

tapply(tpdNYTart$`1`, tpdNYTart$period, mean)
tapply(tpdNYTart$`2`, tpdNYTart$period, mean)
tapply(tpdNYTart$`3`, tpdNYTart$period, mean)

###FACEBOOK COMMENTS
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
#empty strings (fb has messiest data)
#make id for later merging tables
fbClean$id=1:nrow(fbClean)
#make dtm
matFb <- create_matrix(fbClean$textClean, minWordLength = 3)
#extra for fb, because apparently LDa makes some docs with 0 entry
rowTotals <- apply(matFb , 1, sum)#row sums
matFb   <- matFb[rowTotals> 0, ] #keep rows with at least 1 entry
rownames(matFb) <- 1:nrow(matFb)
#matFb
#View(as.matrix(matFb))
#make topic model
mFb= lda.fit(matFb, K=3, alpha=.2)  # K is cut-off, alpha is internal 
#coherence
terms(mFb,10)

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

tapply(tpdFb$`1`, tpdFb$period, mean)
tapply(tpdFb$`2`, tpdFb$period, mean)
tapply(tpdFb$`3`, tpdFb$period, mean)
tapply(tpdFb$`4`, tpdFb$period, mean)
tapply(tpdFb$`5`, tpdFb$period, mean)

##############################plotting topics on timescale
####twitter
tweetsTopicTime=data.frame(list(topic1=c(topic1=tapply(tpdTw$`1`, 
                                                       tpdTw$period, mean),
                           topic2=tapply(tpdTw$`2`, tpdTw$period, mean),
                           topic3=tapply(tpdTw$`3`, tpdTw$period, mean))))
#add time, toopic and dataset vars
tweetsTopicTime$time=rep(c("02-04.07","05-06.07","07-11.07"),3)
tweetsTopicTime$topic=as.factor(c(rep(1,3), rep(2,3), rep(3,3)))
tweetsTopicTime$dataset="Twitter"
####FACEBOOK
fbTopicTime=data.frame(list(topic1=c(topic1=tapply(tpdFb$`1`, 
                                                   tpdFb$period, mean),
                                         topic2=tapply(tpdFb$`2`, 
                                                       tpdFb$period, mean),
                                         topic3=tapply(tpdFb$`3`, 
                                                       tpdFb$period, mean))))

fbTopicTime$time=rep(c("02-04.07","05-06.07","07-11.07"),3)
fbTopicTime$topic=as.factor(c(rep(1,3), rep(2,3), rep(3,3)))
fbTopicTime$dataset="Facebook"

#######NYT articles
NYTTopicTime=data.frame(list(topic1=c(topic1=tapply(tpdNYTart$`1`, 
                                                    tpdNYTart$period, mean),
                                     topic2=tapply(tpdNYTart$`2`, 
                                                   tpdNYTart$period, mean),
                                     topic3=tapply(tpdNYTart$`3`, 
                                                   tpdNYTart$period, mean))))

NYTTopicTime$time=rep(c("02-04.07","05-06.07","07-11.07"),3)
NYTTopicTime$topic=as.factor(c(rep(1,3), rep(2,3), rep(3,3)))
NYTTopicTime$dataset="NY Times"

#plot the results
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

########################calculate topic similarity based on word probs
#use cosine distance for that
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
#do manually
cosintwFb=data.frame(NULL)
cosintwFb[1,1]=topicCosine(mFb, mTw, 1,1)
cosintwFb[1,2]=topicCosine(mFb, mTw, 1,2)
cosintwFb[1,3]=topicCosine(mFb, mTw, 1,3)
cosintwFb[2,1]=topicCosine(mFb, mTw, 2,1)
cosintwFb[2,2]=topicCosine(mFb, mTw, 2,2)
cosintwFb[2,3]=topicCosine(mFb, mTw, 2,3)
cosintwFb[3,1]=topicCosine(mFb, mTw, 3,1)
cosintwFb[3,2]=topicCosine(mFb, mTw, 3,2)
cosintwFb[3,3]=topicCosine(mFb, mTw, 3,3)
colnames(cosintwFb)=c("Twitter tp 1", "Twitter tp 2","Twitter tp 3")
rownames(cosintwFb)=c("Facebook tp 1", "Facebook tp 2","Facebook tp 3")
#make df
cosintwFb2=data.frame(csoinTwNYT[,1][1:3],csoinTwNYT[,2][1:3],
                       csoinTwNYT[,3][1:3])
rownames(cosintwFb2)=c("Facebook tp 1", "Facebook tp 2","Facebook tp 3")
colnames(cosintwFb2)=c("Twitter tp 1", "Twitter tp 2","Twitter tp 3")
#plot it
library(pheatmap)
pheatmap(cosintwFb2, cluster_rows = F, cluster_cols = F, 
         display_numbers = T,color=YlOrBr,fontsize=20,fontsize_number=15,
         number_color="grey50")
#twitter-NYT
csoinTwNYT=data.frame(NULL)
csoinTwNYT[1,1]=topicCosine(mNYTart, mTw, 1,1)
csoinTwNYT[1,2]=topicCosine(mNYTart, mTw, 1,2)
csoinTwNYT[1,3]=topicCosine(mNYTart, mTw, 1,3)
csoinTwNYT[2,1]=topicCosine(mNYTart, mTw, 2,1)
csoinTwNYT[2,2]=topicCosine(mNYTart, mTw, 2,2)
csoinTwNYT[2,3]=topicCosine(mNYTart, mTw, 2,3)
csoinTwNYT[3,1]=topicCosine(mNYTart, mTw, 3,1)
csoinTwNYT[3,2]=topicCosine(mNYTart, mTw, 3,2)
csoinTwNYT[3,3]=topicCosine(mNYTart, mTw, 3,3)
colnames(csoinTwNYT)=c("Twitter tp 1", "Twitter tp 2","Twitter tp 3")
rownames(csoinTwNYT)=c("NYT tp 1", "NYT tp 2","NYT tp 3")

csoinTwNYT2=data.frame(csoinTwNYT[,1][1:3],csoinTwNYT[,2][1:3],
                        csoinTwNYT[,3][1:3])
rownames(csoinTwNYT2)=c("NYT tp 1", "NYT tp 2","NYT tp 3")
colnames(csoinTwNYT2)=c("Twitter tp 1", "Twitter tp 2","Twitter tp 3")
#plot it
pheatmap(csoinTwNYT2, cluster_rows = F, cluster_cols = F, 
         display_numbers = T,color=YlOrBr,fontsize=20,fontsize_number=15,
         number_color="grey50")
##NYT-fb
csoinFbNYT=data.frame(NULL)
csoinFbNYT[1,1]=topicCosine(mNYTart, mFb, 1,1)
csoinFbNYT[1,2]=topicCosine(mNYTart, mFb, 1,2)
csoinFbNYT[1,3]=topicCosine(mNYTart, mFb, 1,3)
csoinFbNYT[2,1]=topicCosine(mNYTart, mFb, 2,1)
csoinFbNYT[2,2]=topicCosine(mNYTart, mFb, 2,2)
csoinFbNYT[2,3]=topicCosine(mNYTart, mFb, 2,3)
csoinFbNYT[3,1]=topicCosine(mNYTart, mFb, 3,1)
csoinFbNYT[3,2]=topicCosine(mNYTart, mTw, 3,2)
csoinFbNYT[3,3]=topicCosine(mNYTart, mFb, 3,3)
colnames(csoinFbNYT)=c("Facebook tp 1", "Facebook tp 2","Facebook tp 3")
rownames(csoinFbNYT)=c("NYT tp 1", "NYT tp 2","NYT tp 3")

cosineFbNYT2=data.frame(csoinFbNYT[,1][1:3],csoinFbNYT[,2][1:3],
                        csoinFbNYT[,3][1:3])
rownames(cosineFbNYT2)=c("NYT tp 1", "NYT tp 2","NYT tp 3")
colnames(cosineFbNYT2)=c("Facebook tp 1", "Facebook tp 2","Facebook tp 3")
#plot it
pheatmap(cosineFbNYT2, cluster_rows = F, cluster_cols = F, 
         display_numbers = T,color=YlOrBr,fontsize=20,fontsize_number=15,
         number_color="grey50")
# topicCosine(mFb, mTw, 3,1)
# topicCosine(mFb, mTw, 3,2)
# topicCosine(mFb, mTw, 3,3)
# topicCosine(mNYTart, mTw, 1,1)
# topicCosine(mNYTart, mTw, 2,2)
# topicCosine(mNYTart, mTw, 3,3)
# 
# pairs=c("Twitter-Fb", "Twitter-NYT", "Fb-NYT") 
# vec=list(mFb, mTw,mNYTart)
# names=c("mFb", "mTw", "mNYTart")
# temp=data.frame(NULL)
# cosinList=list()
# for(i in 1:length(pairs)) {
#   for(j in 1:3) {
#     for(n in 1:3) {
#       temp[i,j]=topicCosine(vec[[i]],vec[[j]], j,n)
#     }
#   }
#   cosinList[[pairs[i]]]=as.data.frame(temp)
# }
# #loop values into a list
# vec=list(mFb, mTw,mNYTart)
# names=c("mFb", "mTw", "mNYTart")
# resultList=list()
# temp=data.frame(NULL)
# for(n in 1:3) {
#   for(i in 1:length(vec)) {
#     for(j in 1:length(vec)) {
#       temp[i,j]=topicCosine(vec[[i]],vec[[j]], n,n)
#     }
#   }
#   resultList[[n]]=as.data.frame(temp)
# }
# #add row and col names
# resultList <- lapply(resultList,function(DF) {rownames(DF) <- names; DF})
# resultList <- lapply(resultList,function(DF) {colnames(DF) <- names; DF})

#make heatmap
# library(pheatmap)
# 
# #colours for heatmap
# YlOrBr <- c('#f7fbff','#deebf7','#c6dbef','#9ecae1','#6baed6',
#             '#4292c6','#2171b5','#08519c','#08306b')
# #got very messy here, needs to be improved
# #topic1
# cosine1=data.frame(resultList[[1]])
# cosineTopic1=data.frame(NULL)
# cosineTopic1=data.frame(cosine1[,1][1:3],cosine1[,2][1:3],cosine1[,3][1:3])
# namesLong=c("Facebook", "Twitter", "NYT")
# rownames(cosineTopic1)=namesLong
# colnames(cosineTopic1)=namesLong
# #plot it
# pheatmap(cosineTopic1, cluster_rows = F, cluster_cols = F, 
#          display_numbers = T,color=YlOrBr,fontsize=20,fontsize_number=15,
#          number_color="grey50")
# #topic2
# cosine2=data.frame(resultList[[2]])
# cosineTopic2=data.frame(NULL)
# cosineTopic2=data.frame(cosine2[,1][1:3],cosine2[,2][1:3],cosine2[,3][1:3])
# namesLong=c("Facebook", "Twitter", "NYT")
# rownames(cosineTopic2)=namesLong
# colnames(cosineTopic2)=namesLong
# #plot it
# pheatmap(cosineTopic2, cluster_rows = F, cluster_cols = F, 
#          display_numbers = T,color=YlOrBr,fontsize=20,fontsize_number=15,
#          number_color="grey50")
# 
# #topic3
# cosine3=data.frame(resultList[[3]])
# cosineTopic3=data.frame(NULL)
# cosineTopic3=data.frame(cosine3[,1][1:3],cosine3[,2][1:3],cosine3[,3][1:3])
# namesLong=c("Facebook", "Twitter", "NYT")
# rownames(cosineTopic3)=namesLong
# colnames(cosineTopic3)=namesLong
# #plot it
# pheatmap(cosineTopic3, cluster_rows = F, cluster_cols = F, 
#          display_numbers = T, color=YlOrBr,fontsize=20,fontsize_number=15,
#          number_color="grey50")

###############compare corporas
cmpFbTW = corpora.compare(matFb,  matTw)
with(arrange(cmpFbTW, -chi)[1:100, ],
     plotWords(x=log(over), words = term, wordfreq = chi, random.y = T))

cmpFbTW = cmpFbTW[order(cmpFbTW$over, decreasing=T), ]
head(cmpFbTW)

cmpFbNYT = corpora.compare(matFb,  matNYTart)
with(arrange(cmpFbNYT, -chi)[1:100, ],
     plotWords(x=log(over), words = term, wordfreq = chi, random.y = T))

cmpTwNYT = corpora.compare(matTw,  matNYTart)
with(arrange(cmpTwNYT, -chi)[1:100, ],
     plotWords(x=log(over), words = term, wordfreq = chi, random.y = T))

###################sentiment analysis
lexicon = readRDS("data/lexicon.rds")

pos_words = lexicon$word1[lexicon$priorpolarity == "positive"]
neg_words = lexicon$word1[lexicon$priorpolarity == "negative"]

library(slam)
twitterClean$npos = row_sums(matTw[, colnames(matTw) %in% pos_words])
twitterClean$nneg = row_sums(matTw[, colnames(matTw) %in% neg_words])
#sentiment score
twitterClean$sent = (twitterClean$npos - twitterClean$nneg) / 
  (twitterClean$npos + twitterClean$nneg)
twitterClean$sent[is.na(twitterClean$sent)] = 0

#NYT
NYTartClean$npos = row_sums(matNYTart[, colnames(matNYTart) %in% pos_words])
NYTartClean$nneg = row_sums(matNYTart[, colnames(matNYTart) %in% neg_words])
#sentiment score
NYTartClean$sent = (NYTartClean$npos - NYTartClean$nneg) / 
  (NYTartClean$npos + NYTartClean$nneg)
NYTartClean$sent[is.na(NYTartClean$sent)] = 0

#facebook
documentsFb=mFb@documents
#add it to original data
fbClean2=fbClean[fbClean$id%in%documentsFb,]

fbClean2$npos = row_sums(matFb[, colnames(matFb) %in% pos_words])
fbClean2$nneg = row_sums(matFb[, colnames(matFb) %in% neg_words])
#sentiment score
fbClean2$sent = (fbClean2$npos - fbClean2$nneg) / 
  (fbClean2$npos + fbClean2$nneg)
fbClean2$sent[is.na(fbClean2$sent)] = 0

##plot sentiment distributions
sentScores=data.frame(sent=c(Facebook=fbClean2$sent, 
                             Twitter=twitterClean$sent,
                      NYT=NYTartClean$sent))
sentScores$dataset=gsub("[[:digit:]]+", "", rownames(sentScores))

library(ggplot2)
#histogram
ggplot(sentScores, aes(x=sent))+
#   geom_histogram(#aes(y=..count../sum(..count..)),
#                  bins =15, fill="lightblue")+
  geom_density(fill="lightblue", color="lightblue")+
  facet_wrap(~dataset, scales="free")+
  theme_minimal()+
  xlab("Sentiment score")+
  ylab("Density")+
  theme(axis.title.y = element_text(colour="grey20",size=15,face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,face="bold"),
        axis.text.y = element_text(colour="grey20",size=12,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=15,face="bold"),
        strip.text.x = element_text(size=12, face="bold"))

#boxplot
ggplot(sentScores, aes(y=sent, x=dataset))+
  geom_boxplot(fill="lightblue")+
  theme_minimal()+
  xlab("Dataset")+
  ylab("Sentiment score")+
  theme(axis.title.y = element_text(colour="grey20",size=15,face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,face="bold"),
        axis.text.y = element_text(colour="grey20",size=12,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=15,face="bold"),
        strip.text.x = element_text(size=12, face="bold"))
##############senitment score per topic in each dataset
##Twitter
#get each doc topic
gammaDFTw <- as.data.frame(mTw@gamma) 
names(gammaDFTw) <- c(1:3)

#assign most prob topic
gammaDFTw$topic=colnames(gammaDFTw)[max.col(gammaDFTw)]
documentsTw=mTw@documents
#add it to original data
twitterClean2=twitterClean[twitterClean$id%in%documentsTw,]
twitterClean2$topic=gammaDFTw$topic

#plot it
ggplot(twitterClean2, aes(x=sent))+
  geom_histogram(bins=7, fill="lightblue")+
  facet_wrap(~topic, scales="free")+
  theme_minimal()

###facebook
#get each doc topic
gammaDFfb <- as.data.frame(mFb@gamma) 
names(gammaDFfb) <- c(1:3)#1:nr of topics

#assign most prob topic
gammaDFfb$topic=colnames(gammaDFfb)[max.col(gammaDFfb)]
#documentsFb=mFb@documents
#add it to original data
#fbClean2=fbClean[fbClean$id%in%documentsFb,]
fbClean2$topic=gammaDFfb$topic

#plot it
ggplot(fbClean2, aes(x=sent))+
  geom_histogram(bins=7, fill="lightblue")+
  facet_wrap(~topic, scales="free")+
  theme_minimal()

###NYT
#get each doc topic
gammaDFNYT <- as.data.frame(mNYTart@gamma) 
names(gammaDFNYT) <- c(1:3)

#assign most prob topic
gammaDFNYT$topic=colnames(gammaDFNYT)[max.col(gammaDFNYT)]
documentsNYT=mNYTart@documents
#add it to original data
NYTClean2=NYTartClean[NYTartClean$id%in%documentsNYT,]
NYTClean2$topic=gammaDFNYT$topic

#plot it
ggplot(NYTClean2, aes(x=sent))+
  geom_histogram(bins=7, fill="lightblue")+
  facet_wrap(~topic, scales="free")+
  theme_minimal()

#make on plot all datasets and all topics sent scores
sentScoresAll=rbind(twitterClean2[, c("sent", "topic")],
                    NYTClean2[, c("sent", "topic")],
                    fbClean2[, c("sent", "topic")])
sentScoresAll$dataset=c(rep("Twitter", nrow(twitterClean2)), 
                        rep("NYT", nrow(NYTClean2)),
                        rep("Facebook", nrow(fbClean2)))

#plot it
ggplot(sentScoresAll, aes(x=sent))+
  #geom_histogram(fill="lightblue", bins=10)+
  geom_density(fill="lightblue", color="lightblue")+
  facet_grid(dataset~topic, scales = "free")+
  theme_minimal()+
  xlab("Sentiment score")+
  ylab("Density")+
  theme(axis.title.y = element_text(colour="grey20",size=15,face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,face="bold"),
        axis.text.y = element_text(colour="grey20",size=12,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=15,face="bold"),
        strip.text.x = element_text(size=12, face="bold"))

#boxplot
ggplot(sentScoresAll, aes(x=topic, y=sent))+
  geom_boxplot(fill="lightblue")+
  facet_wrap(~dataset, scales = "free")+
  theme_minimal()+
  xlab("Topic")+
  ylab("Sentient score")+
  theme(axis.title.y = element_text(colour="grey20",size=15,face="bold"),
        axis.text.x = element_text(colour="grey20",size=12,face="bold"),
        axis.text.y = element_text(colour="grey20",size=12,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=15,face="bold"),
        strip.text.x = element_text(size=12, face="bold"))

####summary tables for rmarkdown
twitterDataSummary=table(twitterClean2$date, twitterClean2$period)
fbDataSummary=table(fbClean2$date, fbClean2$period)
NYTDataSummary=table(NYTartClean$date, NYTartClean$period)

saveRDS(twitterDataSummary, "data/twitterDataSummary.RDS")
saveRDS(fbDataSummary, "data/fbDataSummary.RDS")
saveRDS(NYTDataSummary, "data/NYTDataSummary.RDS")

###terms of each model
termsfb=as.data.frame(terms(mFb,10))
names(termsfb)=c("Police", "People", "Black lives matter")
termsTw=as.data.frame(terms(mTw,10))
names(termsTw)=c("Racism", "Dallas shooting", "Philando shooting")
termsNYT=as.data.frame(terms(mNYTart,10))
names(termsNYT)=c("Foreign policy", "Shooting", "Elections")

saveRDS(termsfb, "data/termsfb.RDS")
saveRDS(termsTw, "data/termsTw.RDS")
saveRDS(termsNYT, "data/termsNYT.RDS")
