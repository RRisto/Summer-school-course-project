#tweets from twitter
library(twitteR)
library(SocialMediaLab)

#set up autentication
load("authTokens.RData")
AuthenticateWithTwitterAPI(api_key=consumer_key, 
                           api_secret=consumer_secret,
                           access_token=access_token, 
                           access_token_secret=access_secret)
#variables where to store data
blacklivesmatter=c()
blacklivesmatterTwitterDf2=data.frame(NULL)
maksID=NULL
searchTermTw='@nytimes AND black OR police OR philando OR castile OR racism OR alton OR sterling OR police -saudi -'
#looping
for(i in 1:30) {
  blacklivesmatter2=searchTwitteR(searchTermTw,
                                 resultType="recent", n = 1500, 
                                 lang = "en", maxID = maksID)
  blacklivesmatterTwitterDf2=rbind(blacklivesmatterTwitterDf2,
                                  twListToDF(blacklivesmatter))
  maksID=blacklivesmatterTwitterDf2$id[nrow(blacklivesmatterTwitterDf2)]
  print(i)
}
#save data
saveRDS(blacklivesmatterTwitterDf, "./data/blacklivesmatterTwitter.RDS")
