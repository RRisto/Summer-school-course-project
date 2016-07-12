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
blacklivesmatterTwitterDf=data.frame(NULL)
maksID=NULL
#looping
for(i in 1:30) {
  blacklivesmatter=searchTwitteR('@nytimes AND black OR police OR philando OR castile OR racism OR alton OR sterling OR police -saudi -',
                                 resultType="recent", n = 1500, 
                                 lang = "en", maxID = maksID)
  blacklivesmatterTwitterDf=rbind(blacklivesmatterTwitterDf,
                                  twListToDF(blacklivesmatter))
  maksID=blacklivesmatterTwitterDf$id[nrow(blacklivesmatterTwitterDf)]
  print(i)
}
#save data
saveRDS(blacklivesmatterTwitterDf, "./data/blacklivesmatterTwitter.RDS")
