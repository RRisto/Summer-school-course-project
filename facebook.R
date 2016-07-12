#facebook
library(Rfacebook)
fb_token=readRDS("Fbtoken.RDS")
nytPosts = getPage(page="nytimes", token=fb_token,feed=T, n=1000 )

saveRDS(nytPosts, "nytPosts.RDS")


post2=getPost("5281959998_10150844804004999", token=fb_token)
post2$post
likes=post2$likes
comments=post2$comments

#find posts that are related to black lives matter subject
relevantPosts=nytPosts[
  grepl("black|police|shoot|philando|castile|racism", nytPosts$message),]
#loop through relevant posts and collect comments
comments=data.frame(NULL)
post=c()
for(i in 1:nrow(relevantPosts)) {
  post2=getPost(relevantPosts$id[i], token=fb_token)
  comments=rbind(comments, post2$comments)
  print(i)
}
#save comments
saveRDS(comments, "fbComments.RDS")