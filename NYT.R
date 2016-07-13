load("NYTkey.RData")
source("helpers.R")
searchTerms='("black lives matter" "dallas shooting" "racism")'
#searchTerms='("black lives matter")'

NYTarticles=getMetaData(apikey = apikey, nrOfArticles = 2000,
                   fq=searchTerms, beginDate = "20160712", dayStep = 30)
saveRDS(NYTarticles, "./data/NYTarticles.RDS")
#get body
NYTarticlesBody=getArticleBody(articleUrls = NYTarticles$urls)
NYTarticles$body=NYTarticlesBody
saveRDS(NYTarticles, "./data/NYTarticles.RDS")

