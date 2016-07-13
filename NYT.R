load("NYTkey.RData")
source("helpers.R")
searchTerms='("black lives matter" "dallas shooting" "racism" "philando" "castile" "alton" "sterling")'
#searchTerms='("black lives matter")'

NYTarticles=getMetaData(apikey = apikey, nrOfArticles = 2000,
                   fq=searchTerms, beginDate = "20160712")
saveRDS(NYTarticles, "./data/NYTarticles.RDS")
#get body
NYTarticlesBody=getArticleBody(articleUrls = NYTarticles$urls)
NYTarticles$body=NYTarticlesBody
NYTarticles$bodyTitle=paste(NYTarticles$titles, NYTarticles$body)#paste title and body
NYTarticles$bodyTitle=gsub(" NA$","",NYTarticles$bodyTitle)#remove NAs
saveRDS(NYTarticles, "./data/NYTarticles.RDS")

