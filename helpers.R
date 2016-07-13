#various functions that help to clean data, because I've copied them some of
#the parts might not be needed, but they do no harm :)
#modifiyng this function might cause some problems when usin LDA on twitter data
cleanTweet=function(tweetText) {
  tweetText <- iconv(tweetText,to="utf-8")
  tweetText <- gsub("'", "", tweetText)  # remove apostrophes
  tweetText <- gsub("[[:punct:]]", " ", tweetText)  # replace punctuation with space
  tweetText <- gsub("[[:cntrl:]]", " ", tweetText)  # replace control characters with space
  tweetText <- gsub("^[[:space:]]+", "", tweetText) # remove whitespace at beginning of documents
  tweetText <- gsub("[[:space:]]+$", "", tweetText) # remove whitespace at end of documents
  tweetText <- tolower(tweetText)  # force to lowercase
  tweetText <- gsub("cnn|nytimes|ā|ä|ā|ā|ä|ā", "", tweetText) # remove whitespace at end of documents
  tweetText=gsub('http\\S+\\s*', '', tweetText)#remove links
  tweetText=gsub('@rt', '', tweetText)#remove retweet char
  tweetText=gsub('^rt ', '', tweetText)#remove retweet char
  tweetText <- gsub("@\\w+", " ", tweetText)#remove usenames
  tweetText <- gsub("[ |\t]{2,}", " ", tweetText) # Remove tabs
  library(stringr)#remove excess whitesapce
  tweetText=gsub("\\s+", " ", str_trim(tweetText))
  #remove documents that have smaller lenght than 3
  tweetText=tweetText[nchar(tweetText)>3]
  tweetText
}
########################################################################
#################functions for scraping metadata and articles
#helper function to make NYT API call url
makeURL <- function(q=NULL, fq=NULL, begin_date=NULL, end_date=NULL, 
                    key=apikey, page=0, sort=NULL, fl=NULL, 
                    hl=NULL, facet_field=NULL,facet_filter=NULL){
  #input= argumenst of NYT API call, more about arguments read here: 
  #https://developer.nytimes.com/article_search_v2.json#/README
  #output (character string)= url for NYT API call
  arglist <- list(q=q, fq=fq, begin_date=begin_date, end_date=end_date, 
                  'api-key'=key, page=page, sort=sort, fl=fl, hl=hl,
                  facet_field=facet_field,
                  facet_filter=facet_filter)
  url <- paste0('http://api.nytimes.com/svc/search/v2/articlesearch.json?')
  for(i in 1:length(arglist)){
    if(is.null(unlist(arglist[i]))==F){
      url <- paste0(url, '&', names(arglist[i]), '=', arglist[i])
    }
  }
  return(url)
}

#small helper function to make section names for query 
makeFq=function(section){
  #https://developer.nytimes.com/article_search_v2.json#/README
  #input (character)= section name(s) (double qoutation!): '"Sports", "Arts"'
  #output (character)= fq variable for getMetaData function.
  paste0('section_name:(', section,')')
}

#function that gets meta data of specified number of articles
#takes them as as dayStep specified chunks 
#(max 1000 articles per day for given filters)
getMetaData=function(apikey,nrOfArticles=300, beginDate="20160619", 
                     backward=T, sort=NULL,fq =NULL, fl=NULL, 
                     hl=NULL, facet_field=NULL, dayStep=1,
                     facet_filter=NULL) {
  #input:
  #apikey (character)=your key for NYT API.
  #nrOfArticles (integer)=nr of articles which metadata you want to scrape. 
  #beginDate (character, format: YYYYMMDD)=date from which the scraping begins. 
  #backward (boolean)=how scraping is done considering time scale: from begin 
  #date to future or to the past, deafult to the past. 
  #section (character, use function makeFq)=section which articles you want 
  #to scrape rest of the arguments are from function makeURL
  #dayStep (integer) = from how big period chunks articles will be taken.
  #by default will take daily, if articles are widely distributed in time 
  #it is reasonable to take bigger chunks
  #output: dataframe with following columns: urls, section_names, titles,
  #dates
  require(jsonlite)
  library(RCurl)
  #initial sanity check, if there are some articles in sepcified nr of loops,
  #and asks user what to do
  beginDateinside=beginDate
  endDateinside=beginDate
  if(backward==T) {
    #date from which begin (max period based on daystep and API call limit)
    checkDate=gsub("-","",as.Date(strptime(beginDateinside, 
                                           "%Y%m%d"))-(999*dayStep))
    checkurl=makeURL(fq =fq,begin_date=checkDate, end_date=endDateinside,
                     sort=sort, fl=fl, hl=hl, facet_field=facet_field,
                     facet_filter=facet_filter)
  } else {
    checkDate=gsub("-","",as.Date(strptime(beginDateinside, 
                                           "%Y%m%d"))+(999*dayStep))
    checkurl=makeURL(fq =fq,begin_date=beginDateinside, end_date=checkDate,
                     sort=sort, fl=fl, hl=hl, facet_field=facet_field,
                     facet_filter=facet_filter)
  }
  print(checkurl)
  #nr of articles for this period
  hits=fromJSON(txt=URLencode(checkurl), flatten = T)$response$meta$hits
  #if hits is smaller than nrOfAticles, than stop
  if(hits<nrOfArticles) {
    stop(paste0("There are ", hits," articles, but you wanted ",nrOfArticles, 
                ". Please specify nrOfArticles/beginData/dayStep"))
  }
  # set up initial user choiche which is False
  userChoice="I have no idea"
  #ask user choice
  while (!userChoice%in%c("1", "0")) {
    userChoice=readline(prompt=paste0(hits, " hits from ", checkDate, " to ", 
                                      beginDate,". To continue press 1,
                                      to stop press 0: "))
  }
  #act according to user choice
  if (userChoice=="0") {
    stop("Stopping this session") #stop session
  } else if (userChoice=="1") {
    cat("Let's rock and roll \n") #continue
  }
  
  #######function main body
  #initialize variables to where we loop information needed
  urls=c()
  section_names=c()
  titles=c()
  dates=c()
  callCounter=1#counts nr of all calls to avoid infinite loops and API limit
  #start looping, second condition is stupid way to escape infinite loop
  while(length(urls)<nrOfArticles&&callCounter<=999) {
    #initialise dates between which we search for articles
    if(backward==T) {
      endDateinside=gsub("-","",
                         as.Date(strptime(beginDateinside, "%Y%m%d"))-1)
      beginDateinside=gsub("-","",
                           as.Date(strptime(beginDateinside, "%Y%m%d"))-
                             dayStep)
    } else {
      beginDateinside=gsub("-","",as.Date(strptime(endDateinside, "%Y%m%d"))+
                             1)
      endDateinside=gsub("-","",as.Date(strptime(endDateinside, "%Y%m%d"))+
                           dayStep)
    }
    #when error occurs display message and continue
    tryget <- try({
      #initial nr of hits
      hits=0
      callTry=1#nr of tries to display the number for user
      #loop until find page that has non-0 hits
      #second condition is stupid way to escape infinite loop
      while(hits==0&&callCounter<=999) {
        initcall=URLencode(makeURL(fq =fq,begin_date=beginDateinside, 
                                end_date=endDateinside,
                                sort=sort, fl=fl, hl=hl, 
                                facet_field=facet_field,
                                facet_filter=facet_filter))
        #nr of articles in query
        hits=fromJSON(txt=URLencode(initcall), flatten = T)$response$meta$hits
        callCounter=callCounter+1
        cat("Looking for calls that have some hits. Try nr ", 
            callTry, " \n")
        callTry=callTry+1
      }
      #nr of loops needed for that query page where there are at leats 1 hits
      #(max 99, because max 100 pages (starting from 0!) are allowed
      #by NYT API per unique url
      nloops=min(max(ceiling(hits/10-1),0), 99)
      #loop meta data from pages
      for(i in 0:nloops) {#start looping from 0 because page nr start from 0
        if(length(urls)<nrOfArticles) {
          url=makeURL(fq =fq,page=i,sort=sort, fl=fl, hl=hl,
                      facet_field=facet_field,
                      facet_filter=facet_filter,
                      begin_date=beginDateinside, 
                      end_date=endDateinside)
          response=fromJSON(txt=URLencode(url), flatten = T)
          #append data into our vectors        
          urls=append(urls, response$response$docs$web_url)
          section_names=append(section_names,response$response$docs$section_name)
          titles=append(titles, response$response$docs$headline.main)
          dates=append(dates,response$response$docs$pub_date )
          #display message for user
          cat("Metadata for", length(urls), "articles. API call nr ",
              callCounter,  " \n")
          #prints also link, needed for debugging
          print(makeURL(fq =fq,page=i,sort=sort, fl=fl, hl=hl, 
                        facet_field=facet_field,facet_filter=facet_filter,
                        begin_date=beginDateinside, end_date=endDateinside))
          callCounter=callCounter+1
          Sys.sleep(0.2) #not ot make too many calls (5 per second are allowed)
        } else {
          #maybe I can delete this
          results=data.frame(urls, section_names, titles,dates)
        }
      }
    }) #end of tryget
    #if some error happened, give message with url and continue
    if(class(tryget)=='try-error') { 
      cat("page number:",length(urls)+1, ' error - body not scraped, url:',
          makeURL(fq =fq,page=i,sort=sort, fl=fl, hl=hl, 
                  facet_field=facet_field,facet_filter=facet_filter,
                  begin_date=beginDateinside, end_date=endDateinside),'\n')
      #write NAs to vairables we are scraping
      urls[i]=NA
      section_names[i]=NA
      titles[i]=NA
      dates[i]=NA
      next
    }
    callCounter=callCounter+1
  }
  results=data.frame(urls, section_names, titles,dates)
  #display results  
  results
  }

#function to get article body from metadata scraped
getArticleBody=function(articleUrls, 
                        selector=c('article > div', '.articleBody')) {
  #input: 
  #articleUrls (character vector): article urls from metadata via NYT API 
  #selector (character vector)= selectors which indicate articles body, might
  #be more, needs to be tested to find out
  #output (character vector)=article bodys
  library(rvest)
  body=c()#initialize vector where we add article body
  for (i in 1:length(articleUrls)) {
    url=as.character(articleUrls[i])
    
    tryget <- try({ #is needed if some error happens, then it continues
      page=read_html(url)
      for (j in 1:length(selector)) {
        if(length(page %>% #if response has 0 characters of body
                  html_nodes(selector[j]) %>%
                  html_text())==0) {
          body[i]=NA
        } else {
          body[i]=paste(page %>% #paste needed to collapse char vec into 1 vec
                          html_nodes(selector[j]) %>%
                          html_text(), collapse = '')
          break #if body found no need to try other selectors
        }
      }
      cat("Worked on article nr",paste0(i, ","),
          paste0("progress: ",round(i/length(articleUrls)*100,3),"%"),"\n")
    })
    #if some error occured, give message and continue
    if(class(tryget)=='try-error') { 
      cat("page number:",i, ' error - body not scraped \n')
      body[i]=NA
      next
    }
  }
  body
}