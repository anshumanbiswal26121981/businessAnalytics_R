rm(list=ls())
require(twitteR)
require(RCurl)
require(tm)
library(qdap)
library(ROAuth)
library(chron)
library(SnowballC)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(data.table)
library(stringi)
library(rmarkdown)
#install.packages("bitops")
library(devtools)
#install_github('sentiment140', 'okugami79')
library(zoo)
#install_github("hrbrmstr/streamgraph")
# install_github('sentiment140', 'okugami79') before using sentiment pkg
library(sentiment)
library(reshape2)
library(qdap)
library(dplyr)
library(streamgraph)
setwd("C:\\AWBackup\\PGPBA\\PGPBA-GreatLakes\\Modules\\capstoneproject\\data")

load("ebay_association_topicmodelling.RData")
rem.some.punct <- function(txt, notpunct=NULL){ 
  punctstr <- "[]!\"#$%&'()*/+,.:;<=>?@[^_`{|}~-]"
  rempunct <- gsub(paste0("",notpunct), "", punctstr)
  gsub(rempunct, "", txt)}

sentiments <- polarity(rem.some.punct(ebay_combined.df$text))
sentiments <- data.frame(sentiments$all$polarity)
sentiments[["polarity"]] <- cut(sentiments[[ "sentiments.all.polarity"]], c(-5,0.0,5),     labels = c("negative","positive"))
table(sentiments$polarity)

#Sentiment Plot by date

sentiments$score<- 0
sentiments$score[sentiments$polarity == "positive"]<-1
sentiments$score[sentiments$polarity == "negative"]<- -1
sentiments$date <-as.IDate(ebay_combined.df$created)
result = aggregate(score ~ date, data = sentiments, sum)
plot(result,type='l')

#stream grapg

Data<-data.frame(sentiments$polarity)
colnames(Data)[1] <- "polarity"
Data$Date <- as.Date(ebay_combined.df$created)
Data$text <- NULL
Data$Count <- 1

graphdata <- aggregate(Count ~ polarity + as.character.Date(Data$Date),data=Data,FUN=length)
colnames(graphdata)[2] <- "Date"
str(graphdata)
str(graphdata)
write.csv(graphdata,file="ebay_sentiments_graphdata.csv")


#stream graph

  graphdata %>%
    streamgraph(polarity, Count, Date) %>%
    sg_axis_x(20) %>%
    sg_axis_x(1, "Date","%d-%b") %>%
    sg_legend(show=TRUE, label="Polarity: ")
  
  
  install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
  library(sentiment)
  ebaytweetsemo = classify_emotion(ebay_combined.df$text,algorithm="bayes", prior=1.0)
  ebaytweetsemotion = ebaytweetsemo[,7]
  ebaytweetsemotion[is.na(ebaytweetsemotion)] = "unknown"
  head(ebaytweetsemotion,20)

plotSentiments1<- function (sentiment_dataframe,title) {
  
  ggplot(sentiment_dataframe, aes(x=emotion)) +
    geom_bar(aes(y=..count.., fill=emotion)) +
    
    scale_fill_brewer(palette="Dark2") +
    
    ggtitle(title) +
    
    theme(legend.position='right') + ylab('Number of Tweets') +
    xlab('Emotion Categories')
  
}

plotSentiments2 <- function (sentiment_dataframe,title) {
  
  library(ggplot2)
  
  ggplot(sentiment_dataframe, aes(x=polarity)) +
    
    geom_bar(aes(y=..count.., fill=polarity)) +
    
    scale_fill_brewer(palette="RdGy") +
    
    ggtitle(title) +
    
    theme(legend.position='right') + ylab('Number of Tweets') +
    xlab('Polarity Categories')
  
}

EbayTweetsClassPol = classify_polarity(ebay_combined.df$text,algorithm="bayes")
head(EbayTweetsClassPol,20)
ebayPol = EbayTweetsClassPol[,4]

ebaySentimentDataFrame = data.frame(text=ebay_combined.df$text,emotion=ebaytweetsemotion, polarity=ebayPol, stringsAsFactors=FALSE)
ebaySentimentDataFrame%>%filter(!emotion %in% c("unknown")) ->ebay.filter.df

ebay.filter.df = within(ebay.filter.df, emotion <-factor(emotion, levels=names(sort(table(emotion),decreasing=TRUE))))
plotSentiments1(ebay.filter.df, 'Sentiment Analysis of Tweets on Twitter about Ebay')
write.csv(ebay.filter.df,file = "ebay_filter_sentiment_by_emotion.csv")


#myntra wordcloud with emotions
#####calculate the frequency of words and sort it by frequency and setting up the Wordcloud
removeCustomeWords <- function (TweetsCleaned) {
  
  for(i in 1:length(TweetsCleaned)){
    
    TweetsCleaned[i] <- tryCatch({
      
      stpword.vector <- readLines("stop-word-list.csv")[1]
      
      TweetsCleaned[i] = removeWords(TweetsCleaned[i],stpword.vector)
      
      TweetsCleaned[i]
      
    }, error=function(cond) {
      
      TweetsCleaned[i]
      
    }, warning=function(cond) {
      
      TweetsCleaned[i]
      
    })
    
  }
  
  return(TweetsCleaned)
  
}
getWordCloud <- function
(sentiment_dataframe, TweetsCleaned, Emotion) {
  
  emos = levels(factor(sentiment_dataframe$emotion))
  
  n_emos = length(emos)
  
  emo.docs = rep("", n_emos)
  
  TweetsCleaned = removeCustomeWords(TweetsCleaned)
  
  
  
  for (i in 1:n_emos){
    
    emo.docs[i] = paste(TweetsCleaned[Emotion ==
                                        emos[i]], collapse=" ")
    
  }
  
  corpus = Corpus(VectorSource(emo.docs))
  
  tdm = TermDocumentMatrix(corpus)
  
  tdm = as.matrix(tdm)
  
  colnames(tdm) = emos
  
  require(wordcloud)
  
  suppressWarnings(comparison.cloud(tdm, colors =
                                      brewer.pal(n_emos, "Dark2"), scale = c(3,.5), random.order =
                                      FALSE, title.size = 1.5))
  
}

getWordCloud(ebaySentimentDataFrame,rem.some.punct(ebay_combined.df$text),ebaytweetsemotion)





save.image("ebay_sentiment_analysis.RData")
