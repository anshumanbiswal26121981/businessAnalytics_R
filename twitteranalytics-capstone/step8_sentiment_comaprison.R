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

amazon_sentiment.df = read.csv("amazon_sentiments_graphdata.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE)
names(amazon_sentiment.df)[1]="Brand"
amazon_sentiment.df$Brand="amazon"

flipkart_sentiment.df = read.csv("flipkart_sentiments_graphdata.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE)
names(flipkart_sentiment.df)[1]="Brand"
flipkart_sentiment.df$Brand="flipkart"

snapdeal_sentiment.df = read.csv("snapdeal_sentiments_graphdata.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE)
names(snapdeal_sentiment.df)[1]="Brand"
snapdeal_sentiment.df$Brand="snapdeal"

ebay_sentiment.df = read.csv("ebay_sentiments_graphdata.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE)
names(ebay_sentiment.df)[1]="Brand"
ebay_sentiment.df$Brand="ebay"

sc_sentiment.df = read.csv("sc_sentiments_graphdata.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE)
names(sc_sentiment.df)[1]="Brand"
sc_sentiment.df$Brand="shop clues"

sentiment.combined.df = rbind(amazon_sentiment.df,snapdeal_sentiment.df,flipkart_sentiment.df,ebay_sentiment.df,sc_sentiment.df)
write.csv(sentiment.combined.df,"sentiment.combined.csv")
sentiment.combined.df%>%group_by(Brand,polarity)%>%summarise(totalpolarity=sum(Count))->summaryplot.polarity
write.csv(summaryplot.polarity,"summaryplot.polarity.csv")
sentiment.combined.df%>%group_by(Date)->sentiment.groupby.date

write.csv(sentiment.groupby.date,"sentiment.groupby.date.csv")

amazon_combined.df = read.csv("amazon_combined.csv",header = TRUE,stringsAsFactors = FALSE)
flipkart_combined.df = read.csv("flipkart_handle.csv",header = TRUE,stringsAsFactors = FALSE)
snapdeal_combined.df = read.csv("snapdeal_combined.csv",header = TRUE,stringsAsFactors = FALSE)
ebay_combined.df=read.csv("ebay_combined.csv",header = TRUE,stringsAsFactors = FALSE)
sc_combined.df=read.csv("sc_combined.csv",header = TRUE,stringsAsFactors = FALSE)

amazon_combined.df$brand=c("amazon")
flipkart_combined.df$brand=c("flipkart")
snapdeal_combined.df$brand=c("snapdeal")
ebay_combined.df$brand=c("ebay")
sc_combined.df$brand=c("shop clues")

combined.tweets.df=rbind(amazon_combined.df,flipkart_combined.df,snapdeal_combined.df,ebay_combined.df,sc_combined.df)

combined.tweets.df$created = as.IDate(combined.tweets.df$created)

# # select top retweeted tweets
# table(combined.tweets.df$retweetCount)
# selected <- which(combined.tweets.df$retweetCount >= 1500)
# # plot them
# dates <- strptime(combined.tweets.df$created, format="%Y-%m-%d")
# plot(x=dates, y=combined.tweets.df$retweetCount, type="l", col="grey",xlab="Date", ylab="Times retweeted")
# colors <- rainbow(10)[1:length(selected)]
# points(dates[selected], combined.tweets.df$retweetCount[selected],pch=19, col=colors)
# text(dates[selected], combined.tweets.df$retweetCount[selected],combined.tweets.df$text[selected], col=colors, cex=0.5,srt=45)

amazon_snetiment_byemotion.df = read.csv("amazon_filter_sentiment_by_emotion.csv",header = TRUE,stringsAsFactors = FALSE)
names(amazon_snetiment_byemotion.df)[1]="Brand"
amazon_snetiment_byemotion.df$brand=c("amazon")
flipkart_snetiment_byemotion.df = read.csv("flipkart_filter_sentiment_by_emotion.csv",header = TRUE,stringsAsFactors = FALSE)
names(flipkart_snetiment_byemotion.df)[1]="Brand"
flipkart_snetiment_byemotion.df$brand=c("flipkart")
snapdeal_snetiment_byemotion.df = read.csv("snapdeal_filter_sentiment_by_emotion.csv",header = TRUE,stringsAsFactors = FALSE)
names(snapdeal_snetiment_byemotion.df)[1]="Brand"
snapdeal_snetiment_byemotion.df$brand=c("snapdeal")
ebay_snetiment_byemotion.df=read.csv("ebay_filter_sentiment_by_emotion.csv",header = TRUE,stringsAsFactors = FALSE)
names(ebay_snetiment_byemotion.df)[1]="Brand"
ebay_snetiment_byemotion.df$brand=c("ebay")
sc_snetiment_byemotion.df=read.csv("sc_filter_sentiment_by_emotion.csv",header = TRUE,stringsAsFactors = FALSE)
names(sc_snetiment_byemotion.df)[1]="Brand"
sc_snetiment_byemotion.df$brand=c("shop clues")
combined_sentiments.df=rbind(amazon_snetiment_byemotion.df,flipkart_snetiment_byemotion.df,snapdeal_snetiment_byemotion.df,ebay_snetiment_byemotion.df,sc_snetiment_byemotion.df)
combined_sentiments.df=combined_sentiments.df[,-1]
combined_sentiments.df=combined_sentiments.df[,-1]
combined_sentiments.df=combined_sentiments.df[,-2]
combined_sentiments.df%>%group_by(emotion,brand)%>%summarise(totalemotion=n())->summarise_by_emotion.df
write.csv(summarise_by_emotion.df,"summarise_by_emotion.csv")

summarise_by_emotion.df%>%group_by(brand,emotion)%>%summarise(total=totalemotion)->temp_combined.total_emotion#summary_emotion_group_by_brandAndEmotion
write.csv(temp_combined.total_emotion,"temp_combined.total_emotion.csv")

write.csv(combined.tweets.df,"combined.tweets.csv")
combined.tweets.df%>%filter(!replyToSN %in% c("amazonIN","AmazonHelp","Flipkart","flipkartsupport","snapdeal","ebay","Ebay","ShopClues")) ->test.filter.df
test.filter.df%>%filter(!is.na(test.filter.df$replyToSN))->test.filter.clean.df
test.filter.clean.df%>% group_by(brand,created)%>% summarise(n = n())->summarize.df 
names(summarize.df)[3]=c("replycount")
write.csv(summarize.df,"summarize.df.replycount.csv")


combined.tweets.df%>%filter(replyToSN %in% c("amazonIN","AmazonHelp","Flipkart","flipkartsupport","snapdeal","ebay","Ebay","ShopClues","ebayindia")) ->test.filter.queries.df
test.filter.queries.df%>%filter(!is.na(test.filter.queries.df$replyToSN))->test.filter.queries.clean.df
test.filter.queries.clean.df%>% group_by(brand,created)%>% summarise(n = n())->summarize.querie.df 
names(summarize.querie.df)[3]=c("querycount")
write.csv(summarize.querie.df,"summarize.df.querycount.csv")

library(dplyr)
inner_join(summarize.df, summarize.querie.df)->querresponse.df
querresponse.df=querresponse.df[-c(4),]
querresponse.df$responserate= (querresponse.df$replycount/querresponse.df$querycount)*100
write.csv(querresponse.df,"querresponse.csv")

querresponse.df%>%group_by(brand)%>%summarise(sumreply=sum(replycount),sumquery=sum(querycount))->querygroupbybrand.df
querygroupbybrand.df$responserate = (querygroupbybrand.df$sumreply/querygroupbybrand.df$sumquery)
write.csv(querygroupbybrand.df,"querygroupbybrand.csv")

#Total tweets count
combined.tweets.df%>%group_by(brand)%>%summarise(totaltweets=n())

#Total favorite count
combined.tweets.df%>%group_by(brand)%>%summarise(totalfavcount=sum(favoriteCount))


#Total retweet count
combined.tweets.df%>%group_by(brand)%>%summarise(totalretweetcount=sum(retweetCount))

consumer_key = 'vitqhIlWGWfRUOUP8JIixb7OB'
consumer_secret = 'gJrNUmTl99eU3bTpUQTnBhO1zs3TST1qluwSjyozfkTm4FOGjW'
access_token = '3306555647-NpzZt0Af4T06E5ea9EWlVQeGRfE4wF4fvQBOBMg'
access_token_secret = '5Y4ojfc0LO8UtHmjrGVdhMlsF9AjkjWIxluBmofH45v2y'


setup_twitter_oauth(consumer_key,consumer_secret,access_token, access_token_secret)

#####Retrieve user info and followers
amazonuser=getUser("@amazonIN")
amazonuser.df=amazonuser$toDataFrame()

flipkartuser=getUser("@Flipkart")
flipkartuser.df=flipkartuser$toDataFrame()

snapdealuser=getUser("@snapdeal")
snapdealuser.df=snapdealuser$toDataFrame()

Ebayuser=getUser("@ebayindia")
Ebayuser.df=Ebayuser$toDataFrame()

scuser=getUser("@ShopClues")
scuser.df=scuser$toDataFrame()

combined.users.brandpercention.df=rbind(amazonuser.df,flipkartuser.df,snapdealuser.df,Ebayuser.df,scuser.df)
write.csv(combined.users.brandpercention.df,"combined.users.brandpercention.csv")
save.image("sentiment_comparison.RData")