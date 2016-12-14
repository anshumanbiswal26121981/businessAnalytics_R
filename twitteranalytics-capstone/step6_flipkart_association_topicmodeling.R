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

load("flipkart.word_corelation.RData")
flipkart_handle.df=read.csv("flipkart_handle.csv",header = TRUE,stringsAsFactors = FALSE,sep = ",")
flipkart_handle.df=flipkart_handle.df[,-c(1)]

findAssocs(tdm, "redmi", 0.2)
findAssocs(tdm, "beard", 0.2)
findAssocs(tdm, "love", 0.2)
findAssocs(tdm, "delivery", 0.2)

##### Topic Modelling to identify latent/hidden topics
#### Create document-term 
dtm <- as.DocumentTermMatrix(tdm)

#### dtm may contain 0 values,we need to clean it
raw.sum=apply(dtm,1,FUN=sum)

####  delete all raws which are all 0 doing
dtm=dtm[raw.sum!=0,]

####collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))

####length should be total number of terms
length(freq)

####create sort order (descending)
ord <- order(freq,decreasing=TRUE)

####List all terms in decreasing order of freq and write to disk
#freq[ord]
#for documenting purpose only the first 10 elements were shown.
head(freq[ord],10)
write.csv(freq[ord],"flipkart_word_freq.csv")



####Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

####Number of topics
k <- 4
######Run LDA using Gibbs sampling
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

####write out results
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"flipkart_DocsToTopics.csv"))

###### find top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,6))
ldaOut.terms
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"flipkart_TopicsToTerms.csv"))

######probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
head(topicProbabilities,6)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"flipkart_TopicProbabilities.csv"))

######Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])

######Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])

######write to file
t12df=as.data.frame(topic1ToTopic2)
t12df[1,1:6]# To check its contents
write.csv(topic1ToTopic2,file=paste("LDAGibbs",k,"flipkart_Topic1ToTopic2.csv")) #save it for complete analysis
t23df=as.data.frame(topic2ToTopic3)
t23df[1,1:6]# To check its contents
write.csv(topic2ToTopic3,file=paste("LDAGibbs",k,"flipkart_Topic2ToTopic3.csv")) #save it for complete analysis

#k means clustering
#
# Let us create a Term-Document matrix with words having sparsity less than 99 percentile 
#
# Remove sparse terms
tdm2<-removeSparseTerms(tdm,sparse=0.98)
m2<-as.matrix(tdm2)

#Cluster terms
distmatrix=dist(scale(m2))
fit=hclust(distmatrix,method="ward.D2")
plot(fit,cex=0.9)

#cut the tree
rect.hclust(fit,k=4)

save.image(file="flipkart_association_topicmodelling.RData")