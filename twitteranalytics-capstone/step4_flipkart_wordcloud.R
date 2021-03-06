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

load("flipkart_handle.df.corpus.cleaned.RData")


#####Find the terms used most frequently
flipkart_handle.corpus.ptd <- tm_map(flipkart_handle.df.corpus, PlainTextDocument)
tdm<- TermDocumentMatrix(flipkart_handle.corpus.ptd, control= list(wordLengths= c(1, Inf)))
freq.terms <- findFreqTerms(tdm, lowfreq = 200)
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq > 200)
df <- data.frame(term = names(term.freq), freq= term.freq)

#####plotting the graph of frequent terms
ggplot(df, aes(reorder(term, freq),freq)) + theme_bw() + geom_bar(stat = "identity")  + coord_flip() +labs(list(title="Term Frequency Chart For Flipkart", x="Terms", y="Term Counts"))+theme(text = element_text(size=10),axis.text.x = element_text(angle=90, vjust=1)) 


#####calculate the frequency of words and sort it by frequency and setting up the Wordcloud
word.freq <-sort(rowSums(as.matrix(tdm)), decreasing= F)
pal<- brewer.pal(8, "Dark2")
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 2, random.order = F, colors = pal, max.words = 1000)

save.image(file="flipkart.wordcloud.RData")