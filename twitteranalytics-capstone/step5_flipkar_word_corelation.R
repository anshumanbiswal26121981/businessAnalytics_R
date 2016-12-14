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
load("flipkart.wordcloud.RData")


###### Identify and plot word correlations. For example - redmi
WordCorr_redmi <- apply_as_df(flipkart_handle.df.corpus[1:10000], word_cor, word = "redmi", r=.25)
plot(WordCorr_redmi)
qheat(vect2df(WordCorr_redmi[[1]], "redmi", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()

###### Identify and plot word correlations. For example - beard
WordCorr_beard <- apply_as_df(flipkart_handle.df.corpus[1:10000], word_cor, word = "beard", r=.25)
plot(WordCorr_beard)
qheat(vect2df(WordCorr_beard[[1]], "beard", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()


###### Identify and plot word correlations. For example - love
WordCorr_love <- apply_as_df(flipkart_handle.df.corpus[1:10000], word_cor, word = "love", r=.25)
plot(WordCorr_love)
qheat(vect2df(WordCorr_love[[1]], "love", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()


###### Identify and plot word correlations. For example - delivery
WordCorr_delivery <- apply_as_df(flipkart_handle.df.corpus[1:10000], word_cor, word = "delivery", r=.25)
plot(WordCorr_delivery)
qheat(vect2df(WordCorr_delivery[[1]], "delivery", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()


###### Find Messages with word - redmi
df <- data.frame(text=unlist(sapply(flipkart_handle.corpus.ptd, '[', "content")), stringsAsFactors=FALSE)
head(unique(df[grep("redmi", df$text), ]), n=5)

###### Find Messages with word - beard
head(unique(df[grep("beard", df$text), ]), n=5)

###### Find Messages with word - love
head(unique(df[grep("love", df$text), ]), n=5)

###### Find Messages with word - delivery
head(unique(df[grep("delivery", df$text), ]), n=5)
save.image(file="flipkart.word_corelation.RData")
