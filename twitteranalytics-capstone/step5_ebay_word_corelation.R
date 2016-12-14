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
load("ebay.wordcloud.RData")


###### Identify and plot word correlations. For example - game
WordCorr_game<- apply_as_df(ebay_combined.df.corpus[1:11409], word_cor, word = "game", r=.25)
plot(WordCorr_game)
qheat(vect2df(WordCorr_game[[1]], "game", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()

###### Identify and plot word correlations. For example - new
WordCorr_new<- apply_as_df(ebay_combined.df.corpus[1:11409], word_cor, word = "new", r=.25)
plot(WordCorr_new)
qheat(vect2df(WordCorr_new[[1]], "new", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()

###### Identify and plot word correlations. For example - india
WordCorr_india<- apply_as_df(ebay_combined.df.corpus[1:11409], word_cor, word = "india", r=.25)
plot(WordCorr_india)
qheat(vect2df(WordCorr_india[[1]], "india", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()

###### Identify and plot word correlations. For example - deal
WordCorr_deal<- apply_as_df(ebay_combined.df.corpus[1:11409], word_cor, word = "deal", r=.25)
plot(WordCorr_deal)
qheat(vect2df(WordCorr_deal[[1]], "deal", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()

###### Find Messages with word - game
df <- data.frame(text=unlist(sapply(ebay_combined.corpus.ptd, '[', "content")), stringsAsFactors=FALSE)
head(unique(df[grep("game", df$text), ]), n=5)

head(unique(df[grep("new", df$text), ]), n=5)

head(unique(df[grep("india", df$text), ]), n=5)

head(unique(df[grep("deal", df$text), ]), n=5)


save.image(file="ebay.word_corelation.RData")