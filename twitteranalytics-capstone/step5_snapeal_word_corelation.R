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
load("snapdeal.wordcloud.RData")


###### Identify and plot word correlations. For example - order
WordCorr_order<- apply_as_df(snapdeal_combined.corpus[1:10392], word_cor, word = "order", r=.20)
plot(WordCorr_order)
qheat(vect2df(WordCorr_order[[1]], "order", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()

###### Identify and plot word correlations. For example - help
WordCorr_help<- apply_as_df(snapdeal_combined.corpus[1:10392], word_cor, word = "help", r=.10)
plot(WordCorr_help)
qheat(vect2df(WordCorr_help[[1]], "help", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()



###### Find Messages with word - help
df <- data.frame(text=unlist(sapply(snapdeal_combined.corpus.ptd, '[', "content")), stringsAsFactors=FALSE)
head(unique(df[grep("help", df$text), ]), n=5)

head(unique(df[grep("order", df$text), ]), n=5)


save.image(file="snapdeal.word_corelation.RData")