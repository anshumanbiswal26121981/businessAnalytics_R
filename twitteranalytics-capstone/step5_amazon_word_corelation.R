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
load("amazon.wordcloud.RData")


###### Identify and plot word correlations. For example - help
WordCorr_help <- apply_as_df(amazon_combined.corpus[1:10395], word_cor, word = "help", r=.20)
plot(WordCorr_help)
qheat(vect2df(WordCorr_help[[1]], "help", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()

###### Identify and plot word correlations. For example - order
WordCorr_order <- apply_as_df(amazon_combined.corpus[1:10395], word_cor, word = "order", r=.25)
plot(WordCorr_order)
qheat(vect2df(WordCorr_order[[1]], "order", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()

###### Identify and plot word correlations. For example - sign
WordCorr_amazonsign <- apply_as_df(amazon_combined.corpus[1:10395], word_cor, word = "sign", r=.25)
plot(WordCorr_amazonsign)
qheat(vect2df(WordCorr_amazonsign[[1]], "sign", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()


###### Identify and plot word correlations. For example - copies
WordCorr_copies <- apply_as_df(amazon_combined.corpus[1:10395], word_cor, word = "copies", r=.25)
plot(WordCorr_copies)
qheat(vect2df(WordCorr_copies[[1]], "copies", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()


###### Identify and plot word correlations. For example - fraud
WordCorr_fraud <- apply_as_df(amazon_combined.corpus[1:10395], word_cor, word = "fraud", r=.25)
plot(WordCorr_fraud)
qheat(vect2df(WordCorr_fraud[[1]], "fraud", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()




###### Find Messages with word - help
df <- data.frame(text=unlist(sapply(amazon_combined.corpus.ptd, '[', "content")), stringsAsFactors=FALSE)
head(unique(df[grep("help", df$text), ]), n=5)

###### Find Messages with word - order
head(unique(df[grep("order", df$text), ]), n=5)

###### Find Messages with word - sign
head(unique(df[grep("sign", df$text), ]), n=5)

###### Find Messages with word - copies
head(unique(df[grep("copies", df$text), ]), n=5)
###### Find Messages with word - fraud
head(unique(df[grep("fraud", df$text), ]), n=5)


save.image(file="amazon.word_corelation.RData")
