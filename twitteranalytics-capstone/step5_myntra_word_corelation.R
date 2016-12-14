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
load("myntra.wordcloud.RData")

###### Identify and plot word correlations. For example - discount
WordCorr_discount <- apply_as_df(myntra_combined.corpus[1:5047], word_cor, word = "discount", r=.20)
plot(WordCorr_discount)
qheat(vect2df(WordCorr_discount[[1]], "discount", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()

###### Identify and plot word correlations. For example - myntrasupport
WordCorr_myntrasupport <- apply_as_df(myntra_combined.corpus[1:5047], word_cor, word = "myntrasupport", r=.20)
plot(WordCorr_myntrasupport)
qheat(vect2df(WordCorr_myntrasupport[[1]], "myntrasupport", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()

###### Identify and plot word correlations. For example - good
WordCorr_good <- apply_as_df(myntra_combined.corpus[1:5047], word_cor, word = "good", r=.20)
plot(WordCorr_good)
qheat(vect2df(WordCorr_good[[1]], "good", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()

###### Identify and plot word correlations. For example - poor
WordCorr_poor <- apply_as_df(myntra_combined.corpus[1:5047], word_cor, word = "poor", r=.20)
plot(WordCorr_poor)
qheat(vect2df(WordCorr_poor[[1]], "poor", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()

save.image(file="myntra.word_corelation.RData")