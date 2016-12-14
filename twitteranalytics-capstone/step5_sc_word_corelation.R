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
load("sc.wordcloud.RData")


###### Identify and plot word correlations. For example - intex
WordCorr_intex<- apply_as_df(sc_combined.df.corpus[1:6924], word_cor, word = "intex", r=.25)
plot(WordCorr_intex)
qheat(vect2df(WordCorr_intex[[1]], "intex", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()

###### Identify and plot word correlations. For example - bad
WordCorr_bad<- apply_as_df(sc_combined.df.corpus[1:6924], word_cor, word = "bad", r=.25)
plot(WordCorr_bad)
qheat(vect2df(WordCorr_bad[[1]], "bad", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()

###### Identify and plot word correlations. For example - marshmallow
WordCorr_marshmallow<- apply_as_df(sc_combined.df.corpus[1:6924], word_cor, word = "marshmallow", r=.25)
plot(WordCorr_marshmallow)
qheat(vect2df(WordCorr_marshmallow[[1]], "marshmallow", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()

###### Find Messages with word - intex
df <- data.frame(text=unlist(sapply(sc_combined.corpus.ptd, '[', "content")), stringsAsFactors=FALSE)
head(unique(df[grep("intex", df$text), ]), n=5)

head(unique(df[grep("bad", df$text), ]), n=5)

head(unique(df[grep("marshmallow", df$text), ]), n=5)


save.image(file="sc.word_corelation.RData")