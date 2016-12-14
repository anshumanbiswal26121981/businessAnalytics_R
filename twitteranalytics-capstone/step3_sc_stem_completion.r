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
load("sc_combined.df.corpus.copy.RData")

sc_combined.df.corpus = sc_combined.df.corpus.copy

#####Stem words in the corpus 
sc_combined.df.corpus=tm_map(sc_combined.df.corpus, stemDocument)

writeLines(strwrap(ebay_combined.df.corpus[[250]]$content,60))

stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  # Unexpectedly, stemCompletion completes an empty string to
  # a word in dictionary. Remove empty string to avoid above issue.
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

#####Stem Complete and Display the same tweet above with the completed and corrected text.
sc_combined.df.corpus=lapply(sc_combined.df.corpus,stemCompletion2, dictionary=sc_combined.df.corpus.copy)

##convert the list to Corpus again
sc_combined.df.corpus= Corpus(VectorSource(sc_combined.df.corpus))

sink("stemwords_sc_combined_after.txt") 
for (i in c(1:3000, 6924)) {
  cat(paste0("[", i, "] "))
  #put the output to a file for inspection
  writeLines(strwrap(as.character(sc_combined.df.corpus[[i]]), 60) )
}
sink()

#####Correcting mis-spelt words

replaceWord <- function(corpus, oldword, newword)
{
  tm_map(corpus, gsub, pattern=oldword, replacement=newword)
}

sc_combined.df.corpus<- replaceWord(sc_combined.df.corpus, "shopclu", "")
sc_combined.df.corpus<- replaceWord(sc_combined.df.corpus, "deliveryplease", "delivery please")
sc_combined.df.corpus<- replaceWord(sc_combined.df.corpus, "deliv", "delivery")
sc_combined.df.corpus<- replaceWord(sc_combined.df.corpus, "delivering", "delivery")
sc_combined.df.corpus<- replaceWord(sc_combined.df.corpus, "deliveryi", "delivery")
sc_combined.df.corpus<- replaceWord(sc_combined.df.corpus, "deliverybut", "delivery")
sc_combined.df.corpus<- replaceWord(sc_combined.df.corpus, "deliver", "delivery")
sc_combined.df.corpus<- replaceWord(sc_combined.df.corpus, "deliveryy", "delivery")

#####remove unwanted words
sc_combined.df.corpus <- tm_map(sc_combined.df.corpus, removeWords, c("also", "article", "Article", "download", "google", "figure","fig", "groups","Google", "however","high", "human", "levels","larger", "may", "number","shown", "study", "studies", "this","using", "two", "the", "Scholar","pubmedncbi", "PubMedNCBI","view", "View", "the", "biol","via", "image", "doi", "one",    
                                                                          "analysis","how","which","when","there","myntra","the","th","for","then","when","httpstcodfvdjzyy"))



#####save the cleaned corpus for future epidemics:is very important else we have to start from begining
sc_combined.df.corpus.cleaned = sc_combined.df.corpus
save.image(file="sc_combined.df.corpus.cleaned.RData")



