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

load("amazon_combined.corpus.copy.RData")
amazon_combined.corpus = amazon_combined.corpus.copy

#####Stem words in the corpus 
amazon_combined.corpus=tm_map(amazon_combined.corpus, stemDocument)

writeLines(strwrap(amazon_combined.corpus[[250]]$content,60))

##### inspect the first 100 documents (tweets)
##### The code below is used for to make text fit for paper width
##### put the output to a file for inspection
#put the output back to

#####The above was used for our analysis,as this would print all the 8107 values so for documenting shake we will look up only first 5  lines
# for (i in c(1:5, 8107)) {
#   cat(paste0("[", i, "] "))
#   writeLines(strwrap(as.character(amazon_combined_twitter.corpus[[i]]), 60) )
# }

#####Function to correct/complete the text after stemming

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
amazon_combined.corpus=lapply(amazon_combined.corpus,stemCompletion2, dictionary=amazon_combined.corpus.copy)

##convert the list to Corpus again
amazon_combined.corpus= Corpus(VectorSource(amazon_combined.corpus))

sink("stemwords_amazon_combined_after.txt") 
for (i in c(1:3000, 10394)) {
  cat(paste0("[", i, "] "))
  #put the output to a file for inspection
  writeLines(strwrap(as.character(amazon_combined.corpus[[i]]), 60) )
}
sink()

#####Correcting mis-spelt words

replaceWord <- function(corpus, oldword, newword)
{
  tm_map(corpus, gsub, pattern=oldword, replacement=newword)
}

amazon_combined.corpus<- replaceWord(amazon_combined.corpus, "amazon", "")
amazon_combined.corpus<- replaceWord(amazon_combined.corpus, "helpfacts", "help facts")
amazon_combined.corpus<- replaceWord(amazon_combined.corpus, "amazonin", "")
amazon_combined.corpus<- replaceWord(amazon_combined.corpus, "amazonhelp", "")
amazon_combined.corpus<- replaceWord(amazon_combined.corpus, "experi", "experiment")
amazon_combined.corpus<- replaceWord(amazon_combined.corpus, "backhvnt", "back havent")
amazon_combined.corpus<- replaceWord(amazon_combined.corpus, "recvd", "received")
amazon_combined.corpus<- replaceWord(amazon_combined.corpus, "delivary", "delivery")
amazon_combined.corpus<- replaceWord(amazon_combined.corpus, "definitaly", "definitely")
amazon_combined.corpus<- replaceWord(amazon_combined.corpus, "referandearn", "refer and earn")
amazon_combined.corpus<- replaceWord(amazon_combined.corpus, "festiva", "festival")
amazon_combined.corpus<- replaceWord(amazon_combined.corpus, "deliveries", "delivery")
#####remove unwanted words
amazon_combined.corpus <- tm_map(amazon_combined.corpus, removeWords, c("also", "article", "Article", "download", "google", "figure","fig", "groups","Google", "however","high", "human", "levels","larger", "may", "number","shown", "study", "studies", "this","using", "two", "the", "Scholar","pubmedncbi", "PubMedNCBI","view", "View", "the", "biol","via", "image", "doi", "one",    
                                                                                        "analysis","how","which","when","there","amazonin","the","th","for","then","when"))


sink("stemwords_amazon_combined_after1.txt") 
for (i in c(1:3000, 10394)) {
  cat(paste0("[", i, "] "))
  #put the output to a file for inspection
  writeLines(strwrap(as.character(amazon_combined.corpus[[i]]), 60) )
}
sink()

#####save the cleaned corpus for future epidemics:is very important else we have to start from begining
amazon_combined.corpus.cleaned = amazon_combined.corpus
save.image(file="amazon_combined.corpus.cleaned.RData")