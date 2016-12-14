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

load("flipkart_handle.df.corpus.copy.RData")
flipkart_handle.df.corpus = flipkart_handle.df.corpus.copy

#####Stem words in the corpus 
flipkart_handle.df.corpus=tm_map(flipkart_handle.df.corpus, stemDocument)




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
flipkart_handle.df.corpus=lapply(flipkart_handle.df.corpus,stemCompletion2, dictionary=flipkart_handle.df.corpus.copy)

##convert the list to Corpus again
flipkart_handle.df.corpus= Corpus(VectorSource(flipkart_handle.df.corpus))

sink("stemwords_flipkart_combined_after.txt") 
for (i in c(1:2400, 10000)) {
  cat(paste0("[", i, "] "))
  #put the output to a file for inspection
  writeLines(strwrap(as.character(flipkart_handle.df.corpus[[i]]), 60) )
}
sink()

#####Correcting mis-spelt words

replaceWord <- function(corpus, oldword, newword)
{
  tm_map(corpus, gsub, pattern=oldword, replacement=newword)
}

flipkart_handle.df.corpus<- replaceWord(flipkart_handle.df.corpus, "flipkart", "")
flipkart_handle.df.corpus<- replaceWord(flipkart_handle.df.corpus, "deliveries", "delivery")

#####remove unwanted words
flipkart_handle.df.corpus <- tm_map(flipkart_handle.df.corpus, removeWords, c("also", "article", "Article", "download", "google", "figure","fig", "groups","Google", "however","high", "human", "levels","larger", "may", "number","shown", "study", "studies", "this","using", "two", "the", "Scholar","pubmedncbi", "PubMedNCBI","view", "View", "the", "biol","via", "image", "doi", "one",    
                                                                        "analysis","how","which","when","there","flipkart","the","th","for","then","when"))


sink("stemwords_aflipkart_combined_after1.txt") 
for (i in c(1:2400, 10000)) {
  cat(paste0("[", i, "] "))
  #put the output to a file for inspection
  writeLines(strwrap(as.character(flipkart_handle.df.corpus[[i]]), 60) )
}
sink()

#####save the cleaned corpus for future epidemics:is very important else we have to start from begining
flipkart_handle.df.corpus.cleaned = flipkart_handle.df.corpus
save.image(file="flipkart_handle.df.corpus.cleaned.RData")
