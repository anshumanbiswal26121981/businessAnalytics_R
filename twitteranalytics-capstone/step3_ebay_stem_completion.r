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

load("ebay_combined.df.corpus.copy.RData")
ebay_combined.df.corpus = ebay_combined.df.corpus.copy

#####Stem words in the corpus 
ebay_combined.df.corpus=tm_map(ebay_combined.df.corpus, stemDocument)

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
ebay_combined.df.corpus=lapply(ebay_combined.df.corpus,stemCompletion2, dictionary=ebay_combined.df.corpus.copy)

##convert the list to Corpus again
ebay_combined.df.corpus= Corpus(VectorSource(ebay_combined.df.corpus))

sink("stemwords_ebay_combined_after.txt") 
for (i in c(1:3000, 11408)) {
  cat(paste0("[", i, "] "))
  #put the output to a file for inspection
  writeLines(strwrap(as.character(ebay_combined.df.corpus[[i]]), 60) )
}
sink()

#####Correcting mis-spelt words

replaceWord <- function(corpus, oldword, newword)
{
  tm_map(corpus, gsub, pattern=oldword, replacement=newword)
}

ebay_combined.df.corpus<- replaceWord(ebay_combined.df.corpus, "ebay", "")
ebay_combined.df.corpus<- replaceWord(ebay_combined.df.corpus, "ebayindia", "")
ebay_combined.df.corpus<- replaceWord(ebay_combined.df.corpus, "deliv", "delivery")
ebay_combined.df.corpus<- replaceWord(ebay_combined.df.corpus, "delivering", "delivery")



#####remove unwanted words
ebay_combined.df.corpus <- tm_map(ebay_combined.df.corpus, removeWords, c("also", "article", "Article", "download", "google", "figure","fig", "groups","Google", "however","high", "human", "levels","larger", "may", "number","shown", "study", "studies", "this","using", "two", "the", "Scholar","pubmedncbi", "PubMedNCBI","view", "View", "the", "biol","via", "image", "doi", "one",    
                                                                            "analysis","how","which","when","there","myntra","the","th","for","then","when","httpstcodfvdjzyy"))


sink("stemwords_myntra_combined_after1.txt") 
for (i in c(1:100, 5047)) {
  cat(paste0("[", i, "] "))
  #put the output to a file for inspection
  writeLines(strwrap(as.character(myntra_combined.corpus[[i]]), 60) )
}
sink()

#####save the cleaned corpus for future epidemics:is very important else we have to start from begining
ebay_combined.df.corpus.cleaned = ebay_combined.df.corpus
save.image(file="ebay_combined.df.corpus.cleaned.RData")

