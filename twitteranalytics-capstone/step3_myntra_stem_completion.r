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

myntra_combined.corpus<- replaceWord(myntra_combined.corpus, "kudosfor", "kudos for")
myntra_combined.corpus<- replaceWord(myntra_combined.corpus, "titanwatches", "titan watches")
myntra_combined.corpus<- replaceWord(myntra_combined.corpus, "theraymondltd", "the raymond ltd")
myntra_combined.corpus<- replaceWord(myntra_combined.corpus, "mensdaynov", "mens day nov")
myntra_combined.corpus<- replaceWord(myntra_combined.corpus, "bigbasketcom", "bigbasket com")
myntra_combined.corpus<- replaceWord(myntra_combined.corpus, "getsimpl", "get simple")
myntra_combined.corpus<- replaceWord(myntra_combined.corpus, "keepitsimpl", "keep it simple")
myntra_combined.corpus<- replaceWord(myntra_combined.corpus, "celebratemensdaynov", "celebrate mens day nov")
myntra_combined.corpus<- replaceWord(myntra_combined.corpus, "eitherdisappointed", "either disappointed")
myntra_combined.corpus<- replaceWord(myntra_combined.corpus, "voguebffs", "vogue bffs")
myntra_combined.corpus<- replaceWord(myntra_combined.corpus, "yuplaygod", "you play god")
myntra_combined.corpus<- replaceWord(myntra_combined.corpus, "aliaa", "alias")
myntra_combined.corpus<- replaceWord(myntra_combined.corpus, "wiht", "with")
myntra_combined.corpus<- replaceWord(myntra_combined.corpus, "agartalareturned", "agartala returned")
myntra_combined.corpus<- replaceWord(myntra_combined.corpus, "serviceitem", "service item")
myntra_combined.corpus<- replaceWord(myntra_combined.corpus, "patheticexperience", "pathetic experience")
myntra_combined.corpus<- replaceWord(myntra_combined.corpus, "carehate", "care hate")


#####remove unwanted words
myntra_combined.corpus <- tm_map(myntra_combined.corpus, removeWords, c("also", "article", "Article", "download", "google", "figure","fig", "groups","Google", "however","high", "human", "levels","larger", "may", "number","shown", "study", "studies", "this","using", "two", "the", "Scholar","pubmedncbi", "PubMedNCBI","view", "View", "the", "biol","via", "image", "doi", "one",    
                                                                            "analysis","how","which","when","there","myntra","the","th","for","then","when","httpstcodfvdjzyy"))


sink("stemwords_myntra_combined_after1.txt") 
for (i in c(1:100, 5047)) {
  cat(paste0("[", i, "] "))
  #put the output to a file for inspection
  writeLines(strwrap(as.character(myntra_combined.corpus[[i]]), 60) )
}
sink()

#####save the cleaned corpus for future epidemics:is very important else we have to start from begining
myntra_combined.corpus.cleaned = myntra_combined.corpus
save.image(file="myntra_combined.corpus.cleaned.RData")

