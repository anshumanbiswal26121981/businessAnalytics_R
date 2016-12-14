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

load("snapdeal_combined.df.corpus.copy.RData")
snapdeal_combined.corpus = snapdeal_combined.df.corpus.copy

#####Stem words in the corpus 
snapdeal_combined.corpus=tm_map(snapdeal_combined.corpus, stemDocument)

writeLines(strwrap(snapdeal_combined.corpus[[250]]$content,60))

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
snapdeal_combined.corpus=lapply(snapdeal_combined.corpus,stemCompletion2, dictionary=snapdeal_combined.df.corpus.copy)

##convert the list to Corpus again
snapdeal_combined.corpus= Corpus(VectorSource(snapdeal_combined.corpus))

sink("stemwords_snapdeal_combined_after.txt") 
for (i in c(1:2400, 10392)) {
  cat(paste0("[", i, "] "))
  #put the output to a file for inspection
  writeLines(strwrap(as.character(snapdeal_combined.corpus[[i]]), 60) )
}
sink()

#####Correcting mis-spelt words

replaceWord <- function(corpus, oldword, newword)
{
  tm_map(corpus, gsub, pattern=oldword, replacement=newword)
}

snapdeal_combined.corpus<- replaceWord(snapdeal_combined.corpus, "secretsashwinsanghionsnapdeal", "secret ashwinsanghi on snapdeal")
snapdeal_combined.corpus<- replaceWord(snapdeal_combined.corpus, "snapdeal", "")
snapdeal_combined.corpus<- replaceWord(snapdeal_combined.corpus, "snapdealhelp", "")
snapdeal_combined.corpus<- replaceWord(snapdeal_combined.corpus, "locationnot", "location not")
snapdeal_combined.corpus<- replaceWord(snapdeal_combined.corpus, "itemreturn", "item return")
snapdeal_combined.corpus<- replaceWord(snapdeal_combined.corpus, "orderyour", "order your")
snapdeal_combined.corpus<- replaceWord(snapdeal_combined.corpus, "customerservic", "customer service")
snapdeal_combined.corpus<- replaceWord(snapdeal_combined.corpus, "returnreplace", "return replace")
snapdeal_combined.corpus<- replaceWord(snapdeal_combined.corpus, "respons", "response")
snapdeal_combined.corpus<- replaceWord(snapdeal_combined.corpus, "st", "sent")
snapdeal_combined.corpus<- replaceWord(snapdeal_combined.corpus, "accusations", "acquisition")
snapdeal_combined.corpus<- replaceWord(snapdeal_combined.corpus, "deliv", "delivery")
snapdeal_combined.corpus<- replaceWord(snapdeal_combined.corpus, "dearzindagithisfriday", "")
snapdeal_combined.corpus<- replaceWord(snapdeal_combined.corpus, "deliveries", "delivery")

#####remove unwanted words
snapdeal_combined.corpus <- tm_map(snapdeal_combined.corpus, removeWords, c("also", "article", "Article", "download", "google", "figure","fig", "groups","Google", "however","high", "human", "levels","larger", "may", "number","shown", "study", "studies", "this","using", "two", "the", "Scholar","pubmedncbi", "PubMedNCBI","view", "View", "the", "biol","via", "image", "doi", "one",    
                                                                        "analysis","how","which","when","there","snapdeal","the","th","for","then","when"))



#####save the cleaned corpus for future epidemics:is very important else we have to start from begining
snapdeal_combined.corpus.cleaned = snapdeal_combined.corpus
save.image(file="snapdeal_combined.corpus.cleaned.RData")