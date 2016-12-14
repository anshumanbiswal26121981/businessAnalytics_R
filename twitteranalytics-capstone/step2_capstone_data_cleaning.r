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

#Clean amazon data
amazon_combined.df=read.csv("amazon_combined.csv",header = TRUE,stringsAsFactors = FALSE,sep = ",")
amazon_combined.df=amazon_combined.df[,-c(1)]
# Remove character string between < >
amazon_combined.df$text=genX(amazon_combined.df$text, " <", ">")

# Create document corpus with tweet text
amazon_combined.corpus<- Corpus(VectorSource(amazon_combined.df$text))

#####convert to Lowercase  
amazon_combined.corpus <- tm_map(amazon_combined.corpus, content_transformer(stri_trans_tolower))

#####Remove the links (URLs)  
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)  
amazon_combined.corpus <- tm_map(amazon_combined.corpus, content_transformer(removeURL))

#####Remove anything except the english language and space  
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)  
amazon_combined.corpus <- tm_map(amazon_combined.corpus, content_transformer(removeNumPunct))

#####Remove Stopwords  
myStopWords<- c((stopwords('english')),c("rt", "use", "used", "via", "amp","should","go","then","what","when","where","than","amazon","amazonIn"))
amazon_combined.corpus <- tm_map(amazon_combined.corpus, removeWords,myStopWords)

#####Remove Single letter words  
removeSingle <- function(x) gsub(" . ", " ", x) 
amazon_combined.corpus <- tm_map(amazon_combined.corpus, content_transformer(removeSingle))

#####Remove Extra Whitespaces  
amazon_combined.corpus<- tm_map(amazon_combined.corpus, stripWhitespace) 
#####keep a copy of "amazon.corpus" for stem completion later 
amazon_combined.corpus.copy<- amazon_combined.corpus
save(amazon_combined.corpus.copy, file = "amazon_combined.corpus.copy.RData")


#Clean flipkart data
flipkart_handle.df=read.csv("flipkart_handle.csv",header = TRUE,stringsAsFactors = FALSE,sep = ",")
flipkart_handle.df=flipkart_handle.df[,-c(1)]
# Remove character string between < >
flipkart_handle.df$text=genX(flipkart_handle.df$text, " <", ">")

# Create document corpus with tweet text
flipkart_handle.df.corpus<- Corpus(VectorSource(flipkart_handle.df$text))

#####convert to Lowercase  
flipkart_handle.df.corpus <- tm_map(flipkart_handle.df.corpus, content_transformer(stri_trans_tolower))

#####Remove the links (URLs)  
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)  
flipkart_handle.df.corpus <- tm_map(flipkart_handle.df.corpus, content_transformer(removeURL))

#####Remove anything except the english language and space  
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)  
flipkart_handle.df.corpus <- tm_map(flipkart_handle.df.corpus, content_transformer(removeNumPunct))

#####Remove Stopwords  
myStopWords<- c((stopwords('english')),c("rt", "use", "used", "via", "amp","should","go","then","what","when","where","than","flipkart"))
flipkart_handle.df.corpus <- tm_map(flipkart_handle.df.corpus, removeWords,myStopWords)

#####Remove Single letter words  
removeSingle <- function(x) gsub(" . ", " ", x) 
flipkart_handle.df.corpus <- tm_map(flipkart_handle.df.corpus, content_transformer(removeSingle))

#####Remove Extra Whitespaces  
flipkart_handle.df.corpus<- tm_map(flipkart_handle.df.corpus, stripWhitespace) 
#####keep a copy of "amazon.corpus" for stem completion later 
flipkart_handle.df.corpus.copy<- flipkart_handle.df.corpus
save(flipkart_handle.df.corpus.copy, file = "flipkart_handle.df.corpus.copy.RData")


# Clean snapdeal
snapdeal_combined.df=read.csv("snapdeal_combined.csv",header = TRUE,stringsAsFactors = FALSE,sep = ",")
snapdeal_combined.df=snapdeal_combined.df[,-c(1)]
# Remove character string between < >
snapdeal_combined.df$text=genX(snapdeal_combined.df$text, " <", ">")

# Create document corpus with tweet text
snapdeal_combined.df.corpus<- Corpus(VectorSource(snapdeal_combined.df$text))

#####convert to Lowercase  
snapdeal_combined.df.corpus <- tm_map(snapdeal_combined.df.corpus, content_transformer(stri_trans_tolower))

#####Remove the links (URLs)  
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)  
snapdeal_combined.df.corpus <- tm_map(snapdeal_combined.df.corpus, content_transformer(removeURL))

#####Remove anything except the english language and space  
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)  
snapdeal_combined.df.corpus <- tm_map(snapdeal_combined.df.corpus, content_transformer(removeNumPunct))

#####Remove Stopwords  
myStopWords<- c((stopwords('english')),c("rt", "use", "used", "via", "amp","should","go","then","what","when","where","than","snapdeal"))
snapdeal_combined.df.corpus <- tm_map(snapdeal_combined.df.corpus, removeWords,myStopWords)

#####Remove Single letter words  
removeSingle <- function(x) gsub(" . ", " ", x) 
snapdeal_combined.df.corpus <- tm_map(snapdeal_combined.df.corpus, content_transformer(removeSingle))

#####Remove Extra Whitespaces  
snapdeal_combined.df.corpus<- tm_map(snapdeal_combined.df.corpus, stripWhitespace) 
#####keep a copy of "amazon.corpus" for stem completion later 
snapdeal_combined.df.corpus.copy<- snapdeal_combined.df.corpus
save(snapdeal_combined.df.corpus.copy, file = "snapdeal_combined.df.corpus.copy.RData")



#Cleaning ebay data

ebay_combined.df=read.csv("ebay_combined.csv",header = TRUE,stringsAsFactors = FALSE,sep = ",")
ebay_combined.df=ebay_combined.df[,-c(1)]
# Remove character string between < >
ebay_combined.df$text=genX(ebay_combined.df$text, " <", ">")

# Create document corpus with tweet text
ebay_combined.df.corpus<- Corpus(VectorSource(ebay_combined.df$text))

#####convert to Lowercase  
ebay_combined.df.corpus <- tm_map(ebay_combined.df.corpus, content_transformer(stri_trans_tolower))

#####Remove the links (URLs)  
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)  
ebay_combined.df.corpus <- tm_map(ebay_combined.df.corpus, content_transformer(removeURL))

#####Remove anything except the english language and space  
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)  
ebay_combined.df.corpus <- tm_map(ebay_combined.df.corpus, content_transformer(removeNumPunct))

#####Remove Stopwords  
myStopWords<- c((stopwords('english')),c("rt", "use", "used", "via", "amp","should","go","then","what","when","where","than","ebay"))
ebay_combined.df.corpus <- tm_map(ebay_combined.df.corpus, removeWords,myStopWords)

#####Remove Single letter words  
removeSingle <- function(x) gsub(" . ", " ", x) 
ebay_combined.df.corpus <- tm_map(ebay_combined.df.corpus, content_transformer(removeSingle))

#####Remove Extra Whitespaces  
ebay_combined.df.corpus<- tm_map(ebay_combined.df.corpus, stripWhitespace) 
#####keep a copy of "amazon.corpus" for stem completion later 
ebay_combined.df.corpus.copy<- ebay_combined.df.corpus
save(ebay_combined.df.corpus.copy, file = "ebay_combined.df.corpus.copy.RData")



#Cleaning shop clues  data

sc_combined.df=read.csv("sc_combined.csv",header = TRUE,stringsAsFactors = FALSE,sep = ",")
sc_combined.df=sc_combined.df[,-c(1)]
# Remove character string between < >
sc_combined.df$text=genX(sc_combined.df$text, " <", ">")

# Create document corpus with tweet text
sc_combined.df.corpus<- Corpus(VectorSource(sc_combined.df$text))

#####convert to Lowercase  
sc_combined.df.corpus <- tm_map(sc_combined.df.corpus, content_transformer(stri_trans_tolower))

#####Remove the links (URLs)  
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)  
sc_combined.df.corpus <- tm_map(sc_combined.df.corpus, content_transformer(removeURL))

#####Remove anything except the english language and space  
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)  
sc_combined.df.corpus <- tm_map(sc_combined.df.corpus, content_transformer(removeNumPunct))

#####Remove Stopwords  
myStopWords<- c((stopwords('english')),c("rt", "use", "used", "via", "amp","should","go","then","what","when","where","than","shopClues","ShopClues"))
sc_combined.df.corpus <- tm_map(sc_combined.df.corpus, removeWords,myStopWords)

#####Remove Single letter words  
removeSingle <- function(x) gsub(" . ", " ", x) 
sc_combined.df.corpus <- tm_map(sc_combined.df.corpus, content_transformer(removeSingle))

#####Remove Extra Whitespaces  
sc_combined.df.corpus<- tm_map(sc_combined.df.corpus, stripWhitespace) 
#####keep a copy of "amazon.corpus" for stem completion later 
sc_combined.df.corpus.copy<- sc_combined.df.corpus
save(sc_combined.df.corpus.copy, file = "sc_combined.df.corpus.copy.RData")


