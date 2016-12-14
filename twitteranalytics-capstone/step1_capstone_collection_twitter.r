###import required packages
rm(list=ls())
require(twitteR)
require(RCurl)
require(tm)
library(qdap)
library(ROAuth)
library(data.table)

setwd("C:\\AWBackup\\PGPBA\\PGPBA-GreatLakes\\Modules\\capstoneproject\\data")


consumer_key = 'vitqhIlWGWfRUOUP8JIixb7OB'
consumer_secret = 'gJrNUmTl99eU3bTpUQTnBhO1zs3TST1qluwSjyozfkTm4FOGjW'
access_token = '3306555647-NpzZt0Af4T06E5ea9EWlVQeGRfE4wF4fvQBOBMg'
access_token_secret = '5Y4ojfc0LO8UtHmjrGVdhMlsF9AjkjWIxluBmofH45v2y'


setup_twitter_oauth(consumer_key,consumer_secret,access_token, access_token_secret)



###Amazon data collection
Sys.sleep(5000)
amazon_handle<-searchTwitteR("@amazonIN",n = 10000,lang = "en",since='2016-11-01')
amazon_handle.df <- do.call(rbind, lapply(amazon_handle, as.data.frame))
save(amazon_handle, file = "amazon_handle.RData")
write.csv(amazon_handle.df,"amazon_handle.csv")

amazon_word<-searchTwitteR("amazonIN",n = 4000,lang = "en",since='2016-11-01')
amazon_word.df <- do.call(rbind, lapply(amazon_word, as.data.frame))
save(amazon_word, file = "amazon_word.RData")
write.csv(amazon_word.df,"amazon_word.csv")

amazon_combined.df= rbind(amazon_handle.df,amazon_word.df)
save(amazon_combined.df, file = "amazon_combined.RData")
write.csv(amazon_combined.df,"amazon_combined.csv")

####Flipkart data collection
flipkart_handle<-searchTwitteR("@Flipkart",n = 10000,lang = "en",since='2016-11-01')
save(flipkart_handle, file = "flipkart_handle.RData")
flipkart_handle.df <- do.call(rbind, lapply(flipkart_handle, as.data.frame))
write.csv(flipkart_handle.df,"flipkart_handle.csv")



####Snapdeal data collection
snapdeal_handle<-searchTwitteR("@snapdeal",n = 10000,lang = "en",since='2016-11-01')
save(snapdeal_handle, file = "snapdeal_handle.RData")
snapdeal_handle.df <- do.call(rbind, lapply(snapdeal_handle, as.data.frame))
write.csv(snapdeal_handle.df,"snapdeal_handle.csv")

snapdeal_hashtag<-searchTwitteR("#snapdeal",n = 3000,lang = "en",since='2016-11-01')
save(snapdeal_hashtag, file = "snapdeal_hashtag.RData")
snapdeal_hashtag.df <- do.call(rbind, lapply(snapdeal_hashtag, as.data.frame))
write.csv(snapdeal_hashtag.df,"snapdeal_hash.csv")

snapdeal_word<-searchTwitteR("snapdeal",n = 3000,lang = "en",since='2016-11-01')
save(snapdeal_word, file = "snapdeal_word.RData")
snapdeal_word.df <- do.call(rbind, lapply(snapdeal_word, as.data.frame))
write.csv(snapdeal_word.df,"snapdeal_word.csv")


snapdeal_help<-searchTwitteR("@Snapdeal_Help",n = 3000,lang = "en",since='2016-11-01')
save(snapdeal_help, file = "snapdeal_help.RData")
snapdeal_help.df <- do.call(rbind, lapply(snapdeal_help, as.data.frame))
write.csv(snapdeal_help.df,"snapdeal_help.csv")

snapdeal_combined.df = rbind(snapdeal_handle.df,snapdeal_hashtag.df,snapdeal_word.df,snapdeal_help.df)
write.csv(snapdeal_combined.df,"snapdeal_combined.csv")
save(snapdeal_combined, file = "snapdeal_combined.RData")


####Ebay data collection
Sys.sleep(5000)
ebay_handle<-searchTwitteR("@ebayindia",n = 10000,lang = "en",since='2016-11-01')
save(ebay_handle, file = "ebay_handle.RData")
ebay_handle.df <- do.call(rbind, lapply(ebay_handle, as.data.frame))
write.csv(ebay_handle.df,"ebay_handle.csv")

ebay_hash<-searchTwitteR("#ebay",n = 10000,lang = "en",since='2016-11-01')
save(ebay_hash, file = "ebay_hash.RData")
ebay_hash.df <- do.call(rbind, lapply(ebay_hash, as.data.frame))
write.csv(ebay_hash.df,"ebay_hash.csv")


ebay_word<-searchTwitteR("ebay",n = 3000,lang = "en",since='2016-11-01')
save(ebay_word, file = "ebay_word.RData")
ebay_word.df <- do.call(rbind, lapply(ebay_word, as.data.frame))
write.csv(ebay_word.df,"ebay_word.csv")

ebay_combined.df = rbind(ebay_handle.df,ebay_hash.df,ebay_word.df)
write.csv(ebay_combined.df,"ebay_combined.csv")
save(ebay_combined.df, file = "ebay_combined.RData")


####shopclues data collection
sc_handle<-searchTwitteR("@ShopClues  ",n = 10000,lang = "en",since='2016-11-01')
save(sc_handle, file = "sc_handle.RData")
sc_handle.df <- do.call(rbind, lapply(sc_handle, as.data.frame))
write.csv(ebay_handle.df,"sc_handle.csv")

sc_word<-searchTwitteR("ShopClues  ",n = 10000,lang = "en",since='2016-11-01')
save(sc_word, file = "sc_word.RData")
sc_word.df <- do.call(rbind, lapply(sc_word, as.data.frame))
write.csv(sc_word.df,"sc_word.csv")

sc_hash<-searchTwitteR("#ShopClues  ",n = 10000,lang = "en",since='2016-11-01')
save(sc_hash, file = "sc_hash.RData")
sc_hash.df <- do.call(rbind, lapply(sc_hash, as.data.frame))
write.csv(sc_hash.df,"sc_hash.csv")

sc_hash2<-searchTwitteR("#ShopClues!  ",n = 10000,lang = "en",since='2016-11-01')
save(sc_hash2, file = "sc_hash2.RData")
sc_hash2.df <- do.call(rbind, lapply(sc_hash2, as.data.frame))
write.csv(sc_hash2.df,"sc_hash2.csv")


sc_combined.df<-rbind(sc_handle.df,sc_word.df,sc_hash.df,sc_hash2.df)
write.csv(sc_combined.df,"sc_combined.csv")
save(sc_combined.df, file = "sc_combined.RData")