#==================== LOAD LIBRARIES =========================
library(rtweet)
library(tidyverse)
library(janitor)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(curl)

# Get user info
vivo<- get_timeline("vivoenergykenya", n= 2000)
vivo<- janitor::remove_empty_cols(vivo)
readr::write_csv(vivo,"vivotweets.csv")
vivo<- as_tibble(vivo) 
head(vivo) %>% View()

#all further operations carried out on read in file
#read in csv file
vivo.tw<-read_csv("vivotweets.csv",col_names = T)
vivo.tw<- janitor::remove_empty_cols(vivo.tw)



#========================== HASHTAGS =======================
#The unique hashtags used by the vivo twitter account

hashtags.u<-unique(vivo.tw$hashtags)
hashtags.u %>% as.matrix() %>% View()
#separate where there are multiple hashtags
#each on its own line
paste(hashtags,sep=" ") %>% as.matrix() %>% View()
#Finding all the unique hashtags used by vivo
#not that all words have been converted to lowercase
hashtags.n<- vivo.tw %>% unnest_tokens(tags,hashtags)
hashtags.n %>% as.matrix() %>% View()
tags<- hashtags.n$tags %>% unique()
tags<- tags[is.na(tags)!=TRUE]
#Not including hashtags in retweets
hashtags.n2<- vivo.tw[vivo.tw$is_retweet==FALSE,] %>% unnest_tokens(tags,hashtags)
hashtags.n2 %>% as.matrix() %>% View()
tags2<- hashtags.n2$tags %>% unique()
tags2<- tags2[is.na(tags2)!=TRUE]
#Different number of unique tags without including retweets i.e tags != tags2
tags.cloud<- hashtags.n2 %>% count(tags, sort = T)
tags.cloud<- tags.cloud[is.na(tags.cloud$tags)==FALSE,]
colnames(tags_cloud)<- c("word","freq")
#wordcloud
wordcloud(tags.cloud$word, tags.cloud$freq,max.words = 70, colors = brewer.pal(6,"Dark2"))
#How to plot the hashtags? geom_text? how to map aesthetics
#Use a wordcloud ofcourse

#====================== FREQUENT WORDS ==========================
#What are the most frequently used words?

#get rid of links
vivo.tidy<- vivo.tw
vivo.tidy$text<- gsub('http.*\\s*', '', vivo.tidy$text)
#one word per row, remove stop words and count
vivo.tidy<- vivo.tidy %>% unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort=T)
#create wordcloud
set.seed(120)
wordcloud(vivo.tidy$word,vivo.tidy$n, max.words = 150,colors = brewer.pal(8,"Paired"))

#======================== POPULAR TWEETS ======================
#How popular are vivo tweets?
#Compare this to other oil marketers

ggplot(vivo.tw) +
  geom_histogram(aes(favorite_count), binwidth = 4) +
  xlim(NA,40)
  

ggplot(vivo.tw) +
  geom_histogram(aes(retweet_count)) +
  xlim(NA,40)

ggplot(vivo.tw, aes(retweet_count, favorite_count)) +
  geom_point()

arrange(vivo.tw, desc(retweet_count)) %>% head() %>% View()
#We find that the highest retweets belong to other users posts retweeted by vivo

#Same plots but with retweeted tweets by vivo removed
ggplot(vivo.tw[vivo.tw$is_retweet==FALSE,], aes(retweet_count, favorite_count)) +
  geom_point()
#much stronger correlation.
ggplot(vivo.tw[vivo.tw$is_retweet==FALSE,]) +
  geom_histogram(aes(retweet_count))

ggplot(vivo.tw[vivo.tw$is_retweet==FALSE,]) +
  geom_histogram(aes(favorite_count))

#find most popular tweets
#by retweet count
arrange(vivo.tw[vivo.tw$is_retweet==FALSE,], desc(retweet_count)) %>% head(n=10) %>%
  View()
#by favorite count
arrange(vivo.tw[vivo.tw$is_retweet==FALSE,], desc(favorite_count)) %>% head(n=10) %>%
  View()

#sentiment analysis of responses to fellow twitter users
#sentiment analysis of all tweets as a whole not including retweets by vivo

#======================= DATE OF PUBLISHING ======================
#When are blog posts published?
days<-c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
vivo.tw<- vivo.tw %>% mutate(wday= created_at %>% weekdays() %>% factor(levels=days,ordered=T), 
                   month= created_at %>% months() %>% as.factor(),
                   year= created_at %>% format("%Y") )
vivo.wd<- vivo.tw %>% group_by(wday) %>% summarise(n=n())

ggplot(vivo.wd, aes(wday,n)) +
  geom_bar(stat = "identity")
#Most common days are monday wednesday and friday

#Most common time for publishing posts?
#split into morning, noon, afternoon, evening, night.

