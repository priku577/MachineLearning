library(httr)
library(stringr)
library(twitteR)
library(RCurl)
library(plyr)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(ROAuth)
library(syuzhet)
library(tm)
library(plotrix)
setwd("C:/Users/G/Desktop/desktop files")
Consumer_Key<- "pYpMGU2LWnSP5DxRAq1NRXKAv"
Consumer_Secret<-	"xPJ09MA9Oz1M2IES6ipcWBoREUWHsPXPhf31i6bQ45JSYQv9Pb"
Access_Token<- "990608111336611840-xHZPvihMYgxqUdaE2V1QWpY3NkDdMFu"
Access_Token_Secret<-"n86Tar8D5NPqpOJg96eGAqYKqUOV2AjwwG5kljtzWf64l"
setup_twitter_oauth(Consumer_Key,Consumer_Secret,Access_Token,Access_Token_Secret)
my_tweet<-searchTwitter("ISIS",n=100,lang="en",since='2018-04-01',until='2018-05-05')
my_tweet1<- sapply(my_tweet,function(x) x$getText())

my_tweet1<- sapply(my_tweet,function(x) x$getText())
cleaning<-function(text)
{
  my_data=text
  corpus<-iconv(my_data,to="UTF-8")
  corpus<-Corpus(VectorSource(corpus))
  
  #remove retweet
  
  
  
  #removing the url
  remove_url = function(x)gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", x) 
  corpus<-tm_map(corpus,content_transformer(remove_url))
  
  
  #remove emojis
  remove_emo<- function(x)gsub("latin1", "", x)
  corpus<-tm_map(corpus,content_transformer(remove_emo))
  
  #removing numbers
  
  corpus<-tm_map(corpus,removeNumbers)
  
  
  #removing words starting with @ and #
  remove_names<- function(x)gsub("(@\\w+ *:)|(@\\w+ *)|(#\\w+ *)", "", x)
  corpus<-tm_map(corpus,content_transformer(remove_names))
  
  #removing punctations
  corpus<-tm_map(corpus,removePunctuation)
  
  
  #convert to lower case
  corpus<-tm_map(corpus,tolower)
  
  
  #remove stop words
  corpus<-tm_map(corpus,removeWords,stopwords('SMART'))
  
  
  #removing other words
  remove_words<-c("the","if","they","but"," rt"," ahhhhh ","amp")
  remove_w<- function(x)gsub("remove_words", "", x)
  corpus<-tm_map(corpus,content_transformer(remove_w))
  
  
  ##substituting control words with empty
  sentence <- function(x)gsub('[[:cntrl:]]','',x)
  corpus<-tm_map(corpus,content_transformer(sentence))
  
  ##removing decimal numbers
  sentence_cntrl<-function(x)gsub('\\d+','',x) 
  corpus<-tm_map(corpus,content_transformer(sentence_cntrl))
  
  #removes new lines
  new_line <-function(a)gsub('\n','',a) 
  corpus<-tm_map(corpus,content_transformer(new_line))
  
  
  #strip white spaces
  corpus<-tm_map(corpus,stripWhitespace)
  
  
  #removing crazy characters
  removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
  corpus <- tm_map(corpus, content_transformer(removeNumPunct))
  
  #removing â
  
  aa<-function(a)gsub("\\w*â","",a)
  corpus <- tm_map(corpus, content_transformer(aa))
  
  aa<-function(a)gsub("í","",a)
  corpus <- tm_map(corpus, content_transformer(aa))
  
  #removing rt
  remove_rt_n <-function(a) gsub("rt", "", a)
  corpus <- tm_map(corpus, content_transformer(remove_rt_n))
  
  #removing amp
  clean_tweet <-function(a) gsub("&amp", "", a)
  corpus <- tm_map(corpus, content_transformer(clean_tweet))
  
  return(inspect(corpus))
}
cleaned_data<-cleaning(my_tweet1)


df <- do.call("rbind", lapply(cleaned_data, as.data.frame,stringsAsFactors=F))
names(df)<-c("text")
sample <- df$text  # why this sample

pos.words = scan('poswords.txt', what='character', comment.char=';')
neg.words = scan('neg3.txt', what='character', comment.char=';')

# pos_list<-unique(str_extract(words, paste(pos.words, collapse="|")))
# neg_list<-str_extract(words, paste(neg.words, collapse="|"))

senti_words<-function(sample,pos.words,neg.words)
{
  ##sentences are splitted ..stringr library does that
  word.list = str_split(sample, '\\s+')
  #unlisting and making as character vector
  words = unlist(word.list, use.names=FALSE) 
  pos_list<-unique(str_extract(words, paste(pos.words, collapse="|")))
  neg_list<-unique(str_extract(words, paste(neg.words, collapse="|")))
  pos_df<-data.frame(pos_list)
  neg_df<-data.frame(neg_list)
  names(pos_df)<-c("words")
  names(neg_df)<-c("words")
  new_pos<-na.omit(pos_df)
  new_neg<-na.omit(neg_df)
  list1<-list(new_pos,new_neg)
  return(list1)
}
result<-senti_words(sample,pos.words,neg.words)
result
no_of_p<-dim(result[[1]])[1]
cat("The number of positive words are",no_of_p)
no_of_n<-dim(result[[2]])[1]
cat("The number of negative words are",no_of_n)

if(no_of_p>no_of_n)
{
  print("The tweets have positive setiment")
}else
{
  print("The tweets have negative setiment")
}


tdm<-TermDocumentMatrix(cleaned_data)
tdm<-as.matrix(tdm)
# tdm[1:10,1:20]
w<-rowSums(tdm)
w<-subset(w,w>=3)
barplot(w,
        las=2,
        col=rainbow(50))
w<-sort(rowSums(tdm),decreasing = TRUE)
set.seed(222)
# #rot.per and min.freq and scale=size
wordcloud(words = names(w),
          freq=w,
          max.words = 150,
          random.order = F,
          random.color = F,
          min.freq = 3,
          scale = c(5,0.6),
          rot.per = 0.7,
          col=rainbow(50))
# w<-data.frame(names(w),w)
# wordcloud2(w,
#            size=2,
#            shape='ellipse',
#            minSize = 6)

slices <- c(no_of_p,no_of_n)
lbls <- c("Positive", "Negative")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of news")

pie3D(slices,labels=lbls,explode=0.1,
      main="Pie Chart of news ")


