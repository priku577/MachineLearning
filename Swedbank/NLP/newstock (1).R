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
setwd("C:/Users/HOME/Desktop/desktop files")
Consumer_Key<- "pYpMGU2LWnSP5DxRAq1NRXKAv"
Consumer_Secret<-	"xPJ09MA9Oz1M2IES6ipcWBoREUWHsPXPhf31i6bQ45JSYQv9Pb"
Access_Token<- "990608111336611840-xHZPvihMYgxqUdaE2V1QWpY3NkDdMFu"
Access_Token_Secret<-"n86Tar8D5NPqpOJg96eGAqYKqUOV2AjwwG5kljtzWf64l"
setup_twitter_oauth(Consumer_Key,Consumer_Secret,Access_Token,Access_Token_Secret)
my_tweet<-searchTwitter("ISIS",n=200,lang="en",since='2017-12-01',until='2018-05-02',retryOnRateLimit=250)
my_tweet1<- sapply(my_tweet,function(x) x$getText())
#my_tweet1<-my_tweet[1:5]



corpus<-iconv(my_tweet1,to="UTF-8")
corpus<-Corpus(VectorSource(corpus))
inspect(corpus[1:20])
#remove retweet



#removing the url
remove_url = function(x)gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", x) 
corpus<-tm_map(corpus,content_transformer(remove_url))
inspect(corpus[1:5])

#remove emojis
remove_emo<- function(x)gsub("latin1", "", x)
corpus<-tm_map(corpus,content_transformer(remove_emo))

#removing numbers

corpus<-tm_map(corpus,removeNumbers)
inspect(corpus[1:20])


remove_names<- function(x)gsub("(@\\w+ *:)|(@\\w+ *)|(#\\w+ *)", "", x)
corpus<-tm_map(corpus,content_transformer(remove_names))

#removing punctations
corpus<-tm_map(corpus,removePunctuation)
inspect(corpus[1:5])

#convert to lower case
corpus<-tm_map(corpus,tolower)
inspect(corpus[1:5])

#remove stop words
corpus<-tm_map(corpus,removeWords,stopwords('SMART'))
inspect(corpus[1:50])

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
inspect(corpus[1:5])


#strip white spaces
corpus<-tm_map(corpus,stripWhitespace)
inspect(corpus[1:20])

#removing crazy characters
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
corpus <- tm_map(corpus, content_transformer(removeNumPunct))

#removing â

aa<-function(a)gsub("\\w*â","",a)
corpus <- tm_map(corpus, content_transformer(aa))

remove_rt_n <-function(a) gsub("rt", "", a)
corpus <- tm_map(corpus, content_transformer(remove_rt_n))


clean_tweet <-function(a) gsub("&amp", "", a)
corpus <- tm_map(corpus, content_transformer(clean_tweet))


tdm<-TermDocumentMatrix(corpus)
tdm<-as.matrix(tdm)
# tdm[1:10,1:20]
w<-rowSums(tdm)
#w<-subset(w,w>=3)
barplot(w,
        las=2,
        col=rainbow(50))
w<-sort(rowSums(tdm),decreasing = TRUE)
set.seed(222)
#rot.per and min.freq and scale=size
wordcloud(words = names(w),
          freq=w,
          max.words = 150,
          random.order = FALSE,
          min.freq = 3,
          color=brewer.pal(8,'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.7)
# library(wordcloud2)
# w<-data.frame(w)
# colnames(w)<-c('word','freq')
# wordcloud2(w,
#            size=0.5,
#            shape='circle',
#            rotateRatio = 0.5,
#            minSize = 1)

twet1<-iconv(corpus,to="UTF-8")
s<-get_nrc_sentiment(twet1)

barplot(colSums(s),
        col=rainbow(10),
        ylab='Count',
        ylim = c(0,160))

slices <- c(sum(s$negative),sum(s$positive))
lbls <- c("negative", "positive")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of news")

pie3D(slices,labels=lbls,explode=0.1,
      main="Pie Chart of news ")


##what =character because the txt has character in it and comment.char ask to comment ';'
pos.words = scan('poswords.txt', what='character', comment.char=';')
neg.words = scan('negative.txt', what='character', comment.char=';')
library(stringr)
score.sentiment = function(sentences, pos.words, neg.words)
{
  list=lapply(sentences, function(sentence, pos.words, neg.words)
  {
  word.list = str_split(sentence, '\\s+')
  #unlisting and making as character vector
  words = unlist(word.list)  
  ##matching each of the words with pos.words 
  pos.matches = match(words, pos.words)
  neg.matches = match(words, neg.words)
  #if that word is not in the lis we have it will return na.So we need to remove NAs
  pos.matches = !is.na(pos.matches)
  neg.matches = !is.na(neg.matches)
  #getting the sum
  pp = sum(pos.matches)
  nn = sum(neg.matches)
  #getting the score
  score = sum(pos.matches) - sum(neg.matches)
  list1 = c(score, pp, nn)
  return (list1)
}, pos.words, neg.words)
  
  score_new = lapply(list, `[[`, 1)
  ##storing the positive score
  pp1 = lapply(list, `[[`, 2)
  ##storing the negative score
  nn1 = lapply(list, `[[`, 3)
  ###changing to dataframe
  scores.df = data.frame(score = score_new, text=corpus)
  positive.df = data.frame(Positive = pp1, text=corpus)
  negative.df = data.frame(Negative = nn1, text=corpus)
  
  list_df = list(scores.df, positive.df, negative.df)
  return(list_df)
}

result=score.sentiment(corpus,pos.words,neg.words)

library(reshape)

##copy of the result
test1=result[[1]]
test2=result[[2]]
test3=result[[3]]

#Creating three different data frames for Score, Positive and Negative
#Removing text column from data frame
test1$text=NULL
test2$text=NULL
test3$text=NULL
#Storing the first row(Containing the sentiment scores) in variable q
q1=test1[1,]
q2=test2[1,]
q3=test3[1,]


##merging into 1 column
qq1=melt(q1,var='Score')
qq2=melt(q2,var='Positive')
qq3=melt(q3,var='Negative')
qq1['Score'] = NULL
qq2['Positive'] = NULL
qq3['Negative'] = NULL
#Creating data frame
table1 = data.frame(Text=result[[1]]$text, Score=qq1)
table2 = data.frame(Text=result[[2]]$text, Score=qq2)
table3 = data.frame(Text=result[[3]]$text, Score=qq3)

#Merging three data frames into one
table_final=data.frame(Text=table1$Text, Score=table1$value, Positive=table2$value, Negative=table3$value)

posSc=table_final$Positive
negSc=table_final$Negative

#Adding column
table_final$PosPercent = posSc/ (posSc+negSc)

#Replacing Nan with zero
pp = table_final$PosPercent
pp[is.nan(pp)] <- 0
table_final$PosPercent = pp

#Negative Percentage

#Adding column
##sometimes it will be 0/0 then it will Nan
table_final$NegPercent = negSc/ (posSc+negSc)

#Replacing Nan with zero
nn = table_final$NegPercent
nn[is.nan(nn)] <- 0

table_final$NegPercent = nn
