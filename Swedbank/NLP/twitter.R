Consumer_Key<- "CGMoOTPBxfNQVo2bfr8JDpLXy"
Consumer_Secret<-	"u96jde8quIlbhIwloZ4J8oH4Y7DbfVYFcAyyDzFomXouTuH3rq"
Access_Token<- "990608111336611840-6oLAwBDxuMekoRWxJgmplf1jjiOFkBO"
Access_Token_Secret<-"hJt7vORpys9IJWpZkXT8IS7Eat8MNe0LkJeAgsDjEVz0F"
setup_twitter_oauth(Consumer_Key,Consumer_Secret,Access_Token,Access_Token_Secret)
my_tweet = searchTwitter("ISIS",n=150,lang="en",since='2017-03-28',until='2018-04-29')  

#converts to data frame
df <- do.call("rbind", lapply(my_tweet, as.data.frame))

#remove odd characters
df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub="")) #remove emoticon
df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df$text) #remove URL
sample <- df$text


trump_text = sapply(my_tweet, function(x) x$getText()) #sapply returns a vector 
df <- do.call("rbind", lapply(my_tweet, as.data.frame)) #lapply returns a list
trump_text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
str(trump_text) #gives the summary/internal structure of an R object

library(tm) #tm: text mining
trump_corpus <- Corpus(VectorSource(trump_text)) #corpus is a collection of text documents
trump_corpus
inspect(trump_corpus[1])

#clean text
library(wordcloud)
trump_clean <- tm_map(trump_corpus, removePunctuation)
trump_clean <- tm_map(trump_clean, removeWords, stopwords("english"))
trump_clean <- tm_map(trump_clean, removeNumbers)
trump_clean <- tm_map(trump_clean, stripWhitespace)
wordcloud(trump_clean, random.order=F,max.words=80, col=rainbow(50), scale=c(3.5,1))




##Scanning the dictionary that is downloaded,whenever you want to add a new word please add it in
##one of those dictionaries

##what =character because the txt has character in it and comment.char ask to comment ';'
pos.words = scan('poswords.txt', what='character', comment.char=';')
neg.words = scan('negative.txt', what='character', comment.char=';')

##function for score sentiments


##.progress='none' can eb removed

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  list=lapply(sentences, function(sentence, pos.words, neg.words)
  {
    ##substituting punctuations with empty or u can say remove punctuations
    sentence = gsub('[[:punct:]]',' ',sentence)
    ##substituting control words with empty
    sentence = gsub('[[:cntrl:]]','',sentence)
    #remove the decimal numbers
    sentence = gsub('\\d+','',sentence)  
    #removes new lines
    sentence = gsub('\n','',sentence) 
    ##sentences to lower cases as words are in lower cases in the dictionary
    sentence = tolower(sentence)
    ##sentences are splitted ..stringr library does that
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
  ##storing the score(first column)
  score_new = lapply(list, `[[`, 1)
  ##storing the positive score
  pp1 = lapply(list, `[[`, 2)
  ##storing the negative score
  nn1 = lapply(list, `[[`, 3)
  ###changing to dataframe
  scores.df = data.frame(score = score_new, text=sentences)
  positive.df = data.frame(Positive = pp1, text=sentences)
  negative.df = data.frame(Negative = nn1, text=sentences)
  
  list_df = list(scores.df, positive.df, negative.df)
  return(list_df)
}
# Clean the tweets and returns merged data frame
result = score.sentiment(sample, pos.words, neg.words)

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


##Histogram

hist(table_final$Positive, col=rainbow(10))
hist(table_final$Negative, col=rainbow(10))
hist(table_final$Score, col=rainbow(10))

#Pie
slices <- c(sum(table_final$Positive), sum(table_final$Negative))
lbls <- c("positive", "negative")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of news")


