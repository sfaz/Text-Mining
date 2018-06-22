

# Sentiment Analysis Of Twitter ............#
#---------Sadaf Azad -----------#

#Libraries required to set up Twitter Credentials.
library(twitteR)
library(ROAuth)
library(sentimentr)
library(wordcloud)
library(tm)
library(syuzhet)
library(Rstem)
library(plyr)
library(ggplot2)

library(SnowballC)

library(RColorBrewer)

#----------Credential required by twitter api establish a connection------------
consumerKey<- "J4LuZAmIUcUBbiCt1gB2Vnmb5"
consumerSecret <- "wRo2HGaXXvtnSywiwnB0pKa4eSAlsOC9XW89LHEydcxX1LL5wX"
access_token <- "3472378521-oOIj1hTYoPhGt2h2PDqXDUr3CVUyGjjrwThR6Jd"
access_secret <- "w8LNte376y8hwTF3tEHGdtd3uWElipKD7X59XWXGrNaLw"

setup_twitter_oauth(consumer_key=consumerKey, consumer_secret=consumerSecret, access_token=access_token, access_secret=access_secret)

#-----------------Access Tweets by specifying count of tweets that you want to download. here it is 300

tweets <- searchTwitter("#RoyalWedding",n=300,lang="en")
#Convert tweets to data frame
tweets <- do.call("rbind", lapply(tweets, as.data.frame))
#check dimensions of the data frame
dim(tweets)

#------------Data Cleaning and Pre-Processing -----------------
#Process these together using text mining package and create wordcould for each 
iconv(tweets$text, from="UTF-8", to="ASCII", sub="")
#Clean text by removing graphic characters
tweets$text=str_replace_all(tweets$text,"[^[:graph:]]", " ")
#Remove Junk Values and replacement words like fffd which appear because of encoding differences
tweets$text <- gsub("[^[:alnum:]///' ]", "", tweets$text)
#Convert all text to lower case
tweets$text <- tolower(tweets$text)
#Remove retweet keyword
tweets$text <- gsub("rt", "", tweets$text)
#Remove Punctuations
tweets$text <- gsub("[[:punct:]]", "", tweets$text)
#Remove links
tweets$text <- gsub("http\\w+", "", tweets$text)
#Remove tabs
tweets$text <- gsub("[ |\t]{2,}", "", tweets$text)
#Remove blankspaces at begining
tweets$text <- gsub("^ ", "", tweets$text)
#Remove blankspaces at the end
tweets$text <- gsub(" $", "", tweets$text)
#Can remove usernames from the tweets using the following code line but I preferred not to 
tweets$text <- gsub("@\\w+", "", tweets$text)

myCorpus <- Corpus(VectorSource(tweets$text))
#Remove English Stopwords (e.g. 'my', 'nodoby', 'do', 'today' etc.) from the tweets



myCorpus <- tm_map(myCorpus, removeWords, stopwords("en"))
#remove words that add no meaning to the tweets 
myCorpus<tm_map(myCorpus, removeWords, c("amp","taejin","since","will","now","like","get","frm","can","take","like","still","twinktaejin"))
#remove while space from corpus
myCorpus <- tm_map(myCorpus, stripWhitespace)
#Remove numbers if necessary
myCorpus <- tm_map(myCorpus, removeNumbers)
corpus_clean <- tm_map(myCorpus, PlainTextDocument) ##this ensures the corpus transformations final output is a PTD
##optional, remove word stems (cleaning, cleaned, cleaner all would become clean): 
wordCorpus <- tm_map(corpus_clean, stemDocument)
#Convert corpus to vector 
corpus_clean <- Corpus(VectorSource(corpus_clean))


#-------------Plot the Document Sparcity
myTdm <- TermDocumentMatrix(corpus_clean, control = list(removePunctuation = TRUE,
                                                         stopwords = TRUE))
myTdm

#--------------Convert to Matrix arrange words in a data frame with frequencies
tdm.matrix<-as.matrix(myTdm)
v<-sort(rowSums(tdm.matrix),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


#Produce word cloud either using the attributes of the data frame or by using the myCorpus, wordCorpus or Corpus_Clean
wordcloud(words=d$word,freq = d$freq , min.freq=1, colors=brewer.pal(6, "Dark2"),random.order=FALSE, max.words = 200,rot.per=0.35)
wordcloud(words=myCorpus, min.freq=1, colors=brewer.pal(6, "Dark2"),random.order=FALSE, max.words = 150,rot.per=0.35)

##or create a word cloud from the corpus_clean data 
pal <- brewer.pal(7,"YlGnBu")
pal <- pal[-(1:4)]
set.seed(123)
wordcloud(words = corpus_clean, min.freq = 5, scale=c(4,0.5), max.words=200, random.order=FALSE, 
          rot.per=0.35, use.r.layout=TRUE, colors=pal)

#-----------------------------Attaching Emotions to the Tweets-using library sentimentr----------------------------------------------------------------

tweets <- searchTwitter("#RoyalWedding",n=300,lang="en")

df <- do.call("rbind", lapply(tweets, as.data.frame))
df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

mySentiment <- get_nrc_sentiment(df$text)

df <- cbind(df, mySentiment)

sentimentTotals <- data.frame(colSums(df[,c(17:25)])) ##select columns with sentiment data
names(sentimentTotals) <- "count" 
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL ##graph would be messy if these were left when plotting

#---Plot the sentiment score using the ggplot 
ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("      Total Sentiment Score")

#-------------------Sentiment Analysis of Tweets using the positive words and negative words dictionaries-------------------------------------------
#Function to tokenize words while assigning +ve and -ve scores on the basis of comments.
##Lexicon Sentiment Analysis

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr);
  require(stringr);
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[^A-z ]','', sentence)
    sentence = tolower(sentence);
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+');
    words = unlist(word.list);
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words);
    neg.matches = match(words, neg.words);
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches);
    neg.matches = !is.na(neg.matches);
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():  
    score = sum(pos.matches) - sum(neg.matches);
    return(score);
  }, pos.words, neg.words, .progress=.progress );
  scores.df = data.frame(score=scores, text=sentences);
  return(scores.df);
}


# load sentiment lexicon

pos <- scan("positive-words.txt",what="character",comment.char=";")# Load +ve words dictionary
neg <- scan("negative-words.txt",what="character",comment.char=";") #Load -ve word dictionary

# clean the twitter messages by removing odd characters
cleanText=df$text
analysis <- score.sentiment(cleanText,pos,neg)
table(analysis$score)
summary(analysis$score)
# save scored data
write.csv(analysis, "sentiscores.csv")

#Output Representation and Evaluation
neutral <- length(which(analysis$score == 0))
positive <- length(which(analysis$score > 0))
negative <- length(which(analysis$score < 0))
Sentiment <- c("Negative","Neutral","Positive")
Count <- c(negative,neutral,positive)
output <- cbind.data.frame(Sentiment,Count)
#View Tweets with +ve, -ve and neutral scores
ggplot(output, aes(x=Sentiment,y = Count,fill=Sentiment)) +geom_bar(stat = "identity")

#----------------------#Classificatoin Techniques-------------------------------------------------------
  
  ## Sentiment Prediction
  # Adapted code from 15.071x Turning Tweets Into Knowledge: An Introduction to Text Analytics
  # Read in the sentiment score data from the lexicon analysis
  tweets = read.csv("sentiscores.csv", stringsAsFactors=FALSE)
# Structure of the data frame
str(tweets)

# Create dependent variable
tweets$Negative = as.factor(tweets$score <= -1)
table(tweets$Negative)
# Install and load text mining packages
library(tm)
library(SnowballC)
# Create corpus
corpus = Corpus(VectorSource(tweets$text))
# Look at corpus
corpus
corpus[[1]]
# Convert to lower-case
corpus = tm_map(corpus, content_transformer(tolower))
corpus[[1]]
# Remove punctuation
corpus = tm_map(corpus, removePunctuation)
corpus[[1]]
# Look at stop words 
stopwords("english")[1:10]
# Remove stopwords and apple
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus[[1]]
# Stem document 
corpus = tm_map(corpus, stemDocument)
corpus[[1]]
# Preparing data to build the predictive model
# Create matrix
frequencies = DocumentTermMatrix(corpus)
frequencies
# Look at matrix 
inspect(frequencies[100:105,1:100])
# Words that appear 100 times in tweets
findFreqTerms(frequencies, lowfreq=100)
# Remove sparse terms. Words that repeat 0.5% or more of the tweets
sparse = removeSparseTerms(frequencies, 0.995)
sparse
# Convert to a data frame
tweetsSparse = as.data.frame(as.matrix(sparse))
# Make all variable names R-friendly (appropriate for R)
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
# Add dependent variable
tweetsSparse$Negative = tweets$Negative
# Split the data
library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

trainSparse = subset(tweetsSparse, split==TRUE)
testSparse = subset(tweetsSparse, split==FALSE)

# building CART and random forest models

# Build a CART model to predict negative sentiments

library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")
prp(tweetCART)

# Evaluate the performance of the model
predictCART = predict(tweetCART, newdata=testSparse, type="class",control=control)
conf<-table(testSparse$Negative, predictCART)

# Accuracy:
(conf[1,1]+conf[2,2]) / sum(conf)

# Baseline accuracy 
table(testSparse$Negative)


# Random forest model
library(randomForest)
set.seed(123)

tweetRF = randomForest(Negative ~ ., data=trainSparse)

# Make predictions:
predictRF = predict(tweetRF, newdata=testSparse)
conf<-table(testSparse$Negative, predictRF)

# Accuracy:
(conf[1,1]+conf[2,2]) / sum(conf)

