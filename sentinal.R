library("twitteR")
library("ROAuth")
library("NLP")
library("twitteR")
library("syuzhet")
library("tm")
library("SnowballC")
library("stringi")
library("topicmodels")
library("syuzhet")
library("ROAuth")
library("wordcloud")
library("ggplot2")

# authorisation keys
consumer_key = "provide your details here" #Consumer key from twitter app
consumer_secret = "provide your details here" #Consumer secret from twitter app
access_token = "provide your details here" #access token from twitter app
access_secret ="provide your details here" #access secret from twitter app

# set up
setup_twitter_oauth(consumer_key,consumer_secret,access_token, access_secret)

#extracting tweets of a particular hashtag, "n" is the no of tweets to be extracted.
tweets_g <- searchTwitter("#google", n=500,lang = "en")

#Convert this extracted data to a dataframe which makes it more readable and easier to work with.
google_tweets <- twListToDF(tweets_g)
View(google_tweets)
google_text<- google_tweets$text

#converting text to lower case
google_text<- tolower(google_text) 

# Replace blank space (“rt”)
google_text <- gsub("rt", "", google_text)

# Replace @UserName
google_text <- gsub("@\\w+", "", google_text)

# Remove punctuation
google_text <- gsub("[[:punct:]]", "", google_text)

# Remove links
google_text <- gsub("http\\w+", "", google_text)

# Remove tabs
google_text <- gsub("[ |\t]{2,}", "", google_text)

# Remove blank spaces at the beginning
google_text <- gsub("^ ", "", google_text)

# Remove blank spaces at the end
google_text <- gsub(" $", "", google_text)

#clean up by removing stop words
google_tweets.text.corpus <- tm_map(google_tweets.text.corpus, function(x)removeWords(x,stopwords()))

#generate wordcloud
wordcloud(google_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)

#getting emotions using in-built function
mysentiment_google<-get_nrc_sentiment((google_text))

#calculationg total score for each sentiment
Sentimentscores_google<-data.frame(colSums(mysentiment_google[,]))

names(Sentimentscores_google)<-"Score"
Sentimentscores_google<-cbind("sentiment"=rownames(Sentimentscores_google),Sentimentscores_google)
rownames(Sentimentscores_google)<-NULL

#plotting the sentiments with scores
ggplot(data=Sentimentscores_google,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on tech giant GOOGLE")
