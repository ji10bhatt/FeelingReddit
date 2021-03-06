#install.packages("tm.lexicon.GeneralInquirer", repos="http://datacube.wu.ac.at", type="source")
library(tm)
library(tm.lexicon.GeneralInquirer)
library(rplos)
library(SnowballC)
library(RedditExtractoR)

<<<<<<< HEAD
prepCorpus <- function(URL) {
   content <- reddit_content(URL)
=======


prepCorpus <- function(comsite) {
   content <- reddit_content(comsite)
>>>>>>> 567205306e4ccbb7a265a0f7834c649f572df249
   comments <- content$comment
   corpus <- Corpus(VectorSource(comments))

   corpus <- tm_map(corpus, content_transformer(tolower), lazy=T) #lower case conversion
   corpus <- tm_map(corpus, removePunctuation, lazy=T)
   corpus <- tm_map(corpus, removeNumbers, lazy=T) #remove numbers
   corpus <- tm_map(corpus, removeWords, stopwords("english"), lazy=T) #remove stop words
   corpus <- tm_map(corpus, stripWhitespace, lazy=T) #strip whitespace
   corpus <- tm_map(corpus, stemDocument, lazy=T)
}

<<<<<<< HEAD
makePosWordCloud <- function(corpus) {

  corpus <- sapply(corpus, tm_term_score, terms_in_General_Inquirer_categories("Positiv"))
  
  (dtm <- DocumentTermMatrix(corpus))
  
  max(as.matrix(dtm))
  
  freq.terms <- sort(colSums(as.matrix(dtm)))
  tail(freq.terms)[1]
  
  print(tail(freq.terms,1))
  
  tfidf <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
  tfidf <- as.matrix(tfidf)
  tfidf[1:10, 1:20]

  wordcloud(names(freq.terms), freq.terms, min.freq = 5, colors = brewer.pal(8, "Dark2"))
}

makeNegWordCloud <- function(corpus) {

  corpus <- sapply(corpus, tm_term_score, terms_in_General_Inquirer_categories("Negative"))
  
  (dtm <- DocumentTermMatrix(corpus))
  
  max(as.matrix(dtm))
  
  freq.terms <- sort(colSums(as.matrix(dtm)))
  tail(freq.terms)[1]
  
  print(tail(freq.terms,1))
  
  tfidf <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
  tfidf <- as.matrix(tfidf)
  tfidf[1:10, 1:20]
  # tfidf[, "absolut"]
  
  wordcloud(names(freq.terms), freq.terms, min.freq = 5, colors = brewer.pal(8, "Dark2"))  
}
=======
>>>>>>> f47bad8dbc98c382f723d37ea466b91cbfac2fa1

analyze <- function(corpus) {




  #class(corpus)
  #class(corpus[1])
  #class(corpus[[1]])


  positive <- sapply(corpus, tm_term_score, terms_in_General_Inquirer_categories("Positiv"))
  negative <- sapply(corpus, tm_term_score, terms_in_General_Inquirer_categories("Negativ"))

  margin <- positive - negative # negative score means more negative than positive

  print(mean(margin))
  print(sum(margin))
}

#load sample scripts
#analyze(prepCorpus("https://www.reddit.com/r/Documentaries/comments/4mikfl/weed_is_not_more_dangerous_than_alcohol_2014_342/"))

