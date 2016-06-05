#install.packages("tm.lexicon.GeneralInquirer", repos="http://datacube.wu.ac.at", type="source")
library(tm)
library(tm.lexicon.GeneralInquirer)
library(rplos)
library(SnowballC)
library(RedditExtractoR)
library(wordcloud)
library(RColorBrewer)

loadComments <- function(comsite) {
  content <- reddit_content(comsite) # grab reddit content
  comments <- content$comment # put contents into 'comments'
}

prepCorpus <- function(comsite) {
   comments <- loadComments(comsite) # grab and put comments into 'comments'
   corpus <- Corpus(VectorSource(comments)) # convert to corpus
   
   corpus <- tm_map(corpus, content_transformer(tolower), lazy=T) 
   corpus <- tm_map(corpus, removePunctuation, lazy=T)
   corpus <- tm_map(corpus, removeNumbers, lazy=T) 
   corpus <- tm_map(corpus, removeWords, stopwords("english"), lazy=T) 
   corpus <- tm_map(corpus, stripWhitespace, lazy=T) 
   corpus <- tm_map(corpus, stemDocument, lazy=T)
}

makePosWordCloud <- function(corpus) {

  corpus <- sapply(corpus, tm_term_score, terms_in_General_Inquirer_categories("Positiv"))
  
  # 1. Create a Document Term Matrix.
  (dtm <- DocumentTermMatrix(corpus))
  
  # 2. Find the most frequent term in a single document.
  max(as.matrix(dtm))
  
  freq.terms <- sort(colSums(as.matrix(dtm)))
  tail(freq.terms)[1]
  
  # 3. Find the most frequent term across the entire corpus.
  print(tail(freq.terms,1))
  
  # Create a Term Frequency-Inverse Document Frequency matrix. Do the data make sense?
  tfidf <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
  tfidf <- as.matrix(tfidf)
  tfidf[1:10, 1:20]
  # tfidf[, "absolut"]
  
  cosineDistance <- function(X) {
    1 - X %*% t(X) / (sqrt(rowSums(X^2) %*% t(rowSums(X^2))))
  }
  round(cosineDistance(tfidf[1:20,]), 3)
  
  # Make a word cloud.
  # install.packages("wordcloud")
  
  wordcloud(names(freq.terms), freq.terms, min.freq = 5, colors = brewer.pal(8, "Dark2"))
  
}

makeNegWordCloud <- function(comsite) {
  corpus <- prepCorpus(comsite)
  
}

analyze <- function(corpus) {

  positive <- sapply(corpus, tm_term_score, terms_in_General_Inquirer_categories("Positiv"))
  negative <- sapply(corpus, tm_term_score, terms_in_General_Inquirer_categories("Negativ"))
  margin <- positive - negative # negative score means more negative than positive

  print(mean(margin))
  print(sum(margin))
}

happyDay <- function(comsite) {
  comments <- loadComments(comsite) # grab and put comments into 'comments'
  comments
}



#load sample scripts
corp1 <- prepCorpus("https://www.reddit.com/r/worstof/comments/tk3cn/redditor_is_a_complete_asshole_and_demands_an/")

analyze(corp1)
analyze(prepCorpus("https://www.reddit.com/r/UpliftingNews/comments/4mjty0/norway_becomes_first_country_in_the_world_to/"))

makePosWordCloud(corp1)
