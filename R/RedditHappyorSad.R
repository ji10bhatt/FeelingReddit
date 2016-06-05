library(tm)
library(tm.lexicon.GeneralInquirer)
library(rplos)
library(SnowballC)
library(RedditExtractoR)

# load a sample thread
test <- reddit_content("https://www.reddit.com/r/science/comments/4mkzrl/childrens_intelligence_mindsets_ie_their_beliefs/")
comments <- test$comment
head(comments)
corpus <- Corpus(VectorSource(comments))

as.character(corpus[[8]]) #original with no transformations

corpus <- tm_map(corpus, content_transformer(tolower), lazy=T) #lower case conversion
as.character(corpus[[8]]) #checked -- works

corpus <- tm_map(corpus, removePunctuation, lazy=T)
as.character(corpus[[8]])

corpus <- tm_map(corpus, removeNumbers, lazy=T) #remove numbers
as.character(corpus[[8]])

corpus <- tm_map(corpus, removeWords, stopwords("english"), lazy=T) #remove stop words
as.character(corpus[[8]])

corpus <- tm_map(corpus, stripWhitespace, lazy=T) #strip whitespace
as.character(corpus[[8]])

corpus <- tm_map(corpus, stemDocument, lazy=T)
as.character(corpus[[8]])

dtm <- DocumentTermMatrix(corpus) #assign to a dtm variable
as.matrix(dtm)

class(corpus)
class(corpus[1])
class(corpus[[1]])

positive <- sapply(corpus, tm_term_score, terms_in_General_Inquirer_categories("Positiv"))
negative <- sapply(corpus, tm_term_score, terms_in_General_Inquirer_categories("Negativ"))

margin <- positive - negative # negative score means more negative than positive
mean(margin)
