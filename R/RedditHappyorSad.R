library(RedditExtractoR)
library(tm)
library(rplos)
library(SnowballC)
library(tm.lexicon.GeneralInquirer)

# load a sample thread
test <- reddit_content("reddit.com/r/UpliftingNews/comments/4mc700/food_truck_drives_social_change_by_giving_jobs_to/")
comments <- test$comment
corpus <- Corpus(VectorSource(comments))

corpus <- tm_map(corpus, content_transformer(tolower)) #lower case conversion
as.character(corpus[[7]]) #check

corpus <- tm_map(corpus, removePunctuation) #remove punctuation
as.character(.Last.value[[7]])
as.character(corpus[[7]])

corpus <- tm_map(corpus, removeNumbers) #remove numbers
as.character(corpus[[7]])

corpus <- tm_map(corpus, removeWords, stopwords("english")) #remove stop words
as.character(corpus[[7]])

corpus <- tm_map(corpus, stripWhitespace) #strip whitespace
as.character(corpus[[7]])

corpus <- tm_map(corpus, stemDocument)
as.character(corpus[[70]])

dtm <- DocumentTermMatrix(corpus) #assign to a dtm variable
as.matrix(dtm)

apply(acq[1:10], tm_term_score, terms_in_General_Inquirer_categories("Positiv"))
apply(acq[1:10], tm_term_score, terms_in_General_Inquirer_categories("Negativ"))

