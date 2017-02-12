library(NLP)
library(tm)

text <- readLines("/home/amaury/R/book.txt")
docs <- Corpus(VectorSource(text))


#Part 1 clean the text
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

docs <- tm_map(docs, content_transformer(tolower)) 
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)