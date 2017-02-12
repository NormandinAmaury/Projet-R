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
#I remove the word Gutenberg to have a more interesting result for the second part.
docs <- tm_map(docs, removeWords, c("gutenberg","gutenbergtm")) 


#Part 2 Most frequent words:

dtm <- TermDocumentMatrix(docs)
matrice <- as.matrix(dtm)
v <- sort(rowSums(matrice),decreasing=TRUE)
words <- data.frame(word = names(v),freq=v)
head(words, 15)


#The two most frequent paired words.
bigram <- 
  function(xs){
    if (length(xs) >= 2) 
      c(paste(xs[seq(2)],collapse='_'),bigram(tail(xs,-1)))
    
  }


res <- unlist(lapply(docs,function(x){
  x <- gsub('  +','',x)
  words <- strsplit(x," ")[[1]]
  bigrams <- bigram(words[nchar(words)>2])
}))

xx <- as.data.frame(table(res))

pair = aggregate(xx$Freq, by=list(xx$res), FUN=mean)
pair2 = head(pair[order(-pair[,2], pair[,1]),],n=2)
pair2


