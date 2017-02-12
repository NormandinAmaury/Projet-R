library(NLP)
library(tm)


text <- readLines("/home/amaury/R/book.txt")
Frank <- Corpus(VectorSource(text))


#Part 1 clean the text
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

Frank <- tm_map(Frank, content_transformer(tolower)) 
Frank <- tm_map(Frank, removeNumbers)
Frank <- tm_map(Frank, removeWords, stopwords("english"))
Frank <- tm_map(Frank, removePunctuation)
Frank <- tm_map(Frank, stripWhitespace)
#I remove the word Gutenberg to have a more interesting result for the second part.
Frank <- tm_map(Frank, removeWords, c("gutenberg","gutenbergtm")) 


#Part 2 Most frequent words:

dtm <- TermDocumentMatrix(Frank)
matrice <- as.matrix(dtm)
v <- sort(rowSums(matrice),decreasing=TRUE)
words <- data.frame(word = names(v),freq=v)
head(words, 15)


barplot(words[1:15,]$freq, las = 2, names.arg = words[1:15,]$word,col ="lightblue", 
        main ="Most frequent words",ylab = "Word frequencies")


#The two most frequent paired words.

bigram <- function(xs){
  if (length(xs) >= 2) 
    c(paste(xs[seq(2)],collapse='_'),bigram(tail(xs,-1)))
}

res <- unlist(lapply(Frank,function(x){
  x <- gsub('  +','',x)
  words <- strsplit(x," ")[[1]]
  bigrams <- bigram(words[nchar(words)>2])
}))

xx <- as.data.frame(table(res))

pair = aggregate(xx$Freq, by=list(xx$res), FUN=mean)
pair2 = head(pair[order(-pair[,2], pair[,1]),],n=2)
pair2



