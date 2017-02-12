#Installation of needed libraries:

library(data.table)
library(cluster)
library(fpc)
library(lattice)
library(proxy)
library(Matrix)
library(arules)
library(registry)
library(recommenderlab)


#Some pre work commandes to discover the database:

beers = fread("beer_reviews.csv")
length(unique(beers$beer_beerid))
summary(beers)
beers[, length(unique(beer_beerid)), by= "review_profilename"]
head(beers[,review_overall],n=10)


head(beers[,review_aroma],n=10)
head(beers[,review_palate],n=10)
head(beers[,review_appearance],n=10)
head(beers[,review_taste],n=10)


#Exercie 1

Aroma = aggregate(beers$review_aroma, by=list(beers$beer_name), FUN=mean)
Aroma10 = head(Aroma[order(-Aroma[,2], Aroma[,1]),],n=10)

Aroma10

Palate = aggregate(beers$review_palate, by=list(beers$beer_name), FUN=mean)
Palate10 = head(Palate[order(-Palate[,2], Palate[,1]),],n=10)

Palate10

Appearance = aggregate(beers$review_appearance, by=list(beers$beer_name), FUN=mean)
Appearance10 = head(Appearance[order(-Appearance[,2], Appearance[,1]),],n=10)

Appearance10

Taste = aggregate(beers$review_taste, by=list(beers$beer_name), FUN=mean)
Taste10 = head(Taste[order(-Taste[,2], Taste[,1]),],n=10)

Taste10


#Exercice 2


mysample = beers[sample(1:nrow(beers), 2000),] 
mysample = na.omit(mysample)
sample = mysample[,c(5,6,9,10)]
sample = data.frame(lapply(sample, jitter))
Cluster = kmeans(sample, 5)



plotcluster(sample, Cluster$cluster)
clusplot(sample, Cluster$cluster, color=TRUE, shade=TRUE, lines=0)



pairs(sample, col=c(5,6,9,10)[Cluster$cluster])
splom(sample, col=c(5,6,9,10)[Cluster$cluster])



#Exercice 3


test = beers[,c(4,5,6,9,10,11)]
affinity.data=read.csv("beer_reviews.csv")
affinity.data=affinity.data[,c(4,11)]

affinity.matrix= as(affinity.data,"realRatingMatrix")
Rec.model=Recommender(affinity.matrix, method = "UBCF")
recommended.items.1 = predict(Rec.model, affinity.matrix["1",], n=5)
as(recommended.items.1, "list")






