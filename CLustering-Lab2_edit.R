library(ggplot2) # visualization
library(dplyr) # data manipulation
library(VIM)
library(data.table)
library(caret)
library(dummies)
library(cluster)
library(dendextend)
library(datasets)
library(NbClust)
library(heatmaply)


#Read IMDB data from moviedata.csv
IMDB <- read.csv("C:\\Users\\ludai\\Desktop\\BAN620\\Lab7\\moviedata.csv")
# View Variables
str(IMDB)
# List only factors
names(IMDB[,sapply(IMDB, is.factor)])
# Make a copy
IMDB2 <- IMDB
# Assign MovieID as rownames
rownames(IMDB2) <- IMDB2$MovieID
# Is a row duplicated?
duplicated(IMDB2$MovieID)
# Get non duplicated rows
IMDB2 <- IMDB[!duplicated(IMDB$MovieID),]
# Assign MovieID as rownames
rownames(IMDB2) <- IMDB2$MovieID
# Factor Variables
names(IMDB2[,sapply(IMDB2, is.factor)])
# Drop some columns, how do we do this?
IMDB2$movie_imdb_link <- NULL
IMDB2$language <- NULL
IMDB2$country <- NULL
IMDB2$movie_title <- NULL
IMDB2$MovieID <- NULL

# Factor Variables
names(IMDB2[,sapply(IMDB2, is.factor)])

# Factors to Dummy Variables
IMDB.new <- dummy.data.frame(IMDB2, sep = ".")

# Clusters for 2016 Movies
mclusters <- agnes(IMDB.new[IMDB.new$title_year == '2016',], method = "complete", metric = "euclidean")

# Omit NA's
na.omit(IMDB.new[IMDB.new$title_year > '2016', c("title_year")])
mclusters <- agnes(na.omit(IMDB.new[IMDB.new$title_year == '2016',]), method = "complete", metric = "euclidean")
mclusters
#Plot the clusters

# Get Dendrogram
hcd <-as.dendrogram(mclusters, cex= 0.5, hang = 0.1)

# Plot saved dendro
plot(hcd)

heights_per_k.dendrogram(hcd)

# Cut Dendro at height 20k
plot(cut(hcd, h = 127058035)$upper, main = "Upper tree of cut at h=20k")
plot(cut(hcd, h = 127058035)$lower[[3]], main = "First branch of lower tree with cut at h=20k")
 
cutree(hcd, k=3)

# Why is height so crazy?

# Normalize
IMDB2016 <- scale(na.omit(IMDB.new[IMDB.new$title_year > '2015', ]))


mclust2 <- mclusters <- agnes(IMDB2016, method = "complete", metric = "euclidean")

plot(mclust2, which.plots=2, cex = 0.5)
