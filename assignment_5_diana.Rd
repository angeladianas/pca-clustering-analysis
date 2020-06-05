## PART 1
setwd("/Users/angeladianas/Desktop/Lund/r_files")
library(dplyr)
HBATred <- read.csv("HBATred.csv", header = TRUE, sep = ",", dec = ".")
head(HBATred)
dim(HBATred)
HBATred <- select(HBATred, -X)

set.seed(2008)
n = length(HBATred$x6)
train <- sample(1:n, 80)
training <- HBATred[train,]
test <- HBATred[-train,]

round(cor(training), digits = 4)

PC <- prcomp(training, scale = TRUE)
biplot(PC, scale = 0, cex = 0.8, xlabs = rep("o", nrow(training)), col = c("blue", "red"), xlab = "First Principal Component", ylab = "Second Principal Component")

round(PC$rotation, digits = 4)
PCvar <- PC$sdev ^ 2
pve <- PCvar/sum(PCvar)
round(pve, digits = 4)
par(mfrow = c(1,2))
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", main = "Proportion of Variance Explained vs Principal Component", cex.main = 0.85, cex.axis = 0.75, cex.lab = 0.75, ylim=c(0,1), type = "b")
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", main = "Cumulative Proportion of Variance Explained vs Principal Component", cex.main = 0.85, cex.axis = 0.75, cex.lab = 0.75, ylim=c(0,1), type = "b")

## PART 2
store <- read.csv("Store.csv", sep = ";", header = TRUE)
head(store)
store <- select(store, -Individual)
round(cor(store), digits = 2)

sel_store <- select(store, -c(x14, x13, x17, x18, x16, x6, x10, x8))
round(cor(sel_store), digits = 4)

set.seed(2008)

ns = nrow(sel_store)
strain <- sample(1:ns, 200)
straining <- sel_store[strain,]
stest <- sel_store[-strain,]
dim(straining)

store.single <- hclust(dist(straining), method = "single")
store.complete <- hclust(dist(straining), method = "complete")
store.average <- hclust(dist(straining), method = "average")
store.ward <- hclust(dist(straining), method = "ward.D")

par(mfrow = c(2,2))
plot(store.single, main ="Single Linkage", xlab ="", sub ="", cex = 0.4)
plot(store.complete, main ="Complete Linkage", xlab ="", sub ="", cex = 0.4)	# tree = 2?
plot(store.average, main ="Average Linkage", xlab ="", sub ="", cex = 0.4)
plot(store.ward, main ="Ward Linkage", xlab ="", sub ="", cex = 0.4)		# tree = 3?
## current interpretation to determine the number of trees: look at heights, if the fuse at approximately similar height, they're one tree. Otherwise, they're different. 

cutstore.complete <- cutree(store.complete, 2)
cutstore.ward <- cutree(store.ward, 3)
aggregate(straining, list(cutstore.complete), mean)
aggregate(straining, list(cutstore.ward), mean)

set.seed(2008)
store.kmeans <- kmeans(straining, 3, nstart = 20)
store.kmeans

## validation statistics
library(fpc)
library(NbClust)
kmeans.stats <- cluster.stats(dist(straining), store.kmeans$cluster)
complete.stats <- cluster.stats(dist(straining), cutstore.complete)
ward.stats <- cluster.stats(dist(straining), cutstore.ward)
kmeans.stats$dunn
complete.stats$dunn
ward.stats$dunn

# optimal number of cluster
# library(clustertend)
# NbClust(straining, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete")

library(pvclust)

