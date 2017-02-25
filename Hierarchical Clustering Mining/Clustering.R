seeds.ori = read.csv(file="https://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt",sep = "\t",header = FALSE)
summary(seeds.ori)
seeds.ori=na.omit(seeds.ori)

row.names(seeds.ori)=1:199

seeds=seeds.ori

seeds$V8 <- NULL

summary(seeds)

#category of distance
dis=dist(seeds,method = "euclidean")

#K-Means Clustering
km.2 = kmeans(seeds,2)
km.3 = kmeans(seeds,3)
km.5 = kmeans(seeds,5)
#Accuracy
accuracy = (nrow(seeds)-length(which(seeds.ori$V8!=km.3$cluster)))/nrow(seeds)
accuracy

#plot
plot(seeds,col=km.2$cluster)
plot(seeds,col=km.3$cluster)
plot(seeds,col=km.5$cluster)

#total sum of squares
km.2$tot.withinss
km.3$tot.withinss
km.5$tot.withinss

#Hierarchial Cluster
hk.complete=hclust(dist(seeds),method = "complete",members = NULL)
hk.single=hclust(dist(seeds),method = "single", members = NULL)
hk.average=hclust(dist(seeds), method = "average", members = NULL)
plot(hk.complete)
hk.complete$height
hk.complete$order
hk.complete$call
hk.complete$merge
what = cutree(hk.complete,3)
what
cop=cophenetic(hk.complete)
cor(cop,dist(seeds))

install.packages("ape")
phyl <-as.phylo(hk.complete)
plot(phyl,type="fan", edge.col=c("black", "green")[1+(phyl$edge.length >40) ])
require("factoextra")
install.packages("factoextra")
library(factoextra)
install.packages("ggplot2")
install.packages("pvclust")
plot(hk.complete,  cex = .5)
groups <- cutree(hk.complete, k=3)
rect.hclust(hk.complete, k=3, border="red")
pvrect(hk.complete, alpha=.95)
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
op = par(bg = "#EFEFEF")
A2Rplot(hk.complete, k = 3, boxes = FALSE, col.up = "gray50", col.down = c("#FF6B6B", "#4ECDC4", "#556270"))
hk.complete$labels
hk.complete$method
hk.complete$merge
plot(hk.complete, labels=seeds.ori$V8)
cutree(hk.complete, k=3)
seed<-seeds.ori$V8
View(seed)
seed[seed==2]=4
seed[seed==3]=2
seed[seed==4]=3
which(seed!= cutree(hk.complete, k=3))