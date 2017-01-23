#Demonstrate Principal Component Analysis

library(ggplot2)

#Read in data
data<-read.table("pca_data.csv", header=TRUE)
str(data)

#Calculate principal components using the prcomp function
numerical<-within(data, rm(class))
data.pca<-prcomp(numerical,center=TRUE,scale=TRUE)
scores = as.data.frame(data.pca$x)

#Create Biplot
biplot(data.pca)


#Check summary of PCA and explained variance and check Screeplot
summary(data.pca)
screeplot(data.pca,type="lines")

#Create traditional plot
plot(scores$PC1,scores$PC2,xlab="PC1",ylab="PC2",pch=rownames(data))

data$PC1<-scores$PC1
data$PC2<-scores$PC2
data$PC3<-scores$PC3
data$PC4<-scores$PC4

ggplot(data,aes(x=PC1,y=PC2,col=class)) + geom_point()
ggplot(data,aes(x=PC1,y=PC2,col=class)) + geom_point() +xlim(1,4) +ylim(-3,-0.5)

#Try out other Principal components
ggplot(data,aes(x=PC1,y=PC3,col=class)) + geom_point()
ggplot(data,aes(x=PC2,y=PC3,col=class)) + geom_point()
ggplot(data,aes(x=PC3,y=PC4,col=class)) + geom_point()

#Try 3D Plot
library(plot3D)
scatter3D(x=data$PC1,y=data$PC2,z=data$PC3)

#Try cluster data
clustering_data<-as.matrix(scores)
clustering<-kmeans(clustering_data,3)
data$cluster<-as.factor(clustering$cluster)
ggplot(data,aes(x=PC1,y=PC2,col=class,shape=cluster)) + geom_point()

#Try clustering data again with 5 clusters
clustering_data<-as.matrix(scores)
clustering<-kmeans(clustering_data,5)
data$cluster<-as.factor(clustering$cluster)
ggplot(data,aes(x=PC1,y=PC2,col=class,shape=cluster)) + geom_point()

