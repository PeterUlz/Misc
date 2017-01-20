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

#Check summary of PCA and explained variance
summary(data.pca)

#Create traditional plot
plot(scores$PC1,scores$PC2,xlab="PC1",ylab="PC2",pch=rownames(data))

data$PC1<-scores$PC1
data$PC2<-scores$PC2

ggplot(data,aes(x=PC1,y=PC2,col=class)) + geom_point()
ggplot(data,aes(x=PC1,y=PC2,col=class)) + geom_point()


