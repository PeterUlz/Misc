# R script to demonstrate clustering

library(ggplot2)

#Generate random data of two classes
#Class1: random data from Gaussian distribution with mean=10
#Class2: random data from Gaussian distribution with mean=10
 
class1 <- data.frame(x=rnorm(100,mean=10),y=rnorm(100,mean=10),class="class1")
class2 <- data.frame(x=rnorm(100,mean=5),y=rnorm(100,mean=5),class="class2")
class <- rbind(class1,class2)
plot(class$x,class$y)

#Combine data from both classes and plot
data_matrix <- as.matrix(cbind(class$x,class$y))
plot(data_matrix)

#Perform K-means cluster and suppose we know that it should be two clusters
clustering <- kmeans(data_matrix,2)
class$cluster_predict <- clustering$cluster


#Plot and color according to classes
colors_class <- rep(rgb(0,0,0,0.2),length(class$x))
colors_class[which(class$class == "class1")]<- "red"
colors_class[which(class$class == "class2")]<- "green"
plot(data_matrix,col=colors_class,pch=16)

#Plot and color according to clustering prediction
colors_clustering <- rep(rgb(0,0,0,0.2),length(class$x))
colors_clustering[which(class$cluster_predict == "1")]<- "red"
colors_clustering[which(class$cluster_predict == "2")]<- "green"
plot(data_matrix,col=colors_clustering,pch=16)

#Plot using different plotting library
ggplot(class, aes(x=x,y=y)) + geom_point()
ggplot(class, aes(x=x,y=y,color=class)) + geom_point()

#Transform Cluster Variable to Factor
ggplot(class, aes(x=x,y=y,color=factor(cluster_predict),shape=class)) + geom_point()


#Try again with harder data
class1 <- data.frame(x=rnorm(1000,mean=5),y=rnorm(1000,mean=5),class="class1")
class2 <- data.frame(x=rnorm(1000,mean=2),y=rnorm(1000,mean=2),class="class2")
class <- rbind(class1,class2)
data_matrix <- as.matrix(cbind(class$x,class$y))
clustering <- kmeans(data_matrix,2)
class$cluster_predict <- clustering$cluster
ggplot(class, aes(x=x,y=y,color=factor(cluster_predict),shape=class)) + geom_point()

#Try again with 3 clusters
class1 <- data.frame(x=rnorm(1000,mean=5),y=rnorm(1000,mean=5),class="class1")
class2 <- data.frame(x=rnorm(1000,mean=2),y=rnorm(1000,mean=2),class="class2")
class <- rbind(class1,class2)
data_matrix <- as.matrix(cbind(class$x,class$y))
clustering <- kmeans(data_matrix,3)
class$cluster_predict <- clustering$cluster
ggplot(class, aes(x=x,y=y,color=factor(cluster_predict),shape=class)) + geom_point()

#Try again with hierarchical clustering
class1 <- data.frame(x=rnorm(100,mean=10),y=rnorm(100,mean=10),class="class1")
class2 <- data.frame(x=rnorm(100,mean=5),y=rnorm(100,mean=5),class="class2")
class <- rbind(class1,class2)
data_matrix <- as.matrix(cbind(class$x,class$y))
dist_matrix<-dist(data_matrix)
clustering <- hclust(dist_matrix)
plot(clustering,label=class$class)
abline(h=2)



