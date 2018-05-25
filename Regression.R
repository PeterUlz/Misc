# R script to demonstrate regression analysis

library(ggplot2)

#Read data from file
# telomere length and age from healthy people
 
telomere_data<-read.csv("read_counts_counting.txt",header=TRUE,sep="\t")
str(telomere_data)

#Plot data
plot(telomere_data$age,telomere_data$telomer_reads_per_1M,pch=16)
ggplot(telomere_data, aes(x=age,y=telomer_reads_per_1M,color=gender)) + geom_point()

#Plot again but with Loess fit
plot(telomere_data$age,telomere_data$telomer_reads_per_1M,pch=16)
ggplot(telomere_data, aes(x=age,y=telomer_reads_per_1M,color=gender)) + geom_point() + geom_smooth()

#Plot again but with Loess fit
ggplot(telomere_data, aes(x=age,y=telomer_reads_per_1M)) + geom_point() + geom_smooth()

#Plot again but with Linear Model fit
ggplot(telomere_data, aes(x=age,y=telomer_reads_per_1M)) + geom_point() + geom_smooth(method=lm)

#Start with calculation of correlation
cor.test(telomere_data$age,telomere_data$telomer_reads_per_1M)
cor.test(telomere_data$age,telomere_data$telomer_reads_per_1M,method="spearman")

#Calculate a linear model
model = lm(age ~ telomer_reads_per_1M, data=telomere_data)
summary(model)


#Predict a new value
new_data<-data.frame(telomer_reads_per_1M=c(15,20,10))
prediction<-predict(model,newdata=new_data)

#Analyze new set of data
breast_data<-read.csv("read_counts_counting_breast.txt",header=TRUE,sep="\t")
str(breast_data)
prediction<-predict(model,newdata=breast_data)
plot(breast_data$age,prediction)
cor.test(breast_data$age,prediction)


#Check breast cancer dataset
ggplot(breast_data,aes(age,fill=gender)) +geom_histogram()

#Compare datasets
ggplot(telomere_data,aes(age)) +geom_histogram(fill="blue") + geom_histogram(data=breast_data,fill="red")
ggplot(telomere_data,aes(age)) +geom_density(fill="blue",alpha=0.2) + geom_density(data=breast_data,fill="red",alpha=0.2)


