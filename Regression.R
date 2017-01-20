# R script to demonstrate regression analysis

library(ggplot2)

#Read data from file
# telomere length and age from healthy people
 
telomere_data<-read.csv("read_counts_counting.txt",header=TRUE,sep="\t")
str(telomere_data)

#Plot data
ggplot(telomere_data, aes(x=age,y=telomer_reads_per_1M,color=gender)) + geom_point()

#Plot again but with Loess fit
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

#Check for new data

