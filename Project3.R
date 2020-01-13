
mydata1 = read.csv("C:/Users/aksha/OneDrive - University of Texas at Arlington/DM/2009-2013-genre.txt",sep=";", header=F)
mydata2 = read.csv("C:/Users/aksha/OneDrive - University of Texas at Arlington/DM/2009-2013-actor.txt",sep=";", header=F)
colnames(mydata1) <- c("Tid","Genre","Year")
colnames(mydata2) <- c("Tid","Actor")
myfulldata = merge(mydata1, mydata2)
data<- subset(myfulldata,myfulldata$Year!="2009")
#data10111213[data10111213 == "?"] <- NA
#remove ? 
#data<- lapply(data, gsub, pattern='\\?', replacement='')
#seprate individual column from the data set 
dataGenre <- data[, c("Tid", "Genre")]
library(tidyr)
#s <- strsplit(dataGenre$Genre, split = ",")
#dataGenre <- data.frame(Tid = rep(dataGenre$Tid, sapply(s, length)), Genre = unlist(s))
dataGenre <- separate_rows(dataGenre,Genre)
#dataGenre <-dataGenre[, c("G1", "G2") := tstrsplit(Genre, ",")]
#dataGenre <-dataGenre %>% separate(dataGenre$Genre, c("Genre1", "Genre2"), ",")
dataYear <- data[, c("Tid", "Year")]
dataActor <- data[, c("Tid", "Actor")]
#remove duplicate rows
library(tidyverse)
dataGenre <-dataGenre %>% distinct()
dataYear <-dataYear %>% distinct()
dataActor <-dataActor %>% distinct()
#now append all tables to make single Tid

colnames(dataGenre) <- c("Tid","item")
colnames(dataYear) <- c("Tid","item")
colnames(dataActor) <- c("Tid","item")

dataAppend <- rbind(dataGenre,dataYear)
dataAppend <- rbind(dataAppend,dataActor)
#remove ? 
#data<- lapply(data, gsub, pattern='\\?', replacement='')
library('dplyr')
dataAppend <- arrange(dataAppend,Tid)
basket<-aggregate(item~Tid,dataAppend,paste,sep="\n")
basket <- apply(basket,2,as.character)
write.table(basket, file = "demo_single")
dataread<-read.transactions("demo_single", format ="basket", 
                  header = TRUE, sep = ",", 
                   rm.duplicates = FALSE, 
                  quote = "\\", skip = 1, 
                  encoding = "scan")
library(arules)

########Candidate First one
cand1<- apriori(dataread,parameter=list(support=0.0001,confidence=0.005,maxlen=1,minlen=1,target = "frequent itemsets"))
cand1len<-c(length(cand1))
i<-1
df <- data.frame(cand1len,i)

i<-2
while(length(cand1)!=0){
  
  cand1<- apriori(dataread,parameter=list(support=0.0001,confidence=0.005,maxlen=i,minlen=i,target = "frequent itemsets"))
  temp<-c(length(cand1),i)
  df <- rbind(df,temp)
  i<-i+1
}

# Simple Bar Plot


barplot(df$cand1len,names.arg=df$i,main="Candidate Itemsets",
        xlab="Number of Iteration(s=.8)",col=c("darkblue"))



#########Frequent First one

freq1<- apriori(dataread,parameter=list(support=0.008,confidence=0.005,maxlen=1,minlen=1,target = "frequent itemsets"))
freq1len<-c(length(freq1))
i<-1
df <- data.frame(freq1len,i)

i<-2
while(length(freq1)!=0){
  
  freq1<- apriori(dataread,parameter=list(support=0.008,confidence=0.005,maxlen=i,minlen=i,target = "frequent itemsets"))
  temp<-c(length(freq1),i)
  df <- rbind(df,temp)
  i<-i+1
}

# Simple Bar Plot


barplot(df$freq1len,names.arg=df$i,main="Frequent Itemsets",
        xlab="Number of Iteration(s=.8)",col=c("darkblue"))




########Candidate Second one
cand2<- apriori(dataread,parameter=list(support=0.0001,confidence=0.007,maxlen=1,minlen=1,target = "frequent itemsets"))
cand2len<-c(length(cand2))
i<-1
df <- data.frame(cand2len,i)

i<-2
while(length(cand2)!=0){
  
  cand2<- apriori(dataread,parameter=list(support=0.0001,confidence=0.007,maxlen=i,minlen=i,target = "frequent itemsets"))
  temp<-c(length(cand2),i)
  df <- rbind(df,temp)
  i<-i+1
}

# Simple Bar Plot


barplot(df$cand2len,names.arg=df$i,main="Candidate Itemsets",
        xlab="Number of Iteration(s=1.1)",col=c("darkblue"))



#########Frequent Second one

freq2<- apriori(dataread,parameter=list(support=0.011,confidence=0.007,maxlen=1,minlen=1,target = "frequent itemsets"))
freq2len<-c(length(freq2))
i<-1
df <- data.frame(freq2len,i)

i<-2
while(length(freq2)!=0){
  
  freq2<- apriori(dataread,parameter=list(support=0.011,confidence=0.007,maxlen=i,minlen=i,target = "frequent itemsets"))
  temp<-c(length(freq2),i)
  df <- rbind(df,temp)
  i<-i+1
}

# Simple Bar Plot


barplot(df$freq2len,names.arg=df$i,main="Frequent Itemsets",
        xlab="Number of Iteration(s=1.1)",col=c("darkblue"))




########Candidate Third one
cand3<- apriori(dataread,parameter=list(support=0.0001,confidence=0.008,maxlen=1,minlen=1,target = "frequent itemsets"))
cand3len<-c(length(cand3))
i<-1
df <- data.frame(cand3len,i)

i<-2
while(length(cand3)!=0){
  
  cand3<- apriori(dataread,parameter=list(support=0.0001,confidence=0.008,maxlen=i,minlen=i,target = "frequent itemsets"))
  temp<-c(length(cand3),i)
  df <- rbind(df,temp)
  i<-i+1
}

# Simple Bar Plot


barplot(df$cand3len,names.arg=df$i,main="Candidate Itemsets",
        xlab="Number of Iteration(s=1.6)",col=c("darkblue"))



#########Frequent Third one

freq3<- apriori(dataread,parameter=list(support=0.016,confidence=0.008,maxlen=1,minlen=1,target = "frequent itemsets"))
freq3len<-c(length(freq3))
i<-1
df <- data.frame(freq3len,i)

i<-2
while(length(freq3)!=0){
  
  freq3<- apriori(dataread,parameter=list(support=0.016,confidence=0.008,maxlen=i,minlen=i,target = "frequent itemsets"))
  temp<-c(length(freq3),i)
  df <- rbind(df,temp)
  i<-i+1
}

# Simple Bar Plot


barplot(df$freq3len,names.arg=df$i,main="Frequent Itemsets",
        xlab="Number of Iteration(s=1.6)",col=c("darkblue"))















###Rule 1
library(arulesViz)
rule1<- apriori(dataread,parameter=list(support=0.008,confidence=0.005))
plot(rule1)
rule1_sort <- sort(rule1,by="confidence",decreasing = TRUE)
plot(rule1_sort,method="grouped")
plot(rule1_sort)
rule1_lift_gr<-sort(rule1,by="lift",decreasing = TRUE)
top <-  head(rule1_lift_gr,5)
inspect(top)
plot(top)
plot(top,method="grouped")
rule1_lift_ls<-sort(rule1,by="lift",decreasing = FALSE)
low <- head(rule1_lift_ls,5)
inspect(low)
plot(low)
plot(low,method="grouped")



###Rule 2
library(arulesViz)
rule2<- apriori(dataread,parameter=list(support=0.011,confidence=0.007))
plot(rule2)
rule2_sort <- sort(rule2,by="confidence",decreasing = TRUE)
plot(rule2_sort,method="grouped")
plot(rule2_sort)
rule2_lift_gr<-sort(rule2,by="lift",decreasing = TRUE)
top <-  head(rule2_lift_gr,5)
inspect(top)
plot(top)
plot(top,method="grouped")
rule2_lift_ls<-sort(rule2,by="lift",decreasing = FALSE)
low <- head(rule2_lift_ls,5)
inspect(low)
plot(low)
plot(low,method="grouped")




###Rule 3
library(arulesViz)
rule3<- apriori(dataread,parameter=list(support=0.016,confidence=0.008))
plot(rule3)
rule3_sort <- sort(rule3,by="confidence",decreasing = TRUE)
plot(rule3_sort,method="grouped")
plot(rule3_sort)
rule3_lift_gr<-sort(rule3,by="lift",decreasing = TRUE)
top <-  head(rule3_lift_gr,5)
inspect(top)
plot(top)
plot(top,method="grouped")
rule3_lift_ls<-sort(rule3,by="lift",decreasing = FALSE)
low <- head(rule3_lift_ls,5)
inspect(low)
plot(low)
plot(low,method="grouped")



