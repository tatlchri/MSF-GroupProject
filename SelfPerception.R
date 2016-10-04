library(ggplot2)
library(RColorBrewer)  
library(reshape2)
library(plyr)
#Import data
teensdata <- read.csv("C:/Users/Christina/Desktop/Imperial College/Core Courses/Maths and Statistics Foundations/Group Project/September 25-October 9, 2014 and February 10-March 6, 2015 - Teens/data.csv") 


##############Clean data###################


#If no teenagers 13 to 17 live in your household, the survey ends->delete these rows
teensdata<-teensdata[!is.na(teensdata$QS1) ,]
teensdata<-teensdata[teensdata$QS1!=0 ,]

#If the adult,who takes part of the survey is not the parent or legal guardian, delete these rows
teensdata<-teensdata[!is.na(teensdata$QS2) ,]
teensdata<-teensdata[teensdata$QS2!=0 ,]


#Delete rows,which don't contains Child gender
teensdata<-teensdata[!is.na(teensdata$Child_gender),]


#####################################################################
perception<-data.frame("KFSNS1_A"=as.character(teensdata[,"KFSNS1_A"]),"KFSNS1_B"=as.character(teensdata[,"KFSNS1_B"]),"KFSNS1_C"=as.character(teensdata[,"KFSNS1_C"]),"KFSNS1_D"=as.character(teensdata[,"KFSNS1_D"]),"KFSNS1_E"=as.character(teensdata[,"KFSNS1_E"]),"KFB1A"=teensdata[,"KFB1A"],"KFB1B"=teensdata[,"KFB1B"],"KFB1C"=teensdata[,"KFB1C"])

#Keep rows with KFSNS2_A,B,C,D in c(1,2,3) (ONLY DOING IT FOR A WORKS)
perception<-perception[perception$KFSNS1_A %in% c("1","2","3"),]


#Split facebook friends, twitter and instagram followers into categories 

perception1<-perception[!is.na(perception[,"KFB1A"]),]
perception1<-perception1[perception1$KFB1A>=0,]
#Split facebook friends into 4 equal categories
KFB1A_bins<-unname(quantile(perception1[,"KFB1A"],c(.33, .66, 1)))


perception2<-perception[!is.na(perception[,"KFB1B"]),]
perception2<-perception2[perception2$KFB1B>=0,]
#Split twitter followers into 4 equal categories
KFB1B_bins<-unname(quantile(perception2[,"KFB1B"],c(.33, .66, 1)))


perception3<-perception[!is.na(perception[,"KFB1C"]),]
perception3<-perception3[perception3$KFB1C>=0,]

#Split instagram followers into 4 equal categories
KFB1C_bins<-unname(quantile(perception3[,"KFB1C"],c(.33, .66, 1)))


for (i in 1:nrow(perception)){
  if (!is.na(perception[i,"KFB1A"])){
    if (perception[i,"KFB1A"]<=KFB1A_bins[1]){
      perception[i,"KFB1A"]<-1
    }else if (perception[i,"KFB1A"]<=KFB1A_bins[2]){
      perception[i,"KFB1A"]<-2
    }else{
      perception[i,"KFB1A"]<-3
    }
  }
  
  if (!is.na(perception[i,"KFB1B"])){
    if (perception[i,"KFB1B"]<=KFB1B_bins[1]){
      perception[i,"KFB1B"]<-1
    }else if (perception[i,"KFB1B"]<=KFB1B_bins[2]){
      perception[i,"KFB1B"]<-2
    }else{
      perception[i,"KFB1B"]<-3
    }
  }
  
  if (!is.na(perception[i,"KFB1C"])){
    if (perception[i,"KFB1C"]<=KFB1C_bins[1]){
      perception[i,"KFB1C"]<-1
    }else if (perception[i,"KFB1C"]<=KFB1C_bins[2]){
      perception[i,"KFB1C"]<-2
    }else {
      perception[i,"KFB1C"]<-3
    }
  }
}


################################################

x<-count(perception, KFSNS1_A,KFSNS1_B)
ggplot(x, aes(x = KFSNS1_A, y = KFSNS1_B, fill = n)) + geom_tile() + scale_fill_gradient(low = "white",   high = "steelblue") + geom_text(data=x, aes(KFSNS1_A, KFSNS1_B, label = n), color="black", size=rel(5))+
  labs(x = "connected to information", y = "Bad about life", title = "")+
  scale_x_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))+
  scale_y_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))

x2<-count(perception, KFSNS1_A,KFSNS1_C)
ggplot(x2, aes(x = KFSNS1_A, y = KFSNS1_C, fill = n)) + geom_tile() + scale_fill_gradient(low = "white",   high = "steelblue") + geom_text(data=x2, aes(KFSNS1_A, KFSNS1_C, label = n), color="black", size=rel(5))+
  labs(x = "connected to information", y = "connected  to friends feelings", title = "")+
  scale_x_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))+
  scale_y_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))

x3<-count(perception, KFSNS1_A,KFSNS1_D)
ggplot(x3, aes(x = KFSNS1_A, y = KFSNS1_D, fill = n)) + geom_tile() + scale_fill_gradient(low = "white",   high = "steelblue") + geom_text(data=x3, aes(KFSNS1_A, KFSNS1_D, label = n), color="black", size=rel(5))+
  labs(x = "connected to information", y = "pressure to post content with many likes/comments", title = "")+
  scale_x_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))+
  scale_y_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))

x4<-count(perception, KFSNS1_A,KFSNS1_E)
ggplot(x4, aes(x = KFSNS1_A, y = KFSNS1_E, fill = n)) + geom_tile() + scale_fill_gradient(low = "white",   high = "steelblue") + geom_text(data=x4, aes(KFSNS1_A, KFSNS1_E, label = n), color="black", size=rel(5))+
  labs(x = "connected to information", y = "pressure to post content to look good to other", title = "")+
  scale_x_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))+
  scale_y_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))

########

perception[,"KFB1C"]<-as.character(perception[,"KFB1C"])
perception[,"KFB1B"]<-as.character(perception[,"KFB1B"])
perception[,"KFB1A"]<-as.character(perception[,"KFB1A"])

y<-count(perception[!is.na(perception[,"KFB1C"]),], KFSNS1_A,KFB1C)
ggplot(y, aes(x = KFSNS1_A, y = KFB1C, fill = n)) + geom_tile() + scale_fill_gradient(low = "white",   high = "steelblue") + geom_text(data=y, aes(KFSNS1_A, KFB1C, label = n), color="black", size=rel(5))+
  labs(x = "connected to information", y = "Number of instagram followers", title = "")+
  scale_x_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))

y2<-count(perception[!is.na(perception[,"KFB1A"]),],KFSNS1_A,KFB1A)
ggplot(y2, aes(x = KFSNS1_A, y = KFB1A, fill = n)) + geom_tile() + scale_fill_gradient(low = "white",   high = "steelblue") + geom_text(data=y2, aes(KFSNS1_A, KFB1A, label = n), color="black", size=rel(5))+
  labs(x = "connected to information", y = "Number of facebook friends", title = "")+
  scale_x_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))

y3<-count(perception[!is.na(perception[,"KFB1B"]),],KFSNS1_A,KFB1B)
ggplot(y3, aes(x = KFSNS1_A, y = KFB1B, fill = n)) + geom_tile() + scale_fill_gradient(low = "white",   high = "steelblue") + geom_text(data=y3, aes(KFSNS1_A, KFB1B, label = n), color="black", size=rel(5))+
  labs(x = "connected to information", y = "Number of twitter followers", title = "")+
  scale_x_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))