#Self perception/authenticity based on social media presence (Christina /Joh)

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
perception<-data.frame("KFSNS1_A"=as.character(teensdata[,"KFSNS1_A"]),"KFSNS1_B"=as.character(teensdata[,"KFSNS1_B"]),"KFSNS1_C"=as.character(teensdata[,"KFSNS1_C"]),"KFSNS1_D"=as.character(teensdata[,"KFSNS1_D"]),"KFSNS1_E"=as.character(teensdata[,"KFSNS1_E"]))

#Keep rows with KFSNS2_A,B,C,D in c(1,2,3) (ONLY DOING IT FOR A WORKS)
perception<-perception[perception$KFSNS1_A %in% c("1","2","3"),]

x<-count(perception, vars=c("KFSNS1_A","KFSNS1_B"))
ggplot(x, aes(x = KFSNS1_A, y = KFSNS1_B, fill = freq)) + geom_tile() + scale_fill_gradient(low = "white",   high = "steelblue") + geom_text(data=x, aes(KFSNS1_A, KFSNS1_B, label = freq), color="black", size=rel(5))+
  labs(x = "connected to information", y = "Bad about life", title = "")+
  scale_x_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))+
  scale_y_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))

x2<-count(perception, vars=c("KFSNS1_A","KFSNS1_C"))
ggplot(x2, aes(x = KFSNS1_A, y = KFSNS1_C, fill = freq)) + geom_tile() + scale_fill_gradient(low = "white",   high = "steelblue") + geom_text(data=x2, aes(KFSNS1_A, KFSNS1_C, label = freq), color="black", size=rel(5))+
  labs(x = "connected to information", y = "connected  to friends feelings", title = "")+
  scale_x_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))+
  scale_y_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))

x3<-count(perception, vars=c("KFSNS1_A","KFSNS1_D"))
ggplot(x3, aes(x = KFSNS1_A, y = KFSNS1_D, fill = freq)) + geom_tile() + scale_fill_gradient(low = "white",   high = "steelblue") + geom_text(data=x3, aes(KFSNS1_A, KFSNS1_D, label = freq), color="black", size=rel(5))+
  labs(x = "connected to information", y = "pressure to post content with many likes/comments", title = "")+
  scale_x_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))+
  scale_y_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))

x4<-count(perception, vars=c("KFSNS1_A","KFSNS1_E"))
ggplot(x4, aes(x = KFSNS1_A, y = KFSNS1_E, fill = freq)) + geom_tile() + scale_fill_gradient(low = "white",   high = "steelblue") + geom_text(data=x4, aes(KFSNS1_A, KFSNS1_E, label = freq), color="black", size=rel(5))+
  labs(x = "connected to information", y = "pressure to post content to look good to other", title = "")+
  scale_x_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))+
  scale_y_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))

