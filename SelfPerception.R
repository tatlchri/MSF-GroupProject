library(ggplot2)
library(RColorBrewer)  
library(reshape2)
library(plyr)
#Import data
teensdata <- read.csv("C:/Users/Christina/Desktop/Imperial College/Core Courses/Maths and Statistics Foundations/Group Project/September 25-October 9, 2014 and February 10-March 6, 2015 - Teens/data.csv") 


##############Clean data###################

"""
#If no teenagers 13 to 17 live in your household, the survey ends->delete these rows
teensdata<-teensdata[!is.na(teensdata$QS1) ,]
teensdata<-teensdata[teensdata$QS1!=0 ,]

#If the adult,who takes part of the survey is not the parent or legal guardian, delete these rows
teensdata<-teensdata[!is.na(teensdata$QS2) ,]
teensdata<-teensdata[teensdata$QS2!=0 ,]


#Delete rows,which don't contains Child gender
teensdata<-teensdata[!is.na(teensdata$Child_gender),]
"""
#load question mapped to short_id etc
question_map<- read.csv("map_column_questions.csv", header = TRUE, sep = ",")
#look up corresponding question in column("Short") and enter Short_question ID
#store search term in search KF13_D/KFR11_H
search<- "KFR11_H"
question_map[question_map[,"Short"] == search,]


############################### Loading and cleaning #################################

perception<-data.frame("KFSNS1_A"=as.character(teensdata[,"KFSNS1_A"]),"KFSNS1_B"=as.character(teensdata[,"KFSNS1_B"]),"KFSNS1_C"=as.character(teensdata[,"KFSNS1_C"]),"KFSNS1_D"=as.character(teensdata[,"KFSNS1_D"]),"KFSNS1_E"=as.character(teensdata[,"KFSNS1_E"]),"KFB1A"=teensdata[,"KFB1A"],"KFB1B"=teensdata[,"KFB1B"],"KFB1C"=teensdata[,"KFB1C"], "Child_age" = teensdata[,"Child_age"],"KF13_D" = teensdata[,"KF13_D"],"KFR11_H" = teensdata[,"KFR11_H"])

#clean out NA answers
perception<-perception[!is.na(perception[,"KFSNS1_A"]),]

#all lines that have neither Fb nor Instagram are removed from the dataset -> 
#WhatsApp doesn´t qualify for the questions but is considered to be social media as well
perception<- perception[!is.na(perception[,"KFB1A"]) | !is.na(perception[,"KFB1C"]),]

#transform to numeric values to calculate cor
for(i in 1:length(questions_short))
{
  perception[,i]<- as.character(perception[,i])
  perception[,i]<- as.numeric(perception[,i])
}

#make added things numeric
perception[,"KFR11_H"]<- as.character(perception[,"KFR11_H"])
perception[,"KFR11_H"]<- as.numeric(perception[,"KFR11_H"])


#clean Facebook column for negative values
perception<- perception[perception$KFSNS1_A>=0,]
perception<- perception[perception$KFSNS1_B>=0,]
perception<- perception[perception$KFSNS1_C>=0,]
perception<- perception[perception$KFSNS1_D>=0,]
perception<- perception[perception$KFSNS1_E>=0,]


#Keep rows with KFSNS2_A,B,C,D in c(1,2,3) (ONLY DOING IT FOR A WORKS)

perception<-perception[perception[,"KFSNS1_A"] %in% c(1,2,3)]


#Split facebook friends, twitter and instagram followers into categories 
perception1<-perception[!is.na(perception[,"KFB1A"]),]
perception1<-perception1[perception1$KFB1A>=0,]

#Split facebook friends into 3 equal categories
KFB1A_bins<-unname(quantile(perception1[,"KFB1A"],c(.33, .66, 1)))


perception2<-perception[!is.na(perception[,"KFB1B"]),]
perception2<-perception2[perception2$KFB1B>=0,]
#Split twitter followers into 3 equal categories
KFB1B_bins<-unname(quantile(perception2[,"KFB1B"],c(.33, .66, 1)))


perception3<-perception[!is.na(perception[,"KFB1C"]),]
perception3<-perception3[perception3$KFB1C>=0,]

#Split instagram followers into 3 equal categories
KFB1C_bins<-unname(quantile(perception3[,"KFB1C"],c(.33, .66, 1)))

#create clusters by quantile of #fried/followers within social media channel
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

#################################### basic analysis #####################################

#Subset, where respondends use both, Facebook and Instagram - this is used to compare some parameters to the big set (perception)
FbIn<- perception[!is.na(perception[,"KFB1A"]) & !is.na(perception[,"KFB1C"]),]

#391user
#there is a acceptable correlation between #friend on Fb and #followers in Instagram based on percentile
cor_FbIn<- cor(FbIn$KFB1A,FbIn$KFB1C)

#This question: How often do you spend time with friendsposting on social media sites? 1.Every day,2.Every few days,3. Less often,4. Never
#with var_FbInKFR11_H = 0.775 and var_perceptionKFR11_H = 0.7704 the values are equally varying regardless
#presence in different social media
var_FbInKFR11_H<- var(FbIn$KFR11_H)
var_perceptionKFR11_H<- var(perception$KFR11_H)

#Calculate the variance of usage among the different cluster of #friend/#follower
var_usage_media<- data.frame()
for(j in 1:3)
{
  var_usage_media[j,1]<- var(FbIn[FbIn[,6]== j,"KFR11_H"])
  var_usage_media[j,2]<- var(perception[perception[,6]== j,"KFR11_H"] , na.rm = TRUE)
  var_usage_media[j,3]<- var(FbIn[FbIn[,8]== j,"KFR11_H"])
  var_usage_media[j,4]<- var(perception[perception[,8]== j,"KFR11_H"], na.rm = TRUE)
}

colnames(var_usage_media)<- c("Facebook_Sub","Facebook_Total", "Instagram_Sub", "Instagram_Total")

# Generally the data of the subset and the set is moving in same direction with almost equal values
# While the medium cohorte has a stronger variance compared to the others

      #Facebook_Sub Facebook_Total Instagram_Sub Instagram_Total
#1    0.7472831      0.7461390     0.7446560       0.7653698
#2    0.8683964      0.8706180     0.9189189       0.8473586
#3    0.7446355      0.7859854     0.7218453       0.6381579


###################### impact by age ##############################


#create subsets by age
perception13<- perception[perception[,"Child_age"]== 13,]
per13_cl_FB<- perception13[!is.na(perception13[,"KFB1A"]),]
per13_cl_IG<- perception13[!is.na(perception13[,"KFB1C"]),]
perception14<- perception[perception[,"Child_age"]== 14,]
per14_cl_FB<- perception14[!is.na(perception14[,"KFB1A"]),]
per14_cl_IG<- perception14[!is.na(perception14[,"KFB1C"]),]
perception15<- perception[perception[,"Child_age"]== 15,]
per15_cl_FB<- perception15[!is.na(perception15[,"KFB1A"]),]
per15_cl_IG<- perception15[!is.na(perception15[,"KFB1C"]),]
perception16<- perception[perception[,"Child_age"]== 16,]
per16_cl_FB<- perception16[!is.na(perception16[,"KFB1A"]),]
per16_cl_IG<- perception16[!is.na(perception16[,"KFB1C"]),]
perception17<- perception[perception[,"Child_age"]== 17,]
per17_cl_FB<- perception17[!is.na(perception17[,"KFB1A"]),]
per17_cl_IG<- perception17[!is.na(perception17[,"KFB1C"]),]


# create subsets of correlation by age group
############### total ###############

cor_sumtot<- data.frame(NA)
cor_sum13<- data.frame(NA)
cor_sum14<- data.frame(NA)
cor_sum15<- data.frame(NA)
cor_sum16<- data.frame(NA)
cor_sum17<- data.frame(NA)

for(i in 1:5)
{
  for(j in 1:5)
  {
    #controle set, calculation for entire set
    if(cor(perception[,i], perception[,j])>=0.99) cor_sumtot[i,j+1]<- 0.0
    else cor_sumtot[i,j+1]<- cor(perception[,i], perception[,j])
    # set for 13 year olds
    if(cor(perception13[,i], perception13[,j])>=0.99) cor_sum13[i,j+1]<- 0.0
    else cor_sum13[i,j+1]<- cor(perception13[,i], perception13[,j])
    # set for 14 year olds
    if(cor(perception14[,i], perception14[,j])>=0.99) cor_sum14[i,j+1]<- 0.0
    else cor_sum14[i,j+1]<- cor(perception14[,i], perception14[,j])
    # set for 15 year olds
    if(cor(perception15[,i], perception15[,j])>=0.99) cor_sum15[i,j+1]<- 0.0
    else cor_sum15[i,j+1]<- cor(perception15[,i], perception15[,j])
    # set for 16 year olds
    if(cor(perception16[,i], perception16[,j])>=0.99) cor_sum16[i,j+1]<- 0.0
    else cor_sum16[i,j+1]<- cor(perception16[,i], perception16[,j])
    # set for 17 year olds
    if(cor(perception17[,i], perception17[,j])>=0.99) cor_sum17[i,j+1]<- 0.0
    else cor_sum17[i,j+1]<- cor(perception17[,i], perception17[,j])
  }
}
colnames(cor_sumtot)<-c("Cohort","KFSNS1_A", "KFSNS1_B", "KFSNS1_C", "KFSNS1_D","KFSNS1_E")
cor_sumtot$Cohort<- 0
colnames(cor_sum13)<-c("Cohort","KFSNS1_A", "KFSNS1_B", "KFSNS1_C", "KFSNS1_D","KFSNS1_E")
cor_sum13$Cohort<- 13
colnames(cor_sum14)<-c("Cohort","KFSNS1_A", "KFSNS1_B", "KFSNS1_C", "KFSNS1_D","KFSNS1_E")
cor_sum14$Cohort<- 14
colnames(cor_sum15)<-c("Cohort","KFSNS1_A", "KFSNS1_B", "KFSNS1_C", "KFSNS1_D","KFSNS1_E")
cor_sum15$Cohort<- 15
colnames(cor_sum16)<-c("Cohort","KFSNS1_A", "KFSNS1_B", "KFSNS1_C", "KFSNS1_D","KFSNS1_E")
cor_sum16$Cohort<- 16
colnames(cor_sum17)<-c("Cohort","KFSNS1_A", "KFSNS1_B", "KFSNS1_C", "KFSNS1_D","KFSNS1_E")
cor_sum17$Cohort<- 17

############### overview of corelation calculation ###################

cor_rownames<-c()
cor_plot<- data.frame(NA)
n<-2
p<-1
for(i in 2:ncol(cor_sum13))
{
  for (j in n:5)
  {
    cor_rownames[p]<-paste(colnames(cor_sum13[i]),"/","\n",colnames(cor_sum13[j+1]))
    cor_plot[p,1]<-cor_sum13[j,i]
    cor_plot[p,2]<-cor_sum14[j,i]
    cor_plot[p,3]<-cor_sum15[j,i]
    cor_plot[p,4]<-cor_sum16[j,i]
    cor_plot[p,5]<-cor_sum17[j,i]
    cor_plot[p,6]<-cor_sumtot[j,i]
    p<- p+1
  }
  n<-n+1
}
cor_plot<- cor_plot[-c(11,12),]
colnames(cor_plot)<- c("Cohort_13","Cohort_14","Cohort_15","Cohort_16","Cohort_17","Total_Cohort")

### correlation plot based on cohorts
ggplot(data = cor_plot, aes(x = cor_rownames, y = 0:1)) + geom_point(aes(y = Cohort_13, colour = "Age_13"))+ 
  geom_point(aes(y = Cohort_14, colour = "Age_14")) +geom_point(aes(y = Cohort_15, colour = "Age_15")) +
  geom_point(aes(y = Cohort_16, colour = "Age_16")) + geom_point(aes(y = Cohort_17, colour = "Age_17")) + 
  geom_point(aes(y = Total_Cohort, colour = "Total"))+ geom_smooth(method = "lm", aes(y = Total_Cohort, Colour = "New"))

###########################################################################################################################
###########################################################################################################################

##### get subsets for by cohorte & # friends
#count(perception[perception[,"Child_age"] == 13 & !is.na(perception[,"KFB1C"]),], vars = c("KFSNS1_B","KFB1C"))

## the following loop calculates all different correlations for all available questions clustered by age and friends usage
#final dateframe with results

cor_plot1<- data.frame()
#sub working set
age_sub_w<- data.frame()
cor_plot1_coln<-c()

#columnnumber for cor_plot1 each column represents the results of a unique combination eg. Age = 13/QuantileFriends = 1
cn<-1
#creates ages to look for
for(i in 13:17)
{
  #creates quantiles to look for
  for(j in 1:3)
  {
    #creates subset with the applicable filters by age and quantile
    age_sub_sm<-perception[perception[,"Child_age"] == i & !is.na(perception[,"KFB1A"]) & perception[, "KFB1A"] == j,]
    #print(summary(age_sub_sm))
    cor_plot1_coln[cn]<-c(paste(as.character(i),"/",as.character(j)))
    #needed to calculate the different correlation (results in 5x5 square matrix)
    for(m in 1:5)
    {
      for(n in 1:5)
      {
        
        #checks whether calculated cor equals 1.0 this would mean same question
        if(cor(age_sub_sm[,m], age_sub_sm[,n])>=0.99) age_sub_w[m,n]<- 0.0
        else age_sub_w[m,n]<- cor(age_sub_sm[,m], age_sub_sm[,n])
      }
    }
    print(age_sub_w)
    # needed to get all values below the trace of the matrix
    s<-2
    p<-1
    #get columns can start at 1
    for(r in 1:ncol(age_sub_w))
    {
      #gets all rows needs to start at because [1,1] equals 0.0000 first field of trace
      for (q in s:5)
      {
        #cor_rownames[p]<-paste(colnames(cor_sum13[r]),"/","\n",colnames(cor_sum13[q+1]))
        #gets the value for each unique combination
        cor_plot1[p,cn]<-age_sub_w[q,r]
        #increases row index for cor_plot1
        p<- p+1
        
      }
      #increases starting index rows for inner loop
      s<-s+1
    }
    print(cor_plot1)
  
  #for each processed unique subset increase column index for cor_plot1
  cn<-cn+1
    
  }
  
}

cor_plot1<- cor_plot1[-c(11,12),]
colnames(cor_plot1)<-c(cor_plot1_coln)


###### split up by Media
questions_short<- c("KFSNS1_A", "KFSNS1_B", "KFSNS1_C", "KFSNS1_D","KFSNS1_E")
colnames(y)<- c("Media_Platform", "Question", "Social_Media_Quantile", "freq")

y<-data.frame(1,count(perception13[!is.na(perception13[,"KFB1A"]),], vars = c(question_name_short,"KFB1A")))
y2<-data.frame(2,count(perception[!is.na(perception[,"KFB1B"]),],vars = c(question_name_short,"KFB1B")))
y3<-data.frame(3,count(perception[!is.na(perception[,"KFB1C"]),],vars = c(question_name_short,"KFB1C")))

#1. KFSNS1_A - In general, does social media make you feel\tmore connected to information about what\222s going on in your friends\222 lives?
###################
r_ig<-count(perception13[!is.na(perception13[,"KFB1A"]),],vars = c("KFSNS1_A","KFB1A"))
r_fb<-count(perception13[!is.na(perception13[,"KFB1C"]),], vars = c("KFSNS1_A","KFB1C"))




#2.  In general, does social media make you feel worse about your own life because of
#what you see from other friends on social media?"

y<-count(perception[!is.na(perception[,"KFB1C"]),], vars = c("KFSNS1_B","KFB1C"))
y2<-count(perception[!is.na(perception[,"KFB1A"]),],vars = c("KFSNS1_B","KFB1A"))
y3<-count(perception[!is.na(perception[,"KFB1B"]),],vars = c("KFSNS1_B","KFB1B"))

#3.In general, does social media make you feel better connected to your friends\222 feelings

y<-count(perception[!is.na(perception[,"KFB1C"]),], vars = c("KFSNS1_C","KFB1C"))
y2<-count(perception[!is.na(perception[,"KFB1A"]),],vars = c("KFSNS1_C","KFB1A"))
y3<-count(perception[!is.na(perception[,"KFB1B"]),],vars = c("KFSNS1_C","KFB1B"))

#4. In general, does social media make you feel pressure to post content that will 
#be popular and get lots of comments or likes?

y<-count(perception[!is.na(perception[,"KFB1C"]),], vars = c("KFSNS1_D","KFB1C"))
y2<-count(perception[!is.na(perception[,"KFB1A"]),],vars = c("KFSNS1_D","KFB1A"))
y3<-count(perception[!is.na(perception[,"KFB1B"]),],vars = c("KFSNS1_D","KFB1B"))

#5. In general, does social media make you feel pressure to only post content
#that makes you look good to others?

y<-count(perception[!is.na(perception[,"KFB1C"]),], vars = c("KFSNS1_E","KFB1C"))
y2<-count(perception[!is.na(perception[,"KFB1A"]),],vars = c("KFSNS1_E","KFB1A"))
y3<-count(perception[!is.na(perception[,"KFB1B"]),],vars = c("KFSNS1_E","KFB1B"))

################
###populating y with connected to information 
perception13[,"KFB1C"]<-as.character(perception[,"KFB1C"])
y[,"KFSNS1_A"]<-as.character(y[,"KFSNS1_A"])
y[,"KFB1A"]<-as.character(y[,"KFB1A"])

#Plotting subset results based on run code
ggplot(y, aes(x = KFSNS1_A, y = KFB1A, fill = freq)) + geom_tile() + scale_fill_gradient(low = "white",   high = "steelblue") + geom_text(data=y, aes(KFSNS1_A, KFB1A, label = freq), color="black", size=rel(5))+
  labs(x = "connected to information", y = "Number of Instagram followers", title = "")+
  scale_x_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))

y2<-count(perception[!is.na(perception[,"KFB1A"]),],vars = c("KFSNS1_A","KFB1A"))
ggplot(y2, aes(x = KFSNS1_A, y = KFB1A, fill = freq)) + geom_tile() + scale_fill_gradient(low = "white",   high = "steelblue") + geom_text(data=y2, aes(KFSNS1_A, KFB1A, label = freq), color="black", size=rel(5))+
  labs(x = "connected to information", y = "Number of facebook friends", title = "")+
  scale_x_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))

y3<-count(perception[!is.na(perception[,"KFB1B"]),],vars = c("KFSNS1_A","KFB1B"))
ggplot(y3, aes(x = KFSNS1_A, y = KFB1B, fill = freq)) + geom_tile() + scale_fill_gradient(low = "white",   high = "steelblue") + geom_text(data=y3, aes(KFSNS1_A, KFB1B, label = freq), color="black", size=rel(5))+
  labs(x = "connected to information", y = "Number of twitter followers", title = "")+
  scale_x_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))


#calculate percentage
#Facebook

calcperFace<- function(y)
{
    y_per<- y
    y_per[,"percentage"]<- c(0)
      for (i in 1:nrow(y))
      {
        tot = sum(y[y[,"KFB1A"] == y[i,"KFB1A"],"freq"])
        y_per[i,"percentage"]<- round(y[i,"freq"]/tot,digits = 2)
        
      }
    
    return (y_per)
}


for (i in 1:nrow(y2))
{
  tot = sum(y[y[,"KFB1A"] == y[i,"KFB1A"],"freq"])
  y_per[i,"percentage"]<- round(y[i,"freq"]/tot,digits = 2)
  
}

ggplot(y_per, aes(x = KFSNS1_A, y = KFB1C, fill = freq)) + geom_tile() + scale_fill_gradient(low = "white",   high = "steelblue") + geom_text(data=y_per, aes(KFSNS1_A, KFB1C, label = percentage), color="black", size=rel(5))+
  labs(x = "connected to information", y = "Number of instagram followers", title = "")+
  scale_x_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))

#Facebook
y2_per<- y2
colnames(y2_per[0,5])<- c("percentage")

for (i in 1:nrow(y2))
{
  tot = sum(y2[y2[,"KFB1A"] == y2[i,"KFB1A"],"freq"])
  y2_per[i,"percentage"]<- round(y2[i,"freq"]/tot,digits = 2)
  
}

ggplot(y2_per, aes(x = KFSNS1_A, y = KFB1A, fill = freq)) + geom_tile() + scale_fill_gradient(low = "white",   high = "steelblue") + geom_text(data=y2_per, aes(KFSNS1_A, KFB1A, label = percentage), color="black", size=rel(5))+
  labs(x = "connected to information", y = "Number of facebook friends", title = "")+
  scale_x_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))


##Twitter analysis
y3_per<- y3
colnames(y3_per[0,4])<- c("percentage")

for (i in 1:nrow(y3))
{
  tot = sum(y3[y3[,"KFB1B"] == y3[i,"KFB1B"],"freq"])
  y3_per[i,"percentage"]<- round(y3[i,"freq"]/tot,digits = 2)
  
}

ggplot(y3_per, aes(x = KFSNS1_A, y = KFB1B, fill = freq)) + geom_tile() + scale_fill_gradient(low = "white",   high = "steelblue") + geom_text(data=y3_per, aes(KFSNS1_A, KFB1B, label = percentage), color="black", size=rel(5))+
  labs(x = "connected to information", y = "Number of twitter followers", title = "")+
  scale_x_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))

resultssum<- data.frame( y$KFSNS1_A, y$KFB1C)



for(i in 1:nrow(resultssum))
{
  cellone<- resultssum[i,"y.KFSNS1_A"]
  celltwo<- resultssum[i,"y.KFB1C"]
  #sum total occurences for unique combiset over different social media channnel
  y_sum<- sum(y_per[y_per[,1]== cellone & y_per[,2] == celltwo,"freq"],y2_per[y2_per[,1]== cellone & y2_per[,2] == celltwo,"freq"],y3_per[y3_per[,1]== cellone & y3_per[,2] == celltwo,"freq"])
  resultssum[i, "TotalOcc"]<- y_sum
  
  #get specific percentages per occurance
  resultssum[i,"Instagram"]<- y_per[y_per[,1]== cellone & y_per[,2] == celltwo,"percentage"]
  resultssum[i,"Facebook"]<- y2_per[y2_per[,1]== cellone & y2_per[,2] == celltwo,"percentage"]
  resultssum[i,"Twitter"]<- y3_per[y3_per[,1]== cellone & y3_per[,2] == celltwo,"percentage"]
  resultssum[i, "Combo"]<- paste(as.character(resultssum[i,"Instagram"]),"/",as.character(resultssum[i,"Facebook"]),"/",as.character(resultssum[i,"Twitter"]))

}

library(ggplot2)

ggplot(resultssum, aes(x = y.KFSNS1_A, y = y.KFB1C, fill = TotalOcc)) + geom_tile() + scale_fill_gradient(low = "white",   high = "steelblue") + geom_text(data=resultssum, aes(y.KFSNS1_A, y.KFB1C, label = Combo), color="black", size=rel(5))+
  labs(x = "feel pressure to post content that creates likes & comments", y = "Number of followers overall", title = "")+
  scale_x_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))

x<-2

print(myf(x))

myf<- function(x)
  {
      x<- x+2
      return(x)
}

cor(perception$Child_age, perception$KFB1A)


for(i in 1:nrow(perception))
{
  perception[i,"KFSNS1_A"]<- as.numeric(perception[i,"KFSNS1_A"])
  
}


ggplot(data = cor_plot, aes(x = 1:10, y = 0:1)) + geom_point(aes(y = Cohort_13, colour = "13"))+ geom_point(aes(y = Cohort_15, colour = "15")) + geom_point(aes(y = Cohort_17, colour = "17")) + geom_smooth( aes(y = Cohort_17, colour = "17"), method = lm)+ geom_smooth( aes(y = Cohort_15, colour = "15"), method = lm)+ geom_smooth( aes(y = Cohort_13, colour = "13"),method = lm)

################################################
perceptiontest<-perception[perception[,"Child_age"] == 13,]

#feel connected through social media vs. feel bad about own life?
x<-count(per17_cl_IG, vars = c("KFSNS1_A","KFSNS1_B"))
ggplot(x, aes(x = KFSNS1_A, y = KFSNS1_B, fill = freq)) + geom_tile() + scale_fill_gradient(low = "white",   high = "steelblue") + geom_text(data=x, aes(KFSNS1_A, KFSNS1_B, label = freq), color="black", size=rel(5))+
  labs(x = "connected to information", y = "Bad about life", title = "")+
  scale_x_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))+
  scale_y_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))

#feel connected through social media vs. better connected to friends feelings
x2<-count(perception13, vars = c("KFSNS1_A","KFSNS1_C"))
ggplot(x2, aes(x = KFSNS1_A, y = KFSNS1_C, fill = freq)) + geom_tile() + scale_fill_gradient(low = "white",   high = "steelblue") + geom_text(data=x2, aes(KFSNS1_A, KFSNS1_C, label = freq), color="black", size=rel(5))+
  labs(x = "connected to information", y = "connected  to friends feelings", title = "")+
  scale_x_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))+
  scale_y_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))

#feel connected through social media vs. feel pressure to post content
x3<-count(perception, vars = c("KFSNS1_A","KFSNS1_D"))
ggplot(x3, aes(x = KFSNS1_A, y = KFSNS1_D, fill = freq)) + geom_tile() + scale_fill_gradient(low = "white",   high = "steelblue") + geom_text(data=x3, aes(KFSNS1_A, KFSNS1_D, label = freq), color="black", size=rel(5))+
  labs(x = "connected to information", y = "pressure to post content with many likes/comments", title = "")+
  scale_x_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))+
  scale_y_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))

#feel connected through social media vs. feel pressure to post certain content
x4<-count(perception, vars = c("KFSNS1_A","KFSNS1_E"))
ggplot(x4, aes(x = KFSNS1_A, y = KFSNS1_E, fill = freq)) + geom_tile() + scale_fill_gradient(low = "white",   high = "steelblue") + geom_text(data=x4, aes(KFSNS1_A, KFSNS1_E, label = freq), color="black", size=rel(5))+
  labs(x = "connected to information", y = "pressure to post content to look good to other", title = "")+
  scale_x_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))+
  scale_y_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))

########################################
