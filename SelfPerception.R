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

############################### Tool to get all relevant information about ask questions #################################
#load question mapped to short_id etc
question_map<- read.csv("map_column_questions.csv", header = TRUE, sep = ",")
#look up corresponding question in column("Short") and enter Short_question ID
#store search term in search KF13_D/KFR11_H
search<- "KFR11_H"
question_map[question_map[,"Short"] == search,]


                      ######################################################
                      ############                              ############
                      ############    -Loading and cleaning-    ############
                      ############                              ############
                      ######################################################


#load dataset as perception inluding the following columns questions:

# Possible answers(1 = Yes, a lot 2 = Yes, a little 3 = No)
# KFSNS1_A: In general, does social media make you feel\tmore connected to information about whats going on in your friends lives?
# KFSNS1_B: In general, does social media make you feel worse about your own life because of what you see from other friends on social media?
# KFSNS1_C: In general, does social media make you feel better connected to your friends feelings?
# KFSNS1_D: In general, does social media make you feel pressure to post content that will be popular and get lots of comments or likes?
# KFSNS1_E: In general, does social media make you feel pressure to only post content that makes you look good to others?

#Possible answers (numeric)
# KFB1A:    How many friends do you have on Facebook?
# KFB1B:    How many followers do you have on Twitter?
# KFB1C:    How many followers do you have on Instagram
#Child_age: How old is your child?

#Possible answers (1 = Yes 2 = No)
#KF13_D:    Do you spend time with your closest friend on a regular basis at any of these places? Online, such as places like social networking sites or gaming sites?

#Possible answers (1 = Every day 2 = Every few days 3 = Less often 4 = Never)
#KFR11_H:   How often do you spend time with friendsposting on social media sites?
perception<-data.frame("KFSNS1_A"=as.character(teensdata[,"KFSNS1_A"]),"KFSNS1_B"=as.character(teensdata[,"KFSNS1_B"]),"KFSNS1_C"=as.character(teensdata[,"KFSNS1_C"]),"KFSNS1_D"=as.character(teensdata[,"KFSNS1_D"]),"KFSNS1_E"=as.character(teensdata[,"KFSNS1_E"]),"KF13_D" = teensdata[,"KF13_D"],"KFR11_H" = teensdata[,"KFR11_H"],"KFB1A"=teensdata[,"KFB1A"],"KFB1B"=teensdata[,"KFB1B"],"KFB1C"=teensdata[,"KFB1C"], "Child_age" = teensdata[,"Child_age"])

############################### CLEANING ###############################

#clean out NA answers (apparently NA in KFSNS1_A also have NA in the othe KFSNS1_ questions)
perception<-perception[!is.na(perception[,"KFSNS1_A"]),]

#all lines that have neither Fb nor Instagram are removed from the dataset -> 
#WhatsApp doesn´t qualify for the questions but is considered to be social media in the questionaire as well
perception<- perception[!is.na(perception[,"KFB1A"]) | !is.na(perception[,"KFB1C"]),]

#All relevant questions that are looked into
questions_short<- c("KFSNS1_A", "KFSNS1_B", "KFSNS1_C", "KFSNS1_D","KFSNS1_E","KF13_D","KFR11_H")

#transform to numeric values to be able to calculate correlation 
for(i in 1:length(questions_short))
{
  perception[,i]<- as.character(perception[,i])
  perception[,i]<- as.numeric(perception[,i])
}

#clean questions from negative values (-> not valid answers but not marked as NA)
for(i in 1:5)
{
  perception<-perception[perception[,i] %in% c(1,2,3),]
}

#Create even groups by 33% - quantiles

#Facebook 
KFB1A_bins<-unname(quantile(perception1[,"KFB1A"],c(.33, .66, 1)))
#Twitter
KFB1B_bins<-unname(quantile(perception2[,"KFB1B"],c(.33, .66, 1)))
#Instagram
KFB1C_bins<-unname(quantile(perception3[,"KFB1C"],c(.33, .66, 1)))



#create clusters by quantile of #fried/followers within social media 
#channel replace actual number with cluster
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

                      ######################################################
                      ############                              ############
                      ############        -Pre Analysis-        ############
                      ############                              ############
                      ######################################################


#   Subset, where respondends use both, Facebook and Instagram - 
#     this is used to compare some parameters to the big set (perception)
#     number of respondends using both is 391
FbIn<- perception[!is.na(perception[,"KFB1A"]) & !is.na(perception[,"KFB1C"]),]


#the correlation between quantiles among the different media is 0.46348
#   Question: Do #friends on facebook and #followers of the same person belong to the same quantile?
cor_FbIn<- cor(FbIn$KFB1A,FbIn$KFB1C)


#This question: How often do you spend time with friendsposting on social media sites? 1.Every day,2.Every few days,3. Less often,4. Never
#with var_FbInKFR11_H = 0.775 (subset) and var_perceptionKFR11_H = 0.7704 (set) 
#the values are equally varying regardless of presence in different social media

var_FbInKFR11_H<- var(FbIn$KFR11_H)
var_perceptionKFR11_H<- var(perception$KFR11_H)


#Calculate the variance of usage among the different quantiles based on #friend/#follower
# in subset and set
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
# While the medium quantile has a stronger variance compared to the others

      #Facebook_Sub Facebook_Total Instagram_Sub Instagram_Total
#1    0.7472831      0.7461390     0.7446560       0.7653698
#2    0.8683964      0.8706180     0.9189189       0.8473586
#3    0.7446355      0.7859854     0.7218453       0.6381579



######################################################
############                              ############
############          -Analysis-          ############
############    -Correlation by age-      ############
############                              ############
######################################################


#create subsets by age
perception13<- perception[perception[,"Child_age"]== 13,]
perception14<- perception[perception[,"Child_age"]== 14,]
perception15<- perception[perception[,"Child_age"]== 15,]
perception16<- perception[perception[,"Child_age"]== 16,]
perception17<- perception[perception[,"Child_age"]== 17,]


# create subsets of correlation by age group (to be recoded like lower example)
############### total ###############

# final df used for plotting
cor_plot_age<- data.frame()
#final df giving #observations per subset
n_per_cluster_age<- data.frame()
#rownames from perception df
perception_rnames<- colnames(perception)

#working subsets
age_sub_age<- data.frame()
age_sub_age_cor<-data.frame()

#vectors for row and column names
cor_sub_rnames<-c()
cor_plot_age_coln<-c()

#columnnumber for cor_plot_age each column represents the results of a unique combination eg. Age = 13/QuantileFriends = 1
cn<-1


######################################################
############         -Main loop-          ############
######################################################


#gets the ages relevant for analysis 13-17 - 18 represents total
for(i in 13:18)
{
    #creates subset with the applicable filters by age
    if(i < 18) age_sub_age<-perception[perception[,"Child_age"] == i,]
    else 
      {
        #take total set as working subset
        age_sub_age<-perception
        i<- "Total"
      }
    
    #creates string that contains relevant information about analysed cohort
    subsetname<- paste("Cohort",as.character(i))
    
    #populates df containing #observations in cohort subset
    n_per_cluster_age[cn,1]<-subsetname
    n_per_cluster_age[cn,2]<- nrow(age_sub_age)
    
    #assigns current cohort name to column names vector
    cor_plot_age_coln[cn]<-subsetname
    
    #needed to calculate the different correlation (results in 5x5 square matrix)
    for(m in 1:5)
    {
      for(n in 1:5)
      {
        #checks whether calculated cor equals 1.0 this would mean same question and creates square matrix
        if(cor(age_sub_age[,m], age_sub_age[,n])>=0.99) age_sub_age_cor[m,n]<- 0.0
        else age_sub_age_cor[m,n]<- cor(age_sub_age[,m], age_sub_age[,n])
      }
    }
    
    # needed to get all values below the trace of the matrix
    #initialize row count at two 
    s<-2
    #initialise row counter for transposing values from square matrix in 1 column
    p<-1
    
    #columns can start at 1
    for(r in 1:ncol(age_sub_age_cor))
    {
      #gets all rows needs to start at because [1,1] equals 0.0000 first field of trace
      #q gets increased by increasing s per total column iteration
      for (q in s:5)
      {
        #cor_rownames[p]<-paste(colnames(cor_sum13[r]),"/","\n",colnames(cor_sum13[q+1]))
        #gets the value for each unique combination
        cor_plot_age[p,cn]<-age_sub_age_cor[q,r]
        
        #assigns question combination to vector
        cor_sub_rnames[p]<-paste(as.character(perception_rnames[r]),"/","\n",as.character(perception_rnames[q]))
        
        #increases row index for cor_plot1
        p<- p+1
      }
      #increases starting index rows for inner loop
      s<-s+1
    }

    #for each processed unique subset increase column index for cor_plot_age
    cn<-cn+1
}

#assign column names and row names - delete last two items from df
colnames(cor_plot_age)<-cor_plot_age_coln
rownames(cor_plot_age)<-cor_sub_rnames
cor_plot_age<- cor_plot_age[(-c(11,12)),]

######################################################
############          -plotting-          ############
######################################################


### correlation plot based on cohorts
ggplot(data = cor_plot_age, aes(x = cor_rownames, y = 0:1)) + 
  geom_boxplot(aes(y = `Cohort 13`, colour = "Age_13"))+
  geom_boxplot(aes(y = `Cohort 14`, colour = "Age_14"))+
  geom_boxplot(aes(y = `Cohort 15`, colour = "Age_15"))+
  geom_boxplot(aes(y = `Cohort 16`, colour = "Age_16"))+
  geom_boxplot(aes(y = `Cohort 17`, colour = "Age_17"))+
  geom_boxplot(aes(y = `Cohort Total`, colour = "Total"))
  


######################################################
############                              ############
############          -Analysis-          ############
############        -Correlation-         ############
############       -Age & Quantile-       ############
############                              ############
######################################################



cor_plot_quantile_FB<- data.frame()
cor_plot_quantile_IG<- data.frame()
#sub working set
age_sub_w_FB<- data.frame()
age_sub_w_IG<- data.frame()

cor_quantile_coln<-c()

n_per_cluster_quantile_FB<- data.frame()
n_per_cluster_quantile_IG<- data.frame()
#columnnumber for cor_plot1 each column represents the results of a unique combination eg. Age = 13/QuantileFriends = 1
cn<-1
#creates ages to look for
for(i in 13:18)
{
  #creates quantiles to look for
  for(j in 1:3)
  {
    if(i == 18)
    {
      age_sub_sm_FB<-perception[!is.na(perception[,"KFB1A"]) & perception[, "KFB1A"] == j,]
      age_sub_sm_IG<-perception[!is.na(perception[,"KFB1C"]) & perception[, "KFB1C"] == j,]
      x<-"Total"
    }
    
    else
      {
        age_sub_sm_FB<-perception[perception[,"Child_age"] == i & !is.na(perception[,"KFB1A"]) & perception[, "KFB1A"] == j,]
        age_sub_sm_IG<-perception[perception[,"Child_age"] == i & !is.na(perception[,"KFB1C"]) & perception[, "KFB1C"] == j,]
        x<-i
      } 
    
    #creates subset with the applicable filters by age and quantile
    
    #print(summary(age_sub_sm))
    n_per_cluster_quantile[cn,1]<-paste(as.character(x),"/",as.character(j))
    n_per_cluster_quantile[cn,2]<- nrow(age_sub_sm_FB)
    n_per_cluster_quantile[cn,3]<- nrow(age_sub_sm_IG)

    cor_quantile_coln[cn]<-c(paste(as.character(x),"/",as.character(j)))
    #needed to calculate the different correlation (results in 5x5 square matrix)
    for(m in 1:5)
    {
      for(n in 1:5)
      {
        
        #checks whether calculated cor equals 1.0 this would mean same question
        if(cor(age_sub_sm_FB[,m], age_sub_sm_FB[,n])>=0.99) age_sub_w_FB[m,n]<- 0.0
        else age_sub_w_FB[m,n]<- cor(age_sub_sm_FB[,m], age_sub_sm_FB[,n])
        
        if(cor(age_sub_sm_IG[,m], age_sub_sm_IG[,n])>=0.99) age_sub_w_IG[m,n]<- 0.0
        else age_sub_w_IG[m,n]<- cor(age_sub_sm_IG[,m], age_sub_sm_IG[,n])
      }
    }
    #print(age_sub_w)
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
        cor_plot_quantile_FB[p,cn]<-age_sub_w_FB[q,r]
        cor_plot_quantile_IG[p,cn]<-age_sub_w_IG[q,r]
        
        #increases row index for cor_plot1
        p<- p+1
        
      }
      #increases starting index rows for inner loop
      s<-s+1
    }
    #print(cor_plot1)
  
  #for each processed unique subset increase column index for cor_plot1
  cn<-cn+1
    
  }
  
}


######################################################
############          -Finalising-        ############
######################################################


#Finalising returns
rownames(cor_plot_quantile_FB)<-cor_sub_rnames
colnames(cor_plot_quantile_FB)<-c(cor_quantile_coln)
cor_plot_quantile_FB<- cor_plot_quantile_FB[-c(11,12),]

rownames(cor_plot_quantile_IG)<-cor_sub_rnames
colnames(cor_plot_quantile_IG)<-c(cor_quantile_coln)
cor_plot_quantile_IG<- cor_plot_quantile_IG[-c(11,12),]



## eliminating the most non representative cluster with the given constraints

#Facebook & Instagram (remove lowest 10% by quantity of occurences)
n_per_cluster_bins_FB<-unname(quantile(n_per_cluster_quantile[,"V2"],c(.1,1)))
n_per_cluster_bins_FB<-unname(quantile(n_per_cluster_quantile[,"V3"],c(.1,1)))

for (i in 1:nrow(n_per_cluster_quantile))
  {
  if (n_per_cluster_quantile[i,"V2"]<=n_per_cluster_bins_FB[1]){n_per_cluster_quantile[i,4]<-1}
  else{n_per_cluster_quantile[i,4]<-2}
  
  if (n_per_cluster_quantile[i,"V3"]<=n_per_cluster_bins[1]){n_per_cluster_quantile[i,5]<-1}
  else{n_per_cluster_quantile[i,5]<-2}
  }

colnames(n_per_cluster_quantile)<- c("Cohort", "Occurences_FB", "Occurences_IG","Quantile_Group_FB","Quantile_Group_FB")

######################################################
############          -plotting-          ############
######################################################


#Showing #number of responses per cohorte (age & quantile) (final)
n_per_cluster_quantile_melt <- melt(n_per_cluster_quantile[,1:3], id = c("Cohort"))
#plot
ggplot(n_per_cluster_quantile_melt, aes(x = Cohort, y = value, fill = variable)) + 
  geom_bar(position = "dodge", stat = "identity")+ 
  labs(x = "Cluster(age & quantile)", y = "# of occurences", title = "")+
  ggtitle("# of observations for each cluster\n Facebook/Instagram")

#final plot for age cor with questions
#1:5 is just for values excluding total results
cor_plot_age_t <- as.data.frame(t(cor_plot_age[,1:5]))
cor_plot_age_t_melt<-melt(cor_plot_age_t[,1:10])
colnames(cor_plot_age_t_melt)<-c("Question_combination", "Correlation")
#plot
ggplot(cor_plot_age_t_melt, aes(x = Question_combination, y = Correlation)) + 
  geom_boxplot(notch = FALSE,outlier.colour = "red", outlier.shape = 1)+
  ggtitle("Calculated correlations for individual\nquestion combination clustered by age")

#age, quantile for Facebook
cor_plot_quantile_FB_t<- as.data.frame(t(cor_plot_quantile_FB[,1:15]))
cor_plot_quantile_FB_t_melt<-melt(cor_plot_quantile_FB_t[,1:10])
colnames(cor_plot_quantile_FB_t_melt)<-c("Question_combination", "Correlation")
#plot
ggplot(cor_plot_quantile_FB_t_melt, aes(x = Question_combination, y = Correlation)) + 
  geom_boxplot(notch = FALSE,outlier.colour = "red",outlier.size = 3 , outlier.shape = 1)+
  geom_jitter(width = 0.3, shape = 2)+
  ggtitle("Calculated correlations for individual question combination\n clustered by age, quantile of friends and Facebook")


#age, quantile for Instagram
cor_plot_quantile_IG_t<- as.data.frame(t(cor_plot_quantile_IG[,1:15]))
cor_plot_quantile_IG_t_melt<-melt(cor_plot_quantile_IG_t[,1:10])
colnames(cor_plot_quantile_IG_t_melt)<-c("Question_combination", "Correlation")
#plot
ggplot(cor_plot_quantile_IG_t_melt, aes(x = Question_combination, y = Correlation), ymax = "blue") + 
  geom_boxplot(notch = FALSE,outlier.colour = "red", outlier.shape = 1)+
  geom_jitter(width = 0.3, shape = 2)+
  ggtitle("Calculated correlations for individual question combination\n clustered by age, quantile of friends and Instagram")


#ggplot(y, aes(x = KFSNS1_A, y = KFB1A, fill = freq)) + geom_tile() + scale_fill_gradient(low = "white",   high = "steelblue") + 
#geom_text(data=y, aes(KFSNS1_A, KFB1A, label = freq), color="black", size=rel(5))+
#  labs(x = "connected to information", y = "Number of Instagram followers", title = "")+
#  scale_x_discrete(labels = c("1"="Yes a lot", "2"="Yes,  a little ","3"="No"))
