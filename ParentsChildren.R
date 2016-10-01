#Relationship between parents and children (Kelvin/Bilal)

# ==========================
# Kelvin's beta code (START)
# ==========================

# Open file
survey_data <- read.csv("data.csv", header = TRUE, sep = ",", na.strings = "NA")
# dim(survey_data) would indicate 1642 rows

# Data cleaning
# -------------

# remove rows where caseID is not a valid number
# first convert caseID from Factor into a character vector, then into integer. Invalid entries will turn into NA
case_id <- as.integer(as.character(survey_data$ï..CaseID))
survey_data <- survey_data[!is.na(case_id), ]
# dim(survey_data) would indicate 1638 rows
#   ---> 4 rows removed. Checked with csv manually. Removed rows matches with rows with string instead of integer

# (could do later)
# go through each question/column, and check if responses are valid. 
# remove rows without valid responses 
#  -> should this removal be done here in global level, 
#     or removed at 'local' level -> when studying each question
# For example, some questions only had options 1, 2; but the recorded response might be 3 or negative number.


# Study on correlations
# ---------------------

# Question 1
# 1.	Relationship between parents who are strict with their children and the teen’s usage of social media (could look at differentiation between male and female children)
#   	Hypothesis: The stricter the parent, the more time teen will use social media.

# Study correlation between these two:
  
#  P4:         Which comes closer to the way you approach the time your child spends with his/her friends?  1.          He/She can spend as much time with his/her friends as he/she wants as long as it doesn’t interfere with other pre-planned activities,2.               I limit the time he/she spends with his/her friends, even if it doesn’t interfere with other pre-planned activities.
# [66% response rate]

#  KFR11_H: How often do you spend time with friends  posting on social media sites? 1.Every day,2.Every few days,3. Less often,4. Never
# [48% response rate]

# remove the NAs first (don't modify the survey_data variable. Adopting question-specific data cleaning rather than 'global' data cleaning)
data_specific <- data.frame(x1 = survey_data$P4, y = survey_data$KFR11_H)
# remove rows where both are not NA
data_specific <- data_specific[ !is.na(data_specific$x1) & !is.na(data_specific$y), ]
# remove rows where rows for 'x' are not 1 to 2 (i.e. remove invalid replies)
data_specific <- data_specific[ data_specific$x1 >= 1 & data_specific$x1 <= 2, ]
# remove rows where rows for 'y' are not 1 to 4 (i.e. remove invalid replies)
data_specific <- data_specific[ data_specific$y >= 1 & data_specific$y <= 4, ]
# Did check in the console, that min and max of data_specific$x1 and data_specific$y falls within valid range

# COMMENT
# the above method might not be the best way to data clean
# because you lose the caseID by removing rows
# Perhaps should just fill in rows of each variable as NA
# e.g. when data_specific$x1 is NA, set data_specific$y NA too
# and when performing functions, use na.rm (lm for example, by default, uses na.omit)
data_specific <- data.frame(x1 = survey_data$P4, y = survey_data$KFR11_H)
data_specific[ is.na(data_specific$x1), ] <- NA
data_specific[ is.na(data_specific$y), ] <- NA
data_specific[ !is.na(data_specific$x1) & (data_specific$x1 < 1 | data_specific$x1 > 2), ] <- NA  # needs the !is.na condition otherwise an error throws up
data_specific[ !is.na(data_specific$y) & (data_specific$y < 1 | data_specific$y > 4), ] <- NA  # needs the !is.na condition otherwise an error throws up


# Change the order of answers, from 1 for 'most' to 1 for 'least'
# To illustrate:
# The choices for each question are numbered.
# For P4,
#   1. He/She can spend as much time with his/her friends as he/she wants as long as it doesn’t interfere with other pre-planned activities
#   2. I limit the time he/she spends with his/her friends, even if it doesn’t interfere with other pre-planned activities.
# Here, 1 is the 'most' because child can spend as much time, 2 is 'least'
# but if we were to do regression, we should flip the order
# 1 should be 'least' and 2 should be 'most'.
# Same for question KFR11_H

data_specific$x1 <- abs( data_specific$x1 - 3 )
data_specific$y <- abs( data_specific$y - 5 )

cor.test(data_specific$x1, data_specific$y)
# TO DO: change these cat commands into text in Rmd file.
cat('p-value = 0.01889')
cat('As p-value is less than 5%, we have 95% confidence to reject H0')
cat('where H0 is that correlation is 0')

ml <- lm(y ~ x1, data = data_specific)
summary(ml)
cat('It does seem to have a positive correlation.')
cat('The stricter the parent, the less time child spends on social media')

# Check with related questions to x

# P13_A:          Have you ever used parental controls or other technological means of blocking, filtering or monitoring your child's online activities?  1(YES)-2(NO)-3(does Not Apply)
data_specific[, "x2"] <- survey_data$P13_A
# data clean for new question
data_specific[ is.na(data_specific$x2), ] <- NA
data_specific[ !is.na(data_specific$x2) & (data_specific$x2 < 1 | data_specific$x2 > 3), ] <- NA  

# convert from 1(YES)-2(NO)-3(does Not Apply)
#           to 0(does Not Apply)-1(NO)-2(YES)
data_specific$x2 <- abs(data_specific$x2 - 4) - 1

ml <- lm(y ~ x1 + x2, data = data_specific)
summary(ml)
cat('the p-value for x2 is 0.3228')
cat("This suggests that parental controls of blocking or monitoring child's online activities does not affect the time child spends on social media")
cor.test(data_specific$x2, data_specific$y)
cat('There is no significant evidence to reject H0 hypothesis that correlation between the two variables is 0')

# ==========================
# Kelvin's beta code (END)
# ==========================
