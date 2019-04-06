
################################################################################
##################### LOADING REQUIRED LIBRARY #################################
################################################################################
library(MASS)
library(ggplot2)
library(fuzzyjoin)
library(stringr)
library(Information)
library(cowplot)
library(lattice)
library(survival)
library(Formula)
library(ggcorrplot)
library(car)
library(caret)
library(dplyr)
library(tidyr)
library(DMwR) # For SMOTE Analysis
library(randomForest)
library(ROSE)
library(rpart)
library(rpart.plot)

################################################################################
####################### LOADING THE GIVEN DATA SETS ############################
################################################################################


demographic<-read.csv("Demographic data.csv",stringsAsFactors = F)
credit_beareau<-read.csv("Credit Bureau data.csv",stringsAsFactors = F)

str(demographic)
str(credit_beareau)

dim(demographic)
dim(credit_beareau)


#-------------------------------------------------------------------------------

#There are no duplicate rows 
sum(duplicated(demographic))
sum(duplicated(credit_beareau))

#Verifying the duplicate entries for the Application ID column for both data sets

sum(duplicated(demographic$Application.ID))
#Found 3 duplicate records

sum(duplicated(credit_beareau$Application.ID))
#Found 3 duplicate records

#Removed the duplicate values from the data frame
demographic <- demographic[-which(duplicated(demographic$Application.ID) == T), ]
sum(duplicated(demographic$Application.ID))

credit_beareau <- credit_beareau[-which(duplicated(credit_beareau$Application.ID) == T), ]
sum(duplicated(credit_beareau$Application.ID))

#There are no more duplicate Application Ids present in the data sets

#Capturing the rejected application

Rejected_demographic <- demographic[is.na(demographic$Performance.Tag),]
Rejected_credit_beareau <- credit_beareau[is.na(credit_beareau$Performance.Tag),]


#------------------------------------------------------------------------------

#Verifying for NA values in data set

sum(is.na(demographic))

#There are 1428 NA values present in the demographic data set

sum(is.na(credit_beareau))

#There are 3028 NA values present in the given credit beareau data set


#------------------------------------------------------------------------------


#Column wise NA counts for demographic data set


sapply(demographic, function(x) sum(is.na(x)))

# There are 3 NA values in "No of dependents" column and 1425 NA values present in "preformance Tag" column

sapply(credit_beareau, function(x) sum(is.na(x)))

#Below are the number of NA values present in the each column of credit beareau data 

# No.of.trades.opened.in.last.6.months   : 1
# Presence.of.open.home.loan             : 272
# Outstanding.Balance                    : 272
# Performance.Tag                        : 1425

# From the above two results we can observe that there 1425 performance tag data is missing.(or is NA values)


#-------------------------------------------------------------------------------

#As we could see the NA values in performance Tag , so we will remove those records from the given data set

#Removing the performance tag from demograpic data set

demographic <- demographic[!is.na(demographic$Performance.Tag),]

#Removing the performance tag from credit beareau data set

credit_beareau <- credit_beareau[!is.na(credit_beareau$Performance.Tag),]

#Verify weather the records contain NA values in performance Tag column got removed from both the data set or not

sapply(demographic, function(x) sum(is.na(x)))
sapply(credit_beareau, function(x) sum(is.na(x)))

#The records contain NA values in performance Tag column got removed successfully from both the data set

#--------------------------------------------------------------------------------

# Verify weather Application ID column in both the data set is same or is ther any different records present in it

setdiff(demographic$Application.ID,credit_beareau$Application.ID)  

#application ids in both demographic and credit beareau data sets are same

####################################################################################
################## Analysis on each column #########################################
####################################################################################


#Analysis on individual columns in demographic data set

summary(as.factor(demographic$Age))

#There Age values containing 0 and -3 are present in the give data set and we need to remove it for further analysis.

summary(as.factor(demographic$Gender))

#There are 16506 Females and 53359 Males present in the given data set.

summary(as.factor(demographic$Marital.Status..at.the.time.of.application.))

#There are 59544 Married individuals and 10317 single indiviuals present in the given data set.

summary(as.factor(demographic$No.of.dependents))

# We could see no of dependents 1,2,3 are more than 5,6
# 1     2     3     4     5       NA's 
# 15218 15128 15645 11998 11875     3 

summary(as.factor(demographic$Income))

# There are some negetive income values present the given data set and it has to be removed

summary(as.factor(demographic$Education))

#There are 118 record which does not contain any education details
#           Bachelor      Masters       Others        Phd   Professional 
# 118        17302        23481          119         4463        24384 

summary(as.factor(demographic$Profession))

# There are more number (39673) of salaried professionals are there in the given data set

#       SAL      SE   SE_PROF 
# 13   39673   13925   16256 

summary(as.factor(demographic$Type.of.residence))

# There are more number of (52277) customers who are staying for rent in the give data set

#           Company provided   Living with Parents        Others             Owned              Rented 
# 8                1603                1778                 198               14003               52277 


summary(as.factor(demographic$No.of.months.in.current.residence))

# There are more number of customers who has been staying in the current residense since 6 months only (Around 33576 customers)

summary(as.factor(demographic$No.of.months.in.current.company))

summary(as.factor(demographic$Performance.Tag))


# 0       1 
# 66920  2947


#----------------------------------------------------------------------------------------

# Analysis on individual columns in credit beareau data set

summary(as.factor(credit_beareau$No.of.times.90.DPD.or.worse.in.last.6.months))

#  0     1      2      3 
# 54664 13219  1776   208 

summary(as.factor(credit_beareau$No.of.times.60.DPD.or.worse.in.last.6.months))

#   0     1     2     3      4     5 
# 51870 11130  4917  1469   411    70 

summary(as.factor(credit_beareau$No.of.times.30.DPD.or.worse.in.last.6.months))

#   0     1     2     3     4      5     6     7 
# 50098  9500  5897  2830  1045   386    96    15 

summary(as.factor(credit_beareau$No.of.times.90.DPD.or.worse.in.last.12.months))

#   0     1     2     3     4      5 
# 50492 11663  6160  1244   272    36 

summary(as.factor(credit_beareau$No.of.times.60.DPD.or.worse.in.last.12.months))

#   0     1     2     3     4     5     6       7 
# 45868 12816  6414  3205  1048   398   111     7 

summary(as.factor(credit_beareau$No.of.times.30.DPD.or.worse.in.last.12.months))

#   0     1     2     3     4     5     6     7       8     9 
# 44857 11474  6116  4136  1924   853   376   107    23     1 

summary(as.factor(credit_beareau$Avgas.CC.Utilization.in.last.12.months))

#There are 1023 NA,s present in the given column.

summary(as.factor(credit_beareau$No.of.trades.opened.in.last.6.months))

# There is one NA value present in the given column

#   0     1     2     3     4     5     6     7     8       9    10    11    12  NA's 
# 12194 20121 12116  9402  6297  3665  2336  1649  1154   618   238    65    11     1

summary(as.factor(credit_beareau$No.of.trades.opened.in.last.12.months))

summary(as.factor(credit_beareau$No.of.PL.trades.opened.in.last.6.months))

#    0     1     2     3     4     5     6 
# 31080 13546 12565  7949  3341  1090   296 

summary(as.factor(credit_beareau$No.of.PL.trades.opened.in.last.12.months))

#   0     1     2     3     4     5     6     7     8     9      10    11    12 
# 25824  6641  6830  8130  7903  6189  4023  2223  1172   601   255    66    10 

summary(as.factor(credit_beareau$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.))

#   0     1     2     3     4     5     6     7     8     9      10 
# 25069 13175 12831  7258  4248  3019  1750  1149   835   425   108 

summary(as.factor(credit_beareau$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.))

#   0     1     2     3     4     5     6     7     8     9      10    11    12    13    14    15    16    17    18    19    20 
# 20581  3899  7907  8978  7113  4927  3614  2992  2345  1777  1508  1231   936   789   553   360   212    97    40     6     2 

summary(as.factor(credit_beareau$Presence.of.open.home.loan))

#There are 272 NA values present in the given column.

# 0       1    NA's 
# 51524 18071   272 

summary(as.factor(credit_beareau$Outstanding.Balance))

#There are 272 NA values present in the given column.

summary(as.factor(credit_beareau$Total.No.of.Trades))

summary(as.factor(credit_beareau$Presence.of.open.auto.loan))

# 0       1 
# 63937  5930 

summary(as.factor(credit_beareau$Performance.Tag))

#   0     1 
# 66920  2947 


#--------------------------------------------------------------------------------


#################################################################################
######################### Univariate Analysis ###################################
#################################################################################

#Ploting the individual columns in demographic data against performance tag to see how they are distributed


#----- AGE ----#

# Binning the age variable and store it into "binning.age".

demographic$binning.age <- as.factor(cut(demographic$Age, breaks = c(16, 20, 30, 40, 50, 60, 70)))


plot1 = ggplot(demographic[-which(demographic$Performance.Tag == 0),],aes(x=binning.age,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))

plot2 = ggplot(demographic,aes(x=binning.age,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))

plot_grid(plot1,plot2,labels = c('Age dist with default data','Age dist on entire data'))

#The age group 30 to 40,40 to 50 and 50 to 60 got most credit cards and they are only bunch of customers who defaulted more


#---- Gender -----#

plot3 = ggplot(demographic[-which(demographic$Performance.Tag == 0),],aes(x=Gender,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))
plot4 = ggplot(demographic,aes(x=Gender,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))

plot_grid(plot3,plot4,labels = c('Gender dist with default data','Gender dist on entire data'))
# There are more number of male customers who got defaulted


#---- Marital status ----#

plot5 = ggplot(demographic[-which(demographic$Performance.Tag == 0),],aes(x=Marital.Status..at.the.time.of.application.,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))
plot6 = ggplot(demographic,aes(x=Marital.Status..at.the.time.of.application.,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))

plot_grid(plot5,plot6,labels = c('Marital status dist with default data','Marital status dist on entire data'))

# There are more number of customers defaulted who got married.




#---- No of dependents ----#

plot7 = ggplot(demographic[-which(demographic$Performance.Tag == 0),],aes(x=No.of.dependents,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))
plot8 = ggplot(demographic,aes(x=No.of.dependents,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))

plot_grid(plot7,plot8,labels = c('No of dependents dist with default data','No of dependents dist on entire data'))
# No of dependents 1,2,3 are defaulted more but as we can say they are equally distributed


#---- Income ----#

demographic$binning.income <- as.factor(cut(demographic$Income, breaks = c(1, 10, 20, 30, 40, 50, 60,70)))

plot9 = ggplot(demographic[-which(demographic$Performance.Tag == 0),],aes(x=binning.income,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))
plot10 = ggplot(demographic,aes(x=binning.income,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))

plot_grid(plot9,plot10,labels = c('Income dist with default data','Income dist on entire data'))
# As the income level increased the default rate got decreased



#---- Education ----#

plot11 = ggplot(demographic[-which(demographic$Performance.Tag == 0),],aes(x=Education,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))
plot12 = ggplot(demographic,aes(x=Education,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))

plot_grid(plot11,plot12,labels = c('Education dist with default data','Education dist on entire data'))

#As there are more number of credit cards are issued to educated customers , so the default rate is also high from them


#---- Profession ----#

plot13 = ggplot(demographic[-which(demographic$Performance.Tag == 0),],aes(x=Profession,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))
plot14 = ggplot(demographic,aes(x=Profession,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))

# Interestingly salaried got defaulted more

plot_grid(plot13,plot14,labels = c('Profession dist with default data','Profession dist on entire data'))


#---- Type of residence ----#

plot15 = ggplot(demographic[-which(demographic$Performance.Tag == 0),],aes(x=Type.of.residence,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))
plot16 = ggplot(demographic,aes(x=Type.of.residence,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))

plot_grid(plot15,plot16,labels = c('type of residence dist with default data','type of residence dist on entire data'))
# Customers who are staying in rented got defaulted more.


#---- No of months in current residence ----#



plot17 = ggplot(demographic[-which(demographic$Performance.Tag == 0),],aes(x=No.of.months.in.current.residence,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))
plot18 = ggplot(demographic,aes(x=No.of.months.in.current.residence,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))

plot_grid(plot17,plot18,labels = c('No of months in current residence dist with default data','No of months in current residence dist on entire data'))
# Customers who are staying last six months got defaulted more


#---- No of months in current company ----#



plot19 = ggplot(demographic[-which(demographic$Performance.Tag == 0),],aes(x=No.of.months.in.current.company,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))
plot20 = ggplot(demographic,aes(x=No.of.months.in.current.company,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))

# Customers who got joined 3 months ago got defaulted more


plot_grid(plot19,plot20,labels = c('No of months in current company dist with default data','No of months in current company dist on entire data'))




#------------------------------------------------------------------------------------------------------------------------


#Ploting the individual columns in credit_beareau data against performance tag to see how they are distributed

#---- No.of.times.90.DPD.or.worse.in.last.6.months ----#

plot1 = ggplot(credit_beareau[-which(credit_beareau$Performance.Tag == 0),],aes(x=No.of.times.90.DPD.or.worse.in.last.6.months,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))
plot2 = ggplot(credit_beareau,aes(x=No.of.times.90.DPD.or.worse.in.last.6.months,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))

# No of 90 days PDT in last 6 months atleast once are more.

plot_grid(plot1,plot2,labels = c('No.of.times.90.DPD.or.worse.in.last.6.months dist with default data','No.of.times.90.DPD.or.worse.in.last.6.months dist on entire data'))



#---- No.of.times.60.DPD.or.worse.in.last.6.months ----#

plot3 = ggplot(credit_beareau[-which(credit_beareau$Performance.Tag == 0),],aes(x=No.of.times.60.DPD.or.worse.in.last.6.months,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))
plot4 = ggplot(credit_beareau,aes(x=No.of.times.60.DPD.or.worse.in.last.6.months,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))

plot_grid(plot3,plot4,labels = c('No.of.times.60.DPD.or.worse.in.last.6.months dist with default data','No.of.times.60.DPD.or.worse.in.last.6.months dist on entire data'))
# No of 60 days PDT in last 6 months atleast once are more.


#---- No.of.times.30.DPD.or.worse.in.last.6.months ----#

plot5 = ggplot(credit_beareau[-which(credit_beareau$Performance.Tag == 0),],aes(x=No.of.times.30.DPD.or.worse.in.last.6.months,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))
plot6 = ggplot(credit_beareau,aes(x=No.of.times.30.DPD.or.worse.in.last.6.months,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))

# No of 30 days PDT in last 6 months once and twice are more.

plot_grid(plot5,plot6,labels = c('No.of.times.30.DPD.or.worse.in.last.6.months dist with default data','No.of.times.30.DPD.or.worse.in.last.6.months dist on entire data'))


#---- No.of.times.90.DPD.or.worse.in.last.12.months ----#

plot7 = ggplot(credit_beareau[-which(credit_beareau$Performance.Tag == 0),],aes(x=No.of.times.90.DPD.or.worse.in.last.12.months,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))
plot8 = ggplot(credit_beareau,aes(x=No.of.times.90.DPD.or.worse.in.last.12.months,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))


# No of 90 days PDT in last 12 months atleast once are more.
plot_grid(plot7,plot8,labels = c('No.of.times.90.DPD.or.worse.in.last.12.months dist with default data','No.of.times.90.DPD.or.worse.in.last.12.months dist on entire data'))


#---- No.of.times.60.DPD.or.worse.in.last.12.months ----#
plot9 = ggplot(credit_beareau[-which(credit_beareau$Performance.Tag == 0),],aes(x=No.of.times.60.DPD.or.worse.in.last.12.months,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))
plot10 = ggplot(credit_beareau,aes(x=No.of.times.60.DPD.or.worse.in.last.12.months,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))

plot_grid(plot9,plot10,labels = c('No.of.times.60.DPD.or.worse.in.last.12.months dist with default data','No.of.times.60.DPD.or.worse.in.last.12.months dist on entire data'))

# No of 60 days PDT in last 12 months atleast once are more.



#---- No.of.times.30.DPD.or.worse.in.last.12.months ----#

plot11 = ggplot(credit_beareau[-which(credit_beareau$Performance.Tag == 0),],aes(x=No.of.times.30.DPD.or.worse.in.last.12.months,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))
plot12 = ggplot(credit_beareau,aes(x=No.of.times.30.DPD.or.worse.in.last.12.months,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))

plot_grid(plot11,plot12,labels = c('No.of.times.30.DPD.or.worse.in.last.12.months dist with default data','No.of.times.30.DPD.or.worse.in.last.12.months dist on entire data'))
# No of 30 days PDT in last 12 months once ,twice and thrice are more.


#---- Avgas.CC.Utilization.in.last.12.months ----#



plot13 = ggplot(credit_beareau[-which(credit_beareau$Performance.Tag == 0),],aes(x=Avgas.CC.Utilization.in.last.12.months,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))
plot14 = ggplot(credit_beareau,aes(x=Avgas.CC.Utilization.in.last.12.months,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))

plot_grid(plot13,plot14,labels = c('Avgas.CC.Utilization.in.last.12.months dist with default data','Avgas.CC.Utilization.in.last.12.months dist on entire data'))
# There are more number of NA's


#---- No.of.trades.opened.in.last.6.months ----#

plot15 = ggplot(credit_beareau[-which(credit_beareau$Performance.Tag == 0),],aes(x=No.of.trades.opened.in.last.6.months,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))
plot16 = ggplot(credit_beareau,aes(x=No.of.trades.opened.in.last.6.months,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))

plot_grid(plot15,plot16,labels = c('No.of.trades.opened.in.last.6.months dist with default data','No.of.trades.opened.in.last.6.months dist on entire data'))
# customers opened 1,2,3,4 number of trades in last 6 months are defaluted more.


#---- No.of.trades.opened.in.last.12.months ----#

plot17 = ggplot(credit_beareau[-which(credit_beareau$Performance.Tag == 0),],aes(x=No.of.trades.opened.in.last.12.months,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))
plot18 = ggplot(credit_beareau,aes(x=No.of.trades.opened.in.last.12.months,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))

plot_grid(plot17,plot18,labels = c('No.of.trades.opened.in.last.12.months dist with default data','No.of.trades.opened.in.last.12.months dist on entire data'))
# customers opened 2 to 11 number of trades in last 12 months are defaluted more.


#---- No.of.PL.trades.opened.in.last.6.months ----#

plot19 = ggplot(credit_beareau[-which(credit_beareau$Performance.Tag == 0),],aes(x=No.of.PL.trades.opened.in.last.6.months,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))
plot20 = ggplot(credit_beareau,aes(x=No.of.PL.trades.opened.in.last.6.months,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))

plot_grid(plot19,plot20,labels = c('No.of.PL.trades.opened.in.last.6.months dist with default data','No.of.PL.trades.opened.in.last.6.months dist on entire data'))
# Customers with 1,2 number of pl trades opened in last 6 months defaulted more.


#---- No.of.PL.trades.opened.in.last.12.months ----#

plot21 = ggplot(credit_beareau[-which(credit_beareau$Performance.Tag == 0),],aes(x=No.of.PL.trades.opened.in.last.12.months,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))
plot22 = ggplot(credit_beareau,aes(x=No.of.PL.trades.opened.in.last.12.months,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))

plot_grid(plot21,plot22,labels = c('No.of.PL.trades.opened.in.last.12.months dist with default data','No.of.PL.trades.opened.in.last.12.months dist on entire data'))
# Customers with 2 to 6 number of pl trades opened in last 12 months defaulted more.

#---- No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. ----#

plot23 = ggplot(credit_beareau[-which(credit_beareau$Performance.Tag == 0),],aes(x=No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))
plot24 = ggplot(credit_beareau,aes(x=No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))

plot_grid(plot23,plot24,labels = c('No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. dist with default data','No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. dist on entire data'))
#customers who inquired once,twice and thrice for loans in last 6 months got defaulted more.


#---- No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. ----#

plot25 = ggplot(credit_beareau[-which(credit_beareau$Performance.Tag == 0),],aes(x=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))
plot26 = ggplot(credit_beareau,aes(x=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))

plot_grid(plot25,plot26,labels = c('No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. dist with default data','No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. dist on entire data'))
#customers who inquired 2 to 5 times for loans in last 12 months got defaulted more.


#---- Presence.of.open.home.loan ----#

plot27 = ggplot(credit_beareau[-which(credit_beareau$Performance.Tag == 0),],aes(x=Presence.of.open.home.loan,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))
plot28 = ggplot(credit_beareau,aes(x=Presence.of.open.home.loan,y = ..count..,fill = as.factor(Performance.Tag)))+geom_bar()+theme(axis.text = element_text(angle = 90,hjust = 1))

plot_grid(plot27,plot28,labels = c('Presence.of.open.home.loan dist with default data','Presence.of.open.home.loan dist on entire data'))
# customers with home loans got defaulted more.


#-------------------------------------------------------------------------------

#Removing the binning variable
demographic <- demographic[,-c(13,14)]

master_data<- merge(x = demographic, y = credit_beareau, by = 'Application.ID',all = FALSE)


# There no difference between Performance.Tag.x and Performance.Tag.y.So we are removing Performance.Tag.x

master_data <- subset(master_data,select = -c(Performance.Tag.x))

colnames(master_data)[29] <- "Performance.Tag"

#Verifying weather the performance tags has NA values are there or not.

sum(is.na(master_data$Performance.Tag))

#There are no NA values present in the performence tag column.

#Removing Application ID dfrom the data set

master_data <- master_data[,-1]

master_data_Backup<-master_data


#-------------------------------------------------------------------------------

#################################################################################
################### Bivariate Analysis ##########################################
#################################################################################


#we could see some correlations between "No.of.trades.opened.in.last.12.months" and "No.of.trades.opened.in.last.6.months", 
# and between "No.of.PL.trades.opened.in.last.12.months" and "No.of.trades.opened.in.last.6.months", and between "Total.No.of.Trades" and
# "No.of.trades.opened.in.last.6.months"
#we will visualize this using ggplot

ggplot(
  master_data,
  aes(
    x = No.of.trades.opened.in.last.12.months,
    y = No.of.trades.opened.in.last.6.months,
    col = factor(Performance.Tag)
  )
) + geom_smooth() + ggtitle("Correlation between No of Trades open in last 6 months and 12 months") + labs(col = "Performance Tag")

#we could see that as the trades in last 12 month increases the number of trades in last 6 month also increases which is obvious

ggplot(
  master_data,
  aes(
    x = No.of.PL.trades.opened.in.last.12.months,
    y = No.of.trades.opened.in.last.12.months,
    col = factor(Performance.Tag)
  )
) + geom_smooth() + ggtitle("Correlation between No of Trades open in last 12 months and PL trades in 12 months") + labs(col = "Performance Tag")

#we see that as no of pl trades in last 12 months increases no of trades in 12 months also increases

ggplot(
  master_data,
  aes(
    x = Total.No.of.Trades,
    y = No.of.trades.opened.in.last.12.months,
    col = factor(Performance.Tag)
  )
) + geom_smooth() + ggtitle("Correlation between Total No of Trades and trades in 12 months") + labs(col = "Performance Tag")

#we see that in the beginning months applicant uses the card very ofthen till they have used 10 transaction and then they slow down their card usage.



# Gender vs Marital Status

#--- with defaulted data set ----#

plot1 = ggplot(filter(master_data,(Performance.Tag == 1)), aes(Gender,group=Marital.Status..at.the.time.of.application.)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) + 
  labs(y = "Percent", fill="Gender") + facet_grid(~Marital.Status..at.the.time.of.application.) + 
  scale_y_continuous(labels = scales::percent)+ xlab("Gender") + ylab("% of Default") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#---- with entire data set ----#

plot2 = ggplot(master_data, aes(Gender,group=Marital.Status..at.the.time.of.application.)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) + 
  labs(y = "Percent", fill="Gender") + facet_grid(~Marital.Status..at.the.time.of.application.) + 
  scale_y_continuous(labels = scales::percent)+ xlab("Gender") + ylab("% of Default") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


plot_grid(plot1,plot2,labels = c('With defaulted data set','With entire data'))

#Married and singe males defaulted more 


# Gender vs Education

#--- with defaulted data set ----#

plot3 = ggplot(filter(master_data,(Performance.Tag == 1)), aes(Education,group=Gender)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) + 
  labs(y = "Percent", fill="Education") + facet_grid(~Gender) + 
  scale_y_continuous(labels = scales::percent)+ xlab("Education") + ylab("% of Default") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot4 = ggplot(master_data, aes(Education,group=Gender)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) + 
  labs(y = "Percent", fill="Education") + facet_grid(~Gender) + 
  scale_y_continuous(labels = scales::percent)+ xlab("Education") + ylab("% of Default") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_grid(plot3,plot4,labels = c('With defaulted data set','With entire data'))

#Irrespective of geneder ,professionla and master educated customers defaulted more.


# Gender vs Profession

plot5 = ggplot(filter(master_data,(Performance.Tag == 1)), aes(Profession,group=Gender)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) + 
  labs(y = "Percent", fill="Profession") + facet_grid(~Gender) + 
  scale_y_continuous(labels = scales::percent)+ xlab("Profession") + ylab("% of Default") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot6 = ggplot(master_data, aes(Profession,group=Gender)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) + 
  labs(y = "Percent", fill="Profession") + facet_grid(~Gender) + 
  scale_y_continuous(labels = scales::percent)+ xlab("Profession") + ylab("% of Default") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_grid(plot5,plot6,labels = c('With defaulted data set','With entire data'))

#Salaried customers defaulted more


# Profession vs Residence

plot7 = ggplot(filter(master_data,(Performance.Tag == 1)), aes(Type.of.residence,group=Profession)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) + 
  labs(y = "Percent", fill="Type.of.residence") + facet_grid(~Profession) + 
  scale_y_continuous(labels = scales::percent)+ xlab("Residence") + ylab("% of Default") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot8 = ggplot(master_data, aes(Type.of.residence,group=Profession)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) + 
  labs(y = "Percent", fill="Type.of.residence") + facet_grid(~Profession) + 
  scale_y_continuous(labels = scales::percent)+ xlab("Residence") + ylab("% of Default") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_grid(plot7,plot8,labels = c('With defaulted data set','With entire data'))

#In all the cases rented customers are defaulted more



#############################################################################################################
############################### NA abnd missing values treatment#############################################
#############################################################################################################

#Check for Outstanding.Balanc
summary(master_data$Outstanding.Balance)

# Min. 1st Qu.  Median    Mean   3rd Qu.    Max.    NA's 
#  0   208396   774241   1253370 2926238  5218801     272 

#Replacing with Median value
master_data[is.na(master_data$Outstanding.Balance),]$Outstanding.Balance=774243




#Check for Presence.of.open.home.loan
summary(master_data$Presence.of.open.home.loan)

# Min.    1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0000  0.0000  0.0000  0.2597  1.0000  1.0000     272 

#Replacing with Nedian value
master_data[is.na(master_data$Presence.of.open.home.loan),]$Presence.of.open.home.loan=0


# check for "No.of.trades.opened.in.last.6.months" .

summary(master_data$No.of.trades.opened.in.last.6.months)

#1
master_data$No.of.trades.opened.in.last.6.months[which(is.na(master_data$No.of.trades.opened.in.last.6.months))] = 1



#Check for No of dependent.
summary(master_data$No.of.dependents)

#  Min.  1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.000   2.000   3.000   2.859   4.000   5.000       3 

#Replacing the NA with meadian
master_data[is.na(master_data$No.of.dependents),]$No.of.dependents=3


#check for Avgas.CC.Utilization.in.last.12.months
summary(master_data$Avgas.CC.Utilization.in.last.12.months)


View(master_data[is.na(master_data$Avgas.CC.Utilization.in.last.12.months),])

# It is evident from the data that there no DPD entiry,It quite possible that this customer may not have a creadit card.
#there for we re imputing the NA values with 0

master_data$Avgas.CC.Utilization.in.last.12.months[which(is.na(master_data$Avgas.CC.Utilization.in.last.12.months))] = 0


# Now we will check for NA's once again.

sapply(master_data, function(x) sum(is.na(x)))

#NA values are replaced successfully from the data set.



###########################################################################################################
################################ Correlation graph ########################################################
###########################################################################################################

# Extarcting only numeric columns from the given data set

con_var <- function(x) {
  cn <- 0
  z <- data.frame()
  for (i in seq_along(x)) {
    if (class(x[, i]) == "numeric" || class(x[, i]) == "integer") {
      cn <- trimws(sub("0,", "", paste(cn, colnames(x)[i],sep = ",")))
    }
  }
  cn <- as.vector(strsplit(cn, ","))
  for (k in cn) {
    z <- x[, (colnames(x) %in% k)]
  }
  return(z)
}

convar <- con_var(master_data[-28])

# Finding the correlation between each numeric variables in data set

cor_df <- cor(convar)


ggcorrplot(cor_df)

View(cor_df)


#############################################################################################################
############################### Outlier Treatment ###########################################################
#############################################################################################################


#----- Age -----#

boxplot.stats(master_data$Age)$out

#we see that the outliers lies before the first quartile

quantile(master_data$Age,seq(0,1,0.01))

#we could see that there is an increase in values for 0 to 1% ,so we will cap the value of 1%

master_data$Age[which(master_data$Age < 27)] <- 27

#now we will again check the distribution of the age variable

ggplot(master_data,aes(x=master_data$Age)) + 
  geom_histogram(binwidth=10,col="black",fill = "orange") + ggtitle("Distribution of Age") + xlab("Age") + ylab("Count")


#we see that the distribution of age is a normal distribution, since both mean and median are 45.


#---- Income ----#

boxplot.stats(master_data$Income)$out

quantile(master_data$Income,seq(0,1,0.01))

#Even thogh the box plot is not showing any outliers but we know that income cannot be negetive.So we cap the value with 0
master_data$Income[which(master_data$Income < 0)] <- 0

#we will check the distribution of income.

ggplot(master_data,aes(x=master_data$Income)) + 
geom_histogram(binwidth=10,col = "red",fill = "orange") + ggtitle("Distribution of Income") + xlab("Income") + ylab("count")

#we see that the distribution of Income is a normal distribution, since both mean and median are 27.


#---- No.of.months.in.current.residence ----#

boxplot.stats(master_data$No.of.months.in.current.residence)$out

quantile(master_data$No.of.months.in.current.residence,seq(0,1,0.01))

#we don't see any outliers in No of months in current residence,

#we will now check its distribution

ggplot(master_data,aes(x = master_data$No.of.months.in.current.residence)) +
  geom_histogram(binwidth=0.2,col ="black", fill = "violet") +
  ggtitle("Distribution of No of months in current residence") + xlab("Months") + ylab("count")

#we see that the "Distribution of No of months in current residence" is highly left skewed, so we have to make some sort of transformation to
#to make it normal, so we will be performing natural log transformation


log_t = function(x) {
  ifelse((x) <= 0, 0, log((x)))
}

ggplot(master_data, aes(x = log_t(
  master_data$No.of.months.in.current.residence
))) + geom_histogram(binwidth=0.2,col = "black", fill = "slategray1") + ggtitle("Distribution of No of months in current residence") + xlab("Months") + ylab("count")

#we see that the distribution of No.of.months.in.current.residence is a normal distribution, since both mean and median are 2 approx.

#so we will transform the variable to log

master_data$No.of.months.in.current.residence <- log_t(master_data$No.of.months.in.current.residence)



#---- No.of.months.in.current.company ---#

boxplot.stats(master_data$No.of.months.in.current.company)$out

quantile(master_data$No.of.months.in.current.company,seq(0,1,0.01))

#we see that there is sudden jump from 99% to 100% so we will cap our value at99%

master_data$No.of.months.in.current.company[which(master_data$No.of.months.in.current.company > 74)] <- 74

#now we will check the distribution

ggplot(master_data,aes(x=master_data$No.of.months.in.current.company)) + geom_histogram(binwidth = 10,fill= "cyan",col = "black") +
  ggtitle("Distribution of Months in current company") + xlab("Months") + ylab("count")


#----- Avgas.CC.Utilization.in.last.12.months ----#

boxplot.stats(master_data$Avgas.CC.Utilization.in.last.12.months)$out

quantile(master_data$Avgas.CC.Utilization.in.last.12.months,seq(0,1,0.01))

#we could see that at 94% there is a sudden increase , so we will cap our value at94% which is 91

master_data$Avgas.CC.Utilization.in.last.12.months[which(master_data$Avgas.CC.Utilization.in.last.12.months > 91)] <- 91

#We will check the distribution

ggplot(master_data,
       aes(x = master_data$Avgas.CC.Utilization.in.last.12.months)) +
  geom_histogram(binwidth = 10, col = "red", fill = "orange") +
  ggtitle("Distribution of Credit Card Utilization") + xlab("No Of Utilization") + ylab("count")


# We could see that the distribution skewed towards left.so we will perform log transformation on this variable

ggplot(master_data,
       aes(log_t(x = master_data$Avgas.CC.Utilization.in.last.12.months))) +
  geom_histogram(binwidth = 1, col = "red", fill = "orange") +
  ggtitle("Distribution of Credit Card Utilization") + xlab("No Of Utilization") + ylab("count")

# Now the values are distributed normally, so we will transform this variable to log


master_data$Avgas.CC.Utilization.in.last.12.months <- log_t(master_data$Avgas.CC.Utilization.in.last.12.months)


#----- Outstanding.Balance ----#

boxplot(master_data$Outstanding.Balance)

#we don't see any outliers in the Outstanding
#now we will check the distribution

ggplot(master_data,
       aes(
         x = master_data$Outstanding.Balance,
         fill = factor(master_data$Performance.Tag)
       )) + geom_histogram(binwidth = 100000,col = "blue", position = "fill") +
  ggtitle("Distribution of Outstanding Balance over good and bad") + xlab("Outstanding Balance") + ylab("count") + labs(fill ="Performance Tag")

#The data is highly skewwd towards right.So we will perform the log transformation

log10_t = function(x) {
  ifelse((x) <= 0, 0, log10((x)))
}


ggplot(master_data, aes(
  x = log10_t(master_data$Outstanding.Balance),
  fill = factor(master_data$Performance.Tag)
)) + geom_histogram(binwidth = 0.2,col = "blue", na.rm = TRUE) + ggtitle("Distribution of Outstanding Balance over good and bad") + xlab("Outstanding Balance") + ylab("count") + labs(fill ="Performance Tag")

#we could see that the distribution is normal now and both mean and median are 5 approx,
#we will convert outstanding balance to log10 

master_data$Outstanding.Balance <- log10_t(master_data$Outstanding.Balance)



#----- No.of.PL.trades.opened.in.last.12.months -----#

boxplot(master_data$No.of.PL.trades.opened.in.last.12.months)

quantile(master_data$No.of.PL.trades.opened.in.last.12.months,seq(0,1,0.01))


#We could see no outliers.

ggplot(master_data,
       aes(x = master_data$No.of.PL.trades.opened.in.last.12.months)) +
  geom_density(col ="blue", fill = "orange") +
  ggtitle("Distribution of No of PL trades opened in last 12 months") +
  xlab("No of PL trades opened in last 12 months") + ylab("Frequency") + labs(fill ="Performance Tag")


#we could see that the distribution is left skewed, so we will use log transformation

master_data$No.of.PL.trades.opened.in.last.12.months <- log_t(master_data$No.of.PL.trades.opened.in.last.12.months)

boxplot(master_data$No.of.PL.trades.opened.in.last.12.months)



#----- No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. ----#

boxplot(master_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)

ggplot(
  master_data,
  aes(
    x = master_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,
    fill = factor(master_data$Performance.Tag)
  )
) + geom_histogram(binwidth = 1,col = "blue") + ggtitle("Distribution of No of Inquiries in last 6 months excluding home auto loans over good and bad
") + xlab("No of Inquiries in last 6 months excluding home auto loans") + ylab("Density") + labs(fill ="Performance Tag")


#we see that the distribution is left skewed, so we will use log transformation

master_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- log_t(master_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)

boxplot(master_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)



#---- No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. ----#

boxplot(master_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.) 


ggplot(
  master_data,
  aes(
    x = master_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,
    fill = factor(master_data$Performance.Tag)
  )
) + geom_histogram(binwidth  = 1,col = "blue") + ggtitle(
  "Distribution of No of Inquiries in last 12 months excluding home auto loans over good and bad"
) + xlab("No of Inquiries in last 12 months excluding home auto loans") + ylab("Density") + labs(fill ="Performance Tag")


#we see that the distribution is left skewed, so we will use log transformation

master_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- log_t(master_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)

boxplot(master_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)



#----- Total.No.of.Trades -----#

boxplot(master_data$Total.No.of.Trades)

ggplot(master_data,
       aes(
         x = master_data$Total.No.of.Trades,
         fill = factor(master_data$Performance.Tag)
       )) + geom_histogram(binwidth = 1,col = "blue", position = "fill") + ggtitle("Distribution of Total No of Trades over good and bad") + xlab("Total No of Trades") + ylab("Density") + labs(fill =
                                                                                                                                                                                    "Performance Tag")

#we see that the distribution is right skewed, so we will use log transformation

master_data$Total.No.of.Trades <- log_t(master_data$Total.No.of.Trades)

boxplot(master_data$Total.No.of.Trades)

#Now the data is normally distributed





###########################################################################################################
####################################### WOE Analysis ######################################################
###########################################################################################################

#Computint IV

IV <-
  Information::create_infotables(data = master_data,
                                 y = "Performance.Tag",
                                 parallel =
                                   TRUE)
# Creating a data frame with WOE ,so Removing the Performance.Tag.y from original the data frame

master_data_new <- master_data[,-28]

# Created a finction to extract the WOE for all the variables in the given data set

woe_replace <- function(df_orig, IV) {
  df <- cbind(df_orig)
  df_clmtyp <- data.frame(clmtyp = sapply(df, class))
  df_col_typ <-
    data.frame(clmnm = colnames(df), clmtyp = df_clmtyp$clmtyp)
  for (rownm in 1:nrow(df_col_typ)) {
    colmn_nm <- toString(df_col_typ[rownm, "clmnm"])    
    if(colmn_nm %in% names(IV$Tables)){
      column_woe_df <- cbind(data.frame(IV$Tables[[toString(df_col_typ[rownm, "clmnm"])]]))
      if (df_col_typ[rownm, "clmtyp"] == "factor" | df_col_typ[rownm, "clmtyp"] == "character") {
        df <-
          dplyr::inner_join(
            df,
            column_woe_df[,c(colmn_nm,"WOE")],
            by = colmn_nm,
            type = "inner",
            match = "all"
          )
        df[colmn_nm]<-NULL
        colnames(df)[colnames(df)=="WOE"]<-colmn_nm
      } else if (df_col_typ[rownm, "clmtyp"] == "numeric" | df_col_typ[rownm, "clmtyp"] == "integer") {
        column_woe_df$lv<-as.numeric(str_sub(
          column_woe_df[,colmn_nm],
          regexpr("\\[", column_woe_df[,colmn_nm]) + 1,
          regexpr(",", column_woe_df[,colmn_nm]) - 1
        ))
        column_woe_df$uv<-as.numeric(str_sub(
          column_woe_df[,colmn_nm],
          regexpr(",", column_woe_df[,colmn_nm]) + 1,
          regexpr("\\]", column_woe_df[,colmn_nm]) - 1
        ))
        column_woe_df[colmn_nm]<-NULL      
        column_woe_df<-column_woe_df[,c("lv","uv","WOE")]      
        colnames(df)[colnames(df)==colmn_nm]<-"WOE_temp2381111111111111697"      
        df <-
          fuzzy_inner_join(
            df,
            column_woe_df[,c("lv","uv","WOE")],
            by = c("WOE_temp2381111111111111697"="lv","WOE_temp2381111111111111697"="uv"),
            match_fun=list(`>=`,`<=`) 
          )      
        df["WOE_temp2381111111111111697"]<-NULL      
        df["lv"]<-NULL      
        df["uv"]<-NULL      
        colnames(df)[colnames(df)=="WOE"]<-colmn_nm      
      }}
  }
  return(df)
}


master_data_WOE <- woe_replace(master_data_new, IV)

IV_Value = data.frame(IV$Summary)

IV_Value$IV <- format(IV_Value$IV, scientific = FALSE)

IV_Value$Variable <- factor(IV_Value$Variable,levels = IV_Value$Variable[order(IV_Value$IV)])

ggplot(IV_Value,aes(x = IV_Value$Variable,y=IV_Value$IV))+
  geom_bar(stat = 'identity')+theme(axis.text.x = element_text(angle = 90,hjust = 1))

View(IV_Value)



#Verifying IV for Age variable

plot_infotables(IV,"Age")

#we can see that the age group between 51 to 53 has the high chances of default and  WOE is also very less.


#Verifying IV for Gender variable

plot_infotables(IV,"Gender")

#We can see that compared females , males has high chance of default.


#Verifying IV for Marital status variable

plot_infotables(IV,"Marital.Status..at.the.time.of.application.")

#We can see that compared single , married customers has high chance of default.


#Verifying IV for No of dependents variable

plot_infotables(IV,"No.of.dependents")

#We can see that customers who are having 2 dependents has high chance of default.



#Verifying IV for Income variable

plot_infotables(IV,"Income")

#we can see that the customers whose income level is in between 49 to 60 has the higher chances of default.


#Verifying IV for Education variable

plot_infotables(IV,"Education")

#We can see that customers with good education got defaulted more.


#Verifying IV for Profession variable

plot_infotables(IV,"Profession")

#We can see that salaried professionals are got defaulted more.


#Verifying IV for Type.of.residence variable

plot_infotables(IV,"Type.of.residence")

#We can see that others type of residence customers got defaulted more.


#Verifying IV for No.of.months.in.current.residence variable

plot_infotables(IV,"No.of.months.in.current.residence")

#We can see that customers who staying in current residence between 6 to 9 months got defaulted more.


#Verifying IV for No.of.months.in.current.company variable

plot_infotables(IV,"No.of.months.in.current.company")

#We can see that customers who has been staying 48 to 61 months in current company got defaulted more.


#Verifying IV for No.of.times.90.DPD.or.worse.in.last.6.months variable

plot_infotables(IV,"No.of.times.90.DPD.or.worse.in.last.6.months")

#we see that the customers whose No.of.times.90.DPD.or.worse.in.last.6.months is 0 has the higher chances of default.


#Verifying IV for No.of.times.60.DPD.or.worse.in.last.6.months variable

plot_infotables(IV,"No.of.times.60.DPD.or.worse.in.last.6.months")


#we see that the customers whose No.of.times.60.DPD.or.worse.in.last.6.months is 0 has the higher chances of default.



#Verifying IV for No.of.times.30.DPD.or.worse.in.last.6.months variable

plot_infotables(IV,"No.of.times.30.DPD.or.worse.in.last.6.months")

#we see that the customers whose No.of.times.30.DPD.or.worse.in.last.6.months is 0 has the higher chances of default.


#Verifying IV for Avgas.CC.Utilization.in.last.12.months variable

plot_infotables(IV,"Avgas.CC.Utilization.in.last.12.months")

#we see that the customers whose Avgas.CC.Utilization.in.last.12.months is between 0 to 14 has the higher chances of default.



#Verifying IV for No.of.trades.opened.in.last.6.months variable

plot_infotables(IV,"No.of.trades.opened.in.last.6.months")

#we see that the customers whose No.of.trades.opened.in.last.6.months is between 0 to 1 has the higher chances of default.


#Verifying IV for No.of.trades.opened.in.last.12.months variable

plot_infotables(IV,"No.of.trades.opened.in.last.12.months")

#we see that the customers whose No.of.trades.opened.in.last.12.months is between 0 to 2 has the higher chances of default.


#Verifying IV for No.of.PL.trades.opened.in.last.12.months variable

plot_infotables(IV,"No.of.PL.trades.opened.in.last.12.months")

#we see that the customers whose No.of.PL.trades.opened.in.last.12.months is 0 and 1 has the higher chances of default.


#Verifying IV for No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. variable

plot_infotables(IV,"No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.")

#we see that the customers whose No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. is 0 has the higher chances of default.


#Verifying IV for No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. variable

plot_infotables(IV,"No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.")

#we see that the customers whose No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. is 0 and 1has the higher chances of default.


#Verifying IV for Presence.of.open.home.loan variable

plot_infotables(IV,"Presence.of.open.home.loan")

#we see that the customers whose Presence.of.open.home.loan is 1 has the higher chances of default.


#Verifying IV for Outstanding.Balance variable

plot_infotables(IV,"Outstanding.Balance")

#we see that the customers whose Outstanding.Balance is between 0 to 56065 and between 1362889 to 3289931 has the higher chances of default.


#Verifying IV for Total.No.of.Trades variable

plot_infotables(IV,"Total.No.of.Trades")

#we see that the customers whose Total.No.of.Trades is between 0 to 5 and between 20 to 44 has the higher chances of default.


#Verifying IV for Presence.of.open.auto.loan variable

plot_infotables(IV,"Presence.of.open.auto.loan")

#we see that the customers whose Presence.of.open.auto.loan is 1 has the higher chances of default.




################################################################################################################
############################################## Modelling #######################################################
################################################################################################################

#Before applying any algorithem we need to convert all catgorical variables to factors.

str(master_data)

master_data$Gender <- as.factor(master_data$Gender)
master_data$Marital.Status..at.the.time.of.application. <- as.factor(master_data$Marital.Status..at.the.time.of.application.)
master_data$Education <- as.factor(master_data$Education)
master_data$Profession <- as.factor(master_data$Profession)
master_data$Type.of.residence <- as.factor(master_data$Type.of.residence)


master_data$Avgas.CC.Utilization.in.last.12.months <- as.numeric(master_data$Avgas.CC.Utilization.in.last.12.months)
master_data$Presence.of.open.home.loan <- as.numeric(master_data$Presence.of.open.home.loan)
master_data$Outstanding.Balance <- as.numeric(master_data$Outstanding.Balance)
master_data$Age <- as.numeric(master_data$Age)
master_data$Income <- as.numeric(master_data$Income)


master_data_final <- master_data


master_data_final$Gender <- as.factor(ifelse(master_data_final$Gender == 'F', 0, 1))

master_data_final$Marital.Status..at.the.time.of.application. <- as.factor(ifelse(master_data_final$Marital.Status..at.the.time.of.application.== 'Single', 0, 1))



#Taking all catagorical variables into a data frame

cat_data <- master_data_final[,c('Education','Profession','Type.of.residence')]

dummies<- data.frame(sapply(cat_data, function(x) data.frame(model.matrix(~x-1,data =cat_data))[,-1]))

#Removing the catagorical variables and replace with dummy values for further analysis.

master_without_cat_var <- master_data_final[,-c(6,7,8)]

master_data_final <- cbind(master_without_cat_var,dummies)

str(master_data_final)

master_data_final$Gender <- as.numeric(master_data_final$Gender)
master_data_final$Marital.Status..at.the.time.of.application. <- as.numeric(master_data_final$Marital.Status..at.the.time.of.application.)
#Converting performance tag to factors

master_data_final$Performance.Tag <- as.factor(master_data_final$Performance.Tag)



#########################################################################################
####################### Logistic Regression on Demographic Data #########################
#########################################################################################

#-- Extracted the demographic data

demographic_final <- master_data_final[,c(1:7,25:38)]

demographic_final$binning.age <- as.factor(cut(demographic_final$Age, breaks = c(0,10, 20, 30, 40, 50, 60, 70)))

demographic_final$binning.income <- as.factor(cut(demographic_final$Income, breaks = c(-1, 10, 20, 30, 40, 50, 60,70)))

#Removing the age and income

demographic_final <- demographic_final[,-c(1,5)]


sapply(demographic_final, function(x) sum(is.na(x)))

#####################################################################################
# creating training and test data for demographic data 
#####################################################################################

set.seed(100)
trainindices= sample(1:nrow(demographic_final), 0.7*nrow(demographic_final))
train_demo = demographic_final[trainindices,]
test_demo = demographic_final[-trainindices,]


#---- Logistic Regression -----#

model_1 <- glm(Performance.Tag~.,data=train_demo,family = "binomial")
summary(model_1)


# used STEPAIC to find the best model
# model_2 <- stepAIC(model_1,direction = "both")
# summary(model_2)

model_3<-glm(formula = Performance.Tag ~ No.of.months.in.current.company +No.of.months.in.current.residence+binning.income , 
             family = "binomial", data = train_demo)
summary(model_3)


test_pred_demo<-predict(model_3, type = "response",newdata = test_demo[,-6])


summary(test_pred_demo)
test_demo$prob <- test_pred_demo


test_actual<- factor(ifelse(test_demo$Performance.Tag==1,"Yes","No"))


s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


perform_fn_demo <- function(cutoff){
  test_actual<- factor(ifelse(test_demo$Performance.Tag==1,"Yes","No"))
  predicted_Attrition <- factor(ifelse(test_pred_demo >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_Attrition, test_actual, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

for(i in 1:100){  
  OUT[i,] = perform_fn_demo(s[i])
}


#Plot to choose best cutoff
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.6,.80,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


best_cutoff <- s[which(abs(OUT[,1]-OUT[,2])==min(abs(OUT[,1]-OUT[,2])))]
best_cutoff



test_pred_Attrition <- as.factor(ifelse(test_pred_demo >= best_cutoff, "Yes", "No"))
confusionmartix_final<-confusionMatrix(test_pred_Attrition, test_actual, positive = "Yes")

confusionmartix_final

#Removing the models

rm(model_1,model_3)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    No   Yes
# No  10604   367
# Yes  9494   496
# 
# Accuracy : 0.5296          
# 95% CI : (0.5228, 0.5363)
# No Information Rate : 0.9588          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0169          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.57474         
# Specificity : 0.52761         
# Pos Pred Value : 0.04965         
# Neg Pred Value : 0.96655         
# Prevalence : 0.04117         
# Detection Rate : 0.02366         
# Detection Prevalence : 0.47660         
# Balanced Accuracy : 0.55118         
# 
# 'Positive' Class : Yes


#Doing over smapling

train_over_demo <- ovun.sample(Performance.Tag ~., data = train_demo, method = "over", N = 93686)$data
table(train_over_demo$Performance.Tag)

#   0     1 
# 46822 46864 

#Doing the synthetic data set

train_synthetic_demo <- ROSE(Performance.Tag ~ ., data = train_demo, seed = 1)$data
table(train_synthetic_demo$Performance.Tag)

#   0     1 
# 24451 24455

#----------------------------------------------

#Logistic regression on over smapled data set

model_over_1 <- glm(Performance.Tag~.,data=train_over_demo,family = "binomial")
summary(model_over_1)


# used STEPAIC to find the best model
# model_over_2 <- stepAIC(model_over_1,direction = "both")
# summary(model_over_2)


model_over_3<-glm(formula = Performance.Tag ~ No.of.months.in.current.residence + No.of.months.in.current.company + 
                    Education.xOthers + Profession.xSAL + Profession.xSE + Profession.xSE_PROF + 
                    Type.of.residence.xCompany.provided + Type.of.residence.xLiving.with.Parents + 
                    Type.of.residence.xOthers + Type.of.residence.xOwned + Type.of.residence.xRented + 
                    binning.age + binning.income + Education.xMasters , 
                  family = "binomial", data = train_over_demo)
summary(model_over_3)

#Removing the Type.of.residence.xOthers  high p values 0.88

model_over_4 <- update(model_over_3,~.-Type.of.residence.xOthers)
summary(model_over_4)
vif(model_over_4)


#Remving Profession.xSAL due to high p value 0.786

model_over_5 <- update(model_over_4,~.-Profession.xSAL)
summary(model_over_5)


#Remving binning.age(50,60] due to high p value 0.69

model_over_6 <- glm(formula = Performance.Tag ~ No.of.months.in.current.residence + No.of.months.in.current.company + 
                      Education.xOthers +  Profession.xSE + Profession.xSE_PROF + 
                      Type.of.residence.xCompany.provided + Type.of.residence.xLiving.with.Parents + 
                      binning.income+ Education.xMasters , 
                    family = "binomial", data = train_over_demo,subset= binning.age =="(50,60]")
summary(model_over_6)

#Remving  Type.of.residence.xLiving.with.Parents d

model_over_7 <- update(model_over_6,~.-Type.of.residence.xLiving.with.Parents)
summary(model_over_7)

#Remving  Type.of.residence.xLiving.with.Parents d

model_over_8 <- update(model_over_7,~.-Education.xOthers)
summary(model_over_8)



test_pred_demo_over<-predict(model_over_8, type = "response",newdata = test_demo[,-6])


summary(test_pred_demo_over)
test_demo$prob <- test_pred_demo_over


test_actual<- factor(ifelse(test_demo$Performance.Tag==1,"Yes","No"))


s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


perform_fn_demo_over <- function(cutoff){
  test_actual<- factor(ifelse(test_demo$Performance.Tag==1,"Yes","No"))
  predicted_Attrition <- factor(ifelse(test_pred_demo_over >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_Attrition, test_actual, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

for(i in 1:100){  
  OUT[i,] = perform_fn_demo_over(s[i])
}


#Plot to choose best cutoff
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.6,.80,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


best_cutoff <- s[which(abs(OUT[,1]-OUT[,2])==min(abs(OUT[,1]-OUT[,2])))]
best_cutoff



test_pred_Attrition <- as.factor(ifelse(test_pred_demo_over >= best_cutoff, "Yes", "No"))
confusionmartix_final<-confusionMatrix(test_pred_Attrition, test_actual, positive = "Yes")

confusionmartix_final

#Removing the model objects

rm(model_over_1,model_over_3,model_over_4,model_over_5,model_over_6,
   model_over_7,model_over_8)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    No   Yes
# No  11239   386
# Yes  8859   477
# 
# Accuracy : 0.5589          
# 95% CI : (0.5522, 0.5657)
# No Information Rate : 0.9588          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0196          
# Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.55272         
#             Specificity : 0.55921         
#          Pos Pred Value : 0.05109         
#          Neg Pred Value : 0.96680         
#              Prevalence : 0.04117         
#          Detection Rate : 0.02276         
#    Detection Prevalence : 0.44540         
#       Balanced Accuracy : 0.55597         
#                                           
#        'Positive' Class : Yes             


#--------------------------------------------------

#Logistic regression using synthetic data set


model_synt_1 <- glm(Performance.Tag~.,data=train_synthetic_demo,family = "binomial")
summary(model_synt_1)

# used STEPAIC to find the best model
# model_synt_2 <- stepAIC(model_synt_1,direction = "both")
# summary(model_synt_2)

model_synt_3<-glm(formula = Performance.Tag ~ Gender + Marital.Status..at.the.time.of.application. + 
                    No.of.dependents + No.of.months.in.current.residence + No.of.months.in.current.company + 
                    Education.xBachelor + Education.xOthers + Education.xPhd + 
                    Profession.xSE + Type.of.residence.xCompany.provided + Type.of.residence.xOthers + 
                    Type.of.residence.xRented + binning.age + binning.income , 
                  family = "binomial", data = train_synthetic_demo)
summary(model_synt_3)

#Removing Education.xOthers
model_synt_4 <- update(model_synt_3,~.-Education.xOthers)
summary(model_synt_4)

#Removing binning.age(50,60]
model_synt_5 <- update(model_synt_4,~.-binning.age)
summary(model_synt_5)

#Removing Gender  
model_synt_6 <- update(model_synt_5,~.-Gender)
summary(model_synt_6)

#Removing Type.of.residence.xCompany.provided

model_synt_7 <- update(model_synt_6,~.-Type.of.residence.xCompany.provided)
summary(model_synt_7)


#Removing Type.of.residence.xOthers 

model_synt_8 <- update(model_synt_7,~.-Type.of.residence.xOthers)
summary(model_synt_8)

#Removing Education.xPhd

model_synt_9 <- update(model_synt_8,~.-Education.xPhd)
summary(model_synt_9)

#Removing Marital.Status..at.the.time.of.application.

model_synt_10 <- update(model_synt_9,~.-Marital.Status..at.the.time.of.application.)
summary(model_synt_10)


#Removing Education.xBachelor
model_synt_11 <- update(model_synt_10,~.-Education.xBachelor)
summary(model_synt_11)


#Removing No.of.dependents
model_synt_12 <- update(model_synt_11,~.-No.of.dependents)
summary(model_synt_12)


#Removing Type.of.residence.xRented
model_synt_13 <- update(model_synt_12,~.-Type.of.residence.xRented)
summary(model_synt_13)

test_pred_demo_synt<-predict(model_synt_13, type = "response",newdata = test_demo[,-6])


summary(test_pred_demo_synt)
test_demo$prob <- test_pred_demo_synt


test_actual<- factor(ifelse(test_demo$Performance.Tag==1,"Yes","No"))


s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


perform_fn_demo_synt <- function(cutoff){
  test_actual<- factor(ifelse(test_demo$Performance.Tag==1,"Yes","No"))
  predicted_Attrition <- factor(ifelse(test_pred_demo_synt >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_Attrition, test_actual, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

for(i in 1:100){  
  OUT[i,] = perform_fn_demo_synt(s[i])
}


#Plot to choose best cutoff
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.6,.80,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


best_cutoff <- s[which(abs(OUT[,1]-OUT[,2])==min(abs(OUT[,1]-OUT[,2])))]
best_cutoff



test_pred_Attrition <- as.factor(ifelse(test_pred_demo_synt >= best_cutoff, "Yes", "No"))
confusionmartix_final<-confusionMatrix(test_pred_Attrition, test_actual, positive = "Yes")

confusionmartix_final

#Removing the model objects
rm(model_synt_1,model_synt_3,model_synt_4,model_synt_5,model_synt_6,model_synt_7,
   model_synt_8,model_synt_9,model_synt_10,model_synt_11,model_synt_12,model_synt_13)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    No   Yes
# No  10699   370
# Yes  9399   493
# 
# Accuracy : 0.5339          
# 95% CI : (0.5272, 0.5407)
# No Information Rate : 0.9588          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0172          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.57126         
# Specificity : 0.53234         
# Pos Pred Value : 0.04984         
# Neg Pred Value : 0.96657         
# Prevalence : 0.04117         
# Detection Rate : 0.02352         
# Detection Prevalence : 0.47192         
# Balanced Accuracy : 0.55180         
# 
# 'Positive' Class : Yes     


#########################################################################################
##### Random Forest Model on demographic data
#########################################################################################

train_demo$Performance.Tag <-  as.factor(ifelse(train_demo$Performance.Tag==0,"no","yes"))
test_demo$Performance.Tag <-  as.factor(ifelse(test_demo$Performance.Tag==0,"no","yes"))


rf_demo <- randomForest(Performance.Tag ~., data = train_demo, proximity = F, do.trace = T, mtry = 5,ntree=1000)
rf_pred_demo <- predict(rf_demo, test_demo[, -6], type = "prob")


perform_fn_rf_demo <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred_demo[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_demo$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}


s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf_demo(s[i])
} 


plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])==min(abs(OUT_rf[,1]-OUT_rf[,2])))]
cutoff_rf


predicted_response_rf_demo<- factor(ifelse(rf_pred_demo[, 2] >= cutoff_rf, "yes", "no"))
conf_rf <- confusionMatrix(predicted_response_rf_demo, test_demo$Performance.Tag, positive = "yes")
conf_rf

#Removing the modeling object

rm(rf_demo)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    no   yes
# no  12228   425
# yes  7870   438
# 
# Accuracy : 0.6043          
# 95% CI : (0.5976, 0.6109)
# No Information Rate : 0.9588          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0226          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.50753         
# Specificity : 0.60842         
# Pos Pred Value : 0.05272         
# Neg Pred Value : 0.96641         
# Prevalence : 0.04117         
# Detection Rate : 0.02090         
# Detection Prevalence : 0.39636         
# Balanced Accuracy : 0.55798         
# 
# 'Positive' Class : yes   


#--------------------------------------

# Random forest using oversampling data set
train_over_demo$Performance.Tag <-  as.factor(ifelse(train_over_demo$Performance.Tag==0,"no","yes"))

rf_demo_over <- randomForest(Performance.Tag ~., data = train_over_demo, proximity = F, do.trace = T, mtry = 5,ntree=500)
rf_pred_demo_over <- predict(rf_demo_over, test_demo[, -6], type = "prob")


perform_fn_rf_demo_over <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred_demo_over[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_demo$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}


s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf_demo_over(s[i])
} 


plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])==min(abs(OUT_rf[,1]-OUT_rf[,2])))]
cutoff_rf


predicted_response_rf_demo<- factor(ifelse(rf_pred_demo_over[, 2] >= cutoff_rf, "yes", "no"))
conf_rf <- confusionMatrix(predicted_response_rf_demo, test_demo$Performance.Tag, positive = "yes")
conf_rf

#Removing the modeling object

rm(rf_demo_over)


# Confusion Matrix and Statistics
# 
# Reference
# Prediction    no   yes
# no  10356   397
# yes  9742   466
# 
# Accuracy : 0.5163          
# 95% CI : (0.5095, 0.5231)
# No Information Rate : 0.9588          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0089          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.53998         
# Specificity : 0.51528         
# Pos Pred Value : 0.04565         
# Neg Pred Value : 0.96308         
# Prevalence : 0.04117         
# Detection Rate : 0.02223         
# Detection Prevalence : 0.48700         
# Balanced Accuracy : 0.52763         
# 
# 'Positive' Class : yes


#Removing all data frames

rm(demographic_final,train_demo,test_demo,train_over_demo,train_synthetic_demo)


#####################################################################################
# creating training and test data  
#####################################################################################

set.seed(100)
trainindices= sample(1:nrow(master_data_final), 0.7*nrow(master_data_final))
train = master_data_final[trainindices,]
test = master_data_final[-trainindices,]


#####################################################################################
#Model Building
#####################################################################################

#---- Logistic Regression -----#

model_1 <- glm(Performance.Tag~.,data=train,family = "binomial")
summary(model_1)


# # used STEPAIC to find the best model
#model_2 <- stepAIC(model_1,direction = "both")
#summary(model_2)


model_3<-glm(formula = Performance.Tag ~ No.of.months.in.current.company + 
               No.of.PL.trades.opened.in.last.6.months + No.of.times.60.DPD.or.worse.in.last.6.months + 
               Avgas.CC.Utilization.in.last.12.months + Outstanding.Balance + 
               No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
               No.of.trades.opened.in.last.12.months  + 
               No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. +  No.of.PL.trades.opened.in.last.12.months + 
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. , 
             family = "binomial", data = train)
summary(model_3)
vif(model_3)


#Removing No.of.months.in.current.company as per p value

model_4 <- update(model_3,~.-No.of.months.in.current.company)
summary(model_4)
vif(model_4)


#Removing No.of.times.60.DPD.or.worse.in.last.6.months due to high VIF
model_5 <- update(model_4,~.-No.of.times.60.DPD.or.worse.in.last.6.months)
summary(model_5)
vif(model_5)


#Removing No.of.PL.trades.opened.in.last.6.months due high p value

model_6 <- update(model_5,~.-No.of.PL.trades.opened.in.last.6.months)
summary(model_6)
vif(model_6)

#Removing Outstanding.Balance  due to high p value

model_7 <- update(model_6,~.-Outstanding.Balance)
summary(model_7)
vif(model_7)


#Removing No.of.times.30.DPD.or.worse.in.last.6.months due to high p value

model_8 <- update(model_7,~.-No.of.times.30.DPD.or.worse.in.last.6.months)
summary(model_8)
vif(model_8)

# We have got all important variable for prediction.

test_pred<-predict(model_8, type = "response",newdata = test[,-23])


summary(test_pred)
test$prob <- test_pred


test_actual<- factor(ifelse(test$Performance.Tag==1,"Yes","No"))


s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


perform_fn <- function(cutoff){
  test_actual<- factor(ifelse(test$Performance.Tag==1,"Yes","No"))
  predicted_Attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_Attrition, test_actual, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

for(i in 1:100){  
  OUT[i,] = perform_fn(s[i])
}


#Plot to choose best cutoff
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.6,.80,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


best_cutoff <- s[which(abs(OUT[,1]-OUT[,2])==min(abs(OUT[,1]-OUT[,2])))]
best_cutoff

#Cutt off value is 0.04

test_pred_Attrition <- as.factor(ifelse(test_pred >= best_cutoff, "Yes", "No"))
confusionmartix_final<-confusionMatrix(test_pred_Attrition, test_actual, positive = "Yes")

confusionmartix_final

# Prediction    No   Yes
# No  13079   360
# Yes  7019   503
# 
# Accuracy : 0.648           
# 95% CI : (0.6415, 0.6544)
# No Information Rate : 0.9588          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0498          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.58285         
# Specificity : 0.65076         
# Pos Pred Value : 0.06687         
# Neg Pred Value : 0.97321         
# Prevalence : 0.04117         
# Detection Rate : 0.02400         
# Detection Prevalence : 0.35886         
# Balanced Accuracy : 0.61681         
# 
# 'Positive' Class : Yes 


# We have got the accurecy level of 64.8%

##Removing the modeling object 

rm(model_1,model_3,model_4,model_5,model_6,model_7,model_8)


#####################################################################################
# As we know that give data is highly imbalnace and the default count is less.So we need to 
#balance the data set to improve the accurecy level.Accurecy level increases with data availability.
# To overcome the imbalance data set we are using SMOTE package.


# Logistic Regression: using SMOTE analysis 

#Initial model 
train_smote <- SMOTE(Performance.Tag ~ ., train, perc.over = 100, perc.under=200)

summary(train_smote$Performance.Tag)

# 0     1 
# 4168 4168 


#Buliding model on smote data set

smote_model_1 = glm(Performance.Tag ~ ., data = train_smote, family = "binomial")
summary(smote_model_1)


# using STEPAIC to find the best model
# smote_model_2 <- stepAIC(smote_model_1,direction = "both")
# summary(smote_model_2)
# 
# vif(smote_model_2)


#Next Model Analysis
smote_model_3<-glm(formula = Performance.Tag ~ Income + No.of.times.60.DPD.or.worse.in.last.6.months + 
                     Presence.of.open.auto.loan + Profession.xSE + Education.xMasters+
                     No.of.trades.opened.in.last.6.months + No.of.months.in.current.residence + 
                     No.of.times.60.DPD.or.worse.in.last.12.months + No.of.trades.opened.in.last.12.months  + 
                     No.of.PL.trades.opened.in.last.6.months + No.of.months.in.current.company +
                     No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.6.months  + 
                     No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + Outstanding.Balance + 
                     No.of.PL.trades.opened.in.last.12.months + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                     Avgas.CC.Utilization.in.last.12.months, family = "binomial", data = train_smote)
summary(smote_model_3)
vif(smote_model_3)


# Removing No.of.times.60.DPD.or.worse.in.last.6.months due to high p value and vif value

smote_model_4 <- update(smote_model_3,~.-No.of.times.60.DPD.or.worse.in.last.6.months)
summary(smote_model_4)
vif(smote_model_4)


# Removing Income due to high p value

smote_model_5 <- update(smote_model_4,~.-Income)
summary(smote_model_5)
vif(smote_model_5)


# Removing Profession.xSE   due to high p value

smote_model_6 <- update(smote_model_5,~.-Profession.xSE)
summary(smote_model_6)
vif(smote_model_6)


# Removing Presence.of.open.auto.loan due to high p value

smote_model_7 <- update(smote_model_6,~.-Presence.of.open.auto.loan)
summary(smote_model_7)
vif(smote_model_7)

# Removing No.of.trades.opened.in.last.6.months due to high p value and vif value

smote_model_8 <- update(smote_model_7,~.-No.of.trades.opened.in.last.6.months)
summary(smote_model_8)
vif(smote_model_8)

# Removing Education.xMasters due to high p value

smote_model_9 <- update(smote_model_8,~.-Education.xMasters)
summary(smote_model_9)
vif(smote_model_9)

# Removing No.of.months.in.current.residence due to high p value

smote_model_10 <- update(smote_model_9,~.-No.of.months.in.current.residence)
summary(smote_model_10)
vif(smote_model_10)


# Removing No.of.PL.trades.opened.in.last.6.months due to high p value

smote_model_11 <- update(smote_model_10,~.-No.of.PL.trades.opened.in.last.6.months)
summary(smote_model_11)
vif(smote_model_11)


# Removing No.of.months.in.current.company  due to high p value

smote_model_12 <- update(smote_model_11,~.-No.of.months.in.current.company)
summary(smote_model_12)
vif(smote_model_12)


# Removing No.of.times.90.DPD.or.worse.in.last.12.month due to high p value

smote_model_13 <- update(smote_model_12,~.-No.of.times.90.DPD.or.worse.in.last.12.month)
summary(smote_model_13)
vif(smote_model_13)


# Removing No.of.times.90.DPD.or.worse.in.last.12.months due to high p value

smote_model_14 <- update(smote_model_13,~.-No.of.times.90.DPD.or.worse.in.last.12.months)
summary(smote_model_14)
vif(smote_model_14)

# Removing No.of.times.60.DPD.or.worse.in.last.12.months  to high p value

smote_model_15 <- update(smote_model_14,~.-No.of.times.60.DPD.or.worse.in.last.12.months)
summary(smote_model_15)
vif(smote_model_15)


# We have got the important variable from the algorithem for furhter prediction.

test_pred_smote<-predict(smote_model_15, type = "response",newdata = test[,-23])
summary(test_pred_smote)
test$prob_smote <- test_pred_smote

test_actual_smote<- as.factor(ifelse(test$Performance.Tag==1,"Yes","No"))

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)

perform_fn_smote <- function(cutoff){
  test_actual<- factor(ifelse(test$Performance.Tag==1,"Yes","No"))
  predicted_Attrition <- factor(ifelse(test_pred_smote >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_Attrition, test_actual, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

for(i in 1:100){  
  OUT[i,] = perform_fn_smote(s[i])
}


#Plot to choose best cutoff
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.6,.80,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


best_cutoff_smote <- s[which(abs(OUT[,1]-OUT[,2])==min(abs(OUT[,1]-OUT[,2])))]
best_cutoff_smote
# best_cutoff is 0.52
test_pred_Attrition_smote <- as.factor(ifelse(test_pred_smote >= best_cutoff_smote, "Yes", "No"))
confusionmartix_final_smote<-confusionMatrix(test_pred_Attrition_smote, test_actual_smote, positive = "Yes")

confusionmartix_final_smote


# Confusion Matrix and Statistics
# 
# Reference
# Prediction    No   Yes
# No  12586   323
# Yes  7512   540
# 
# Accuracy : 0.6262          
# 95% CI : (0.6196, 0.6328)
# No Information Rate : 0.9588          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0505          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.62572         
# Specificity : 0.62623         
# Pos Pred Value : 0.06706         
# Neg Pred Value : 0.97498         
# Prevalence : 0.04117         
# Detection Rate : 0.02576         
# Detection Prevalence : 0.38414         
# Balanced Accuracy : 0.62598         
# 
# 'Positive' Class : Yes

#Using smote also we have got 62.62 accurecy


##Removing the modeling object 

rm(smote_model_1,smote_model_3,smote_model_4,smote_model_5,smote_model_6,smote_model_7,smote_model_8,smote_model_9,
   smote_model_10,smote_model_11,smote_model_12,smote_model_13,smote_model_14)

###############################################################################################
###Descision Tress
###############################################################################################

#Replicating the original data frame to a new data frame for analysis purpose

master_data_tree <-  master_data_final

master_data_tree$Performance.Tag <-  as.factor(ifelse(master_data_tree$Performance.Tag==0,"no","yes"))

set.seed(100)
trainindices= sample(1:nrow(master_data_tree), 0.7*nrow(master_data_tree))
train_rf = master_data_tree[trainindices,]
test_rf = master_data_tree[-trainindices,]



#oversampling and balance the data.
train_over <- ovun.sample(Performance.Tag ~., data = train_rf, method = "over", N = 93686)$data
table(train_over$Performance.Tag)

#   no   yes 
# 46822 46864

# undersampling the data is done without replacement.
train_down <- ovun.sample(Performance.Tag ~., data = train_rf, method = "under", N = 4126)$data
table(train_down$Performance.Tag)

#  no   yes 
# 2042 2084 

#Using both we can do both under sampling and over sampling
#In this case minority class will be oversampled and majaority class will be undersamples with replacement

train_both <- ovun.sample(Performance.Tag ~., data = train_rf, method = "both",p=0.5,N=nrow(train_rf))$data
table(train_both$Performance.Tag)

#  no    yes 
# 24592 24314

# ROSE package will create the sybthetic data.

train_synthetic <- ROSE(Performance.Tag ~ ., data = train_rf, seed = 1)$data
table(train_synthetic$Performance.Tag)

#  no     yes 
# 24451 24455 


###########################################################################################################
##################################### Modelling  ##########################################################
###########################################################################################################



tree_rose_model <- rpart(Performance.Tag ~ .,                     
                         data = train_synthetic,                  
                         method = "class")              

tree_under_model <- rpart(Performance.Tag ~ .,                     
                          data = train_down,                  
                          method = "class")    

tree_over_model <- rpart(Performance.Tag ~ .,                     
                         data = train_over,                  
                         method = "class")  

tree_both_model <- rpart(Performance.Tag ~ .,                     
                         data = train_both,                  
                         method = "class")           

prp(tree_rose_model)

prp(tree_under_model)

prp(tree_over_model)

prp(tree_both_model)


#Predicting the output

tree_predict_rose <- predict(tree_rose_model, test_rf, type = "class")
confusionMatrix(tree_predict_rose, test_rf$Performance.Tag, positive = "yes")

# 
# Confusion Matrix and Statistics
# 
# Reference
# Prediction    no   yes
# no  16305   507
# yes  3793   356
# 
# Accuracy : 0.7949          
# 95% CI : (0.7893, 0.8003)
# No Information Rate : 0.9588          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0793          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.41251         
# Specificity : 0.81127         
# Pos Pred Value : 0.08580         
# Neg Pred Value : 0.96984         
# Prevalence : 0.04117         
# Detection Rate : 0.01698         
# Detection Prevalence : 0.19794         
# Balanced Accuracy : 0.61189         
# 
# 'Positive' Class : yes 


tree_predict_under <- predict(tree_under_model, test_rf, type = "class")
confusionMatrix(tree_predict_under, test_rf$Performance.Tag, positive = "yes")

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    no   yes
# no  10494   207
# yes  9604   656
# 
# Accuracy : 0.5319          
# 95% CI : (0.5252, 0.5387)
# No Information Rate : 0.9588          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0455          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.76014         
# Specificity : 0.52214         
# Pos Pred Value : 0.06394         
# Neg Pred Value : 0.98066         
# Prevalence : 0.04117         
# Detection Rate : 0.03130         
# Detection Prevalence : 0.48948         
# Balanced Accuracy : 0.64114         
# 
# 'Positive' Class : yes   


tree_predict_over <- predict(tree_over_model, test_rf, type = "class")
confusionMatrix(tree_predict_over, test_rf$Performance.Tag, positive = "yes")

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    no   yes
# no  11060   239
# yes  9038   624
# 
# Accuracy : 0.5574          
# 95% CI : (0.5507, 0.5642)
# No Information Rate : 0.9588          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0465          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.72306         
# Specificity : 0.55030         
# Pos Pred Value : 0.06458         
# Neg Pred Value : 0.97885         
# Prevalence : 0.04117         
# Detection Rate : 0.02977         
# Detection Prevalence : 0.46095         
# Balanced Accuracy : 0.63668         
# 
# 'Positive' Class : yes    

tree_predict_both <- predict(tree_both_model, test_rf, type = "class")
confusionMatrix(tree_predict_both, test_rf$Performance.Tag, positive = "yes")

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    no   yes
# no  11060   239
# yes  9038   624
# 
# Accuracy : 0.5574          
# 95% CI : (0.5507, 0.5642)
# No Information Rate : 0.9588          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0465          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.72306         
# Specificity : 0.55030         
# Pos Pred Value : 0.06458         
# Neg Pred Value : 0.97885         
# Prevalence : 0.04117         
# Detection Rate : 0.02977         
# Detection Prevalence : 0.46095         
# Balanced Accuracy : 0.63668         
# 
# 'Positive' Class : yes 

###
#Removing objects from memory

rm(tree_rose_model,tree_under_model,tree_over_model,tree_both_model)


#######################################################################################################
### Random Forest Model
#######################################################################################################

#Random forest using train_df data set

rf_model <- randomForest(Performance.Tag ~., data = train_rf, proximity = F, do.trace = T, mtry = 5,ntree=1000)
rf_pred <- predict(rf_model, test_rf[, -25], type = "prob")


perform_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_rf(s[i])
} 


# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])==min(abs(OUT_rf[,1]-OUT_rf[,2])))]
cutoff_rf

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.01)]


predicted_response<- factor(ifelse(rf_pred[, 2] >= best_cutoff, "yes", "no"))
conf_rf <- confusionMatrix(predicted_response, test_rf$Performance.Tag, positive = "yes")
conf_rf

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    no   yes
# no  12013   294
# yes  8085   569
# 
# Accuracy : 0.6003          
# 95% CI : (0.5936, 0.6069)
# No Information Rate : 0.9588          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0483          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.65933         
# Specificity : 0.59772         
# Pos Pred Value : 0.06575         
# Neg Pred Value : 0.97611         
# Prevalence : 0.04117         
# Detection Rate : 0.02715         
# Detection Prevalence : 0.41286         
# Balanced Accuracy : 0.62852         
# 
# 'Positive' Class : yes


##Removing the modeling object 

rm(rf_model)


##----- Model using synthetic data set

rf_synthetic <- randomForest(Performance.Tag ~., data = train_synthetic, proximity = F, do.trace = T, mtry = 5,ntree=1000)
rf_pred_synthetic <- predict(rf_synthetic, test_rf[, -25], type = "prob")




perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred_synthetic[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 


# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))



cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])==min(abs(OUT_rf[,1]-OUT_rf[,2])))]
cutoff_rf


predicted_response_rf<- factor(ifelse(rf_pred_synthetic[, 2] >= cutoff_rf, "yes", "no"))
conf_rf <- confusionMatrix(predicted_response_rf, test_rf$Performance.Tag, positive = "yes")
conf_rf



# Confusion Matrix and Statistics
# 
# Reference
# Prediction    no   yes
# no  12813   305
# yes  7285   558
# 
# Accuracy : 0.6379          
# 95% CI : (0.6314, 0.6444)
# No Information Rate : 0.9588          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0583          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.64658         
# Specificity : 0.63753         
# Pos Pred Value : 0.07115         
# Neg Pred Value : 0.97675         
# Prevalence : 0.04117         
# Detection Rate : 0.02662         
# Detection Prevalence : 0.37417         
# Balanced Accuracy : 0.64205         
# 
# 'Positive' Class : yes             


##Removing the modeling object 

rm(rf_synthetic)


##--- model using under sampled data set

rf_down <- randomForest(Performance.Tag ~., data = train_down, proximity = F, do.trace = T, mtry = 5,ntree=1000)
rf_pred_down <- predict(rf_down, test_rf[, -25], type = "prob")


perform_fn_rf_dn <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred_down[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}


s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf_dn(s[i])
} 


plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])==min(abs(OUT_rf[,1]-OUT_rf[,2])))]
cutoff_rf


predicted_response_rf_down<- factor(ifelse(rf_pred_down[, 2] >= cutoff_rf, "yes", "no"))
conf_rf <- confusionMatrix(predicted_response_rf_down, test_rf$Performance.Tag, positive = "yes")
conf_rf


# Confusion Matrix and Statistics
# 
# Reference
# Prediction    no   yes
# no  12782   329
# yes  7316   534
# 
# Accuracy : 0.6353          
# 95% CI : (0.6287, 0.6418)
# No Information Rate : 0.9588          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0523          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.61877         
# Specificity : 0.63598         
# Pos Pred Value : 0.06803         
# Neg Pred Value : 0.97491         
# Prevalence : 0.04117         
# Detection Rate : 0.02548         
# Detection Prevalence : 0.37451         
# Balanced Accuracy : 0.62738         
# 
# 'Positive' Class : yes  

##Removing the modeling object 

rm(rf_down)

#--- Modeling using over sampled data set

rf_over <- randomForest(Performance.Tag ~., data = train_over, proximity = F, do.trace = T, mtry = 5,ntree=500)
rf_pred_over <- predict(rf_over, test_rf[, -25], type = "prob")


perform_fn_rf_over <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred_over[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}


s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf_over(s[i])
} 


plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])==min(abs(OUT_rf[,1]-OUT_rf[,2])))]
cutoff_rf


predicted_response_rf_over<- factor(ifelse(rf_pred_over[, 2] >= cutoff_rf, "yes", "no"))
conf_rf <- confusionMatrix(predicted_response_rf_over, test_rf$Performance.Tag, positive = "yes")
conf_rf

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    no   yes
# no  12782   329
# yes  7316   534
# 
# Accuracy : 0.6353          
# 95% CI : (0.6287, 0.6418)
# No Information Rate : 0.9588          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0523          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.61877         
# Specificity : 0.63598         
# Pos Pred Value : 0.06803         
# Neg Pred Value : 0.97491         
# Prevalence : 0.04117         
# Detection Rate : 0.02548         
# Detection Prevalence : 0.37451         
# Balanced Accuracy : 0.62738         
# 
# 'Positive' Class : yes 

##Removing the modeling object 

rm(rf_over)


#-- Modeling using balanced data set

rf_both <- randomForest(Performance.Tag ~., data = train_both, proximity = F, do.trace = T, mtry = 5,ntree=1000)
rf_pred_both <- predict(rf_both, test_rf[, -25], type = "prob")


perform_fn_rf_both <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred_both[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}


s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf_both(s[i])
} 


plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])==min(abs(OUT_rf[,1]-OUT_rf[,2])))]
cutoff_rf


predicted_response_rf_both<- factor(ifelse(rf_pred_both[, 2] >= cutoff_rf, "yes", "no"))
conf_rf <- confusionMatrix(predicted_response_rf_both, test_rf$Performance.Tag, positive = "yes")
conf_rf


# Confusion Matrix and Statistics
# 
# Reference
# Prediction    no   yes
# no  12338   349
# yes  7760   514
# 
# Accuracy : 0.6131          
# 95% CI : (0.6065, 0.6197)
# No Information Rate : 0.9588          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.041           
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.59560         
# Specificity : 0.61389         
# Pos Pred Value : 0.06212         
# Neg Pred Value : 0.97249         
# Prevalence : 0.04117         
# Detection Rate : 0.02452         
# Detection Prevalence : 0.39473         
# Balanced Accuracy : 0.60474         
# 
# 'Positive' Class : yes

##Removing the modeling object 

rm(rf_both)


#########################################################################################
########################### credit score generation process##############################
#########################################################################################

## Build an application scorecard with the good to bad odds of 10 to 1 
## at a score of 400 doubling every 20 points. 

master_data_final$perdict_default  <- predict(smote_model_15, type = "response", newdata = master_data_final)
master_data_final$predict_NonDefault <- 1 - master_data_final$perdict_default
master_data_final$odds <-  log(master_data_final$predict_NonDefault/master_data_final$perdict_default)

Offset = 400
PDO = 20
log_odds=10
Factor = PDO/log(2)
Factor  #28.8539

master_data_final$Score = ceiling(Offset + (Factor*master_data_final$odds))

str(master_data_final$Score)
summary(master_data_final$Score)
## min - 349 to max - 453
quantile(master_data_final$Score,seq(0,1,0.2))  

## From the plot it is evident that score cut off could be set to 412.
cutoff_score =412

num_of_defaults_below_412<-length(which(master_data_final$Performance.Tag==1 & master_data_final$Score<412))
total_no_of_defaults<-length(which(master_data_final$Performance.Tag==1))

pc_defaults_covered_under_412<-ceiling((num_of_defaults_below_412/total_no_of_defaults)*100)

pc_defaults_covered_under_412





#################### financial analysis###########################

approval_rate <-(nrow(master_data_final) -nrow(Rejected_demographic))/nrow(master_data_final) *100

approval_rate
# current approval rate : 98%

default_users_outstanding <- master_data_Backup$Outstanding.Balance[which(master_data_Backup$Performance.Tag==1)]

current_credit_loss<- sum(as.numeric(default_users_outstanding),na.rm = TRUE)

current_credit_loss
#Current credit loss : 3704984230

#default_users_ID <- master_data_Backup$Application.ID [which(master_data_Backup$Performance.Tag==1)]

t1<-master_data_Backup$Outstanding.Balance[which(master_data_final$Performance.Tag==1)]
t2<-master_data_final$Score[which(master_data_final$Performance.Tag==1)]    

outstanding_ref <- cbind(default_users_outstanding,scale(default_users_outstanding),t1,t2)

possible_defaults_with_more_than_412_score<-data.frame(subset(outstanding_ref,t2>412))

sum(as.numeric(possible_defaults_with_more_than_412_score$default_users_outstanding),na.rm = TRUE)
# New credit loss : 615945997


nrow(data.frame(subset(master_data_final,Score>412)))/nrow(master_data_final)
# New approval rate : 42%

## Although net credit loss  is much lesser, Approval rate also goes down sharply. 
## Which in turn could cause less sales. Hence in this Trade-off between sales and risk,
## we could afford to be less conservative in terms of score cut off.
## from score distribution plot we can observe that by setting cut off to 395
## we can cover  much higher sales without risking too much credit loss.

possible_defaults_with_more_than_395_score<-data.frame(subset(outstanding_ref,t2>395))

sum(possible_defaults_with_more_than_395_score$default_users_outstanding,na.rm = TRUE)
# New credit loss : 1543066605


nrow(data.frame(subset(master_data_final,Score>395)))/nrow(master_data_final)
# New approval rate : 65%

## This gives a more balanced result.


################################################################################################
######################### Analysis of Application scoreboard on Rejected Population ###############################################
################################################################################################


sapply(Rejected_demographic, function(x) sum(is.na(x)))
sapply(Rejected_credit_beareau, function(x) sum(is.na(x)))


setdiff(Rejected_demographic$Application.ID,Rejected_credit_beareau$Application.ID)  


Rejected_master_data<- merge(x = Rejected_demographic, y = Rejected_credit_beareau, by = 'Application.ID',all = FALSE)

Rejected_master_data <- subset(Rejected_master_data,select = -c(Performance.Tag.x))

colnames(Rejected_master_data)[29] <- "Performance.Tag"

#Removing Application ID dfrom the data set

Rejected_master_data <- Rejected_master_data[,-1]

Rejected_master_data$No.of.months.in.current.residence <- log_t(Rejected_master_data$No.of.months.in.current.residence)

Rejected_master_data$Avgas.CC.Utilization.in.last.12.months <- log_t(Rejected_master_data$Avgas.CC.Utilization.in.last.12.months)

Rejected_master_data$Outstanding.Balance <- log10_t(Rejected_master_data$Outstanding.Balance)

Rejected_master_data$No.of.PL.trades.opened.in.last.12.months <- log_t(Rejected_master_data$No.of.PL.trades.opened.in.last.12.months)

Rejected_master_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- log_t(Rejected_master_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)

Rejected_master_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- log_t(Rejected_master_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)

Rejected_master_data$Total.No.of.Trades <- log_t(Rejected_master_data$Total.No.of.Trades)

Rejected_master_data$Gender <- as.factor(Rejected_master_data$Gender)
Rejected_master_data$Marital.Status..at.the.time.of.application. <- as.factor(Rejected_master_data$Marital.Status..at.the.time.of.application.)
Rejected_master_data$Education <- as.factor(Rejected_master_data$Education)
Rejected_master_data$Profession <- as.factor(Rejected_master_data$Profession)
Rejected_master_data$Type.of.residence <- as.factor(Rejected_master_data$Type.of.residence)

Rejected_master_data$Avgas.CC.Utilization.in.last.12.months <- as.numeric(Rejected_master_data$Avgas.CC.Utilization.in.last.12.months)
Rejected_master_data$Presence.of.open.home.loan <- as.numeric(Rejected_master_data$Presence.of.open.home.loan)
Rejected_master_data$Outstanding.Balance <- as.numeric(Rejected_master_data$Outstanding.Balance)
Rejected_master_data$Age <- as.numeric(Rejected_master_data$Age)
Rejected_master_data$Income <- as.numeric(Rejected_master_data$Income)

Rejected_master_data_final <- Rejected_master_data


Rejected_master_data_final$Gender <- as.factor(ifelse(Rejected_master_data_final$Gender == 'F', 0, 1))

Rejected_master_data_final$Marital.Status..at.the.time.of.application. <- as.factor(ifelse(Rejected_master_data_final$Marital.Status..at.the.time.of.application.== 'Single', 0, 1))



#Taking all catagorical variables into a data frame

Rej_cat_data <- Rejected_master_data_final[,c('Education','Profession','Type.of.residence')]

dummies_rej<- data.frame(sapply(Rej_cat_data, function(x) data.frame(model.matrix(~x-1,data =Rej_cat_data))[,-1]))

#Removing the catagorical variables and replace with dummy values for further analysis.

Rej_master_without_cat_var <- Rejected_master_data_final[,-c(6,7,8)]

Rejected_master_data_final <- cbind(Rej_master_without_cat_var,dummies_rej)

str(Rejected_master_data_final)

Rejected_master_data_final$Gender <- as.numeric(Rejected_master_data_final$Gender)
Rejected_master_data_final$Marital.Status..at.the.time.of.application. <- as.numeric(Rejected_master_data_final$Marital.Status..at.the.time.of.application.)
#Converting performance tag to factors

Rejected_master_data_final$Performance.Tag <- as.factor(Rejected_master_data_final$Performance.Tag)
Rejected_master_data_final$Performance.Tag <- as.factor(ifelse(Rejected_master_data_final$Performance.Tag==0,"no","yes"))



Rejected_master_data_final$perdict_default  <- predict(smote_model_15, type = "response", newdata = Rejected_master_data_final)
Rejected_master_data_final$predict_NonDefault <- 1 - Rejected_master_data_final$perdict_default
Rejected_master_data_final$odds <-  log(Rejected_master_data_final$predict_NonDefault/Rejected_master_data_final$perdict_default)

Offset = 400
PDO = 20
log_odds=10
Factor = PDO/log(2)
Factor  #28.8539

Rejected_master_data_final$Score = ceiling(Offset + (Factor*Rejected_master_data_final$odds))

optimal_cutoff_score=395

num_of_defaults_below_395<-length(which(Rejected_master_data_final$Score<395))
total_no_of_defaults<-nrow(num_of_defaults_below_395)

pc_defaults_covered_under_395<-ceiling((num_of_defaults_below_395/nrow(Rejected_master_data_final))*100)

pc_defaults_covered_under_395

#out of 1425 rejected customers 1344 are rejected by our model and 81 customers are approved.(5% of customers are approved)

hist(master_data_final$Score)


#Gain And Lift Charts

predictions_logit_fullData <- predict(smote_model_15, newdata = master_data_final, type = "response")
master_data_final$predicted_probs <- predictions_logit_fullData

master_data_final$predicted_response <-  factor(ifelse(master_data_final$predicted_probs >= 0.52, "no", "yes"))

master_data_final <- master_data_final[order(-master_data_final$predicted_probs),]

require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob, groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}



master_data_final_lg = lift(master_data_final$Performance.Tag, master_data_final$predicted_probs, groups = 10)

plot(master_data_final_lg$bucket,master_data_final_lg$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")

plot(master_data_final_lg$bucket,master_data_final_lg$Cumlift,col="red",type="l",main="Lift Chart",xlab="% of total targeted",ylab = "Lift")


View(master_data_final_lg)








