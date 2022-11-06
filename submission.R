# loading required  libraries
library(ggplot2)
library(readxl)
library(tidyverse)
library(corrplot) 
library(caTools)
# import the dataset
df <- read_excel('/Users/hardey/Desktop/Fortray/R/submission/1555054100_hospitalcosts.xlsx')
#View(df)

# shape of the dataset
dim(df)

# basic statistics of the dataset
summary(df)
# structure of the dataset
str(df)
table(df$FEMALE)
table(df$RACE)
# check missing value
sum(is.na(df))
new_df <- na.omit(df)
attach(new_df)
# AGE, APRDRG, FEMALE, LOS, RACE, TOTCHG
min(new_df$AGE)
max(new_df$AGE)


new_df$AGEGROUP <- ifelse (AGE <= 5,'0-5',
                ifelse (AGE <= 10,'6-10',
                ifelse (AGE <= 15,'11-15','16-20')))

new_df$GENDER <- ifelse(FEMALE==1,'Female','Male')

#View(new_df)
attach(new_df)

# convert the female and race column to factors
new_df$FEMALE  <-  as.factor(FEMALE)
new_df$RACE  <-  as.factor(RACE)
new_df$APRDRG  <-  as.factor(APRDRG)

#check the new structure to confirm if they have been converted
str(new_df)


# Q1: 

#Age category who frequently visited the hospital most
age_group<- table(AGEGROUP) 
age_group_visit <-  barplot(age_group,
                  main='Visit To Hospital for each Age Group',
                  xlab='Age Group',
                  ylab='frequency',
                  col=c('red','green','blue','yellow'))

        
# Age category with the maximum hospitalization and expenditure
new_df %>% 
  group_by(AGEGROUP) %>% 
  summarise(Total_Exp = sum(TOTCHG)) %>% 
  ggplot(aes(x=AGEGROUP,y=Total_Exp,fill=AGEGROUP))+
  geom_bar(stat='identity')+
  geom_text(aes(label= (Total_Exp)),vjust=1.0,color="white",size=3.0)+
  ggtitle("Total Expenditure for each Age Group")+
  xlab("Age Group") + ylab('Total Expenditure')


#Q2: 
# Diagnosis Related group that has maximum hospitalization.
new_df %>% 
  group_by(APRDRG) %>% 
  summarise(Total_Visit = sum(LOS)) %>% 
  arrange(desc(Total_Visit)) %>% 
  slice(1:10) %>% 
  ggplot(aes(x=APRDRG,y=Total_Visit,fill=APRDRG))+
  geom_bar(stat='identity')+
  geom_text(aes(label= (Total_Visit)),vjust=1.0,color="white",size=3.0)+
  ggtitle("Total Visit for each Diagonosis Group")+
  xlab("Diagnosis Related group") + ylab('Total Visit')


#Diagnosis Related group that has maximum expenditure.

new_df %>% 
  group_by(APRDRG) %>% 
  summarise(Total_Exp= sum(TOTCHG)) %>% 
  arrange(desc(Total_Exp)) %>% 
  slice(1:10) %>% 
  ggplot(aes(x=APRDRG,y=Total_Exp,fill=APRDRG))+
  geom_bar(stat='identity')+
  geom_text(aes(label= (Total_Exp)),vjust=1.0,color="white",size=3.0)+
  ggtitle("Total Expenditure for each Diagonosis Group")+
  xlab("Diagnosis Related group") + ylab('Total Expenditure')


#Q3:Relationship between race of the patient and hospitalization costs.

ggplot(new_df, aes(x=RACE, y=TOTCHG)) + geom_col(aes(fill=RACE))+
  ggtitle("Total Expenditure for each Race")
summary(aov(TOTCHG~RACE))
#Q4:

# severity of the hospital costs by age 
new_df %>% 
  group_by(AGE) %>% 
  summarise(Total_Exp = sum(TOTCHG)) %>% 
  ggplot(aes(x=AGE,y=Total_Exp,fill=AGE))+
  geom_bar(stat='identity')+
  geom_text(aes(label= (Total_Exp)),vjust=1.0,color="white",size=3.0)+
  ggtitle("Total Expenditure for each Age Group")+
  xlab("Age") + ylab('Total Expenditure')


# severity of the hospital costs by gender 
new_df %>% 
  group_by(GENDER) %>% 
  summarise(Total_Exp= sum(TOTCHG)) %>% 
  ggplot(aes(x=GENDER,y=Total_Exp,fill=GENDER))+
  geom_bar(stat='identity')+
  geom_text(aes(label= (Total_Exp)),vjust=1.0,color="white",size=3.0)+
  ggtitle("Total Expenditure by Gender")+
  xlab("Gender") + ylab('Total Expenditure')



# Q5:
# predicting length of stay from age, gender, and race.

# convert the female and race column back to numerical
new_df$FEMALE  <-  as.numeric(FEMALE)
new_df$RACE  <-  as.numeric(RACE)
new_df$APRDRG  <-  as.numeric(APRDRG)

# Finding the correlation between the variables
cor(new_df$AGE,new_df$RACE)
cor(new_df$AGE,new_df$FEMALE)
cor(new_df$RACE,new_df$FEMALE)

# finding the covariance of the variables
cov(new_df$AGE,new_df$RACE)
cov(new_df$AGE,new_df$FEMALE)
cov(new_df$RACE,new_df$FEMALE)


# set seed for reproducibility
set.seed(94)

# Train:Test split = 75:25
sample1 <- sample.split(new_df$LOS,
                       SplitRatio = 0.75)

train1 <- subset(new_df,sample1==TRUE)
test1 <- subset(new_df,sample1==FALSE)
model1 <- lm(LOS~AGE+FEMALE+RACE,data=train1)
prediction <- predict(model1,test1)
prediction
summary(model1)


# Q6: 

# Finding the variable which is significant to Hospital costs. 

# set seed for reproducibility
set.seed(94)
# Train:Test split = 75:25
sample2 <- sample.split(new_df$APRDRG,
                       SplitRatio = 0.75)
train2 <- subset(new_df,sample2==TRUE)
test2 <- subset(new_df,sample2==FALSE)
model2 <- lm(TOTCHG~AGE+FEMALE+LOS+RACE+APRDRG,data=train2)
summary(model2)