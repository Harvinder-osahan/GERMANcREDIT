#Harvinder Singh, 
#UiD -17BCS1672
#Datascience Project On German Credit Data set


#loading packages

library(tidyverse)
library(readxl)

#reading files

a<-read_csv("german_credit_data.csv")
a<-na.omit(a)
raw_data<-a

library(dplyr)

select(raw_data,Age,Sex)
raw_data <- rename(raw_data,Gender=Sex)

#Adding new column to the table


raw_data<-mutate(raw_data ,amt_per_duration = Credit.amount/Duration)

#Arrange age in desceneding order

arrange(raw_data,desc(Age))


#Mutate and reduced data

vv<-select(raw_data,
           Age:Housing,
           ends_with("account"),
           starts_with("D")
)
vv

#Summarize group data

by_data<-group_by(raw_data,Age,Gender,Job)
summarise(by_data,german=mean(Age,na.rm = TRUE))

#Extracting all rows and col through 1 to 3

raw_data[,1:3]

#Vector 

set.seed(123)
v1<-sample(60:70,25,replace=TRUE)
v1
m1<-matrix(v1,nrow = 5)
m1
mean(m1)

#Histogram

raw_data
hist(raw_data$Age)
avg<-mean(raw_data$Age)

#Graph Ploting

ggplot(raw_data,aes(x=Age , y= Housing , color = raw_data$Housing))+geom_point()
ggplot(raw_data,aes(x=Age))+
  geom_bar(color = "yellow")

#Bivarient Geoms

ggplot(data=raw_data,aes(x=Age,y=Housing,color = raw_data$Housing))+
  geom_boxplot()

ggplot(raw_data,aes(x=Credit.amount,y=Duration , color=raw_data$Duration ))+
  geom_violin()

ggplot(data=raw_data,aes(x=Age,y=Housing , color = raw_data$Age>=40))+
  geom_smooth()

#Univarient Geoms

ggplot(data=raw_data,aes(x=Age,color = raw_data$Housing))+
  geom_density()

ggplot(data=raw_data,aes(x=Job, color=raw_data$Housing))+
  geom_bar()

#Title and Axis
library(tidyverse)
library(ggplot2)
ggplot(data = raw_data,aes(x=Age,y=Duration)) +
  geom_point() +
  ggtitle("AGE vs DURATION")

ggplot(data=raw_data,aes(x=Job,y=Duration))+
  geom_boxplot() +
  ggtitle("JOB vs DURATION",
          subtitle = "GERMAN_DATA")


#Layering
ggplot(data=raw_data,aes(x=Age,y=Duration))+
  geom_point(alpha=0.35) +
  scale_x_log10()


ggplot(data=raw_data,aes(x=Age,y=Duration))+
  geom_point(alpha=0.35) +
  scale_x_log10() +
  geom_smooth(method="lm") +
  facet_wrap(~Age)

#Liner Regression
library(caTools)

dataset=a
set.seed(123)
split=sample.split(dataset$Age,SplitRatio=2/3)

training_set=subset(dataset,split==TRUE)
testing_set=subset(dataset,split==FALSE)

#Fiting Simple Liner Regression to Training set

regressor=lm(formula=Credit.amount~Duration,
             data=training_set)

#Predicting test result

y_prd=predict(regressor,newdata = testing_set)

#Visualising

library(ggplot2)
ggplot()+
  geom_line(aes(x=training_set$Credit.amount,y=training_set$Duration),
             color='red')+
  geom_line(aes(x=testing_set$Credit.amount,y=predict(regressor,newdata = testing_set)),
            color='blue')+
  ggtitle('Credit.amount vs Duration')+
  xlab('Credit.amount')+
  ylab('Duration')

#linear regression

raw_data$`Saving accounts`=factor(raw_data$`Saving accounts`,
                        levels = c('little','rich','quite rich','moderate'),
                        labels = c(1,2,3,4))


library(caTools)
set.seed(123)
split=sample.split(dataset$Age , SplitRatio = 2/3)
training_set=subset(dataset,split==FALSE)
testing_set=subset(dataset,split==TRUE)

training_set<-mutate(training_set ,Credit_Score= Credit.amount%%2)
raw_data<-mutate(raw_data ,Credit_Score= Credit.amount%%2)

regressor = lm(formula =Credit.amount ~ `Saving accounts`,
               data=training_set)
y_prd=predict(regressor,newdata=testing_set)

testing_set<-mutate(training_set ,Credit_Score= Credit.amount%%2)
raw_data
library(ggplot2)
ggplot()+
  geom_line(aes(x=training_set$Credit.amount,y=training_set$`Saving accounts`),
            color='red')+
  geom_point(aes(x=testing_set$Credit.amount,y=predict(regressor,newdata = testing_set)),
            color='black')+
  ggtitle('Credit_Score vs Amount')+
  xlab('Credit amount')+
  ylab('Credit Score')



