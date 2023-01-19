
library(tidyverse)
#install.packages("ROSE")
library(ROSE)
library(DMwR)


df<-read.csv("C:/Users/HP/Desktop/Datasets/bank-full.csv", header = TRUE,sep=";")
banktest<-read.csv("C:/Users/HP/Desktop/Datasets/bank.csv", header = TRUE,sep=";")
#write.csv(x=banktest, file="banktest.csv")
str(df)
dim(df)



table(df$y)
prop.table(table(df$y))


#Oversampling

data_balanced_over <- ovun.sample(y ~ ., data = df, method = "over")$data

table(data_balanced_over$y)


#Under and OverSampling "Both"

data_balanced_both <- ovun.sample(y ~ ., data = df, method = "both", p=0.5, N=45211, seed = 42)$data

table(data_balanced_both$y)

str(df)

df$job <- as.factor(df$job)
df$marital <- as.factor(df$marital)
df$education <- as.factor(df$education)
df$default <- as.factor(df$default)
df$housing <- as.factor(df$housing)
df$loan <- as.factor(df$loan)
df$contact <- as.factor(df$contact)
df$month <- as.factor(df$month)
df$poutcome <- as.factor(df$poutcome)
df$y <- as.factor(df$y)
df$pdays<-as.factor(df$pdays)
df$previous<-as.factor(df$previous)
df$campaign<-as.factor(df$campaign)
df$duration<-as.factor(df$campaign)
df$age<-as.factor(df$age)
df$balance<-as.factor(df$balance)
df$day<-as.factor(df$day)


banktest$job <- as.factor(banktest$job)
banktest$marital <- as.factor(banktest$marital)
banktest$education <- as.factor(banktest$education)
banktest$default <- as.factor(banktest$default)
banktest$housing <- as.factor(banktest$housing)
banktest$loan <- as.factor(banktest$loan)
banktest$contact <- as.factor(banktest$contact)
banktest$month <- as.factor(banktest$month)
banktest$poutcome <- as.factor(banktest$poutcome)
banktest$y <- as.factor(banktest$y)
banktest$pdays<-as.factor(banktest$pdays)
banktest$previous<-as.factor(banktest$previous)
banktest$campaign<-as.factor(banktest$campaign)
banktest$duration<-as.factor(banktest$campaign)
banktest$age<-as.factor(banktest$age)
banktest$balance<-as.factor(banktest$balance)
banktest$day<-as.factor(banktest$day)

str(df)
str(banktest)
table(df$y)

#SMOTE
new_df_smote <- SMOTE(y ~ ., df, perc.over = 755,k=5,perc.under=115,learner = NULL)

table(new_df_smote$y)



#ROSE

data.rose <- ROSE(y ~ ., data = df, seed = 1)$data

table(data.rose$y)

5289*7.5481

library(rpart)



#MODEL

tree.smote <- rpart(y ~ ., data = new_df_smote)
pred.tree.smote <- predict(tree.smote, newdata = banktest)

#AUC SMOTE

roc.curve(banktest$y, pred.tree.smote[,2])


tree.rose <- rpart(y ~ ., data = data.rose)
pred.tree.rose <- predict(tree.rose, newdata = banktest)

#AUC ROSE

roc.curve(banktest$y, pred.tree.rose[,2])

#write.csv(x=data.rose, file = "bank-full-rose.csv")

df$pdays<-as.integer(df$pdays)
df$previous<-as.integer(df$previous)
df$campaign<-as.integer(df$campaign)
df$duration<-as.integer(df$campaign)
df$age<-as.integer(df$age)
df$balance<-as.integer(df$balance)
df$day<-as.integer(df$day)

banktest$pdays<-as.integer(banktest$pdays)
banktest$previous<-as.integer(banktest$previous)
banktest$campaign<-as.integer(banktest$campaign)
banktest$duration<-as.integer(banktest$campaign)
banktest$age<-as.integer(banktest$age)
banktest$balance<-as.integer(banktest$balance)
banktest$day<-as.integer(banktest$day)

tree.over <- rpart(y ~ ., data = data_balanced_over)
tree.both <- rpart(y ~ ., data = data_balanced_both)


pred.tree.over <- predict(tree.over, newdata = banktest)
pred.tree.both <- predict(tree.both, newdata = banktest)



#AUC Oversampling

roc.curve(banktest$y, pred.tree.over[,2])


#AUC Both

roc.curve(banktest$y, pred.tree.both[,2])









