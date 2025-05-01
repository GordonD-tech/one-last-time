df<-read.csv('https://raw.githubusercontent.com/GordonD-tech/one-last-time/refs/heads/main/Sleep_health_and_lifestyle_dataset.csv')
#LOADING THE LIBRARIES
library(tidymodels) #INCLUDES parsnip PACKAGE FOR decision_tree()
library(caret) #FOR confusionMatrix()

#IMPORTING THE DATA


##PARTITIONING THE DATA##
set.seed(123)
split1<-initial_split(df, prop=.7, strata=Stress.Level)
train<-training(split1)
holdout<-testing(split1)

set.seed(123)
split2<-initial_split(holdout, prop=.5, strata=Stress.Level)
val<- training(split2)
test<-testing(split2)
library(sm) #loads library for the next function to work
##BUILD A UNIVARIATE REGRESSION MODEL TO LOOK AT THE EFFECT OF PROMO ON SALES##
M2<-lm(Stress.Level~Quality.of.Sleep, train)  #builds the model: Sales = B_0+B_1(Promo)+e
summary(M2)  #returns summary output from the model M2
M3<-lm(Stress.Level~Quality.of.Sleep, val)  #builds the model: Sales = B_0+B_1(Promo)+e
summary(M3)
library(datasets)
M4<-lm(Stress.Level~log(Quality.of.Sleep), df)  ##level-log model: y=B0+B1*ln(x)+u
summary(M4)
M5<-lm(Stress.Level~log(Quality.of.Sleep), train)  
M6<-lm(Stress.Level~log(Quality.of.Sleep), val)
#SET UP GRID OF REGULARIZATION PARAMETER (LAMBDA) VALUES
lambda <- seq(0, 2,.001)
