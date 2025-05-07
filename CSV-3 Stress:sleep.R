df<-read.csv('https://raw.githubusercontent.com/GordonD-tech/one-last-time/refs/heads/main/Sleep_health_and_lifestyle_dataset.csv')
#LOADING THE LIBRARIES
library(tidymodels) #INCLUDES parsnip PACKAGE FOR decision_tree()
library(caret) #FOR confusionMatrix()

view(df)


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
library(tidyverse)

#STEP 1: FORM THE INPUT MATRIX X:

#STEP 1.1: MAKE A COLUMN OF ONES TO INCLUDE AS REGRESSORS FOR INTERCEPT
col_of_ones <- rep(1, dim(train)[1])

#STEP 1.2: BIND COLUMN OF ONES WITH OTHER INPUT DATA COLUMNS
#AND COERCE TO MATRIX OBJECT
X <- as.matrix(cbind(col_of_ones, train[,-c(1,2,4,9,13)]
                  
 #STEP 2: FORM THE OUTPUT VECTOR y
  y <- train[,1]
                     
#STEP 3: COMPUTE THE PSEUDOINVERSE MATRIX
 X_pseudo <- solve(t(X)%*%X)%*%t(X)
                     
#STEP 4: MULTIPLY THE PSEUDOINVERSE MATRIX BY THE OUTPUT VECTOR
Betas <- X_pseudo%*%y
                     
#SET UP GRID OF REGULARIZATION PARAMETER (LAMBDA) VALUES
lambda <- seq(0, 2,.001)
#INITIALIZE EMPTY MATRIX TO STORE ESTIMATED MODEL COEFFICIENTS FOR EACH LAMBDA
BETA_RIDGE <- matrix(NA, nrow = dim(t(X)%*%X) [1], ncol = length(lambda))
#INITIALIZE EMPTY MATRICES FOR STORING PREDICTION AND ERRORS
PRED_IN <- matrix(NA, nrow = dim(training) [1], ncol = length(lambda))
PRED_OUT <- matrix(NA, nrow = dim(holdout) [1], ncol = length (lambda))
E_IN <- matrix(NA, nrow = 1, ncol=length(lambda))
E_OUT <- matrix(NA, nrow = 1, ncol = length(lambda))
                     
 for (i in 1: length(lambda)){}
