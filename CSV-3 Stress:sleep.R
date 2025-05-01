#LOADING THE LIBRARIES
library(tidymodels) #INCLUDES parsnip PACKAGE FOR decision_tree()
library(caret) #FOR confusionMatrix()

#IMPORTING THE DATA
df <-read.csv("https://raw.githubusercontent.com/slevkoff/CLASS_DATA/master/Armband.csv")
df$action<-as.factor(df$action) #CONVERT OUTPUT TO FACTOR

##PARTITIONING THE DATA##
set.seed(123)
split1<-initial_split(df, prop=.7, strata=Stress.Level)
train<-training(split1)
holdout<-testing(split1)

set.seed(123)
split2<-initial_split(holdout, prop=.5, strata=Stress.Level)
val<- training(split2)
test<-testing(split2)