rm(list=ls())

#Loading libraries 
library(MASS)
library(caret)
library(foreign)
library(nnet)
library(reshape2)
library(tree)
library(randomForest)

nfl_df<- read.csv("train.csv")


nfl_df$OffenseFormation<-as.factor(nfl_df$OffenseFormation)
nfl_df$Quarter<-as.factor(nfl_df$Quarter)

#This is a player tracking dataset, only want 1 row per play not 22 rows
nfl_subset<-nfl_df[!duplicated(nfl_df$PlayId), ]
nfl_subset$OffenseFormation<-as.factor(nfl_subset$OffenseFormation)

#Remove all non-labeled offensive formations
nfl_subset<-nfl_subset[-which(nfl_subset$OffenseFormation == ""), ]


#Only 1 instance of ACE formation, just another way to label SINGLEBACK
levels(nfl_subset$OffenseFormation)[levels(nfl_subset$OffenseFormation)=='ACE'] <- 'SINGLEBACK'
nfl_subset<-droplevels(nfl_subset
                       )
nfl_subset$Down<-as.factor(nfl_subset$Down)
nfl_subset$Quarter<-as.factor(nfl_subset$Quarter)
levels(nfl_subset$Quarter)[levels(nfl_subset$Quarter)=='5'] <- 'OT'

nfl_subset$ScoreDisc <- NA
nfl_subset$ScoreDisc <- as.numeric(nfl_subset$ScoreDisc)

nfl_subset$ScoreDisc <- with(nfl_subset, ifelse(PossessionTeam==HomeTeamAbbr,
                                              HomeScoreBeforePlay-VisitorScoreBeforePlay,
                                              VisitorScoreBeforePlay-HomeScoreBeforePlay))

#YardLine is a 1-50 yard variable that includes field position, want a new variable (Yrd) that measures from 1-99 yards
nfl_subset$Yrd <- NA
nfl_subset$Yrd <- as.numeric(nfl_subset$Yrd)
nfl_subset$YardLine <- as.numeric(nfl_subset$YardLine)

nfl_subset$Yrd<-nfl_subset$YardLine

nfl_subset$Yrd<-with(nfl_subset,ifelse(PossessionTeam!=FieldPosition,
                                     Yrd + 50,
                                     Yrd + 0))

nfl_subset$Yrd<-with(nfl_subset,ifelse(Yrd==100,
                                     Yrd - 50,
                                     Yrd + 0))

#Set reference levels for each of the categorical variables that will be used
nfl_subset <- within(nfl_subset, Quarter <- relevel(Quarter, ref = "1"))
nfl_subset <- within(nfl_subset, Down <- relevel(Down, ref = "1"))
nfl_subset <- within(nfl_subset, OffenseFormation <- relevel(OffenseFormation, ref = "SINGLEBACK"))

#New dataset that only includes the relevant columns from the dataset before
nfl_data <- nfl_subset[, c("OffenseFormation","Down", "Distance", "Quarter", "Yrd", "ScoreDisc", "PossessionTeam")]


# Creating train and test 
set.seed(0)

trainindex<-createDataPartition(nfl_data$Down, p=4/5, list=F)
train <- nfl_data[trainindex,]
test<- nfl_data[-trainindex,]

train <- within(train, Quarter <- relevel(Quarter, ref = "1"))
train <- within(train, Down <- relevel(Down, ref = "1"))
train <- within(train, OffenseFormation <- relevel(OffenseFormation, ref = "SINGLEBACK"))
test <- within(test, OffenseFormation <- relevel(OffenseFormation, ref = "SINGLEBACK"))

# random forest model 

tree_nfl<-randomForest(OffenseFormation~Down+Distance+Quarter+Yrd+ScoreDisc+PossessionTeam+
                         PossessionTeam:Quarter,data=train,mtry=sqrt(6),importance=TRUE)



lda<- lda(OffenseFormation~ Down+Distance+Quarter+Yrd+ScoreDisc+PossessionTeam
          data=nfl_data,subset=train)

#boosting

