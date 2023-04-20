options(scipen = 999)

library(tidyverse)
library(dplyr)
library(caret)
library(glmnet)
library(gbm)


### loading and cleaning the dataset
nfl_df<- read.csv("C:/Users/ekdeu/Downloads/train.csv")


#Classifying Weather Data
rain <- c('Rainy', 'Rain Chance 40%', 'Showers',
          'Cloudy with periods of rain, thunder possible. Winds shifting to WNW, 10-20 mph.',
          'Scattered Showers', 'Cloudy, Rain', 'Rain shower', 'Light Rain', 'Rain')

overcast <- c('Cloudy, light snow accumulating 1-3"', 'Party Cloudy', 'Cloudy, chance of rain',
              'Coudy', 'Cloudy, 50% change of rain', 'Rain likely, temps in low 40s.',
              'Cloudy and cold', 'Cloudy, fog started developing in 2nd quarter',
              'Partly Clouidy', '30% Chance of Rain', 'Mostly Coudy', 'Cloudy and Cool',
              'cloudy', 'Partly cloudy', 'Overcast', 'Hazy', 'Mostly cloudy', 'Mostly Cloudy',
              'Partly Cloudy', 'Cloudy')

clear <- c('Partly clear', 'Sunny and clear', 'Sun & clouds', 'Clear and Sunny',
           'Sunny and cold', 'Sunny Skies', 'Clear and Cool', 'Clear and sunny',
           'Sunny, highs to upper 80s', 'Mostly Sunny Skies', 'Cold',
           'Clear and warm', 'Sunny and warm', 'Clear and cold', 'Mostly sunny',
           'T: 51; H: 55; W: NW 10 mph', 'Clear Skies', 'Clear skies', 'Partly sunny',
           'Fair', 'Partly Sunny', 'Mostly Sunny', 'Clear', 'Sunny')

snow <- c('Heavy lake effect snow', 'Snow')

none <- c('N/A Indoor', 'Indoors', 'Indoor', 'N/A (Indoors)', 'Controlled Climate')

convert_weather <- function(x) {
        if(x %in% rain) {
                "rain"
        } else if(x %in% overcast) {
                "overcast"
        } else if(x %in% clear) {
                "clear"
        } else if(x %in% snow) {
                "snow"
        } else if( x %in% none) {
                "indoors"
        } else {
                "unknown"
        }
        
}    

#Combining Weather Data into DF
nfl_df_clean <- 
        nfl_df %>%
        mutate(GameWeather = mapply(convert_weather, GameWeather),
               isRunner = ifelse(NflId == NflIdRusher, 1, 0)) %>%
        select(-Temperature, -Humidity, -WindSpeed, -WindDirection, -StadiumType)

#Converting Appropriate Columns to Factors
nfl_df_clean$Stadium<- as.factor(nfl_df_clean$Stadium)
nfl_df_clean$GameWeather<- as.factor(nfl_df_clean$GameWeather)
nfl_df_clean$Turf<- as.factor(nfl_df_clean$Turf)
nfl_df_clean$OffenseFormation <- with(nfl_df_clean, ifelse(OffenseFormation == "", "Unknown", OffenseFormation))
nfl_df_clean$OffenseFormation <- as.factor(nfl_df_clean$OffenseFormation)

#Filtering down to smaller subset of relevance with only Running Back Plays
nfl_df_clean <- nfl_df_clean %>% 
        filter(Position == "RB") %>% 
        group_by(Season, PossessionTeam, PlayId, NflIdRusher) %>%
        slice_head(n = 1) %>% 
        group_by(Season, PossessionTeam) %>% 
        mutate(NextYards = lead(Yards, 1)) %>% 
        group_by()

#Getting Rid of NA's
nfl_df_clean<- na.omit(nfl_df_clean)


# Exploratory Data Analysis -----------------------------------------------

summary(nfl_df_clean)


#Plotting Yards on one play vs the Yards on the Next Play
ggplot(nfl_df_clean, aes(x = Yards, y = NextYards))+
        geom_point()

#Very Uncorrelated from one play to the next


#Plotting Yards vs Defenders in Box
ggplot(nfl_df_clean, aes(x = Yards, y = DefendersInTheBox))+
        geom_point()

#Most Plays have between 6-8 Defenders in the Box, but in general as you add
#more defenders the yards per carry goes down

#Plotting Yards vs Weather
ggplot(nfl_df_clean, aes(x = Yards, y = GameWeather))+
        geom_point()

#Weather has an effect as running in the snow or rain tend to reduce the high end
#of yards gained on a play

#Plotting Yards vs Formation
ggplot(nfl_df_clean, aes(x = Yards, y = OffenseFormation))+
        geom_point()

#Some formations are more common than others (such as Singleback, Shotgun, and I-Form)
#and tend to have different average yards per carry


#Filtering Outlier Plays for future modeling
nfl_df_clean<- distinct(nfl_df_clean) %>% 
        filter(Yards <= 15)


# Linear Model ------------------------------------------------------------


set.seed(123)

#Selecting Variables to Include in LM
nfl_rush_df <- select(nfl_df_clean, "Yards", "Down", "Distance", "OffenseFormation", "YardLine", "Quarter", "DefendersInTheBox", "Season", "Week")

#Train Test Split by Time
trainIndex <- if_else(nfl_rush_df$Week<13, 1, 0)
train_data <- nfl_rush_df[trainIndex == 1,]
test_data <-nfl_rush_df[trainIndex == 0,]

#Making Model
model_lm <- lm(Yards ~ ., data = train_data)

# Use the model to predict rushing yards for the test data
predictions_lm <- predict(model_lm, newdata = test_data)

#Example of Predictions
print(predictions_lm)

#Calculating the RMSE
rmse_lm <- sqrt(mean((predictions_lm - test_data$Yards)^2))
rmse_lm

#Comparing Predictions to Actual
plot(predictions_lm, test_data$Yards)


# Elastic Net -------------------------------------------------------------------

#Converting to Matrix format

x<- model.matrix(nfl_rush_df$Yards ~ ., nfl_rush_df)[,-1]

#Test Train Split
xtrain_data <- x[trainIndex == 1,]
xtest_data <-x[trainIndex == 0,]
ytrain_data <- nfl_rush_df$Yards[trainIndex == 1]
ytest_data <-nfl_rush_df$Yards[trainIndex == 0]

#Parameter setup
grid <- 10 ^ seq(10, -2, length=100)
alphas = seq(0,1,0.1)
RMSES_lasso <- rep(0, 10)
loop = 1

#Tuning Model Loop
for (i in alphas){
lasso_model <- cv.glmnet(xtrain_data, ytrain_data, alpha =i, lambda= grid)

#Selecting the Best Lambda with the smallest MSE
Min_Lambda <- lasso_model$lambda.min

# use the model to make predictions on the test set
test_pred <- predict(lasso_model, s = Min_Lambda, newx = xtest_data )

# calculate root mean squared error on the test set
RMSES_lasso[loop] <- sqrt(mean((ytest_data - test_pred)^2))
loop = 1 + loop
}

RMSES_lasso[which.min(RMSES_lasso)]

# GBM ---------------------------------------------------------------------

#Parameter Setup
RMSES <- rep(0,24)
loopcount = 1


#Tuning Model in Loop
for (i in 2:5){
        for (j in seq(0.05,0.3,0.05)){
gbm.model = gbm(Yards ~ .,
                data = nfl_rush_df[trainIndex == 1,],
                distribution = "gaussian",
                n.trees = 100, 
                interaction.depth = i, 
                shrinkage = j, 
                verbose = F) 

#Making Predictions
Prediction = predict(gbm.model,
                     newdata = nfl_rush_df[trainIndex == 0,],
                     n.trees = 100)

# RMSE
RMSES[loopcount] = sqrt(mean((Prediction - ytest_data)^2))
loopcount = 1 + loopcount
        }
}

RMSES[which.min(RMSES)]


#########Summary############
#In Summary, all of our models produced similar results and ultimately didnt do a great job at predicting future yards gained on a rush. Football is incredibly complicated to try and predict given the small sample size nature and the interconnectedness of the 22 players on the field every play.''

##########


