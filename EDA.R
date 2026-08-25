

player_data=read.csv("../complete_data_for_referance/Player_database.csv")
names(player_data)
#we have a total of 32 columns and 736 rows in the dataset recording the performance of 735 players in the NBA season 2024-'25
#There are total 32 columns, among which we exclude the index column, Rk denoting rank of the players, player name, player team, and award
#so we have 27 variables in hsnd now
#we want to predict the points earned by a player per 100 team possessions(PTS), in the season 2024-25
#using the other covariates and fitting a multiple linear regression


# Loading required libraries
install.packages('dplyr')
install.packages('readr')
install.packages('ggplot2')
install.packages('corrplot')
install.packages('car')
library(dplyr)
library(readr)
library(ggplot2)
library(corrplot)
library(car)  # for VIF multicollinearity checks

# cleaning data

player_data_clean <- player_data[1:(nrow(player_data)-6), ]  

#now we have 730 observations since many observations were missing in the last 6 rows

names(player_data_clean)
attach(player_data_clean)
player_data_cleaned <- player_data_clean[-c(1,2,3,5,10,11,13,14,16,17,19,20,21,32)]
names(player_data_cleaned)
#now the dataset contains only the numeric discrete/continuous variables along with the only nominal attribute Pos with 5 levels
#y(response variable)=PTS
colSums(is.na(player_data_cleaned))
#so many columns with NA's and since we can't afford to lose on so much data by dropping the respective rows,
#we replace Na with 0's
cols_to_fill <- c("FG.", "X3P.", "X2P.", "FT.")
player_data_cleaned[cols_to_fill] <- lapply(player_data_cleaned[cols_to_fill], function(x) {
  replace(x, is.na(x), 0)
})

#but we fill the NA s in columns of derived percentages and mutate them accordingly to avoid dividing by 0

player_data_cleaned= player_data_cleaned %>%
  mutate(
    X3P_perc = ifelse(X3PA == 0, 0, X3P / X3PA * 100),
    X2P_perc = ifelse(X2PA == 0, 0, X2P / X2PA * 100),
    FT_perc  = ifelse(FTA == 0, 0, FT / FTA * 100)
  )

#turning categorical predictor into dummy variables
plyer_data_cleaned <- player_data_cleaned %>%
  mutate(Pos = as.factor(Pos))

#finally, we introduce the regression with a full model first

# formula for predicting PTS or points per 100 possessions
model <- lm(PTS ~ Age + G + GS + MP + X3P_perc + X2P_perc + FT_perc +
              ORB + DRB + TRB + AST + STL + BLK + TOV + PF + Pos,
            data = player_data_cleaned)
summary(model)

#now we check for the CLRM assumptions
#diagnostic tests
#we have to check the following assumptions needed for a Gauss Markov model
#Linearity of response and covariates,standard normally distributed residuals, homoscadesticity, uncorrelated errors, multicollinearity, outliers/leverage points

#linearity
plot(model, which = 1)  # Residuals vs Fitted

#normality of errors
plot(model, which = 2)   # Q-Q Plot #normally distributed in the middle but outliers at tail ends
shapiro.test(residuals(model)) 

#uncorrelated errors

durbinWatsonTest(model) #no autocorrelated errors

#heteroscedasticity
install.packages('lmtest')
library(lmtest)
bptest(model)

#p-value<0.05
#so there is heteroscedasticity, as suspected earlier from residual plot, showing the funnel shape
#we can use methods like weighted least squares after identifying the variables causing heteroscedasticity, but it's beyond the scope of current course
#instead we transform the response variable using a variance stabilising transformation like log(y+1)
model_log <- lm(log(PTS + 1) ~ Age + G + GS + MP + X3P_perc + X2P_perc +
                  FT_perc + ORB + DRB + TRB + AST + STL + BLK + TOV + PF + Pos,
                data = player_data_cleaned)

plot(model_log,which=1)
#now the residual plot looks way more evenly spread or homoscedastic
summary(model_log)

plot(model_log,which=2)

durbinWatsonTest(model_log)

#multicollinearity
vif(model)
#there is moderate muticollinearity amongst the covariates X3P%, X2P%
#so Lasso,Ridge regressions can be better alternatives for the model, but it's beyond the scope of current syllabus

#outlier detection
# Cook's distance
plot(model_log, which = 4)  # Cook's distance
abline(h = 4/(nrow(player_data_cleaned)-length(coef(model_log))), col="red", lty=2)