# Module 6 Steam Game Reviews + prediction probabilities

steam_reviews.df <- read.csv("1030830_MafiaIIDefinitiveEdition.csv")

library(psych)

# remove irrelevant columns for our correlation (otherwise pairs.panels() takes too long and crashes)
steam_reviews.df <- steam_reviews.df[,c("weighted_vote_score", "votes_up", "votes_funny")]

pairs.panels(steam_reviews.df)

steam_model.df <- lm(weighted_vote_score ~ votes_up+votes_funny, data = steam_reviews.df)
summary(steam_model.df)

# predict weighted_vote_score (Lantz did something similar since there's a normal distribution!)
train.df <- steam_reviews.df[1:((3/4)*(nrow(steam_reviews.df))),]
test.df <- steam_reviews.df[((3/4)*(nrow(steam_reviews.df))+1):(nrow(steam_reviews.df)),]

# =================== Regression Trees ==============

library(rpart)
steam.rpart <- rpart(weighted_vote_score ~ ., train.df)

predictions.rpart <- predict(steam.rpart, test.df)
cor(predictions.rpart, test.df$weighted_vote_score)

predictions_PROB.rpart <- predict(steam.rpart, test.df, type = "vector")
#hist(aoe3_pred_boost_PREDICTED_PROB[,1], main="Prediction Probabilities of < Median Playtime")
#hist(aoe3_pred_boost_PREDICTED_PROB[,2], main="Prediction Probabilities of >= Median Playtime")
hist(predictions_PROB.rpart, main="Prediction Probabilities of Review Weighted Vote Score")

MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}
MAE(predictions.rpart, test.df$weighted_vote_score) 


# =================== Model Trees ==============

library(Cubist)
model <- cubist(x=train.df[-1], y=train.df$weighted_vote_score)
predictions.Cubist <- predict(model, test.df)

cor(predictions.Cubist, test.df$weighted_vote_score)
MAE(predictions.Cubist, test.df$weighted_vote_score)
predictions_PROB.Cubist <- predict(model, test.df, type = "vector")
hist(predictions_PROB.Cubist, main="Prediction Probabilities of Review Weighted Vote Score")

