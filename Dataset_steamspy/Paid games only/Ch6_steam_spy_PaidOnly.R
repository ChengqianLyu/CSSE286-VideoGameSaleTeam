# siliconninja and Timothy Ren
# Module 6 MODIFICATION - Paid Games Only

steamspy.df <- read.csv("steamspy_data.csv")

# remove irrelevant columns for our correlation
steamspy.df <- steamspy.df[,c("positive", "negative", "initialprice", "average_forever")]
# remove invalid columns (with NAs) for our correlation
steamspy.df <- na.omit(steamspy.df)

# filter out games with 0 (positive AND negative) reviews
# also remove fake games with reviews with 0 minutes of playtime
steamspy.df <- subset(steamspy.df, steamspy.df$positive > 0 & steamspy.df$negative > 0 & steamspy.df$average_forever > 0)

# make new columsn for better analysis (better correlations). percentage of positive reviews ("review rate")/total reviews
# ex. on steam page -> review rate "88% of people recommend this game"
steamspy.df$total_reviews <- steamspy.df$positive + steamspy.df$negative
steamspy.df$review_rate <- steamspy.df$positive / steamspy.df$total_reviews

steamspy.df <- steamspy.df[,c("total_reviews", "review_rate", "initialprice", "average_forever")]

isPaid <- function(price) {
  price > 0
}

# the return value of this function (which would be the return value of the abline call) doesn't matter
displayResults <- function(predictions, dependentVar) {
  plot(predictions, dependentVar)
  abline(a = 0, b = 1, col = "red", lwd = 3, lty = 2)
}

# MODIFICATION: only paid games for better correlations
# this reduces the dataset to 5521 games, down from the ~7000 games.
steamspy.df <- subset(steamspy.df, isPaid(steamspy.df$initialprice))

library(psych)

pairs.panels(steamspy.df)

# =============

# make the linear regression model
steam_model <- lm(initialprice ~ average_forever + review_rate, data = steamspy.df)

summary(steam_model)

steamspy.df$pred <- predict(steam_model, steamspy.df)

displayResults(steamspy.df$pred, steamspy.df$initialprice)

cor(steamspy.df$pred, steamspy.df$initialprice) # 0.282, nice!

# ===========

# remove pred column
steamspy.df <- steamspy.df[,-5]

# squared and interaction effects
steamspy.df$average_forever2 <- steamspy.df$average_forever^2

steam_model2 <- lm(initialprice ~ average_forever*total_reviews + review_rate + average_forever2, data = steamspy.df)

summary(steam_model2)

steamspy.df$pred <- predict(steam_model2, steamspy.df)

displayResults(steamspy.df$pred, steamspy.df$initialprice)

cor(steamspy.df$pred, steamspy.df$initialprice) # 0.364, this is even better!