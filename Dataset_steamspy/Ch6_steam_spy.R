# siliconninja and Timothy Ren
# Module 6

steamspy.df <- read.csv("steamspy_data.csv")

# Make our own correlation line based on 1 variable (Lantz p196) 
# Predict based on positive upvotes (independent) => average_forever (dependent)
b <- cov(steamspy.df$positive, steamspy.df$average_forever) / var(steamspy.df$positive)

a <- mean(steamspy.df$average_forever) - b*mean(steamspy.df$positive)
# y = 133.4 + 0.015x

# r = 0.14 (slight positive)
r <- cov(steamspy.df$positive, steamspy.df$average_forever) / (sd(steamspy.df$average_forever) * sd(steamspy.df$positive))


# remove irrelevant columns for our correlation
steamspy.df <- steamspy.df[,c("positive", "negative", "initialprice", "average_forever")]
# remove invalid columns (with NAs) for our correlation
steamspy.df <- na.omit(steamspy.df)

# filter out games with 0 (positive AND negative) reviews
# also remove fake reviews with 0 minutes of playtime
steamspy.df <- subset(steamspy.df, steamspy.df$positive > 0 & steamspy.df$negative > 0 & steamspy.df$average_forever > 0)
#steamspy.df <- subset(steamspy.df, steamspy.df$positive ==1)


# make new columsn for better analysis (better correlations). percentage of positive reviews ("review rate")/total reviews
# ex. on steam page -> review rate "88% of people recommend this game"
steamspy.df$total_reviews <- steamspy.df$positive + steamspy.df$negative
steamspy.df$review_rate <- steamspy.df$positive / steamspy.df$total_reviews

steamspy.df <- steamspy.df[,c("total_reviews", "review_rate", "initialprice", "average_forever")]


reg <- function(y, x) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  b <- solve(t(x) %*% x) %*% t(x) %*% y
  colnames(b) <- "estimate"
  print(b)
}

# predict playtime from postive and negative and initialprice (no steam sales being weird)
x_factors.m <- steamspy.df[,c("positive","negative","initialprice")]
reg(y=steamspy.df$average_forever, x=x_factors.m)

#                  estimate
# Intercept    93.446839234
# positive      0.008100054
# negative      0.040945007
# initialprice  0.047921059

# get matrix
cor(steamspy.df)

library(psych)

pairs.panels(steamspy.df)

# make the linear regression model - this has 0.07 which is best multivariable correlation
steam_model <- lm(initialprice ~ average_forever + review_rate, data = steamspy.df)

# squared and interaction effects
steamspy.df$average_forever2 <- steamspy.df$average_forever^2

steam_model2 <- lm(initialprice ~ average_forever*total_reviews + review_rate + average_forever2, data = steamspy.df)

steamspy.df$pred <- predict(steam_model2, steamspy.df)
cor(steamspy.df$pred, steamspy.df$initialprice)

plot(steamspy.df$pred, steamspy.df$initialprice)
abline(a = 0, b = 1, col = "red", lwd = 3, lty = 2)

steamspy.df <- steamspy.df[,c("total_reviews", "review_rate", "initialprice", "average_forever")]

# predict review_rate (that's what the book did with the distribution!)
steamspy_train.df <- steamspy.df[1:((3/4)*(nrow(steamspy.df))),]
steamspy_test.df <- steamspy.df[((3/4)*(nrow(steamspy.df))+1):(nrow(steamspy.df)),]

library(rpart)
steamspy_m.rpart <- rpart(review_rate ~ ., steamspy_train.df)

library(rpart.plot)
rpart.plot(steamspy_m.rpart, digits=3)

predictions.rpart <- predict(steamspy_m.rpart, steamspy_test.df)
cor(predictions.rpart, steamspy_test.df$initialprice) # 0.58

MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}
MAE(predictions.rpart, steamspy_test.df$initialprice) # predicts by $8 (867 cents) off on average

library(Cubist)
model <- cubist(x=steamspy_train.df[-3], y=steamspy_train.df$initialprice)
predictions.Cubist <- predict(model, steamspy_test.df)

cor(predictions.Cubist, steamspy_test.df$initialprice)
MAE(steamspy_test.df$initialprice, predictions.Cubist) # $7.10 off! Even better!

