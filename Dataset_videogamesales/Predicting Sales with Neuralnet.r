# preprocess data
vgsales <- read.csv("vgsales.csv")
str(vgsales)
vgsales <- vgsales[-c(1:2,4:6,11)]
str(vgsales)

vgsales$Platform <- as.factor(vgsales$Platform)
vgsales$Platform <- as.numeric(vgsales$Platform)
set.seed(12345)
vgsales <- vgsales[sample(nrow(vgsales)),]

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
vgsales_norm <- as.data.frame(lapply(vgsales, normalize))
summary(vgsales_norm$JP_Sales)

#set test and train sets
vgsales_train <- vgsales_norm[1:5000,]
vgsales_test <- vgsales_norm[5001:6667,]

# train the model
library(neuralnet)

vgsales_model <- neuralnet(formula = JP_Sales ~ Platform + NA_Sales +
                              EU_Sales + Other_Sales,
                            data = vgsales_train)

# eval the model
plot(vgsales_model)
model_results <- compute(vgsales_model, vgsales_test[1:5])
predicted_JP_Sales <- model_results$net.result
cor(predicted_JP_Sales, vgsales_test$JP_Sales)

# improve the model
vgsales_model2 <- neuralnet(formula = JP_Sales ~ Platform + NA_Sales +
                             EU_Sales + Other_Sales,
                           data = vgsales_train, hidden = 5)
plot(vgsales_model2)
model_results2 <- compute(vgsales_model2, vgsales_test[1:5])
predicted_JP_Sales2 <- model_results2$net.result
cor(predicted_JP_Sales2, vgsales_test$JP_Sales)

# try softplus
softplus <- function(x){ log(1 + exp(x))}
vgsales_model3 <- neuralnet(formula = JP_Sales ~ Platform + NA_Sales +
                              EU_Sales + Other_Sales,
                            data = vgsales_train, hidden = c(5,5),act.fct = softplus)
plot(vgsales_model3)
model_results3 <- compute(vgsales_model3, vgsales_test[1:5])
predicted_JP_Sales3 <- model_results3$net.result
cor(predicted_JP_Sales3, vgsales_test$JP_Sales)
