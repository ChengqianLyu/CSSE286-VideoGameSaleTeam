# preprocess data
vgsales <- read.csv("vgsales.csv")
vgsales <- vgsales[-c(1:2,4,11)]
str(vgsales)

vgsales <- vgsales[vgsales$Publisher == "Ubisoft" | vgsales$Publisher == "Zoo Digital Publishing" | 
                                     vgsales$Publisher == "Warner Bros. Interactive Entertainment"|
                                     vgsales$Publisher == "Vivendi Games" |
                                     vgsales$Publisher == "Virgin Interactive" |
                                     vgsales$Publisher == "THQ" |
                                     vgsales$Publisher == "Tecmo Koei" |
                                     vgsales$Publisher == "Take-Two Interactive" |
                                     vgsales$Publisher == "Square Enix" |
                                     vgsales$Publisher == "SquareSoft" |
                                     vgsales$Publisher == "Sony Computer Entertainment" |
                                     vgsales$Publisher == "Sega" |
                                     vgsales$Publisher == "Rising Star Games" |
                                     vgsales$Publisher == "Nippon Ichi Software" |
                                     vgsales$Publisher == "Namco Bandai Games", ]

vgsales$Platform <- as.factor(vgsales$Platform)
vgsales$Platform <- as.numeric(vgsales$Platform)
vgsales$Genre <- as.factor(vgsales$Genre)
vgsales$Genre <- as.numeric(vgsales$Genre)
vgsales$Publisher <- as.factor(vgsales$Publisher)



set.seed(12345)
vgsales <- vgsales[sample(nrow(vgsales)),]
str(vgsales)

#set train and test sets
vgsales_train <- vgsales[1:4000,]
vgsales_test <- vgsales[4001:5000,]

#train the model
library(kernlab)
vgsales_classfier <- ksvm(Publisher ~ ., data = vgsales_train, kernel = "vanilladot")
vgsales_classfier

#eval the model
vgsales_predictions <- predict(vgsales_classfier,vgsales_test)
head(vgsales_predictions)
table(vgsales_predictions,vgsales_test$Publisher)
agreement <- vgsales_predictions == vgsales_test$Publisher
prop.table(table(agreement))

#improve the model with rbf
vgsales_classfier_rbf <- ksvm(Publisher ~ ., data = vgsales_train, kernel = "rbfdot")
vgsales_predictions_rbf <- predict(vgsales_classfier_rbf,vgsales_test)
head(vgsales_predictions_rbf)
table(vgsales_predictions_rbf,vgsales_test$Publisher)
agreement_rbf <- vgsales_predictions_rbf == vgsales_test$Publisher
prop.table(table(agreement_rbf))

#improve the model with cost parameter
cost_value <- c(1, seq(from = 5, to = 40, by = 5))
accuracy_values <- sapply(cost_value, function(x){
                          m <- ksvm(Publisher ~ ., data = vgsales_train, kernel = "rbfdot", C =x)
                          pred <- predict(m, vgsales_test)
                          agree <- ifelse(pred == vgsales_test$Publisher, 1 ,0)
                          accuracy <- sum(agree) / nrow(vgsales_test)
                          return(accuracy)
})
plot(cost_value, accuracy_values, type = "b")
