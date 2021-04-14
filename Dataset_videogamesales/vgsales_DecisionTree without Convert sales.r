vgsales <- read.csv("vgsales.csv")
table(vgsales$Genre)
set.seed(123)
str(vgsales)
vgsales <- vgsales[-1]
vgsales <- vgsales[-1]
vgsales <- vgsales[-c(4)]
vgsales <- DropNA(vgsales)
train_sample <- sample(16598,14938)
str(train_sample)
vgsales_train <- vgsales[train_sample, ]
vgsales_test <- vgsales[-train_sample, ]
vgsales_test <- vgsales_test[-c(1439), ]
prop.table(table(vgsales$Genre))
prop.table(table(vgsales_train$Genre))
prop.table(table(vgsales_test$Genre))

library(C50)
vgsales_model <- C5.0(vgsales_train[-3], as.factor(vgsales_train$Genre))
vgsales_model
summary(vgsales_model)

vgsales_pred <- predict(vgsales_model, vgsales_test)

library(gmodels)
CrossTable(vgsales_test$Genre, vgsales_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual Genre', 'predicted Genre'))
