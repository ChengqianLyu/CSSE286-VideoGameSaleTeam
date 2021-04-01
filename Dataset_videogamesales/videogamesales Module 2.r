vgsales <- read.csv("vgsales.csv", stringsAsFactors = FALSE)
str(vgsales)
vgsales <- vgsales[-1]
str(vgsales)
vgsales <- vgsales[-1]
str(vgsales)
vgsales <- DropNA(vgsales)
table(vgsales$Genre)
vgsales$Genre <- factor(vgsales$Genre, levels = c("Action", "Adventure", "Fighting", "Misc", "Platform", "Puzzle", "Racing", "Role-Playing", "Shooter", "Simulation", "Sports", "Strategy"), 
                        labels = c("Action", "Adventure", "Fighting", "Misc", "Platform", "Puzzle", "Racing", "Role-Playing", "Shooter", "Simulation", "Sports", "Strategy"))
round(prop.table(table(vgsales$Genre)) * 100, digits = 1)
summary(wbcd[c("NA_Sales", "EU_Sales", "JP_Sales","Other_Sales", "Global_Sales")])
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
vgsales <- vgsales[-c(1,2,4,8,9)]
str(vgsales)
vgsales_n <-as.data.frame(lapply(vgsales[2:4], normalize))
summary(vgsales_n$NA_Sales)
vgsales_train <- vgsales_n[1:15500, ]
vgsales_test <- vgsales_n[15600:16500, ]
vgsales_train_labels <- vgsales[1:15500, 1]
vgsales_test_labels <- vgsales[15600:16500, 1]
library(class)
vgsales_test_pred <- knn(train = vgsales_train, test = vgsales_test, cl = vgsales_train_labels, k =124)
CrossTable(x = vgsales_test_labels, y = vgsales_test_pred, prop.chisq = FALSE)
table(vgsales_test_pred == vgsales_test_labels)
