# siliconninja, Timothy Ren, Nick Lyu
# Module 8

# Data source: https://www.kaggle.com/tamber/steam-video-games

# 1. Pre-processing

# For the purposes of making this easier, genre does not need to be a factor;
# the market basket analysis's sparse matrix will ignore factors anyway.

gameSales <- read.csv("steam-200k.csv", stringsAsFactors = FALSE, quote="\"")
gameSales <- gameSales[c("userid", "gamename", "purchase")]

gameSales <- subset(gameSales, purchase=="purchase")

print(paste("Analyzing review set ", name))
# https://stackoverflow.com/questions/26946956/r-convert-transaction-format-dataset-to-basket-format-for-market-basket-analysis

gameSales <-  gameSales[c("userid", "gamename")]
write.csv(gameSales, "blah.csv")
# remove row names and column names in excel

transData <- read.transactions("blah.csv", format="single", sep=",", cols=c("userid", "gamename"))

image(transData[1:1000])
summary(transData)

library(arules)
itemFrequencyPlot(transData, support=0.05)
gameRules <- apriori(transData, parameter = list(support = 0.01, confidence = 0.25, minlen = 2))
summary(gameRules)

inspect(sort(gameRules, by = "lift")[1:5])

dotaRules <- subset(gameRules, lhs %in% "Dota 2")
inspect(sort(dotaRules,by="lift"))

unturnedRules <- subset(gameRules, lhs %in% "Unturned")
inspect(sort(unturnedRules,by="lift"))

lfdRules <- subset(gameRules, lhs %in% "Left 4 Dead 2")
inspect(sort(lfdRules,by="lift"))

gtaRules <- subset(gameRules, lhs %in% "Grand Theft Auto IV")
inspect(sort(gtaRules,by="lift"))