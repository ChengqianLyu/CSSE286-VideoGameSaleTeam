# CSSE286 ML Game Sales Project
# Module 3 - k-NN Steam Review Algorithm Prediction

library(dplyr)
setwd("data")
columns.name <- c("<10hrsAvg", "10-50hrsAvg", "50-100hrsAvg", ">100hrsAvg", "WeightedVoteScoreAvg") 
result <- matrix(nrow = length(list.files()), ncol = 5)
colnames(result) <- columns.name
rownames(result) <- list.files()
counter <- 1
for (name in list.files()){
  temp <- read.csv(name, stringsAsFactors = FALSE)
  temp <- temp %>% mutate(playtimeCategory = case_when(author.playtime_at_review <= 10 ~ '<10hrs',
                                                       author.playtime_at_review <= 50 ~ '10-50hrs',
                                                       author.playtime_at_review <= 100 ~ '50-100hrs',
                                                       TRUE ~ '>100hrs'))
  result[counter, 1] <- mean(temp$votes_up[temp$playtimeCategory=='<10hrs'])
  result[counter, 2] <- mean(temp$votes_up[temp$playtimeCategory=='10-50hrs'])
  result[counter, 3] <- mean(temp$votes_up[temp$playtimeCategory=='50-100hrs'])
  result[counter, 4] <- mean(temp$votes_up[temp$playtimeCategory=='>100hrs'])
  result[counter, 5] <- mean(temp$weighted_vote_score)
  counter <- counter + 1
}

result[is.nan(result)] <- 0
median <- median(result[,5])
result.df <- as.data.frame(result)
result.df <- result.df %>% mutate(HelpfulComments = if_else(WeightedVoteScoreAvg >= 0.1697936, TRUE, FALSE))

result.df <- result.df[sample(nrow(result.df)),]

train.df <- result.df[1:(192*(2/3)),c(-5)] 
test.df <- result.df[(192*(2/3)):192,c(-5)]

train_label<- result.df[1:(192*(2/3)),6]
test_label<- result.df[(192*(2/3)):192,6]

library(class)

pred.df <- knn(train = train.df, test = test.df, cl = train_label, k = floor(sqrt(length(list.files()))))

# build the CrossTable

library(gmodels)

CrossTable(x=test_label, y=pred.df, prop.chisq = FALSE)