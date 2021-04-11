# siliconninja
# Module 5 - Helpfulness Decision Tree
aoe3_data <- read.csv("105450_AgeofEmpiresIII2007.csv", stringsAsFactors = TRUE)

# Remove any columns that have NAs for playtime - why bother analyzing
# data with no classification?
# https://stackoverflow.com/a/28496107
aoe3_data <- subset(aoe3_data, !is.na(author.playtime_at_review))

# Prepare the above-median and below-median playtime column
# Median rather than mean because there are some people with 500k hours
# probably trying to game the review system
aoe3_playtime_median <- median(aoe3_data$author.playtime_at_review, na.rm = TRUE)

categorizeReview <- function(playtime) {
  ifelse(playtime < aoe3_playtime_median,
         #"At or Above Median Playtime",
          #"Below Median Playtime"
         # shorter titles make the plot neater. Mdn = median.
         # I had to resize this to fit ~30% of my whole RStudio window to make it
         # readable.
         ">= Mdn PT",
         "< Mdn PT"
  )
}

aoe3_data$playtime_above_median <- sapply(
  aoe3_data$author.playtime_at_review, 
  categorizeReview
)

# C5.0 prefers factors for the column (feature) we want to predict.
aoe3_data$playtime_above_median <- as.factor(aoe3_data$playtime_above_median)

# Drop any not so useful columns (like time the review was published, that seems
# unlikely to have much  to do with helpfulness, for example?) for the purposes
# of getting a decision tree that's slimmer (but doesn't have to necessarily
# make the most sense, as you learned about in the slide guide)

# NOTE-1: I dropped language because all reviews were in English, and steam_purchase/
# written_during_early_access because all values were true and false
# for those individual factors, respectively.
# The output of the below command verifies this.

str(aoe3_data)

# Remember also that we want to drop IDs!!!

# https://www.listendata.com/2015/06/r-keep-drop-columns-from-data-frame.html
aoe3_data <- subset(aoe3_data, select = -c(X, recommendationid,
                                          review, timestamp_created,
                                          timestamp_updated, weighted_vote_score,
                                          author.steamid, author.last_played,
                                          votes_up, votes_funny,
                                          author.playtime_at_review, language,
                                          steam_purchase, written_during_early_access))
# NOTE: we should not use votes_up, because that would make it too easy, and
# generate a useless decision branch like
# (votes_up > 5) => helpfulness_above_median - "Above Median Helpful Votes".
# Because we already know this and we want to find _new_ decision branches.

RNGversion("3.5.2")
# You can use this if you want to reproduce the same results.
# To try a different sample, change the number or comment this out
set.seed(7390)

# Lantz used 90% so I might as well stick with that
train_proportion <- (0.9)*nrow(aoe3_data)
test_proportion <- nrow(aoe3_data) - train_proportion

train_sample <- sample(nrow(aoe3_data), train_proportion)

aoe3_train <- aoe3_data[train_sample,]
aoe3_test <- aoe3_data[-train_sample,]

library(C50)

# I think this syntax makes what we're doing a bit clearer,
# but it does involve extra data frames floating around
# after the C5.0 model is run.
aoe3_train_without_label <- subset(aoe3_train, select = -c(playtime_above_median))

# NOTE-2: apparently for me, removing the "redundant" factors specified in
# NOTE-1 fixed this error (that prevented making a decision tree) when I run the algorithm:
# "c50 code called exit with value 1"
# This StackOverflow post helped me find that this "redundant" factors
# were in fact causing an issue with C5.0 in R: https://stackoverflow.com/a/36992047
aoe3_model <- C5.0(x = aoe3_train_without_label, y = aoe3_train$playtime_above_median)
#aoe3_model <- C5.0(x = aoe3_train[-11], y = aoe3_train$playtime_above_median)
aoe3_model 

# R doesn't like printing the decision tree out with the above command, but apparently plot works
# https://stats.stackexchange.com/a/198613
plot(aoe3_model)

# Get some subtrees
library(partykit)
aoe3_model_with_subtrees <- C50::as.party.C5.0(aoe3_model)

# Get a crosstable
aoe3_pred <- predict(aoe3_model, aoe3_test)
library(gmodels)

CrossTable(aoe3_test$playtime_above_median, aoe3_pred,
            prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Median Playtime Category', 'Predicted Median Playtime Category'))

# Improvement 1: Boosting
aoe3_model_boost10 <- C5.0(x = aoe3_train_without_label, y = aoe3_train$playtime_above_median,
                           trials = 10)
aoe3_pred_boost10 <- predict(aoe3_model_boost10, aoe3_test)

CrossTable(aoe3_test$playtime_above_median, aoe3_pred_boost10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Median Playtime Category', 'Predicted Median Playtime Category'))

# Improvement 2: Cost Matrix Penalty

# NOTE-3: For some reason the labels of the matrix must match the actual data values.
# (Not just yes/no, which is what Lantz did)
# Or else C50 will complain by saying:
# "c50 code called exit with value 1"
matrix_dimensions <- list(c("< Mdn PT", ">= Mdn PT"), c("< Mdn PT", ">= Mdn PT"))

# These don't matter to C50 though, but maybe it does matter in some cases.
# predicted => Predicted doesn't make it complain, thankfully.
names(matrix_dimensions) <- c("predicted", "actual")
# Why not try 2 to slightly lessen the effects on accuracy
# of a severe penalty that Lantz got?
error_cost <- matrix(c(0,1,2,0), nrow=2, dimnames=matrix_dimensions)
# To verify it looks right
error_cost

aoe3_model_cp <- C5.0(x = aoe3_train_without_label, y = aoe3_train$playtime_above_median,
                           costs = error_cost)
aoe3_pred_cp <- predict(aoe3_model_cp, aoe3_test)

CrossTable(aoe3_test$playtime_above_median, aoe3_pred_cp,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Median Playtime Category', 'Predicted Median Playtime Category'))
