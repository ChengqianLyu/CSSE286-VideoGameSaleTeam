# siliconninja
# Module 5 - Helpfulness Decision Rules
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

# Using OneR
# Recall, . uses all the features currently in the dataframe, but we filtered out
# useless ones like recommendationid. OneR uses all columns including ID unless
# specific ones are excluded or removed!
library(OneR)
aoe3_1R <- OneR(playtime_above_median ~ ., data = aoe3_data)

aoe3_1R_pred <- predict(aoe3_1R, aoe3_data)

table(
  actual = aoe3_data$playtime_above_median,
  predicted = aoe3_1R_pred
)

# Using JRip
library(RWeka)

aoe3_JRip <- JRip(playtime_above_median ~ ., data = aoe3_data)
# Display the rules
aoe3_JRip