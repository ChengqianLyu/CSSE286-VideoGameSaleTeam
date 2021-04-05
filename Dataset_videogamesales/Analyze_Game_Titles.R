# siliconninja and Nick Lyu
vg_sales_info <- read.csv("vgsales.csv", stringsAsFactors = FALSE)

# https://stackoverflow.com/a/14521861
vg_sales_info <- as.data.frame(lapply(vg_sales_info, unlist))

# 33%, 66%, 100%
sales_categories <- quantile(vg_sales_info$Global_Sales, c(.33, .66, 1))

# https://stackoverflow.com/a/28442296
sales_categories <- unname(sales_categories)

categorizeSale <- function(saleAmount) {
  ifelse(saleAmount < sales_categories[1], "<33%",
         ifelse(saleAmount < sales_categories[2], "<66%", "<=100%"))
}

# we don't want a list so don't use lapply
vg_sales_info$Sales_Category <- sapply(vg_sales_info$Global_Sales,
                                       categorizeSale)
# convert to factor (Lantz p105)
vg_sales_info$Sales_Category <- factor(vg_sales_info$Sales_Category)

# randomize data order
vg_sales_info <- vg_sales_info[sample(nrow(vg_sales_info)),]

library(tm)

# filter out platform name like Wii, Kinect, different games can be on a 
# similar platform (this isn't the most useful, as most successful ones
# are Wii and Xbox Kinect usually...)

vg_names_corpus <- VCorpus(VectorSource(vg_sales_info$Name))

# tm_map filters
vg_names_corpus <- tm_map(vg_names_corpus, content_transformer(tolower))
vg_names_corpus <- tm_map(vg_names_corpus, content_transformer(removePunctuation))

# lantz p.110
# everything is in lowercase (ex. line 788: Project Gotham Racing (JP weekly sales))
# note, this doesn't work for games with multiple parentheses (or parens after parens) but there are none
# http://uc-r.github.io/regex
removeArtifacts <- function(x) {
  gsub("\\(jp+\\)", " ", x)
  gsub("\\(us+\\)", " ", x)
  gsub("\\(others+\\)", " ", x)
  gsub("\\(weekly+\\)", " ", x)
  gsub("\\(all region+\\)", " ", x)
}

removePlatforms <- function(x) {
  gsub("\\(ds+\\)", " ", x)
  gsub("\\(3ds+\\)", " ", x)
  gsub("\\(wii+\\)", " ", x)
  gsub("\\(wii u+\\)", " ", x) 
  gsub("\\(kinect+\\)", " ", x)
  gsub("\\(xbox+\\)", " ", x)
  gsub("ds+", " ", x)
  gsub("3ds+", " ", x)
  gsub("wii+", " ", x) # does Not match wii u since + stops with spaces
  gsub("wii u+", " ", x) # does Not match wii u since + stops with spaces
  gsub("kinect+", " ", x)
  gsub("xbox+", " ", x)
  
}

# regex leaves us with some whitespace left over
vg_names_corpus <- tm_map(vg_names_corpus, content_transformer(stripWhitespace))

library(SnowballC)

# TODO: make wordStem do -er endings as well.
#> wordStem(c("fighting", "fighter"))
# [1] "fight"   "fighter"

vg_names_corpus <- tm_map(vg_names_corpus, stemDocument)

vg_names_corpus_clean <- tm_map(vg_names_corpus, removeNumbers)
vg_names_corpus_clean <- tm_map(vg_names_corpus_clean, removeWords, stopwords())
vg_names_corpus_clean <- tm_map(vg_names_corpus_clean, removePunctuation)

#splitting text into words
vg_names_dtm <- DocumentTermMatrix(vg_names_corpus_clean)

#create train and test sets
vg_names_train <- vg_names_dtm[1:11618, ]
vg_names_test <- vg_names_dtm[11619:16598, ]

vg_names_train_labels <- vg_sales_info[1:11618, ]$Sales_Category
vg_names_test_labels <- vg_sales_info[11619:16598, ]$Sales_Category

prop.table(table(vg_names_train_labels))
prop.table(table(vg_names_test_labels))

#visualizing text data
library(wordcloud)
wordcloud(vg_names_corpus_clean, min.freq = 50, random.order = FALSE)

vg_top30 <- subset(vg_sales_info, vg_sales_info$Sales_Category == "<33%")

vg_top30_corpus <- VCorpus(VectorSource(vg_top30$Name))
vg_top30_corpus <- tm_map(vg_top30_corpus, stemDocument)
vg_top30_corpus <- tm_map(vg_top30_corpus, content_transformer(tolower))
vg_top30_corpus <- tm_map(vg_top30_corpus, content_transformer(removePunctuation))
vg_top30_corpus_clean <- tm_map(vg_top30_corpus, removeNumbers)
vg_top30_corpus_clean <- tm_map(vg_top30_corpus_clean, removeWords, stopwords())
vg_top30_corpus_clean <- tm_map(vg_top30_corpus_clean, removePunctuation)

wordcloud(vg_top30_corpus_clean, min.freq = 50, random.order = FALSE)

#create indicator features for freq words

vg_names_freq_words <- findFreqTerms(vg_names_train, 10)
vg_names_freq_words_train <- vg_names_train[ , vg_names_freq_words]
vg_names_freq_words_test <- vg_names_test[ , vg_names_freq_words]

convert_counts <- function(x){
  x <- ifelse(x>0, "Yes", "No")
}

vg_train <- apply(vg_names_freq_words_train, MARGIN = 2, convert_counts)
vg_test <- apply(vg_names_freq_words_test, MARGIN = 2, convert_counts)

#train the model
library(e1071)
vg_classifier <- naiveBayes(vg_train, vg_names_train_labels)

#evaluate the performance
vg_names_pred <- predict(vg_classifier, vg_test)

library(gmodels)
CrossTable(vg_names_pred, vg_names_test_labels, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))

#improve the performance
vg_classifier2 <- naiveBayes(vg_train, vg_names_train_labels, laplace = 2)
vg_names_pred2 <- predict(vg_classifier2, vg_test)
CrossTable(vg_names_pred2, vg_names_test_labels, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))