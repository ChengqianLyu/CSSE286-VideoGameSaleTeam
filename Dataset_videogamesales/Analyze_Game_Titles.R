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

