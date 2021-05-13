reviews.df <- read.csv("steam_reviews.csv")

##reviews.df <- subset(reviews.df, select = -c(review))

reviews.df$min_played <- reviews.df$hour_played
reviews.df<- subset(reviews.df, select = -c(hour_played))
reviews.df <- reviews.df[, c(1, 2, 3, 4, 5, 8, 6, 7)]

reviews.df$is_early_access_review <- ifelse(reviews.df$is_early_access_review == "False", 0, 1)

reviews.df$recommendation <- ifelse(reviews.df$recommendation == "Recommended", 1, 0)

interest <- reviews.df[2:6]

interest_z <- as.data.frame(lapply(interest, scale))

RNGversion("3.5.2")
set.seed(1337)

reviews_clusters <- kmeans(interest_z, 6)
reviews_clusters$size

reviews_clusters$centers

reviews.df$clusters <- reviews_clusters$cluster
aggregate(data = reviews.df, min_played ~ clusters, mean)

reviews_cluster4 <- subset(reviews.df, clusters == 4)
reviews_cluster4

interest2 <- reviews.df[2:3]
# add on the min_played column separately since it's in reviews.df[6]
interest2$min_played <- reviews.df$min_played
interest2_z <- as.data.frame(lapply(interest2, scale))

RNGversion("3.5.2")
set.seed(1337)
reviews_clusters2 <- kmeans(interest2_z, 4)
reviews_clusters2$size
reviews_clusters2$centers

reviews.df$clusters <- reviews_clusters2$cluster
aggregate(data = reviews.df, min_played ~ clusters, mean)
