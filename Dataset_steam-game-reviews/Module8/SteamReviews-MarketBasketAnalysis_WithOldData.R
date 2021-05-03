# siliconninja, Timothy Ren, Nick Lyu
# Module 8

# OLD dataset (only covers 2 months' worth of data): https://www.kaggle.com/luthfim/steam-reviews-dataset
# and https://www.kaggle.com/nikdavis/steam-store-raw?select=steamspy_data.csv (still used; cant find a better one with genres)

# 1. Pre-processing

# read all of the game CSVs (2GB)
setwd("data")
# we only care whether the user reviewed a certain game, and the game's genres
result <- data.frame("author.steamid" = c(""), "genre" = c(""))
# this syntax makes one blank row so remove that row
result <- result[-c(1),]

# For the purposes of making this easier, genre does not need to be a factor;
# the market basket analysis's sparse matrix will ignore factors anyway.
genres <- read.csv("steamspy_data.csv", stringsAsFactors = FALSE, quote="\"")
genres <- genres[c("appid", "genre")]

for (name in list.files()){
  temp <- read.csv(name, stringsAsFactors = FALSE)
  
  # get stuff before _ to get the steam appID
  # https://stackoverflow.com/a/38291800
  steam_appID <- strsplit(name, "_")[[1]][1]
  # join this with the steam spy data
  genre <- subset(genres, appid == as.numeric(steam_appID), select=c("appid", "genre"))$genre[1]
  # replace all commas in genre with a mishmash like "ActionAdventure" so as not to confuse the csv reader
  genre <- gsub(",", "", genre)
  print(paste("Analyzing game ", name, " with genre ", genre))
  # ignore null genres (the data without null genres is a lot of data anyway)
  # TODO remove arbitrary restrictions; only for testing whether there are multiple genres per user
  if(!is.na(genre) && (name != "105600_Terraria.csv" && nrow(temp) >= 10000)) {
    # https://campus.datacamp.com/courses/intermediate-r-for-finance/loops-3?ex=10
    # https://stackoverflow.com/questions/28467068/how-can-a-add-a-row-to-a-data-frame-in-r/
    
    # TODO replace arbitrary 10K with nrow(temp)
    for(row in 1:10000) {
      # Easier way to analyze it: the transaction a user did is to review a game of X genre, Y genre, Z genre, etc.
      # 1 row per user ID at max. 1 user ID per "shopping cart".
      steamID <- temp[row,]$author.steamid
      if(nrow(subset(result, author.steamid == steamID)) > 0) {
        # apriori doesn't like duplicate genres
        genreIsIn <- 0
        numCol <- ncol(result[result$author.steamid == steamID,])
        for(col in 1:numCol) {
          if(result[result$author.steamid == steamID,col] == genre) {
            genreIsIn <- 1
            #print(paste("[i] DUPLICATE GENRES WITH THE SAME USER DETECTED. The user's genres are ",result[result$author.steamid == steamID,2:numCol]))
          }
        }
        if(!genreIsIn) {
          result[result$author.steamid == steamID,ncol(result$author.steamid == steamID)+1] <- genre
        }
      }
      # append new row with genre otherwise
      else {
        result[nrow(result)+1,] <- list(steamID, genre)
      }
    }
  }
  else {
    print("[i] Ignoring game with NA genre.")
  }
}

# so we don't have to re-run pre-processing in case something fails during execution
# of the algorithm
write.csv(result, "reviews_market_basket_data.csv")

