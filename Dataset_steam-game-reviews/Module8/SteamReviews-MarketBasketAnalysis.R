# siliconninja, Timothy Ren, Nick Lyu
# Module 8

# NEW dataset (covers 3 YEARS' worth of data, 1.49b to 1.62b Unix timestamps): https://www.kaggle.com/luthfim/steam-reviews-dataset
# and https://www.kaggle.com/nikdavis/steam-store-games?select=steam.csv

# 1. Pre-processing

# For the purposes of making this easier, genre does not need to be a factor;
# the market basket analysis's sparse matrix will ignore factors anyway.
setwd("..")
genres <- read.csv("steam.csv", stringsAsFactors = FALSE, quote="\"")
genres <- genres[c("appid", "genres")]

# read all of the game review CSVs (7GB)
setwd("newData")
# we only care whether the user reviewed a certain game, and the game's genres
result <- data.frame("steamid" = c(""), "genre" = c(""))
# this syntax makes one blank row so remove that row
result <- result[-c(1),]

for (name in list.files()){
  temp <- read.csv(name, stringsAsFactors = FALSE)
  
  print(paste("Analyzing review set ", name))
   # https://campus.datacamp.com/courses/intermediate-r-for-finance/loops-3?ex=10
  # https://stackoverflow.com/questions/28467068/how-can-a-add-a-row-to-a-data-frame-in-r/
    
  for(row in 1:nrow(temp)) {
      # join app ID with the steam spy data
      #print(temp[row,]$appid)
    
      genre <- subset(genres, temp[row,]$appid == appid, select=c("genres"))$genres[1]
      # replace all semicolons in genre with a mishmash like "ActionAdventure" so as not to confuse the apriori reader
      genre <- gsub(";", "", genre)
      
      #print(paste("Genre of ", temp[row,]$appid, " is ", genre))
      
      if(!is.na(genre)) {
        # Easier way to analyze it: the transaction a user did is to review a game of X genre, Y genre, Z genre, etc.
        # 1 row per user ID at max. 1 user ID per "shopping cart".
        steamID <- temp[row,]$steamid
        if(nrow(subset(result, steamid == steamID)) > 0) {
          # apriori doesn't like duplicate genres
          genreIsIn <- 0
          numCol <- ncol(result[result$steamid == steamID,])
          for(col in 1:numCol) {
            if(result[result$steamid == steamID,col] == genre) {
              genreIsIn <- 1
              #print(paste("[i] DUPLICATE GENRES WITH THE SAME USER DETECTED. The user's genres are ",result[result$author.steamid == steamID,2:numCol]))
            }
          }
          if(!genreIsIn) {
            result[result$steamid == steamID,ncol(result$steamid == steamID)+1] <- genre
          }
        }
        # append new row with genre otherwise
        else {
          result[nrow(result)+1,] <- list(steamID, genre)
        }
      }
      else {
       # print("[i] Ignoring game with NA genre.")
      }
  }
  
}

# so we don't have to re-run pre-processing in case something fails during execution
# of the algorithm
write.csv(result, "reviews_market_basket_data.csv")

