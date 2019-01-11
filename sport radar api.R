


library(jsonlite)

library(tidyverse)




#FUNCTION TO UNNEST THE STATS VARIABLE IN EACH PERIOD
unlist_func <- function(period){
  
  library(jsonlite)
  
  library(tidyverse)
  

  cleandata <- period
  
  #print(cleandata)
  
  
  stats <- cleandata$statistics
  
  
  ##need to unnest stats list, make a dataframe, and cbind it to period1##
  #use the View function to get an idea of what these things are
  
  end <- nrow(cleandata)
  
  
  
  for(index in 1:end){ 
    if(is.null(stats[[index]])){
      
    }else{
      stats[[index]][["id"]] = cleandata$id[index]
    }
  }
  
  ## First Problem ##
  # Problem is that "stats" is a list that contains dataframes with dataframes in it.
  # `do.call(data.frame, [list in stats])` below fixes it by breaking up second-level dataframes
  # so that "fixedStats" is a list that only contains dataframes with variables
  fixedStats = list()
  index = 1
  for(list in stats){
    if(is.null(list)){
      fixedStats[[index]] = NULL
    }else{
      fixedStats[[index]] = do.call(data.frame, list) 
    }
    index = index + 1
  }
  
  library(plyr)
  # Testing that `bind_rows` will work
  row2 = fixedStats[[2]]
  row3 = fixedStats[[3]]
  test = bind_rows(row2, row3)
  
  # Using `bind_rows` to combine everything in fixedStats
  fixedStatsFINAL = do.call(bind_rows, fixedStats)
  #print(fixedStatsFINAL)
  
  
  
  #NEED TO GO BACK AND INVESTIGATE ON_COURT DATAFRAME#
  drop <- c("attribution", "location", "possession", "on_court", "duration")
  fixedCleanData <- cleandata %>% dplyr::select(-which(names(cleandata) %in% drop))
  


  ## Third Problem ##
  # There are "variables" in cleandata that are data frames.
  # In order to join we need to fix these to just be variables
  # `do.call(data.frame, cleandata)` didn't work here
  #fixedCleanData = cleandata[ , -c(6:8)]
  fixedCleanData$attribution.name = cleandata$attribution$name
  fixedCleanData$attribution.market = cleandata$attribution$market
  fixedCleanData$attribution.id = cleandata$attribution$id
  fixedCleanData$attribution.team_basket = cleandata$attribution$team_basket
  fixedCleanData$location.coord_x = cleandata$location$coord_x
  fixedCleanData$location.coord_y = cleandata$location$coord_y
  fixedCleanData$possession.name = cleandata$possession$name
  fixedCleanData$possession.market = cleandata$possession$market
  fixedCleanData$possession.id = cleandata$possession$id
  
  
  # Joining the original data with fixed variables and the statistics for each event together
  # Some events have to be listed multiple times in order to include all statistics; could change this
  # Doesn't fit the joel berry example yet; have to add to "mydata"; would also have edit/add
  # a bunch of variables as evidenced by `names(mydata) %in% names(exampleJoelBerry)`
  dataStats = full_join(select(fixedCleanData, -statistics), fixedStatsFINAL, by = "id")
  
  

  
  return(dataStats)
  
}


#FUNCTION THAT TAKES IN A URL, AND SPITS OUT THE GAME (CALLS UNLIST_FUNC)
sportradar <- function(url){
  
  
  library(jsonlite)
  
  library(tidyverse)
  

mydata <- fromJSON(url)


theperiods <- mydata$periods

period1 <- theperiods[1,]
period2 <- theperiods[2,]


period1 <- period1$events[[1]]
# 
# 

period2 <- period2$events[[1]]

firsthalf <- unlist_func(period1)
secondhalf <- unlist_func(period2)


# 
wholegame <- suppressMessages(full_join(firsthalf, secondhalf, by = NULL))

return(wholegame)

}


library(readr)
#setwd("~/analytics/NCAA.SHOT.CHART.APP")

mygames <- read_csv("gameids_2018.csv")


all2018 <- mygames %>% filter(games.coverage == "full")
#& games.status == "closed")
unc_games <- mygames %>% filter(games.home.alias == "UNC" | games.away.alias == "UNC")
all_future_teams <- c(unc_games$games.home.alias, unc_games$games.away.alias)

all_future_teams <- unique(all_future_teams)


all_non_conf <- all_future_teams %in%  c("WOF", "ELON", 
    "MICH", "HARV", 
    "DAV", "UK", 
    "STAN", "TNTC", "SFPA",
    "TEX", "UCLA", "UNCW",
    "GONZ")


all_future_acc <- all_future_teams[which(all_non_conf == FALSE)]



thisyear <- all2018 %>% filter(games.home.alias %in% all_future_acc | games.away.alias %in% all_future_acc)

thisyear <- thisyear %>% filter(games.status == "closed" &  games.track_on_court == 'TRUE')



lastgame <- nrow(thisyear)

lastgame


#NEED TO RUN LINES BELOW FOR ALL GAMES PLAYED FOR FUTURE UNC OPPONENTS##


for(i in 1:lastgame){
  print(i)
  
  thegame <- paste('https://api.sportradar.us/ncaamb/trial/v4/en/games/', thisyear[i, 'games.id'], "/pbp.json?api_key=7tzd7gaqgrctphx9f3kb4uaf", sep = "")
  if(i == 1){
 data2018  <- sportradar(thegame)
  }


  if(i > 1){
 onegame  <- sportradar(thegame)
  data2018 <- suppressMessages(full_join(onegame, data2018))
  }
}

print(lastgame)
write.csv(data2018, file = "sportradar_2018 (2018 unc schedule only).csv", row.names = F)
#gonzaga_url <- 'https://api.sportradar.us/ncaamb/trial/v4/en/games/f456beba-e734-4b09-a813-c215c385670e/pbp.json?api_key=2c5yp34zwqb7eandb3ta7jzg'




# test_url <- paste('https://api.sportradar.us/ncaamb/trial/v4/en/games/', unc[9, 'games.id'], "/pbp.json?api_key=f3xcubgdbsc5f934xztn6vfk", sep = "")
# testgame <- sportradar(test_url)
#thegame <- paste('https://api.sportradar.us/ncaamb/trial/v4/en/games/', all2018[615, 'games.id'], "/pbp.json?api_key=f3xcubgdbsc5f934xztn6vfk", sep = "")




###ONLY PROBLEM 
#IF THERE are two players involved in a play: one row for one player, 
#another row for the other player, same play ID
