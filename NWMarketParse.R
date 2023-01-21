##Library Declarations
library(jsonlite)
library(dplyr)
##Global Variable Declarations
urlStructure <- "https://nwmarketprices.com/api/latest-prices/"
defaultFilePath <- "C:\\Users\\44jmn\\OneDrive\\Documents\\NWMAT\\"
highInterestItems <- c("Iron Ore", "Iron Ingot", "Steel Ingot", "Starmetal Ore", "Starmetal Ingot", "Orichalcum Ore", "Orichalcum Ingot", "Green Wood", "Timber", "Aged Wood", "Lumber", "Wyrdwood", "Wyrdwood Planks", "Ironwood", "Ironwood Planks", "Fibers", "Linen", "Sateen", "Silk Threads", "Silk", "Wirefiber", "Infused Silk", "Rawhide", "Coarse Leather", "Thick Hide", "Rugged Leather", "Layered Leather", "Iron Hide", "Infused Leather", "Stone", "Stone Block", "Stone Brick", "Lodestone", "Lodestone Brick", "Obsidian Voidstone", "Obsidian Sandpaper", "Obsidian Flux", "Aged Tannin", "Pure Solvent", "Wireweave", "Life Mote", "Death Mote", "Water Mote", "Earth Mote", "Air Mote", "Soul Mote", "Fire Mote", "Runic Leather", "Glittering Ebony", "Phoenixweave", "Asmodeum")
activeDF <- data.frame()

#Populates all market data as data frames titled by Server Name. Used for establishing global variables, will not update already existing data tables.
#Args: N/A
populateMarketData <- function(){
  serverIDCross <- fromJSON("https://nwmarketprices.com/api/servers_updated")
  serverIDCrossClean <<- serverIDCross[["server_last_updated"]]
  #R, I do care that my columns have names.
  colnames(serverIDCrossClean) <- c("ServerID","ServerName","LastUpdate")
  #R, I don't care that my rows don't have names.
  rownames(serverIDCrossClean, do.NULL = FALSE)
  #Loop through server ID matrix. Prevents statically assigning servers variables so I don't have to update this every time a merge occurs.
  for (i in 1:nrow(serverIDCrossClean)){
    #Print current loop value for posterity's sake.
    print(paste(serverIDCrossClean[i,1],serverIDCrossClean[i,2]))
    #Manage population of already existent variables.
    if(exists(serverIDCrossClean[i,2])){
      print("Server data already populated!")}
    else{
      assign(serverIDCrossClean[i,2],fromJSON(paste(urlStructure, serverIDCrossClean[i,1], sep = "")), envir = .GlobalEnv)
      #Rate limiting helper so I don't get 403'd.
     Sys.sleep(15)}
  }
}

#Allows specific cross reference of item in server
#Args: String queryItemName, String queryServerName
filterByItemAndServer <- function(queryItemName, queryServerName){
  filter (queryServerName, ItemName == queryItemName)
}

#Returns a data frame object of the targeted string for item across servers.
#Args: String queryItemName
#TODO: Error handling and validation. Probably need to reference a matrix of items to validate-- but that sounds slow.
filterByItem <- function(queryItemName){
  returnDf <- data.frame()
  for (i in 1:nrow(serverIDCrossClean)){
    returnDf <- rbind(returnDf,get(serverIDCrossClean[i,2], envir = .GlobalEnv) %>% filter(ItemName == queryItemName))
  }
  returnDf <- cbind(returnDf,serverIDCrossClean[,2])
  returnDf <- returnDf[, c(8,2,3,4,1,5,6,7)]
  colnames(returnDf)[1] = "ServerName"
  return(returnDf)
}

#Moves a returned data frame object into the global environment for manipulation. Probably bandaid for poor variable scoping.
#Args: Data.Frame PassiveFrame
setActiveFrame <- function(passiveFrame){
  activeDF <<- passiveFrame
  return(activeDF)
}

#Converts active data frame into CSV with title of type string in arguments.
#Args:String docTitle
activeToCSV <- function(docTitle){
  print(paste(defaultFilePath,docTitle,".csv", sep = ""))
  tibble::glimpse(activeDF)
  write.csv(activeDF, paste(defaultFilePath, docTitle,".csv", sep = ""), row.names = FALSE)
}

#Polls targeted data in vector "highInterestItems", exports as CSV, saves at "defaultFilePath" with title "<item>Query".
#Args: N/A
pollHighInterest <- function(){
  for(i in 1:length(highInterestItems)){
    pollHelpString <- highInterestItems[i]
    print(pollHelpString)
    setActiveFrame(filterByItem(pollHelpString))
    activeToCSV(paste(highInterestItems[i]," Query", sep = ""))
  }
}

#Polls targeted data in vector "highInterestItems", exports as CSV, saves at "defaultFilePath" with title "<date> Query". 
#Consolidated feature for archival of PollHIghInterest
#Args: N/A
pollHighInterestLong <- function(){
  returnLongDf <- data.frame()
  date <- Sys.Date()
  timeStamp.str <- as.character(date)
  for(i in 1:length(highInterestItems)){
    pollHelpString <- highInterestItems[i]
    print(pollHelpString)
    returnLongDf <- rbind(returnLongDf, filterByItem(pollHelpString))
  }
  setActiveFrame(returnLongDf)
  activeToCSV(paste(timeStamp.str," Query", sep = ""))
}

#Appends high interest vector with new string.
#Args: String addHighString
addHighInterest <- function(addHighString){
  for(i in 1:length(highInterestItems)){
    if(highInterestItems[i]==addHighString){
      return("Item Exists!")
      }
    else{
      highInterestItems <- append(highInterestItems,addHighString)
      return(highInterestItems)
      }
    }
}

#Searches high interest vector and removes matched string.
#Args: String removeHighString
removeHighInterest <- function (removeHighString){
  highInterestItems[ !highInterestItems == removeHighString]
  return(highInterestItems)
}

#Gets average price for item across all servers
#Args: String itemNameAvg
getAveragePrice <- function(itemNameAvg){
  avgHelp <- filterByItem(itemNameAvg)
  avgHelp <- transform(avgHelp, Price = as.numeric(Price))
  print(itemNameAvg)
  return(mean(avgHelp$Price))
}

#Check Outliers loops through each server's specific value and checks it against the expected value of the global average to identify significant outliers.
#A value of 1 is expected if item fits global average; a value of 2 would indicate item is twice the price as global average, .5 would indicate half.
#Args: String itemNameOutlier
checkOutliers <- function(itemNameOutlier){
  avgPrice <- getAveragePrice(itemNameOutlier)
  priceDf <- filterByItem(itemNameOutlier)
  priceDf <- transform(priceDf, Price = as.numeric(Price))
  for (i in 1:nrow(priceDf)){
    print(paste(priceDf[i,1], priceDf[i,3]/avgPrice, sep = " "))
  }
}
