library("dplyr")

highInterestItems <- c("Iron Ore", "Iron Ingot", "Steel Ingot", "Starmetal Ore", "Starmetal Ingot", "Orichalcum Ore", "Orichalcum Ingot", "Green Wood", "Timber", "Aged Wood", "Lumber", "Wyrdwood", "Wyrdwood Planks", "Ironwood", "Ironwood Planks", "Fibers", "Linen", "Sateen", "Silk Threads", "Silk", "Wirefiber", "Infused Silk", "Rawhide", "Coarse Leather", "Thick Hide", "Rugged Leather", "Layered Leather", "Iron Hide", "Infused Leather", "Stone", "Stone Block", "Stone Brick", "Lodestone", "Lodestone Brick", "Obsidian Voidstone", "Obsidian Sandpaper", "Obsidian Flux", "Aged Tannin", "Pure Solvent", "Wireweave", "Life Mote", "Death Mote", "Water Mote", "Earth Mote", "Air Mote", "Soul Mote", "Fire Mote", "Runic Leather", "Glittering Ebony", "Phoenixweave", "Asmodeum")
activeDF <- data.frame()

#Moves a returned data frame object into the global environment for manipulation. Probably bandaid for poor variable scoping.
#Args: Data.Frame PassiveFrame
setActiveFrame <- function(passiveFrame){
  activeDF <<- passiveFrame
  return(activeDF)
}

#Converts active data frame into CSV with title of type string in arguments.
#Args:String docTitle
activeToCSV <- function(docTitle){
  tibble::glimpse(activeDF)
  write.csv(activeDF, paste(here(),"/Output/", docTitle,".csv", sep = ""), row.names = FALSE)
}

#Allows specific cross reference of item in server
#Args: String queryItemName, Data.Frame queryServerName
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

#TODO: Use appropriate locations to write CSV
#Polls targeted data in vector "highInterestItems", exports as CSV, saves at "defaultFilePath" with title "<item>Query".
#Args: N/A
printHighInterest <- function(){
  for(i in 1:length(highInterestItems)){
    pollHelpString <- highInterestItems[i]
    print(pollHelpString)
    setActiveFrame(filterByItem(pollHelpString))
    activeToCSV(paste(highInterestItems[i]," Query", sep = ""))
  }
}

#TODO: Use appropriate locations to write CSV
#Polls targeted data in vector "highInterestItems", exports as CSV, saves at "defaultFilePath" with title "<date> Query". 
#Consolidated feature for archival of PollHIghInterest
#Args: N/A
printHighInterestLong <- function(){
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
  return(mean(avgHelp$Price))
}

#Average Deviation loops through each server's specific value and checks it against the expected value of the global average to identify significant outliers.
#A value of 1 is expected if item fits global average; a value of 2 would indicate item is twice the price as global average, .5 would indicate half.
#Args: String avgDeviationItem
avgDeviation <- function(avgDeviationItem){
  avgDf <- data.frame()
  avgPrice <- getAveragePrice(avgDeviationItem)
  priceDf <- filterByItem(avgDeviationItem)
  priceDf <- transform(priceDf, Price = as.numeric(Price))
  for (i in 1:nrow(priceDf)){
    avgVecHelp <- c(avgDeviationItem, priceDf[i,1], priceDf[i,3]/avgPrice)
    avgDf <- rbind(avgDf,avgVecHelp)
  }
  colnames(avgDf) <- c("itemname", "server", "deviation")
  return(avgDf)
}

#Returns a data frame object of all items in the high interest list and their deviations from the average price. 
#Args: N/A
getHighInterestDeviation <- function(){
  hidDf <- data.frame()
  for(i in 1:length(highInterestItems)){
    hidDf <- rbind(hidDf, avgDeviation(highInterestItems[i]))
  }
  return(hidDf)
}

#Returns a data frame object of a server's relative health by calculating the average of the average deviation across all items in the high interest list.
#Args: String serverString
getServerHealth <- function(serverString){
  healthDf <- getHighInterestDeviation() %>% filter(server == serverString) %>% transform(deviation = as.numeric(deviation))
  return(c(serverString, mean(healthDf$deviation)))
}

#TODO: This is very slow. 
#Returns a data frame object of all server's relative health by calculating the mean average deviation across the high interest list.
#A value's proximity to 1 indicates its relative economic health, where 1 indicates expected average. Increasing indicates inflation, decreasing is deflation.
#Args: N/A
getGlobalHealth <- function(){
  globalHealthDf <<- getHighInterestDeviation()
  ghReturn <- data.frame()
  ghDf <- data.frame()
  for(i in 1:nrow(serverIDCrossClean)){
    ghDf <- filter(globalHealthDf, server == serverIDCrossClean[i,2]) %>% transform(deviation = as.numeric(deviation))
    ghReturn <- rbind(ghReturn, c(serverIDCrossClean[i,2], mean(ghDf$deviation), serverIDCrossClean[i,3]))
  }
  colnames(ghReturn) <- c("server", "deviation", "dateupdated")
  return (ghReturn)
}

#TODO: Use appropriate locations to write CSV
#Exports global health as a CSV at default file location.
#Args: N/A
printGlobalHealth <- function(){
  date <- Sys.Date()
  timeStamp.str <- as.character(date)
  setActiveFrame(getGlobalHealth())
  activeToCSV(paste(timeStamp.str," Health Query", sep = ""))
}