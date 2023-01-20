##Library Declarations
library(jsonlite)
#I want to use lubridate now() to timestamp query outputs, I just haven't done it yet.
#library(lubridate)
library(dplyr)
##Global Variable Declarations
urlStructure <- "https://nwmarketprices.com/api/latest-prices/"
defaultFilePath <- "C:\\Users\\44jmn\\OneDrive\\Documents\\NWMAT\\"
highInterestItems <- c("Iron Ore", "Iron Ingot", "Steel Ingot", "Starmetal Ore", "Starmetal Ingot", "Orichalcum Ore", "Orichalcum Ingot")
activeDF <- data.frame()

##Populates all market data as data frames titled by Server Name. Used for establishing global variables, will not update already existing data tables.
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
filterByItemAndServer <- function(queryItemName, queryServerName){
  filter (queryServerName, ItemName == queryItemName)
}

#Returns a data frame object of the targeted string for item across servers. 
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
setActiveFrame <- function(passiveFrame){
  activeDF <<- passiveFrame
  return(activeDF)
}

#Converts active data frame into CSV with title of type string in arguments.
activeToCSV <- function(docTitle){
  print(paste(defaultFilePath,docTitle,".csv", sep = ""))
  tibble::glimpse(activeDF)
  write.csv(activeDF, paste(defaultFilePath, docTitle,".csv", sep = ""), row.names = FALSE)
}

#Polls targeted data in vector "highInterestItems", exports as CSV, saves at "defaultFilePath" with title "<item>Query".
pollHighInterest <- function(){
  for(i in 1:length(highInterestItems)){
    pollHelpString <- highInterestItems[i]
    print(pollHelpString)
    setActiveFrame(filterByItem(pollHelpString))
    activeToCSV(paste(highInterestItems[i]," Query", sep = ""))
  }
}