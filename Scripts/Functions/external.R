library("jsonlite")

#Populates all market data as data frames titled by Server Name. Used for establishing global variables, will not update already existing data tables.
#Args: N/A
populateMarketData <- function(){
  urlStructure <- "https://nwmarketprices.com/api/latest-prices/"
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