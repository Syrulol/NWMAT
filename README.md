# NWMAT
A practical application of learned skills in R to query New World Marketplace's API and pass desired information into CSV-format for manipulation in Excel, Sheets, Tableau, etc. 

# WHAT IS NWMAT
NWMAT is a suite of R scripts designed to poll information from NWMarketPrices for the online MMORPG "New World", specifically identifying 51 key commodity goods to create
an economic health index utilizing the Average Deviation and Absolute Average Deviation statistical variation modeling. 

# HOW DOES IT WORK
NWMAT is explicitly designed with the scope of feeding dashboard applications such as Tableau with properly formatted data for visualization and quick reference. It also
servers to chart and archive economic trends in New World's simulated economies. 

NWMAT queries 28 JSON streams from NWMarketPrices, parses this information into easily manipulated formats, and passes this out in CSV format through various queries. 

NWMAT has the ability to parse over 5800 itemized entries, but by default polls only 51 predefined key commodity goods, those of which have extremely high trade-volume.

NWMAT is dependant on NWMarketPrices API, but is futureproofed. Server merges, environment changes, etc., should not impact the tool nor the economic data or fitment models used. 
