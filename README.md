# New World Market Assessment Toolkit
A practical application of learned skills in R to query New World Marketplace's API and pass desired information into CSV-format for manipulation in Excel, Sheets, Tableau, etc. 

# WHAT IS NWMAT
NWMAT is a suite of R scripts designed to poll information from NWMarketPrices for the online MMORPG "New World", specifically identifying 51 key commodity goods to create an economic health index utilizing the Average Deviation and Absolute Average Deviation statistical variation modeling. 

# HOW DOES IT WORK
New World Market Asssessment Tool implements jsonlite to automatically detect and query up to date information in JSON format from https://www.nwmarketprices.com. These JSON objects are parsed by R into data frame objects, which are then further manipulated to allow for rapid querying and filtering of targeted data. Utilizing Average-Absolute Deviation and Average Distribution variation calculations, we are able to assign an economic health rating to servers based on their deviation from the global economy. 

NWMAT is designed to primarily function from the console to output .CSV data which is used to feed dashboards in Tableau and Excel to more rapidly identify trends and compare/contrast the statuses of various economies. 

# TODO
- Content aware updating utilizing timestamping in server-ID API. 
- Data validation for specific item queries
- Data caching to streamline redundant queries
