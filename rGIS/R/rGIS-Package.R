#' GeoCoder (using Google maps)
#'
#' This function allows user to enter a list of Addresses and use Google maps to locate the coordinate (WGS) of each address. This function requires a valid Google Geocoding API key.
#' @param Data a dataframe that contains addresses. It may also contain additional columns.
#' @param AddressVar variable that contains addresses
#' @param api_key Google's geocode API key
#' @export
#' @examples
#' GeoCoder(AddressDF, Addresses, "XXX")

GeoCoder <- function(Data, AddressVar, api_key) {
  message("Initializing Geocoding")
  
  library(jsonlite)
  library(RCurl)
  
  #write function to geocode
  getGeoData <- function(location, api_key){
    
    location <- gsub(" " ,"+",location)
    geo_data <- getURL(paste("https://maps.googleapis.com/maps/api/geocode/json?address=",location,sprintf("&key=%s",api_key), sep=""))
    geo_data <- fromJSON(geo_data)
    
    #output
    Coordinate <- geo_data$results$geometry$location
    Status <- geo_data$status
    Accuracy <- geo_data$results$geometry$location_type
    Output <- list(Coordinate=Coordinate, Status=Status, Accuracy=Accuracy)
    return(Output)
  }  
  
  ##Geocode Addresses
  ColumnCount <- dim(Data)[2]
  AddressCount <- dim(Data)[1]
  
  LocationMatrix <- matrix(nrow=0, ncol=ColumnCount+4)
  message(paste("Geocoding",AddressCount,"Addresses"))
  
  ##loop through each address
  GeoCodedAddresses <-  for (i in 1:AddressCount) {
    
    Codes <- getGeoData(as.character(Data[i, AddressVar]), api_key)
    Location <- cbind(Data[i,1:ColumnCount], Codes$Coordinate[1,1:2], row.names = NULL)
    Accuracy <- cbind(Location, Codes$Accuracy, row.names = NULL)
    Status <- cbind(Accuracy, Codes$Status, row.names = NULL)
    LocationMatrix <- rbind(LocationMatrix, Status)
    
    #Progress Bar
    pb = txtProgressBar(min = 0, max = AddressCount, initial = 0, style = 3)
    setTxtProgressBar(pb,i/(100)*100)
    
    next
    
  }
  
  names(LocationMatrix)[names(LocationMatrix)=="Codes$Accuracy"] <- "Accuracy_Type"
  names(LocationMatrix)[names(LocationMatrix)=="Codes$Status"] <- "Accuracy_Success"
  
  #remove Address duplicates
  # duplicates occur when google tries different methods of finding an address
  # all methods yield the same coordinates but different search algorithm and therefore
  # "accuracy type" differs. But since coordinates are identical, drop duplicates
  LocationMatrix <- LocationMatrix[!duplicated(LocationMatrix[,AddressVar]),]
  
  Output <- list(LocationData=LocationMatrix)
  
  return(Output)
  #function ends  
}


#' GeoSearch (using Google maps)
#'
#' This function allows user to enter a keyword that is searched through a set of points, each of which searches within a 50 mile radius.
#' This function requires a valid Google Geocoding API key.
#' @param Keyword A string. Keyword to search.
#' @param Fishpoints A two column dataframe where column POINT_X contains X coordinates and POINT_Y contains Y coordinates. 
#' Fishpoints should cover entire area of interest.
#' @param APIkey A string. Google's geocode API key
#' @export
#' @examples
#' GeoSearch(Keyword = "walmart", Fishpoints, APIkey = "XXX")

GeoSearch <- function(Keyword, Fishpoints, APIkey) {
  
  #bring in libraries
  library(jsonlite)
  library(RCurl)
  
  #convert Keyword to searchable text
  KeywordSearch <- gsub(" " ,"+",Keyword)
  
  #loop through each fishpoint
  ResultsTable <- as.data.frame(matrix(nrow=0, ncol=6))
  
  for (i in 1:dim(Fishpoints)[1]) {
    geodatatest1 <- getURL(paste("https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=",
                                 as.character(Fishpoints[i,"POINT_Y"]),",",as.character(Fishpoints[i,"POINT_X"]),
                                 "&radius=50000&keyword=",as.character(KeywordSearch),"&key=",as.character(APIkey), sep=""))
    geo_data <- fromJSON(geodatatest1)
    
    geo_data$results$name <- cat(gsub("\\\"" ,"\"",geo_data$results$name))
    
    Name <- geo_data$results$name
    Location <- geo_data$results$geometry$location
    Ratings <- geo_data$results$rating
    Accuracy <- geo_data$status
    Vicinity <- geo_data$results$vicinity
    
    Name[is.null(Name)] <- 0
    Location[is.null(Location)] <- 0
    Ratings[is.null(Ratings)] <- 0
    Accuracy[is.null(Accuracy)] <- 0  
    Vicinity[is.null(Vicinity)] <- 0
    
    
    Geocoded_Data <- cbind(Name, Location, Ratings, Accuracy, Vicinity)
    
    if (Accuracy!="ZERO_RESULTS"){
      #row.names(Geocoded_Data) <- seq(from = dim(ResultsTable)[1]+1, to = dim(Results)[1]+dim(ResultsTable)[1])
      names(ResultsTable) <- names(Geocoded_Data)  
      ResultsTable <- rbind(ResultsTable,Geocoded_Data)
    }
    
    pb = txtProgressBar(min = 0, max = dim(Fishpoints)[1], initial = 0, style = 3)
    setTxtProgressBar(pb,i/(100)*100)
    
    SearchResults <- ResultsTable
    
  }
  
  return(SearchResults)
  
}
