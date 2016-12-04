#' GeoCoder
#'
#' This function allows you to enter a list of Addresses and use Google maps to locate the coordinate (WGS) of each address. This function requires a valid Google Geocoding API key.
#' @param Data a dataframe that contains addresses. It may also contain additional columns.
#' @param AddressVar variable that contains addresses
#' @param api_key Google's geocode API key
#' @export datafile with coordinates
#' @examples
#' function(AddressDF, Addresses, "XXX")

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
