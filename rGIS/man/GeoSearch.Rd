% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rGIS-Package.R
\name{GeoSearch}
\alias{GeoSearch}
\title{GeoSearch (using Google maps)}
\usage{
GeoSearch(Keyword, Fishpoints, APIkey)
}
\arguments{
\item{Keyword}{A string. Keyword to search.}

\item{Fishpoints}{A two column dataframe where column POINT_X contains X coordinates and POINT_Y contains Y coordinates. 
Fishpoints should cover entire area of interest.}

\item{APIkey}{A string. Google's geocode API key}
}
\description{
This function allows user to enter a keyword that is searched through a set of points, each of which searches within a 50 mile radius.
This function requires a valid Google Geocoding API key.
}
\examples{
GeoSearch(Keyword = "walmart", Fishpoints, APIkey = "XXX")
}

