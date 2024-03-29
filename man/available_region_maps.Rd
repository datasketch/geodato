% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/available_maps.R
\name{available_region_maps}
\alias{available_region_maps}
\title{Retrieve available region maps in geodato}
\usage{
available_region_maps(map_name = NULL)
}
\arguments{
\item{map_name}{A character string specifying the map name to filter available
region maps. Default is NULL.}
}
\value{
A character vector of available region map names based on the specified
 map_name (if provided).
}
\description{
This function returns a list of available region maps in the geodato package.
If a map_name is provided, it filters the list based on the given map_name.
}
\examples{
available_region_maps()
available_region_maps(map_name = "world_countries")
available_region_maps(map_name = "col_municipalities")
}
