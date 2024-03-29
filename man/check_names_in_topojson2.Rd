% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/topojson.R
\name{check_names_in_topojson2}
\alias{check_names_in_topojson2}
\title{Check if all names in TopoJSON data match the provided codes (grouped by zone)}
\usage{
check_names_in_topojson2(tj, codes)
}
\arguments{
\item{tj}{A TopoJSON object or a file path to a TopoJSON file.}

\item{codes}{A data frame containing codes with a 'name' column and a 'zone' column to compare with the TopoJSON names.}
}
\value{
NULL if all names match, otherwise an error message is displayed with the list of mismatched names.
}
\description{
This function takes a TopoJSON object and a codes data frame to check if all
names in the TopoJSON data match the provided codes, grouped by zone. If not,
an error message is returned with a list of mismatched names.
The function can handle both TopoJSON files and 'sf' objects.
}
\examples{
tj <- gd_tj("col_municipalities")
codes <- gd_regions("col_municipalities")
check_names_in_topojson2(tj, codes)
}
