% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gd_paths.R
\name{gd_topo_path}
\alias{gd_topo_path}
\title{Find the path to a topojson file for a given map name}
\usage{
gd_topo_path(map_name, system = FALSE)
}
\arguments{
\item{map_name}{The name of the map whose topojson file path is to be found.}

\item{system}{Logical, if TRUE, an absolute system path is returned. If FALSE (default), a relative path is returned.}
}
\value{
A string representing the path to the topojson file.
}
\description{
This function searches for a topojson file within a directory structure
based on a provided map name. It can return either a relative path or
an absolute system path.
}
\examples{
gd_topo_path(map_name = "col_departments", system = FALSE)
gd_topo_path(map_name = "col_departments", system = TRUE)

}
