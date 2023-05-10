#' List directories in a specified path with the option to filter by map name
#'
#' This function uses `fs::dir_ls` to list all directories in a specified path, recursively.
#' The directories can be filtered by regular expression and/or a map name.
#'
#' @param dir The directory path to list. Default is "data-raw/geodato".
#' @param regex The regular expression pattern to filter the directories. Default is "/.*/.*/", which matches any directory with two or more levels.
#' @param map_name An optional string to further filter the directories by map name.
#'
#' @return A character vector of directories that match the given parameters.
#'
#' @examples
#' gd_dir()
#' gd_dir(map_name = "col_departments")
#'
#' @export
gd_dir <- function(dir = "data-raw/geodato",
                   regex = "/.*/.*/",
                   map_name = NULL) {
  dirs <- fs::dir_ls(dir,
                     recurse = 1,
                     type = "directory",
                     regexp = regex)

  if (!is.null(map_name)) {
    dirs <- dirs[grep(map_name, dirs)]
  }

  dirs
}

#' Find the path to a topojson file for a given map name
#'
#' This function searches for a topojson file within a directory structure
#' based on a provided map name. It can return either a relative path or
#' an absolute system path.
#'
#' @param map_name The name of the map whose topojson file path is to be found.
#' @param system Logical, if TRUE, an absolute system path is returned. If FALSE (default), a relative path is returned.
#'
#' @return A string representing the path to the topojson file.
#'
#' @examples
#' gd_topo_path(map_name = "col_departments", system = FALSE)
#' gd_topo_path(map_name = "col_departments", system = TRUE)
#'
#' @export
gd_topo_path <- function(map_name, system = FALSE) {
  dir <- gd_dir(map_name = map_name)
  name_topojson <- list.files(dir, pattern = ".topojson")

  path <- paste0(dir, "/", name_topojson)

  if (system) {
    path <- system.file(path, package = "geodato")
  }

  path
}


#' Read topojson file as lines
#'
#' This function reads a topojson file associated with a given map name and returns its content as lines.
#'
#' @param map_name The name of the map whose topojson file is to be read.
#' @param system Logical, if TRUE (default), the function will look for an absolute system path. If FALSE, a relative path is used.
#'
#' @return A character vector where each element represents a line in the topojson file.
#'
#' @examples
#' gd_topo_lines(map_name = "col_departments", system = TRUE)
#' gd_topo_lines(map_name = "col_departments", system = FALSE)
#'
#' @export
gd_topo_lines <- function(map_name, system = TRUE) {
  path <- gd_topo_path(map_name = map_name, system = system)
  readLines(path)
}






