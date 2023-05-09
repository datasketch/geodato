
#' Validate Geocodes for a given map
#'
#' This function checks if the input values are valid geocodes for the specified map.
#'
#' @param v A vector of geocodes to be validated.
#' @param map_name The name of the map for which the geocodes should be validated.
#' @return No return value. The function stops with an error message if any of
#' the provided geocodes do not match the map's geocodes.
#' @examples
#' valid_geoids(c("05", "08", "11"), "col_departments")
#' valid_geoids(c("ARG", "BRA", "COL"), "world_countries")
#' valid_geoids(c("123", "B1RA", "4COL"), "world_countries") # out error
#' @export
valid_geoids <-function(v, map_name){

  codes <- gd_codes(map_name)

  l <- gd_meta(map_name)

  if(!is.null(gd_id_format(map_name))){
    if(is.character(v)) {v <- as.numeric(v)}
    v <- sprintf(l$id_format,v)
  }
  if(!any(v %in% codes$id))
    stop("Provided ids did not match the ids of: ", map_name,
         "\nCheck gd_codes(map_name) to see available ids")
}



#' Validate Centroids
#'
#' This function checks if the input centroids have the required columns: id, name, lon, and lat.
#'
#' @param centroids A dataframe containing the centroids to be validated.
#' @param map_name (Optional) The name of the map associated with the centroids. This is used for the error message if validation fails.
#' @return No return value. The function stops with an error message if any of the required columns are missing.
#' @examples
#' centroids <- gd_centroids("col_departments")
#' validate_centroids(centroids)
#' @export
validate_centroids <- function(centroids, map_name = NULL){
  if(!all(c("id", "name", "lon", "lat") %in% names(centroids)))
    stop("Centroids with no id, name, lon lat columns: ", map_name)
}

#' Validate Alternate Names
#'
#' This function checks if the input alternate names have the required columns: id and altname.
#' It also checks if the column names match the corresponding centroid column names.
#'
#' @param altnames A dataframe containing the alternate names to be validated.
#' @param map_name (Optional) The name of the map associated with the alternate names.
#'  This is used for the error message if validation fails.
#' @return No return value. The function stops with an error message if any of the
#' required columns are missing or if the column names do not match.
#' @export
validate_altnames <- function(altnames, map_name = NULL){
  if(!all(c("id", "altname") %in% names(altnames)))
    stop("No id and altname: ", map_name)
  if(!all(gsub("alt","", names(altnames)) == names(gd_codes(map_name))))
    stop("Altnames colnames must be the same as centroids for: ", map_name)
}

#' Validate Alternate IDs
#'
#' This function checks if the input alternate IDs have the required column: id.
#'
#' @param altids A dataframe containing the alternate IDs to be validated.
#' @param map_name (Optional) The name of the map associated with the alternate IDs. This is used for the error message if validation fails.
#' @return No return value. The function stops with an error message if the required column is missing.
#' @export
validate_altids <- function(altids, map_name = NULL){
  if(!all(c("id") %in% names(altids)))
    stop("No id and altids: ", map_name)
}

#' Validate Regions
#'
#' This function validates the input regions, checking if they have the required
#' columns and if all region codes are in gd_codes.
#'
#' @param regions A dataframe containing the regions to be validated.
#' @param map_name The name of the map associated with the regions.
#' @return No return value. The function stops with an error message if the required
#'  columns are missing or if the region codes are not in gd_codes.
#' @examples
#' regions <- gd_regions("col_municipalities")
#' validate_regions(regions, "col_municipalities")
#' @export
validate_regions <- function(regions, map_name){
  if(!all(c("region_code", "region_name", "id") %in% names(regions)))
    stop("Regions file column names must be 'region_code', 'region_name', 'id' ")
  # All region codes must be in the gd_codes
  codes <- gd_codes(map_name)
  if (!all( regions$id %in% codes$id)) {
    missing <- dstools::which_not_in(regions$id, codes$id)
    message("Not all region geocodes in gd_codes: ",
            paste(missing, collapse = ", "))
  }

}

#' Validate TopoJSON
#'
#' This function validates the input TopoJSON file by checking if it has the required
#' columns and if all centroids are included in the TopoJSON.
#'
#' @param tj A TopoJSON object to be validated.
#' @param scope A scope value associated with the TopoJSON object.
#' @param centroids A dataframe containing the centroids to be checked against the TopoJSON object.
#' @param map_name The name of the map associated with the TopoJSON object (optional).
#' @return No return value. The function stops with an error message if the required columns are missing or if the centroids are not included in the TopoJSON.
#' @examples
#' tj <- gd_tj("col_departments")
#' centroid <- gd_centroids("col_departments")
#' validate_topojson(tj, centroids)
#' @export
validate_topojson <- function(tj, scope, centroids, map_name = NULL){
  if (!all(c("id", "name") %in% names(tj)))
    stop("Topojson mus have id and name for the features: ", map_name)
  if (!all(centroids$name %in% tj$name))
    stop(map_name,"\n   ",
         "Not all centroids name in topojson name: ",
         collapse_char(which_not_in(centroids$name, tj$name)), "\n   ",
         "Not all topojson name in centroids name: ",
         collapse_char(which_not_in(tj$name, centroids$name))
    )
  if (!all(centroids$id %in% tj$id))
    stop("Not all centroids id in topojson id: ", map_name)
}
