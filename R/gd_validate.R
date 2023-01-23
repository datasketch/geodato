
#' @export
valid_geoids <-function(v, map_name){

  codes <- gd_codes(map_name)

  l <- gd_meta(map_name)

  if(!is.null(gd_id_format(map_name))){
    # TODO INCLUDE FUNCTION TO VALIDATE DIFFERENT CODE IDS
    if(is.character(v)) {v <- as.numeric(v)}
    v <- sprintf(l$id_format,v)
  }
  if(!any(v %in% codes$id))
    stop("Provided ids did not match the ids of: ", map_name,
         "\nCheck gd_codes(map_name) to see available ids")
}



#' @export
validate_centroids <- function(centroids, map_name = NULL){
  if(!all(c("id", "name", "lon", "lat") %in% names(centroids)))
    stop("Centroids with no id, name, lon lat columns: ", map_name)
}

#' @export
validate_altnames <- function(altnames, map_name = NULL){
  if(!all(c("id", "altname") %in% names(altnames)))
    stop("No id and altname: ", map_name)
  if(!all(gsub("alt","", names(altnames)) == names(gd_codes(map_name))))
     stop("Altnames colnames must be the same as centroids for: ", map_name)
}

#' @export
validate_altids <- function(altids, map_name = NULL){
  if(!all(c("id") %in% names(altids)))
    stop("No id and altids: ", map_name)
}

#' @export
validate_topojson <- function(tj, scope, centroids, map_name = NULL){
  if(!all(c("id", "name") %in% names(tj)))
    stop("Topojson mus have id and name for the features: ", map_name)
  if(!all(centroids$name %in% tj$name))
    stop(map_name,"\n   ",
         "Not all centroids name in topojson name: ",
         collapse_char(which_not_in(centroids$name, tj$name)), "\n   ",
         "Not all topojson name in centroids name: ",
         collapse_char(which_not_in(tj$name, centroids$name))
    )
  if(!all(centroids$id %in% tj$id))
    stop("Not all centroids id in topojson id: ", map_name)
}
