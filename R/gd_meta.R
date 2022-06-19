
#' @export
available_maps <- function(){
  names(geodato:::maps)
}

#' @export
validate_map_name <- function(map_name){
  if(!map_name %in% available_maps())
    stop(map_name," not available, check `available_maps()`")
}

#' @export
gd_meta <- function(map_name){
  #validate_map_name(map_name)
  geodato:::maps[[map_name]]
}

#' @export
gd_codes <- function(map_name){
  validate_map_name(map_name)
  l <- geodato:::maps[[map_name]]
  if(!is.null(l$parent_map_name))
    codes <- l$centroids %>% dplyr::select(id, name, zone, zone_id)
  else
    codes <- l$centroids %>% dplyr::select(id, name)
  codes
}

#' @export
gd_altnames <- function(map_name){
  #validate_map_name(map_name)
  l <- geodato:::maps[[map_name]]
  l$altnames |>
    dplyr::filter(!is.na(id))
}

#' @export
gd_possiblenames <- function(map_name){
  unique(str_clean(c(gd_codes(map_name)$name, gd_altnames(map_name)$altname)))
}

#' @export
gd_altids <- function(map_name){
  #validate_map_name(map_name)
  l <- geodato:::maps[[map_name]]
  l$altids
}

#' @export
gd_parent_map_name <- function(map_name){
  #validate_map_name(map_name)
  l <- geodato:::maps[[map_name]]
  l$parent_map_name
}

#' @export
gd_id_format <- function(map_name){
  #validate_map_name(map_name)
  l <- geodato:::maps[[map_name]]
  l$id_format
}


#' @export
gd_tj <- function(map_name){
  l <- geodato:::maps[[map_name]]
  l$tj
}


