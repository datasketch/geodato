

#' @export
gd_meta <- function(map_name){
  #validate_map_name(map_name)
  map_name <- map_name_from_region(map_name)
  geodato:::maps[[map_name]]
}

#' @export
gd_codes <- function(map_name){
  validate_map_name(map_name)

  map_name_main <- map_name_from_region(map_name)
  region_filter_codes <- map_name_from_region_filter_codes(map_name)

  l <- geodato:::maps[[map_name_main]]
  if(!is.null(l$parent_map_name)){
    codes <- l$centroids %>% dplyr::select(id, name, zone, zone_id)
  }else{
    codes <- l$centroids %>% dplyr::select(id, name)
  }
  if(!is.null(region_filter_codes)){
    codes <- codes |> dplyr::filter(id %in% region_filter_codes)
  }
  codes
}

#' @export
gd_altnames <- function(map_name){
  #validate_map_name(map_name)

  map_name_main <- map_name_from_region(map_name)
  region_filter_codes <- map_name_from_region_filter_codes(map_name)

  l <- geodato:::maps[[map_name_main]]
  altnames <- l$altnames |>
    dplyr::filter(!is.na(id))

  if(!is.null(region_filter_codes)){
    altnames <- altnames |> dplyr::filter(id %in% region_filter_codes)
  }
  altnames

}

#' @export
gd_region_codes <- function(map_name){
  if(main_or_region_map(map_name) == "region"){
    stop("No regions defined for region maps")
  }
  l <- geodato:::maps[[map_name]]
  l$region_codes
}

#' @export
gd_regions <- function(map_name){
  if(main_or_region_map(map_name) == "region"){
    stop("No regions defined for region maps")
  }
  l <- geodato:::maps[[map_name]]
  l$regions
}

#' @export
gd_possiblenames <- function(map_name, with_parent = TRUE){
  parent_possible_names <- NULL
  map_name <- map_name_from_region(map_name)
  if(with_parent){
    parent <- gd_parent_map_name(map_name)
    if(!is.null(parent)){
      parent_possible_names <- c(gd_codes(parent)$name,
                         gd_altnames(parent)$altname)
    }
  }
  unique(str_clean(
    c(gd_codes(map_name)$name,
      gd_altnames(map_name)$altname,
      parent_possible_names)
    )
  )
}

#' @export
gd_altids <- function(map_name){
  #validate_map_name(map_name)
  map_name <- map_name_from_region(map_name)
  l <- geodato:::maps[[map_name]]
  l$altids
}

#' @export
gd_parent_map_name <- function(map_name){
  #validate_map_name(map_name)
  map_name <- map_name_from_region(map_name)
  l <- geodato:::maps[[map_name]]
  l$parent_map_name
}

#' @export
gd_level <- function(map_name){
  map_name <- map_name_from_region(map_name)
  l <- geodato:::maps[[map_name]]
  l$level
}



#' @export
gd_id_format <- function(map_name){
  #validate_map_name(map_name)
  map_name <- map_name_from_region(map_name)
  l <- geodato:::maps[[map_name]]
  l$id_format
}


#' @export
gd_tj <- function(map_name){

  map_name_main <- map_name_from_region(map_name)
  region_filter_codes <- map_name_from_region_filter_codes(map_name)

  l <- geodato:::maps[[map_name_main]]
  tj <- l$tj

  if(!is.null(region_filter_codes)){
    tj <- tj |> dplyr::filter(id %in% region_filter_codes)
  }
  tj

}

#' @export
gd_centroids <- function(map_name){
  map_name <- map_name_from_region(map_name)
  l <- geodato:::maps[[map_name]]
  ## OJOOOOO
  d <- l$centroids |>
    dplyr::mutate_at(c("lon", "lat"), as.numeric)
}

