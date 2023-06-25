#' Retrieve metadata for a map in geodato
#'
#' This function takes a map name and returns its metadata available in the geodato package.
#' The function also handles region maps by retrieving the corresponding main map metadata.
#'
#' @param map_name A character string specifying the map name to retrieve the metadata.
#' @return A list containing metadata for the specified map.
#' @examples
#' gd_meta("col_departments")
#' gd_meta("mex_cdmx_localities")
#' @export
gd_meta <- function(map_name){
  map_name <- map_name_from_region(map_name)
  geodato:::maps[[map_name]]
}

#' Retrieve geocodes for a map in geodato
#'
#' This function takes a map name and returns its geocodes available in the geodato package.
#' The function also handles region maps by retrieving the corresponding main map geocodes and
#' filtering them based on the region.
#'
#' @param map_name A character string specifying the map name to retrieve the geocodes.
#' @return A data frame containing geocodes for the specified map.
#' @importFrom dplyr filter select
#' @examples
#' gd_codes("col_municipalities")
#' gd_codes("world_countries")
#' @export
gd_codes <- function(map_name, ...){
  # validate_map_name(map_name)

  map_name_main <- map_name_from_region(map_name, ...)
  region_filter_codes <- map_name_from_region_filter_codes(map_name)

  l <- geodato:::maps[[map_name_main]]
  if(!is.null(l$parent_map_name)){
    codes <- l$centroids |>  dplyr::select(id, name, zone, zone_id)
  }else{
    codes <- l$centroids |>  dplyr::select(id, name)
  }
  if(!is.null(region_filter_codes)){
    codes <- codes |> dplyr::filter(id %in% region_filter_codes)
  }
  codes
}

#' Retrieve alternative names for a map in geodato
#'
#' This function takes a map name and returns its alternative names available in the geodato package.
#' The function also handles region maps by retrieving the corresponding main map alternative names and
#' filtering them based on the region.
#'
#' @param map_name A character string specifying the map name to retrieve alternative names.
#' @return A data frame containing alternative names for the specified map.
#' @importFrom dplyr filter
#' @examples
#' gd_altnames("col_departments")
#' gd_altnames("col_municipalities_suroccidente_de_cundinamarca") #this as region without regions
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

#' Retrieve region codes for a main map in geodato
#'
#' This function takes a main map name and returns its region codes available in the geodato package.
#' If the input map is a region map, the function stops with an error message.
#'
#' @param map_name A character string specifying the main map name to retrieve region codes.
#' @return A data frame containing region codes for the specified main map, or an error message if the input map is a region map.
#' @examples
#' gd_region_codes("col_municipalities")
#' gd_region_codes("col_municipalities_alto_putumayo") # This will result in an error
#' @export
gd_region_codes <- function(map_name){
  if(main_or_region_map(map_name) == "region"){
    stop("No regions defined for region maps")
  }
  l <- geodato:::maps[[map_name]]
  l$region_codes
}

#' Retrieve regions for a main map in geodato
#'
#' This function takes a main map name and returns its regions available in the geodato package.
#' If the input map is a region map, the function stops with an error message.
#'
#' @param map_name A character string specifying the main map name to retrieve regions.
#' @return A data frame containing regions for the specified main map, or an error message if the input map is a region map.
#' @examples
#' gd_regions("col_municipalities")
#' gd_regions("col_municipalities_sarare") # This will result in an error
#' @export
gd_regions <- function(map_name, path = NULL){
  if(main_or_region_map(map_name) == "region"){
    stop("No regions defined for region maps")
  }
  if(!is.null(path)){

  }else{
    l <- geodato:::maps[[map_name]]
    regions <- l$regions
  }
  regions
}

#' Retrieve possible names for a map in geodato
#'
#' This function takes a map name and returns its possible names available in the
#' geodato package, including main names, alternative names, and optionally parent map names.
#'
#' @param map_name A character string specifying the map name to retrieve possible names.
#' @param with_parent A logical value indicating whether to include parent map names in the result. Default is TRUE.
#' @return A character vector containing unique possible names for the specified map.
#' @examples
#' gd_possiblenames("world_countries")
#' gd_possiblenames("world_countries", with_parent = FALSE)
#' gd_possiblenames("world_countries_western_africa")
#' gd_possiblenames("col_departments")
#' length(gd_possiblenames("col_municipalities")) # return names with his parent (deparments)
#' length(gd_possiblenames("col_municipalities", with_parent = FALSE))
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

#' Retrieve alternative IDs for a map in geodato
#'
#' This function takes a map name and returns its alternative IDs available in the geodato package.
#' The function also handles region maps by retrieving the corresponding main map alternative IDs.
#'
#' @param map_name A character string specifying the map name to retrieve alternative IDs.
#' @return A data frame containing alternative IDs for the specified map.
#' @examples
#' gd_altids("col_departments")
#' gd_altids("world_countries")
#' gd_altids("world_countries_northern_europe")
#' @export
gd_altids <- function(map_name){
  #validate_map_name(map_name)
  map_name <- map_name_from_region(map_name)
  l <- geodato:::maps[[map_name]]
  l$altids
}

#' Retrieve the parent map name for a map in geodato
#'
#' This function takes a map name and returns its parent map name if available in
#' the geodato package. The function also handles region maps by retrieving the
#' corresponding main map parent map name.
#'
#' @param map_name A character string specifying the map name to retrieve the parent map name.
#' @return A character string containing the parent map name for the specified map, or NULL if no parent map is available.
#' @examples
#' gd_parent_map_name("world_countries")
#' gd_parent_map_name("world_countries_northern_europe") # agregar
#' @export
gd_parent_map_name <- function(map_name){
  #validate_map_name(map_name)
  map_name <- map_name_from_region(map_name)
  l <- geodato:::maps[[map_name]]
  l$parent_map_name
}

#' Retrieve the level of a map in geodato
#'
#' This function takes a map name and returns its level in the geodato package.
#' The function also handles region maps by retrieving the corresponding main map level.
#'
#' @param map_name A character string specifying the map name to retrieve the level.
#' @return A character string containing the level for the specified map.
#' @examples
#' gd_level("col_departments")
#' gd_level("col_departments_pacifico")
#' @export
gd_level <- function(map_name){
  map_name <- map_name_from_region(map_name)
  l <- geodato:::maps[[map_name]]
  l$level
}



#' Retrieve the ID format of a map in geodato
#'
#' This function takes a map name and returns its ID format in the geodato package.
#' The function also handles region maps by retrieving the corresponding main map ID format.
#'
#' @param map_name A character string specifying the map name to retrieve the ID format.
#' @return A character string containing the ID format for the specified map.
#' @examples
#' gd_id_format("col_municipalities")
#' gd_id_format("col_municipalities_sumapaz")
#' @export
gd_id_format <- function(map_name){
  #validate_map_name(map_name)
  map_name <- map_name_from_region(map_name)
  l <- geodato:::maps[[map_name]]
  l$id_format
}


#' Retrieve the TopoJSON data for a map in geodato
#'
#' This function takes a map name and returns its TopoJSON data available in the geodato package.
#' The function also handles region maps by retrieving the corresponding main map TopoJSON data and
#' filtering by the region codes.
#'
#' @param map_name A character string specifying the map name to retrieve the TopoJSON data.
#' @return A data frame containing the TopoJSON data for the specified map.
#' @importFrom dplyr filter
#' @examples
#' gd_tj("world_countries")
#' gd_tj("mex_cdmx_colonies")
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

#' Retrieve the centroids data for a map in geodato
#'
#' This function takes a map name and returns its centroids data available in the
#'  geodato package.
#' The function also handles region maps by retrieving the corresponding main map
#'  centroids data.
#'
#' @param map_name A character string specifying the map name to retrieve the centroids data.
#' @return A data frame containing the centroids data for the specified map.
#' @importFrom dplyr mutate_at
#' @examples
#' gd_centroids("col_departments")
#' gd_centroids("col_departments_pacifico")
#' @export
gd_centroids <- function(map_name){
  map_name <- map_name_from_region(map_name)
  l <- geodato:::maps[[map_name]]
  d <- l$centroids |>
    dplyr::mutate_at(c("lon", "lat"), as.numeric)
  d
}

