
#' Search available maps in geodato
#'
#' This function searches for maps in the geodato package based on a query string
#' and an optional type parameter. It returns a character vector of map names that
#' match the search criteria.
#'
#' @param q A character string specifying the search query.
#' @param type A character string specifying the type of map to search for. Default is "all".
#'             Acceptable values are "all", "main", and "region".
#' @return A character vector of map names that match the search criteria.
#' @importFrom dplyr filter if_any everything pull
#' @examples
#' search_maps("colombia", type = "all")
#' search_maps("cdmx", type = "main")
#' search_maps("europe", type = "region")
#' @export
search_maps <- function(q, type = "all") {
  maps_df <- geodato::available_maps_df
  sel_type <- type
  if (type == "main") {
    maps_df <- maps_df |> dplyr::filter(type == sel_type)
  } else if (type == "region"){
    maps_df <- maps_df |> dplyr::filter(type == sel_type)
  }
  results <- maps_df |>
    dplyr::filter(dplyr::if_any(dplyr::everything(), ~ grepl(q, .))) |>
    dplyr::pull(map_name)
  unname(results)

}


#' Retrieve available maps in geodato
#'
#' This function returns a list of available maps in the geodato package based on
#' the specified type. The type parameter can be set to "all", "main", or "regions"
#' to filter the list of available maps.
#'
#' @param type A character string specifying the type of maps to retrieve. Default is "all".
#'             Acceptable values are "all", "main", and "regions".
#' @return A character vector of available map names based on the specified type.
#' @examples
#' available_maps(type = "all")
#' available_maps(type = "main")
#' available_maps(type = "regions")
#' @export
available_maps <- function(type = "all"){

  main_maps <- names(geodato:::maps)
  if(type == "main"){
    return(main_maps)
  } else if(type == "regions"){
    return(available_region_maps())
  } else if(type == "all"){
    return(c(main_maps, available_region_maps()))
  }else{
    stop("type must be one of: all or regions")
  }
}


#' Retrieve available region maps in geodato
#'
#' This function returns a list of available region maps in the geodato package.
#' If a map_name is provided, it filters the list based on the given map_name.
#'
#' @param map_name A character string specifying the map name to filter available
#'  region maps. Default is NULL.
#' @return A character vector of available region map names based on the specified
#'  map_name (if provided).
#' @importFrom dplyr filter pull
#' @examples
#' available_region_maps()
#' available_region_maps(map_name = "world_countries")
#' available_region_maps(map_name = "col_municipalities")
#' @export

available_region_maps <- function(map_name = NULL){
  regs <- geodato::available_maps_df |>
    dplyr::filter(type == "region")

  if (!is.null(map_name)) {
    regs <- regs[regs$main_map == map_name, ]
    if (nrow(regs) == 0) return()
  }

  regs <- regs |>
    dplyr::pull(map_name)
  unname(regs)
}


#' Validate map name
#'
#' This function validates the provided map_name by checking if it exists in the
#' list of available maps in the geodato package. If the map_name is not found,
#' the function stops with an error message.
#'
#' @param map_name A character string specifying the map name to validate.
#' @return None. If the map_name is not found, the function stops with an error message.
#' @examples
#' validate_map_name("col_departments")
#' validate_map_name("colombia")
#' @export
validate_map_name <- function(map_name){
  if(!map_name %in% available_maps(type = "all"))
    stop(map_name," not available, check `available_maps()`")
}

#' Retrieve the main map name from a region map
#'
#' This function takes a map name and returns the main map name if the input map
#' is a region map. If the input map is not a region map, the input map name is
#' returned. The function also validates the input map name to ensure it exists in
#' the list of available maps in the geodato package.
#'
#' @param map_nm A character string specifying the map name to retrieve the main map name from.
#' @return A character string representing the main map name if the input map is a region map,
#'  or the input map name if it's not a region map.
#' @importFrom dplyr filter
#' @examples
#' map_name_from_region("col_municipalities_sur_de_narino")
#' map_name_from_region("world_countries_latin_america")
#' @export
map_name_from_region <- function(map_nm, ...){
  # validate_map_name(map_nm)
  #map_nm <- map_name

  map_type_info <- geodato::available_maps_df |>
    dplyr::filter(map_name == map_nm)
  if(map_type_info$type == "region"){
    map_nm <- map_type_info$main_map
  }
  map_nm
}

#' Retrieve geocode ids for a region map
#'
#' This function takes a map name and returns the geocode ids for that region if
#' the input map is a region map. If the input map is not a region map,
#' the function returns NULL.
#'
#' @param map_nm A character string specifying the map name to retrieve geocode ids for.
#' @return A character vector of geocode ids for the specified region map, or NULL if the
#'  input map is not a region map.
#' @examples
#' map_name_from_region_filter_codes("world_countries_western_africa")
#' map_name_from_region_filter_codes("col_departments_andina")
#' map_name_from_region_filter_codes("col_departments") # its not a region
#' @export
map_name_from_region_filter_codes <- function(map_nm){
  map_type_info <- geodato::available_maps_df |>
    dplyr::filter(map_name == map_nm)
  if(map_type_info$type != "region"){
    return(NULL)
  }
  main_map <- map_type_info$main_map
  regs <- gd_regions(main_map) |>
    dplyr::filter(region_code == map_type_info$region)
  regs$id
}

#' Determine if a map is a main or region map
#'
#' This function takes a map name and returns its type, either "main" or "region",
#'  based on the information available in the geodato package.
#'
#' @param map_nm A character string specifying the map name to retrieve the type for.
#' @return A character string representing the type of the map, either "main" or "region".
#' @importFrom dplyr filter pull
#' @examples
#' main_or_region_map("col_departments")
#' main_or_region_map("col_departments_andina")
#' @export
main_or_region_map <- function(map_nm){
  geodato::available_maps_df |>
    dplyr::filter(map_name == map_nm) |>
    dplyr::pull(type)
}
