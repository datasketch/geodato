
#' @export
search_maps <- function(q, type = "all"){
  # q <- "colombia"
  maps_df <- geodato::available_maps_df
  sel_type <- type
  if(type == "main"){
    maps_df <- maps_df |> dplyr::filter(type == sel_type)
  }else if(type == "region"){
    maps_df <- maps_df |> dplyr::filter(type == sel_type)
  }
  results <- maps_df |>
    dplyr::filter(dplyr::if_any(dplyr::everything(), ~ grepl(q, .))) |>
    dplyr::pull(map_name)
  unname(results)

}


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

#' @export
available_region_maps <- function(map_name = NULL){
  regs <- geodato::available_maps_df |>
    dplyr::filter(type == "region") |>
    dplyr::pull(map_name)
  unname(regs)
}


#' @export
validate_map_name <- function(map_name){
  if(!map_name %in% available_maps(type = "all"))
    stop(map_name," not available, check `available_maps()`")
}


map_name_from_region <- function(map_nm){
  #map_nm <- map_name
  map_type_info <- geodato::available_maps_df |>
    dplyr::filter(map_name == map_nm)
  if(map_type_info$type == "region"){
    map_nm <- map_type_info$main_map
  }
  map_nm
}

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

main_or_region_map <- function(map_nm){
  geodato::available_maps_df |>
    dplyr::filter(map_name == map_nm) |>
    dplyr::pull(type)
}
