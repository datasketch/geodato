#' Write a TopoJSON object to a file
#'
#' This function takes a TopoJSON object and writes it to a file specified by the given path.
#'
#' @param tj A TopoJSON object to be written to a file.
#' @param path The file path where the TopoJSON object should be saved.
#' @importFrom geojsonio topojson_json
#' @importFrom readr write_lines
#' @examples
#' #write_topojson(topojson_object, "path/to/save/topojson_file.topojson")
#' @export
write_topojson <- function(tj, path){
  json <- geojsonio::topojson_json(tj)
  readr::write_lines(json, path)
}

#' Calculate centroids from TopoJSON data
#'
#' This function takes a TopoJSON object and calculates the centroids for each polygon.
#' It can handle both TopoJSON files and 'sf' objects.
#'
#' @param tj A TopoJSON object or a file path to a TopoJSON file.
#' @return A data frame containing the centroids for each polygon in the input TopoJSON data.
#' @importFrom sf read_sf st_centroid st_coordinates st_drop_geometry
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
#' @examples
#' centroids_from_topojson("data-raw/geodato/col/col_bog_localidades/bog-localidades.topojson")
#' @export
centroids_from_topojson <- function(tj){
  if(!"sf" %in% class(tj)){
    tj <- read_sf(tj)
  }
  df <- suppressWarnings(st_centroid(tj, of_largest = TRUE))
  centroids <- tibble::as_tibble(sf::st_coordinates(df$geometry)) |>
    purrr::set_names(c("lon", "lat"))
  centroids <- df |>
    sf::st_drop_geometry() |>
    dplyr::bind_cols(centroids)
  centroids
}

#' Check if all names in TopoJSON data match the provided codes
#'
#' This function takes a TopoJSON object and a codes data frame to check if all names in the TopoJSON data
#' match the provided codes. If not, an error message is returned with a list of mismatched names.
#' The function can handle both TopoJSON files and 'sf' objects.
#'
#' @param tj A TopoJSON object or a file path to a TopoJSON file.
#' @param codes A data frame containing codes with a 'name' column to compare with the TopoJSON names.
#' @return NULL if all names match, otherwise an error message is displayed with the list of mismatched names.
#' @importFrom sf read_sf
#' @examples
#' df_names <- centroids_from_topojson(gd_tj("col_departments"))
#' check_names_in_topojson(tj = gd_tj("col_departments"), codes = df_names)
#' @export
check_names_in_topojson <- function(tj, codes){
  if(!"sf" %in% class(tj)){
    tj <- read_sf(tj)
  }
  if(!all(codes$name %in% tj$name) || !all(tj$name %in% codes$name))
    stop("Not all codes name in topojson name: \n",
         collapse_char(which_not_in(codes$name, tj$name)), "\n  ",
         "Not all topojson name in codes name: \n",
         collapse_char(which_not_in(tj$name, codes$name))
    )

  cat("All good")
}


#' Check if all names in TopoJSON data match the provided codes (grouped by zone)
#'
#' This function takes a TopoJSON object and a codes data frame to check if all
#' names in the TopoJSON data match the provided codes, grouped by zone. If not,
#' an error message is returned with a list of mismatched names.
#' The function can handle both TopoJSON files and 'sf' objects.
#'
#' @param tj A TopoJSON object or a file path to a TopoJSON file.
#' @param codes A data frame containing codes with a 'name' column and a 'zone' column to compare with the TopoJSON names.
#' @return NULL if all names match, otherwise an error message is displayed with the list of mismatched names.
#' @importFrom sf read_sf
#' @importFrom dplyr group_split filter
#' @importFrom purrr walk
#' @examples
#' tj <- gd_tj("col_municipalities")
#' codes <- gd_regions("col_municipalities")
#' check_names_in_topojson2(tj, codes)
#' @export
check_names_in_topojson2 <- function(tj, codes){
  if(!"sf" %in% class(tj)){
    tj <- read_sf(tj)
  }
  tj_list <- tj |> group_split(zone)
  purrr::walk(tj_list, function(tjj) {
    current_zone <- unique(tjj$zone)
    message(current_zone)
    zone_codes <- codes |> dplyr::filter(zone == current_zone)
    check_names_in_topojson(tjj, zone_codes)
  })
}




#' Change TopoJSON names with new names
#'
#' This function takes a TopoJSON object and a new_names data frame to replace the
#' original names in the TopoJSON data. If a write_path is provided, the modified
#' TopoJSON will be saved to the specified file path.
#' The function can handle both TopoJSON files and 'sf' objects.
#'
#' @param tj A TopoJSON object or a file path to a TopoJSON file.
#' @param new_names A data frame containing the original names in the 'name' column and the new names in the 'new_name' column.
#' @param write_path An optional file path where the modified TopoJSON will be saved. Default is NULL (no file will be saved).
#' @return The modified TopoJSON object with updated names.
#' @importFrom sf read_sf st_drop_geometry
#' @importFrom dplyr select coalesce
#' @examples
#' tj <- gd_tj("col_municipalities")
#' new_names_data_frame <- data.frame(name = c("BOGOTA D.C.", "SAN ANDRES"), new_name = c("Bogotá", "Archipielago de San Andrés"))
#' modified_tj <- change_topojson_names(tj, new_names_data_frame)
#' @export
change_topojson_names <- function(tj, new_names, write_path = NULL){
  if(!"sf" %in% class(tj)){
    tj <- read_sf(tj)
  }
  nms <- st_drop_geometry(tj) |> dplyr::select(name)
  nms <- left_join(nms, new_names, by = "name")
  tj$name <- coalesce(nms$new_name, nms$name)
  if(!is.null(write_path))
    write_topojson(tj, write_path)
  tj
}



#' Check if all IDs in TopoJSON data match the provided codes
#'
#' This function takes a TopoJSON object and a codes data frame to check if all IDs in the TopoJSON data
#' match the provided codes. If not, an error message is returned with a list of mismatched IDs.
#' The function can handle both TopoJSON files and 'sf' objects.
#'
#' @param tj A TopoJSON object or a file path to a TopoJSON file.
#' @param codes A data frame containing codes with an 'id' column to compare with the TopoJSON IDs.
#' @return NULL if all IDs match, otherwise an error message is displayed with the list of mismatched IDs.
#' @importFrom sf read_sf
#' @examples
#' tj <- gd_tj("col_departments")
#' codes <-  centroids_from_topojson(tj)
#' check_ids_in_topojson(tj, codes[1:5,])
#' check_ids_in_topojson(tj, codes[1:5,]) # error with ids missings
#' @export
check_ids_in_topojson <- function(tj, codes){
  if(!"sf" %in% class(tj)){
    tj <- read_sf(tj)
  }
  if(!all(codes$id %in% tj$id) || !all(tj$id %in% codes$id))
    stop("Not all codes id in topojson id: ",
         collapse_char(which_not_in(codes$id, tj$id)), "\n  ",
         "Not all topojson id in code id: ",
         collapse_char(which_not_in(tj$id, codes$id))
    )
}


#' Change TopoJSON IDs based on provided codes
#'
#' This function takes a TopoJSON object and a codes data frame to change the IDs
#' in the TopoJSON data based on the provided codes. The function can handle both
#' TopoJSON files and 'sf' objects. Optionally, the modified TopoJSON object can be
#' written to a file.
#'
#' @param tj A TopoJSON object or a file path to a TopoJSON file.
#' @param codes A data frame containing codes with 'id' and 'name' columns to update the TopoJSON IDs.
#' @param write_path (Optional) A file path to write the modified TopoJSON object. Default is NULL.
#' @return A TopoJSON object with updated IDs based on the provided codes.
#' @importFrom sf read_sf st_drop_geometry
#' @importFrom dplyr left_join filter rename
#' @examples
#' tj <- gd_tj("col_municipalities")
#' codes <- data.frame(name = c("NECOCLI", "BARRANQUILLA"), id = c("5490", "8001"))
#' modified_tj <- change_topojson_names(tj, codes)
#' @keywords internal
change_topojson_ids <- function(tj, codes, write_path = NULL){
  if(!"sf" %in% class(tj)){
    tj <- read_sf(tj)
  }
  tj_codes <- st_drop_geometry(tj) |>
    rename(id_old = id)
  tj_codes <- left_join(tj_codes, codes, by = "name")

  # verify all values are joined
  no_match <- tj_codes |> filter(is.na(id))
  if(nrow(no_match) > 0)
    stop("No match in: ", collapse_char(no_match$name))

  tj2 <- tj
  tj2$id <- tj_codes$id

  if(!is.null(write_path))
    write_topojson(tj, write_path)
  tj2
}



#' Simplify a TopoJSON object
#'
#' This function simplifies a TopoJSON object to a specified output size while preserving the original shape.
#'
#' @param tj A TopoJSON object to be simplified.
#' @param output_size The desired output size (in bytes) of the simplified TopoJSON object. Default is 3e5.
#' @return A simplified TopoJSON object.
#' @importFrom pryr object_size
#' @importFrom rmapshaper ms_simplify
#' @examples
#' tj <- gd_tj("col_municipalities")
#' simplified_topojson <- gd_simplify(tj, output_size = 0.10)
#' @export
gd_simplify <- function(tj, output_size = 3e5){
  init_size <- pryr::object_size(tj)
  message(paste0("init size: ", init_size))
  keep <- 1.2 * output_size/as.numeric(init_size)
  tj_out <- rmapshaper::ms_simplify(tj, keep = keep,
                                    keep_shapes = TRUE)

  message(paste0("out size: ", pryr::object_size(tj_out)))
  tj_out
}


