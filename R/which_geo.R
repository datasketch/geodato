#' Identify the column with geocode data in a dataframe
#'
#' This function takes a dataframe and a map name as inputs, and returns the name
#' of the column that has the geocode data for the specified map. If data have
#' more than 50 rows, the function looks at the first 50 rows of the dataframe
#' and checks if any column has values that match the geocode ids for the given map.
#' The column with the highest number of matching geocode ids is returned, provided
#' that at least 90% of the values in that column agree with the geocode.
#'
#' @param d A dataframe containing the data to be analyzed.
#' @param map_name A character string specifying the name of the map to be used,
#' you can view available maps with geodato::available_maps().
#' @return The name of the column with geocode data, or NA if no column meets the
#' 90\% agreement threshold.
#' @importFrom dplyr slice arrange filter pull
#' @importFrom purrr map_df
#' @importFrom tidyr pivot_longer
#' @examples
#' df <- data.frame(id_country = c("ARG", "COL", "AGO", "BRA"), value = runif(4))
#' which_geocode_col(df, "world_countries")
#' @keywords internal
which_geocode_col <- function(d, map_name){
  x <- d |> dplyr::slice(1:50)
  code_counts <- purrr::map_df(x, function(col){
    sum(str_clean(col) %in% str_clean(gd_codes(map_name)$id))
  })
  if(sum(code_counts) == 0) return(NA)
  code_top_count <- code_counts |>
    tidyr::pivot_longer(everything()) |>
    dplyr::filter(value > 0) |>
    dplyr::arrange(desc(value)) |>
    dplyr::slice(1)
  # At least 90% of values should agree with the geocode
  if(code_top_count$value < 0.9 * nrow(d)) return(NA)
  code_top_count <- code_top_count |>
    dplyr::pull(name)
  code_top_count
}


#' Identify the column with geoname data in a dataframe
#'
#' This function takes a dataframe and a map name as inputs, and returns the name
#' of the column that has the geoname data for the specified map. If data have
#' more than 50 rows, the function looks at the first 50 rows of the dataframe
#' and checks if any column has values that match the geocode ids for the given map.
#' The column with the highest number of matching geocode ids is returned, provided
#' that at least 90% of the values in that column agree with the geocode.
#'
#' @param d A dataframe containing the data to be analyzed.
#' @param map_name A character string specifying the name of the map to be used,
#' you can view available maps with geodato::available_maps().
#' @return The name of the column with geoname data, or NA if no column meets the
#' 90\% agreement threshold.
#' @examples
#' df <- data.frame(country = c("Argentina", "Colombia", "Angora", "Brasil"), value = runif(4))
#' which_geoname_col(df, "world_countries")
#' @keywords internal
which_geoname_col <- function(d, map_name){

  x <- d |> dplyr::slice(1:50)
  name_counts <- purrr::map_df(x, function(col){
    sum(str_clean(col) %in% gd_possiblenames(map_name, with_parent = FALSE))
  })
  name_top_count <- name_counts |>
    tidyr::pivot_longer(everything()) |>
    dplyr::filter(value > 0) |>
    dplyr::arrange(desc(value)) |>
    dplyr::slice(1) |>
    dplyr::pull(name)

  parent <- gd_parent_map_name(map_name)
  if(!is.null(parent)){
    name_counts_parent <- purrr::map_df(x, function(col){
      sum(str_clean(col) %in% gd_possiblenames(parent, with_parent = FALSE))
    })
    name_top_count_parent <- name_counts_parent |>
      tidyr::pivot_longer(everything()) |>
      dplyr::filter(value > 0) |>
      dplyr::arrange(desc(value)) |>
      dplyr::slice(1) |>
      dplyr::pull(name)
    name_top_count <- unique(c(name_top_count, name_top_count_parent))
  }
  name_top_count
}

#' Check if input values are codes or names for a given map
#'
#' This function checks if the input values are codes or names for the specified map.
#'
#' @param v A vector of values to be checked.
#' @param map_name The name of the map for which the values should be checked.
#' @return A character string: "code" if the majority of the input values are codes, "name" otherwise.
#' @examples
#' is_code_or_name(c("Bogotá", "Quindio", "Caldas", "Atlantico"), "col_departments")
#' @export
is_code_or_name <- function(v, map_name){
  v <- v[1:min(50,length(v))]
  codes <- gd_codes(map_name)
  n_codes <- sum(v %in% codes$id)
  n_names <- sum(str_clean(v) %in% gd_possiblenames(map_name))
  if(n_codes > n_names) return("code")
  return("name")
}

