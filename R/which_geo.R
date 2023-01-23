



which_geocode_col <- function(d, map_name){
  x <- d |> dplyr::slice(1:50)
  code_counts <- purrr::map_df(x, function(col){
    sum(str_clean(col) %in% gd_codes(map_name)$id)
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

#' @export
is_code_or_name <- function(v, map_name){
  v <- v[1:min(50,length(v))]
  codes <- gd_codes(map_name)
  n_codes <- sum(v %in% codes$id)
  n_names <- sum(str_clean(v) %in% gd_possiblenames(map_name))
  if(n_codes > n_names) return("code")
  return("name")
}

