
#' Match Geodato Data to a Geodato Map
#'
#' This function matches input data to a geodato map, based on the provided map_name
#' and the specified column.
#'
#' @param d The input data frame to be matched.
#' @param map_name The name of the geodato map.
#' @param col The column name or index to be used for matching. If NULL, the function
#' will try to guess the appropriate column for matching.
#' @param centroids A logical value indicating whether to include centroids in the
#'  output. Defaults to TRUE.
#' @return A data frame with the matched data and centroids if requested with ..gd_id and ..gd_name.
#' @examples
#' data <- readr::read_csv("data-raw/sample_data/col_departments/plebiscito_2016_departamentos.csv")
#' gd_match(data, map_name = "col_departments")
#' @export
gd_match <- function(d, map_name, col = NULL, centroids = TRUE){

  validate_map_name(map_name)

  if(is.null(d)) return(NULL)
  out <- NULL
  if(is.null(col)){
    #if(length(col == 2)) stop("TODO: auto match parent_geography")
    geocode_col <- which_geocode_col(d, map_name)
    if(!is.na(geocode_col)){
      # Guess first which is the geocode col
      out <- gd_match_codes(d, map_name = map_name, col = geocode_col)
    } else{
      # If none found try names
      geoname_col <- which_geoname_col(d, map_name)
      if(!all(is.na(geoname_col))){
        if(length(geoname_col) == 1){
          out <- gd_match_names(d, map_name = map_name, col = geoname_col)
        }
        if(length(geoname_col) == 2){
          out <- gd_match_names2(d, map_name = map_name, col = geoname_col)
        }
      }
    }
  } else{
    col <- parse_col(d, col)

    if(length(col) == 1){
      code_or_name <- is_code_or_name(d[[col]], map_name)
      if(code_or_name == "code"){
        out <- gd_match_codes(d, map_name = map_name, col = col)
      }
      if(code_or_name == "name"){
        out <- gd_match_names(d, map_name = map_name, col = col)

      }
    }
    if(length(col) == 2){
      out <- gd_match_names2(d, map_name = map_name, col = col)
    }
  }
  if(centroids){
    centroids <- gd_centroids(map_name) |>
      dplyr::rename(..gd_id = id, ..gd_name = name)
    if(all(c("zone", "zone_id") %in% names(centroids)))
      centroids <- centroids |>
        dplyr::rename(..gd_zone_id = zone_id, ..gd_zone = zone)
    out <- out |> dplyr::left_join(centroids)
  }
  out
}



#' Match Geodato Data to a Geodato Map by Codes
#'
#' This function matches input data to a geodato map based on the provided map_name and the specified code column.
#'
#' @param d The input data frame to be matched.
#' @param map_name The name of the geodato map. Defaults to NULL.
#' @param col The column name or index to be used for matching by codes. Defaults to NULL.
#' @return A data frame with the matched data based on codes.
#' @examples
#' data <- data.frame(id_depto = c("05", "08", "81", "11"), value = runif(4))
#' gd_match_codes(data, map_name = "col_departments")
#' gd_match_codes(data, map_name = "col_departments", "id_depto")
#' @export
gd_match_codes <- function(d, map_name = NULL, col = NULL){
  if(is.null(map_name)){
    stop("Need a map_name to match")
  }
  validate_map_name(map_name)
  col <- parse_col(d, col)

  join_by <- "id"
  names(join_by) <- col

  codes <- gd_codes(map_name) |> rename_dotdot()
  codes$id <- codes$..gd_id

  # Quick fix when the codes in input table are numbers
  if(is.character(codes$id)){
    d[[col]] <- as.character(d[[col]])
  }

  if(grepl("col_departments", map_name)){
    d[[col]] <- leading_zeros(d[[col]], 2)
  }
  if(grepl("col_municipalities", map_name)){
    d[[col]] <- leading_zeros(d[[col]], 5)
  }

  dplyr::left_join(d, codes, by = join_by)

}

#' Match Geodato Data to a Geodato Map by Names
#'
#' This function matches input data to a geodato map based on the provided map_name
#'  and the specified name column.
#'
#' @param d The input data frame to be matched.
#' @param map_name The name of the geodato map. Defaults to NULL.
#' @param col Optional column with name to be used for matching by names. Defaults to NULL.
#' @param codes Optional codes data frame to be used for matching. Defaults to NULL.
#' @param altnames Optional alternative names data frame to be used for matching. Defaults to NULL.
#' @return A data frame with the matched data based on names.
#' @examples
#' data <- data.frame(id_name = c("Antioquia", "Bogota", "Quindio", "Caldas"), value = runif(4))
#' gd_match_names(data, map_name = "col_departments")
#' @export
gd_match_names <- function(d, map_name = NULL, col = NULL,
                           codes = NULL, altnames = NULL){
  if(is.null(map_name)){
    stop("Need a map_name to match")
  }
  validate_map_name(map_name)
  col <- parse_col(d, col)

  join_by <- "..gd_clean_name"

  if(is.null(codes)){
    codes0 <- gd_codes(map_name) |> rename_dotdot()
    # Add altnames if they exist
    altnames <- gd_altnames(map_name)
    if(!is.null(altnames)){
      altnames <- altnames |> purrr::set_names(names(codes0)[1:2])
      codes <- dplyr::bind_rows(codes0, altnames)
    } else{
      codes <- codes0
    }
  } else{
    codes0 <- codes  |> rename_dotdot()
    if(!is.null(altnames)){
      altnames <- altnames |> purrr::set_names(names(codes0)[1:2])
      codes <- dplyr::bind_rows(codes0, altnames)
    }else{
      codes <- codes0
    }
  }

  codes$..gd_clean_name <- str_clean(codes$..gd_name)
  codes <- codes |>
    dplyr::distinct(..gd_id, ..gd_clean_name, .keep_all = TRUE)
  d$..gd_clean_name <- str_clean(d[[col]])

  match <- dplyr::left_join(d, codes, by = join_by)
  helper_cols <- c("..gd_clean_name", "..gd_name","..gd_zone", "..gd_zone_id")
  match <- match |> dplyr::select(-any_of(helper_cols))
  # Match again with original codes and not altnames
  match |> dplyr::left_join(codes0, by = "..gd_id")
}

#' Match Geodato Data to a Geodato Map by Names with Two Columns
#'
#' This function matches input data to a geodato map based on the provided map_name and two specified name columns.
#'
#' @param d The input data frame to be matched.
#' @param map_name The name of the geodato map. Defaults to NULL.
#' @param col A vector of column names or indices to be used for matching by names. Must have a length of 2.
#' @return A data frame with the matched data based on names.
#' @examples
#' data <- data.frame(id_depto = c("Antioquia", "Antioquia", "Quindio", "Quindio", "Quindio"), muni = c("ALEJANDRIA", "CHIGORODO", "ARMENIA", "BUENAVISTA", "CALARCA"), valor = runif(5))
#' gd_match_names2(data, map_name = "col_municipalities", col = c("muni", "id_depto"))
#' @export
gd_match_names2 <- function(d, map_name = NULL, col = NULL){
  if(is.null(map_name)){
    stop("Need a map_name to match")
  }
  if(!length(col) == 2)
    stop("Need two columns to match region by name and parent region")
  validate_map_name(map_name)
  # The first column has the id
  col <- parse_col(d, col)

  join_by <- "..gd_clean_name"

  # get codes
  codes0 <- gd_codes(map_name) |> rename_dotdot()
  # Add altnames if they exist
  altnames <- gd_altnames(map_name)
  if(!is.null(altnames)){
    ncols <- ncol(altnames)
    altnames <- altnames |>
      purrr::set_names(names(codes0)[1:ncols])
    codes <- dplyr::bind_rows(codes0, altnames)
  } else{
    codes <- codes0
  }

  # First match zone_id
  d_parent <- d |> dplyr::select(all_of(col[2])) |> dplyr::distinct()
  d_parent_match <- gd_match_names(d = d_parent,
                                   map_name = gd_parent_map_name(map_name))

  dl <- d |> dplyr::group_split(across(col[2]))

  l <- purrr::map(dl, function(dd){
    #dd <- dl[[13]]
    parent <- unique(dd[[col[2]]])
    # message(parent)
    parent_gd_id <- d_parent_match$..gd_id[d_parent_match[[1]] == parent]
    parent_codes <- codes |> dplyr::filter(..gd_zone_id == parent_gd_id)
    gd_match_names(dd, map_name = map_name, col = col[1],codes = parent_codes)
  })
  l |> dplyr::bind_rows()
}


#' Get Rows with No Match in Geodato Data
#'
#' This function returns the rows in the input data frame that do not match the
#'  specified geodato map.
#'
#' @param d The input data frame to be checked for non-matching rows.
#' @param map_name The name of the geodato map. Defaults to NULL.
#' @param col A column name or index to be used for matching. Defaults to NULL.
#' @return A data frame containing rows with no match in the geodato data.
#' @examples
#' data <- data.frame(geo_name = c("AR", "COL", "PER", "EC", "BLA"), val = runif(5))
#' gd_no_match(data, map_name = "world_countries", col = "geo_name")
#' @export
gd_no_match <- function(d, map_name, col = NULL){
  match <- gd_match(d, map_name, col = col)
  match |> dplyr::filter(is.na(..gd_id))
}



