
#' @export
gd_match <- function(d, map_name, col = NULL){
  if(is.null(d)) return(NULL)
  out <- NULL
  if(is.null(col)){
    if(length(col == 2)) stop("TODO: auto match parent_geography")
    geocode_col <- which_geocode_col(d, map_name)
    if(!is.na(geocode_col)){
      # Guess first which is the geocode col
      out <- gd_match_codes(d, map_name = map_name, col = geocode_col)
    } else{
      # If none found try names
      geoname_col <- which_geoname_col(d, map_name)
      if(!is.na(geoname_col)){
        out <- gd_match_names(d, map_name = map_name, col = geoname_col)
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
  out
}



#' @export
gd_match_codes <- function(d, map_name = NULL, col = NULL){
  if(is.null(map_name)){
    stop("Need a map_name to match")
  }
  validate_map_name(map_name)
  col <- parse_col(d, col)

  join_by <- "id"
  names(join_by) <- col

  codes <- gd_codes(map_name) %>% rename_dotdot()
  codes$id <- codes$..gd_id

  if(map_name == "col_departments"){
    d[[col]] <- leading_zeros(d[[col]], 2)
  }

  dplyr::left_join(d, codes, by = join_by)

}

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
    codes0 <- gd_codes(map_name) %>% rename_dotdot()
    # Add altnames if they exist
    altnames <- gd_altnames(map_name)
    if(!is.null(altnames)){
      altnames <- altnames %>% purrr::set_names(names(codes0)[1:2])
      codes <- dplyr::bind_rows(codes0, altnames)
    } else{
      codes <- codes0
    }
  } else{
    codes0 <- codes  %>% rename_dotdot()
    if(!is.null(altnames)){
      altnames <- altnames %>% purrr::set_names(names(codes0)[1:2])
      codes <- dplyr::bind_rows(codes0, altnames)
    }
  }

  codes$..gd_clean_name <- str_clean(codes$..gd_name)
  codes <- codes %>%
    dplyr::distinct(..gd_id, ..gd_clean_name, .keep_all = TRUE)
  d$..gd_clean_name <- str_clean(d[[col]])

  match <- dplyr::left_join(d, codes, by = join_by)
  helper_cols <- c("..gd_clean_name", "..gd_name","..gd_zone", "..gd_zone_id")
  match <- match %>% dplyr::select(-any_of(helper_cols))
  # Match again with original codes and not altnames
  match %>% dplyr::left_join(codes0, by = "..gd_id")
}

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
  codes0 <- gd_codes(map_name) %>% rename_dotdot()
  # Add altnames if they exist
  altnames <- gd_altnames(map_name)
  if(!is.null(altnames)){
    altnames <- altnames %>% purrr::set_names(names(codes0)[1:2])
    codes <- dplyr::bind_rows(codes0, altnames)
  } else{
    codes <- codes0
  }

  # First match zone_id
  d_parent <- d %>% dplyr::select(all_of(col[2])) %>% dplyr::distinct()
  d_parent_match <- gd_match_names(d = d_parent,
                                   map_name = gd_parent_map_name(map_name))

  dl <- d %>% dplyr::group_split(across(col[2]))

  l <- purrr::map(dl, function(dd){
    #dd <- dl[[13]]
    parent <- unique(dd[[col[2]]])
    # message(parent)
    parent_gd_id <- d_parent_match$..gd_id[d_parent_match[[1]] == parent]
    parent_codes <- codes %>% dplyr::filter(..gd_zone_id == parent_gd_id)
    gd_match_names(dd, map_name = map_name, codes = parent_codes)
  })
  match <- l %>% dplyr::bind_rows()
  dplyr::left_join(d, match, by = c(col, "value"))

}


parse_col <- function(d, col = NULL){
  if(is.null(col)){
    col <- names(d)[1]
  }else{
    if(is.numeric(col)) col <- names(d)[col]
    if(!all(col %in% names(d)))
      stop("Column not found in table")
  }
  col
}

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
    dplyr::slice(1) |>
    dplyr::pull(name)
  code_top_count
}

which_geoname_col <- function(d, map_name){
  x <- d |> dplyr::slice(1:50)
  name_counts <- purrr::map_df(x, function(col){
    sum(str_clean(col) %in% gd_possiblenames(map_name))
  })
  name_top_count <- name_counts |>
    tidyr::pivot_longer(everything()) |>
    dplyr::filter(value > 0) |>
    dplyr::arrange(desc(value)) |>
    dplyr::slice(1) |>
    dplyr::pull(name)
  name_top_count
}

is_code_or_name <- function(v, map_name){
  v <- v[1:min(50,length(v))]
  codes <- gd_codes(map_name)
  n_codes <- sum(v %in% codes$id)
  n_names <- sum(str_clean(v) %in% gd_possiblenames(map_name))
  if(n_codes > n_names) return("code")
  return("name")
}



