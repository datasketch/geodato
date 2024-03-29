
geodato_sys <- function(...){
  system.file(..., package = "geodato")
}

which_in <- function (x, y) x[x %in% y]

which_not_in <- function (x, y) x[!x %in% y]

collapse_char <- function(x) paste0(x, collapse = ", ")

noWrnMsg <- function(x){
  suppressWarnings(suppressMessages(x))
}

leading_zeros <- function(x, n = 1){
  if(!is.numeric(x)) x <- as.numeric(x)
  sprintf(paste0("%0",n,"d"), x)
}




#' @export
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


rename_dotdot <- function(x){
  no_dotdot_idx <- !grepl("^\\.\\.gd", names(x))
  names(x)[no_dotdot_idx] <- paste0("..gd_", names(x)[no_dotdot_idx])
  x
}

str_clean <- function(x){
  x <- as.character(iconv(remove_accents(tolower(x)), to = "ASCII//TRANSLIT"))
  tolower(x)
}

remove_accents <- function (string){
  accents <- "àèìòùÀÈÌÒÙáéíóúýÁÉÍÓÚÝäëïöüÄËÏÖÜâêîôûÂÊÎÔÛñÑç"
  translation <- "aeiouAEIOUaeiouyAEIOUYaeiouAEIOUaeiouAEIOUnNc"
  chartr(accents, translation, string)
}

trim_punct <- function(x){
  gsub("[[:punct:]]", "", x)
}

match_replace <- function (v, dic, na = NA, force = TRUE){
  matches <- dic[[2]][match(v, dic[[1]])]
  out <- matches
  if(!is.na(na)){
    na_to_chr(out, na)
  }
  if (!force)
    out[is.na(matches)] <- v[is.na(matches)]
  out
}


`%||%` <- function (x, y)
{
  if (is.empty(x))
    return(y)
  else if (is.null(x) || is.na(x))
    return(y)
  else if (class(x) == "character" && nchar(x) == 0)
    return(y)
  else x
}

is.empty <- function (x)
{
  !as.logical(length(x))
}

file_path_sans_ext <- function (x){
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}

url_exists <- function(x, printUrl = FALSE){
  r <- !httr::http_error(x)
  if(printUrl)
    message(r, ": ", x)
  r
}
