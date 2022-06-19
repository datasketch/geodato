

#' @export
availablegeodato <- function(){
  lifecycle::deprecate_stop(when = "1.1", "availablegeodato()",
                            "available_maps()")
  names(geodatoMeta())
}

#' @export
geodatoMeta <- function(mapName = NULL, load_data = FALSE, debug = FALSE){
  lifecycle::deprecate_stop(when = "1.1", "geodatoMeta()",
                            "geodato:::gd_meta()")
  if(!is.null(mapName)) load_data <- TRUE
  dir <- system.file("meta",package="geodato", mustWork=TRUE)
  files <- list.files(dir,pattern = ".*.yaml",full.names = TRUE)
  l <- purrr::map(files,function(x){
    #x <- files[[11]]
    if(debug) message("\n--- ",basename(x))
    ll <- yaml::yaml.load_file(x)
    purrr::map(ll, function(y){
      #y <- ll[[1]]
      y$geoname = basename(file_path_sans_ext(x))
      if(!"basename" %in% names(y))
        stop("No basename in yaml: ", y)
      codesFilename <- system.file(file.path("geodato",y$geoname,paste0(y$basename, ".csv")),package = "geodato")
      if(debug) message("codes: ",codesFilename)
      if(!file.exists(codesFilename)){
        #stop("File ",codesFilename, " does not exist")
        y$codes <- NULL
      }else{
        if(load_data){
          y$codes <- noWrnMsg(readr::read_csv(codesFilename,
                                              col_types = readr::cols(id = 'c', name='c')))
        }else{
          y$codes <- codesFilename
        }
      }

      regionFilename <- file.path("geodato",y$geoname,paste0(y$basename, "-regions.csv"))
      if(file.exists(system.file(regionFilename, package = "geodato"))){
        if(debug) message("regions: ",regionFilename)
        if(load_data){
          y$regions <- noWrnMsg(readr::read_csv(system.file(regionFilename, package = "geodato"),
                                                col_types = readr::cols(id = 'c')))
        }else{
          y$regions <- regionFilename
        }
      }else{
        y$regions <- NULL
      }
      altnamesFilename <- file.path("geodato",y$geoname,paste0(y$basename, "-altnames.csv"))
      if(file.exists(system.file(altnamesFilename, package = "geodato"))){
        if(debug) message("altnames: ", altnamesFilename)
        if(load_data){
          y$altnames <- noWrnMsg(readr::read_csv(system.file(altnamesFilename, package = "geodato"),
                                                 col_types = readr::cols(id = 'c')))
        }else{
          y$altnames <- altnamesFilename
        }
      }else{
        y$altnames <- NULL
      }
      y$codes_name_col <- y$codes_name_col %||% "name"
      y
    })
  }) %>% purrr::flatten()
  if(!is.null(mapName)){
    return(l[[mapName]])
  }
  l
}


#' @export
geodatoCodes <- function(mapName = NULL, load_data = FALSE){
  lifecycle::deprecate_stop(when = "1.1", "geodatoCodes()",
                            "geodato:::gd_codes()")
  dm <- geodatoMeta(mapName, load_data = load_data)
  dm$codes
}



#' @export
geodatoAltnames <- function(mapName = NULL, load_data = FALSE){
  lifecycle::deprecate_stop(when = "1.1", "geodatoAltnames()",
                            "geodato:::gd_altnames()")
  dm <- geodatoMeta(mapName, load_data = load_data)
  dm$altnames
}



#' @export
geodatoPolygon <- function(mapName = NULL){
  lifecycle::deprecate_stop(when = "1.1", "geodatoAltnames()",
                            "geodato:::gd_tj()")
  dm <- geodatoMeta(mapName, load_data = load_data)
  path <- file.path("geodato", dm$geoname, paste0(dm$basename,".topojson"))
  dm$centroids <- file.path("geodato", dm$geoname, paste0(dm$basename, ".csv"))
  tj <- topojson_read(system.file(path, package = "geodato"))
  data_map <- ggplot2::fortify(tj) %>% dplyr::mutate(.id = as.numeric(id)) %>%
    dplyr::select(-id)
  data_info <- tj@data %>% mutate(.id = 0:(nrow(.) - 1))
  left_join(data_map, data_info)
}

#' @export
geodatoProjections <- function(mapName){
  lifecycle::deprecate_stop(when = "1.1", "geodatoProjections()",
                            "geodato:::gd_tj()")
  l <- geodatoMeta(mapName)
  names(l$projections)
}

#' @export
geodatoProjectionOptions <- function(mapName, projection, withDefaults = TRUE){
  lifecycle::deprecate_stop(when = "1.1", "geodatoProjectionOptions()",
                            "NULL")
  l <- geodatoMeta(mapName)
  if(!projection %in% names(l$projections))
    stop(mapName, "does not support this projection")
  projection <- l$projections[[projection]]
  if(!withDefaults) return(names(projection))
  projection
}

#' @export
geodatoCsv <- function(mapName){
  lifecycle::deprecate_stop(when = "1.1", "geodatoCsv()",
                            "geodato:::gd_codes()")
  geodatoMeta(mapName)$codes
}

#' @export
geodatoTopojsonPath <- function(mapName){
  lifecycle::deprecate_stop(when = "1.1", "geodatoTopojsonPath()",
                            "geodato:::gd_tj()")
  y <- geodatoMeta(mapName, load_data = FALSE)
  system.file(file.path("geodato",y$geoname,paste0(y$basename, ".topojson")),package = "geodato")
}

#' @export
geodatoCsvPath <- function(mapName){
  lifecycle::deprecate_stop(when = "1.1", "geodatoCsvPath()",
                            "geodato:::gd_codes()")
  y <- geodatoMeta(mapName, load_data = FALSE)
  system.file(file.path("geodato",y$geoname,paste0(y$basename, ".csv")),package = "geodato")
}


#' @export
geodatoRdsPath <- function(mapName){
  lifecycle::deprecate_stop(when = "1.1", "geodatoRdsPath()",
                            "geodato:::gd_tj()")
  y <- geodatoMeta(mapName, load_data = FALSE)
  system.file(file.path("geodato",y$geoname,paste0(y$basename, "-centroids.rds")),package = "geodato")
}


#' @export
geoinfo <- function(mapName) {
  lifecycle::deprecate_stop(when = "1.1", "geoinfo()",
                            "geodato:::gd_meta()")
  dir <- system.file("meta",package="geodato", mustWork=TRUE)
  files <- list.files(dir,pattern = ".*.yaml",full.names = TRUE)
  l <- purrr::map(files,function(x){
    ll <- yaml::yaml.load_file(x)
    purrr::map(ll, function(y){
      geoprep <- NULL
      geoprep$geoname <- basename(file_path_sans_ext(x))
      geoprep$basename <- y$basename
      geoprep
    })
  }) %>% purrr::flatten()

  geoprep <- l[[mapName]]
  centroids_rds <- readr::read_rds(file =
                                     system.file(
                                       file.path("geodato", geoprep$geoname,paste0(geoprep$basename, "-centroids.rds")),
                                       package = "geodato"
                                     )
  )
  topo_sf <- sf::read_sf(dsn =
                           system.file(
                             file.path("geodato", geoprep$geoname,paste0(geoprep$basename, ".topojson")),
                             package = "geodato"
                           ))
  topo_rds <- readr::read_rds(file =
                                system.file(
                                  file.path("geodato", geoprep$geoname,paste0(geoprep$basename, ".rds")),
                                  package = "geodato"
                                ))

  list (
    centroids = centroids_rds,
    geo_sf = topo_sf,
    geo_rds = topo_rds
  )
}

