

#' @export
read_meta <- function(path){
  l <- yaml::yaml.load_file(fs::path(path, "meta.yaml"))
  map_name <- fs::path_file(path)
  l$map_name <- map_name

  # Validate centroids
  l$centroids <- read_csv(fs::path(path, l$centroids),
                          col_types = cols(.default = "c"))
  geodato::validate_centroids(l$centroids, map_name)
  # Validate altnames
  if(!is.null(l$altnames)){
    l$altnames <- read_csv(fs::path(path, l$altnames),
                           col_types = cols(.default = "c"))
    geodato::validate_altnames(l$altnames, map_name)
  }
  # Validate altids
  if(!is.null(l$altids)){
    l$altids <- read_csv(fs::path(path, l$altids),
                           col_types = cols(.default = "c"))
    geodato::validate_altids(l$altids, map_name)
  }
  if(!is.null(l$parent_map_name)){
    geodato::validate_map_name(l$parent_map_name)
  }
  # Validate topojson
  tj <- sf::read_sf(file.path(path, l$topojson$path))
  geodato::validate_topojson(tj,
                             scope = l$topojson$scope,
                             centroids = l$centroids,
                             map_name)
  l$tj <- tj
  l
}

#' @export
read_meta_path <- function(path){
  l <- yaml::yaml.load_file(fs::path(path, "meta.yaml"))
  map_name <- fs::path_file(path)
  l$map_name <- map_name

  # Validate centroids
  l$centroids <- read_csv(fs::path(path, l$centroids_file),
                          col_types = cols(.default = "c",
                                           lat = "n", lon = "n"))
  validate_centroids(l$centroids, map_name)

  if(!is.null(l$parent_map_name)){
    codes <- l$centroids %>% dplyr::select(id, name, zone, zone_id)
  } else{
    codes <- l$centroids %>% dplyr::select(id, name)
  }

  # Validate altnames
  if(!is.null(l$altnames_file)){
    l$altnames <- read_csv(fs::path(path, l$altnames_file),
                           col_types = cols(.default = "c"))
    #validate_altnames(l$altnames, map_name)
    if(!all(c("id", "altname") %in% names(l$altnames)))
      stop("No id and altname: ", map_name)


    if(!all(gsub("alt","", names(l$altnames)) == names(l$codes)))
      stop("Altnames colnames must be the same as centroids for: ", map_name)
  }
  # Validate altids
  if(!is.null(l$altids_file)){
    l$altids <- read_csv(fs::path(path, l$altids_file),
                         col_types = cols(.default = "c"))
    validate_altids(l$altids, map_name)
  }
  # if(!is.null(l$parent_map_name)){
  #   geodato::validate_map_name(l$parent_map_name)
  # }
  # Validate topojson
  tj <- sf::read_sf(file.path(path, l$topojson$path))
  geodato::validate_topojson(tj,
                             scope = l$topojson$scope,
                             centroids = l$centroids,
                             map_name)
  l$tj <- tj
  l
}


