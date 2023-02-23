

#' @export
gd_sample <- function(map_name, frtype = NULL){

  x <- geodato::sample_data[[map_name]]

}

available_samples <- function(){
  x <- geodato::sample_data
  map(x, ~ names(.))
}
