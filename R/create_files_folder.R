
### Función para crear carpeta en data-raw/files

#' @export
create_files_folder <- function(country, geo_div){

  country <- tolower(substr(country, 1, 3))
  geo_div <- tolower(geo_div)

  file <- paste()

  # Creates the country folder if it doesn´t exist
  if(!dir.exists(glue::glue("data-raw/files/{country}"))){

    dir.create(glue::glue("data-raw/files/{country}"))

  } else {
    cat("The country folder already exist \n")
  }

  if(!dir.exists(glue::glue("data-raw/files/{country}/{country}_{geo_div}"))){

    dir.create(glue::glue("data-raw/files/{country}/{country}_{geo_div}"))
    file.create(glue::glue("data-raw/files/{country}/{country}_{geo_div}/{country}_{geo_div}_geo_downloader.R"))

    cat("A geo_downloader script was created. Please, if posible, write the necesary code to reproduce how to obtain the layer and, if necesary, convert it to a topoJSON file")

  }
  #
  # if(!file.exists(glue::glue("{country}_{geo_div}_geo_downloader.R"))){
  #   file.create(glue::glue("data-raw/files/{country}/{country}_{geo_div}/{country}_{geo_div}_geo_downloader.R"))
  # }

  ## Extraer los códigos del país de la base de códigos "world"

}
#create_files_folder(country = "argentina", geo_div = "provincia")
