
### Función para crear carpeta en data-raw/files

#' @export
new_geo_pipeline <- function(country, adm1,
                             adm2 = NULL,
                             adm3 = NULL){

  if(nchar(country) > 3){
    stop("ONly ISO country reference is valid")
  }

  if(is.null(country) | is.null(adm1)){
    stop("Need a country and a minimum split reference or sufix of a lower layer than the country")
  }

  country <- tolower(country)
  adm1 <- tolower(adm1)
  if(!is.null(adm2)) {adm2 <- tolower(adm2)}
  if(!is.null(adm3)) {adm3 <- tolower(adm3)}


  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##          Creates the country in files folder if it doesn´t exist         ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if(!dir.exists(glue::glue("data-raw/files/{country}"))){

    dir.create(glue::glue("data-raw/files/{country}"))

  } else {
    cat("The country folder already exist in file folder \n")
  }


  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##          Creates the country in geodato folder if it doesn´t exist         ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if(!dir.exists(glue::glue("data-raw/geodato/{country}"))){

    dir.create(glue::glue("data-raw/geodato/{country}"))

  } else {
    cat("The country folder already exist in geodato folder \n")
  }



  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                        Create the subregion folder in files              ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if(is.null(adm2) & is.null(adm3)){

    if(!dir.exists(glue::glue("data-raw/files/{country}/{country}_{adm1}"))){

      dir.create(glue::glue("data-raw/files/{country}/{country}_{adm1}"))
      file.create(glue::glue("data-raw/files/{country}/{country}_{adm1}/{country}_{adm1}_geo_downloader.R"))

      cat("A geo_downloader script was created. Please, if posible, write the necesary code to reproduce how to obtain the layer")
    }
  }

  if(!is.null(adm2) & is.null(adm3)){

    if(!dir.exists(glue::glue("data-raw/files/{country}/{country}_{adm1}_{adm2}"))){

      dir.create(glue::glue("data-raw/files/{country}/{country}_{adm1}_{adm2}"))
      file.create(glue::glue("data-raw/files/{country}/{country}_{adm1}/{country}_{adm1}_{adm2}_geo_downloader.R"))

      cat("A geo_downloader script was created. Please, if posible, write the necesary code to reproduce how to obtain the layer")

    }
  }

  if(!is.null(adm2) & !is.null(adm3)){

    if(!dir.exists(glue::glue("data-raw/files/{country}/{country}_{adm1}_{adm2}_{adm3}"))){

      dir.create(glue::glue("data-raw/files/{country}/{country}_{adm1}_{adm2}_{adm3}"))
      file.create(glue::glue("data-raw/files/{country}/{country}_{adm1}/{country}_{adm1}_{adm2}_{adm3}_geo_downloader.R"))

      cat("A geo_downloader script was created. Please, if posible, write the necesary code to reproduce how to obtain the layer")

    }
  }



  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                        Create the subregion folder in geodato              ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(is.null(adm2) & is.null(adm3)){

    if(!dir.exists(glue::glue("data-raw/geodato/{country}/{country}_{adm1}"))){

      dir.create(glue::glue("data-raw/geodato/{country}/{country}_{adm1}"))

    }
  }

  if(!is.null(adm2) & is.null(adm3)){

    if(!dir.exists(glue::glue("data-raw/geodato/{country}/{country}_{adm1}_{adm2}"))){

      dir.create(glue::glue("data-raw/geodato/{country}/{country}_{adm1}_{adm2}"))

    }
  }

  if(!is.null(adm2) & !is.null(adm3)){

    if(!dir.exists(glue::glue("data-raw/geodato/{country}/{country}_{adm1}_{adm2}_{adm3}"))){

      dir.create(glue::glue("data-raw/geodato/{country}/{country}_{adm1}_{adm2}_{adm3}"))

    }
  }
}

#create_files_folder(country = "argentina", adm1 = "provincia")
