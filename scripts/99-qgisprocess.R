# setup ---------------------------------------------------------------------------------
require(pacman)
pacman::p_load(magrittr, rgdal, ggplot2, sp, sf, data.table, tictoc, ggplot2,
               tidyverse, stringr, dplyr, tibble, geojsonio, readr, glue,
               webdriver, purrr, cli, fs)
install.packages("remotes")
remotes::install_github("r-spatial/qgisprocess")

# library(arcgisbinding)
# arc.check_product()
library(qgisprocess)
qgis_configure(use_cached_data = TRUE)
qgis_algorithms() %>%  view()

# read geojson files in data folder  ----------------------------------------------------
path<-"data/"
jsonnames<-c("mpuntos_fusion.geojson", "mlineas_fusion.geojson")


# read into R and adjust columns  -------------------------------------------------------
for(json in jsonnames){
  cat("cargando", json, "...", '\n\r')
  nombre<-str_extract(json, "\\S+(?=\\.)")
  assign(str_glue("{nombre}"), st_read(str_glue("{path}{json}"), as_tibble=TRUE))
}
mpuntos <- st_cast(mpuntos_fusion, "POINT")

coords <- st_coordinates(mpuntos) # Extract the coordinates of each point
zero_coords <- (coords[,1] == 0) & (coords[,2] == 0) # Identify the points with zero coordinates
mpuntos <- mpuntos[!zero_coords, ] # Subset the shapefile to exclude points with zero coordinates
mpuntos<-mpuntos %>% select(cod_bip, geometry)

qgis_algorithms(query = FALSE, quiet = TRUE, "native:dissolve")
