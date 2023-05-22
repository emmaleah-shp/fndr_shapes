# setup ---------------------------------------------------------------------------------
require(pacman)
pacman::p_load(magrittr, rgdal, ggplot2, sp, sf, data.table, tictoc, stringr, dplyr,
               tibble, geojsonio, readr, glue, webdriver, purrr, cli, fs)
library(tidyverse)

# library(arcgisbinding)
# arc.check_product()
# install.packages("webdriver")
# webdriver::install_phantomjs()

getwd()

# start and session -------------------------------------------------------
pjs <- run_phantomjs()
# pjs

# session
url_geo_server <- "https://sitmds.ministeriodesarrollosocial.gob.cl/geoserver/web/?wicket:bookmarkablePage=:org.geoserver.web.demo.MapPreviewPage"
url_geo_json_template <- "https://sitmds.ministeriodesarrollosocial.gob.cl/geoserver/sit/ows?service=WFS&version=1.0.0&request=GetFeature&typeName={name}&maxFeatures=80000"

ses <- Session$new(port = pjs$port)
ses$go(url_geo_server)

# descargar tabla ---------------------------------------------------------
# son 10 páginas
# revisa cada fila por página y guarda geojson si existe  codigo_bip o cod_bip
tbl_geoserver <- map(1:10, function(i = 1){

  cli::cli_progress_step("Page {i}")

  tbl_id <- ses$findElement("table")$getAttribute("id")

  tbl_html <- ses$executeScript(str_glue("return document.getElementById('{ tbl_id }').innerHTML ;"))
  tbl_html <- str_c("<table>", tbl_html, "<table>", collapse = "")

  tbl <- tbl_html |>
    rvest::read_html() |>
    rvest::html_table() |>
    first() |>
    janitor::clean_names() |>
    mutate(page = i, .before = 1)

  # glimpse(tbl)

  # avanzar de página
  ses$findElement(".next")$click()
  Sys.sleep(4)

  tbl

})

tbl_geoserver <- list_rbind(tbl_geoserver)


# descarga archivos -------------------------------------------------------
archivos <- tbl_geoserver |> pull(name)
xx<-archivos %>% str_detect("fusion")
archivos<-archivos[xx]

# revisa que archivos tienen columnas bip y las descarga
resultados <- map(archivos, safely(function(name = "sit:mpuntos_fusion"){

  cli::cli_progress_step(name)

  nn <- str_replace_all(name, " ", "%20")

  url_geo_json <- str_glue(url_geo_json_template, name = nn)

  layer <- st_read(dsn = url_geo_json, quiet = TRUE)
  # mapview::mapview(layer)

  # si tiene codigo_bip o cod_bip
  if(any(str_detect(names(layer), "codigo_bip|cod_bip"))){

    nn <- str_remove_all(name, "^sit|\\:+")
    layer |>
      select(-contains("bbox")) |>
      st_write(
        fs::path("data", nn, ext = "geojson"),
        driver = "GeoJSON",
        quiet = TRUE,
        delete_dsn = TRUE)

    cli::cli_alert_success("writing geojson {nn}")
    return("descargado")
  } else {
    return("no posee bip")
  }
}))


# read geojson files in data folder  ----------------------------------------------------
path<-"data/"
jsonnames<-c("mpuntos_fusion.geojson", "mlineas_fusion.geojson")


# read into R and adjust columns  -------------------------------------------------------
for(json in jsonnames){
  cat("cargando", json, "...", '\n\r')
  nombre<-str_extract(json, "\\S+(?=\\.)")
  assign(str_glue("{nombre}"), st_read(str_glue("{path}{json}"), as_tibble=TRUE))
}

# dissolve function  --------------------------------------------------------------------
dissolve<-function(sf, tipo1){
  tic()
  n1= nrow(sf)
  suppressWarnings(sf1<- st_cast(sf, tipo1))
  n2= nrow(sf1)
  if(tipo1=="POINT"){
    coords <- st_coordinates(sf1) # Extract the coordinates of each point
    zero_coords <- (coords[,1] == 0) & (coords[,2] == 0) # Identify the points with zero coordinates
    sf1 <- sf1[!zero_coords, ] # Subset the shapefile to exclude points with zero coordinates
    sf1<-sf1 %>% select(cod_bip, geometry)
    n3= n2 - nrow(sf1)
  }else{
    sf1<-sf1 %>% select(cod_bip, geometry)
    n3= 0
  }
  sf1<-sf1[!duplicated(sf1),]
  sf1<-sf1 %>% filter(str_count(cod_bip)>=8, cod_bip!="undefined")
  sf1$cod_bip<-substr(sf1$cod_bip, 1, 8)
  n4=nrow(sf1)
  shp<-sf1 |> group_by(cod_bip) |> dplyr::summarize(st_combine(geometry)) |> dplyr::ungroup()
  n5 = nrow(shp)
  shp<-st_cast(shp, str_glue("MULTI{tipo1}"))
  cat("INFORME: \n",
  "1) SF original venía con ", str_glue("{n1}"), " filas. \n",
  "2) Se expandió a ", str_glue("{n2}"), " filas en paso dos. \n",
  "3) Se borró ", str_glue("{n3}"), " filas con geometria de c(0,0). \n",
  "4) En paso cuatro, se eliminó ", str_glue("{n4}"), " filas duplicadas y filas con errores en su cod_bip. \n",
  "5) Al final, el dissolve nos deja con un SF con ", str_glue("{n5}"), " filas. ")
  toc()
  shp
}

# perform dissolve function  ------------------------------------------------------------
puntos_dissolve<-dissolve(mpuntos_fusion, "POINT")
lineas_dissolve<-dissolve(mlineas_fusion, "LINESTRING")

# export as shapes into folder*  --------------------------------------------------------
st_write(puntos_dissolve, str_glue("{path}shapes/puntos_dissolve.shp"))
st_write(lineas_dissolve, str_glue("{path}shapes/lineas_dissolve.shp"))

