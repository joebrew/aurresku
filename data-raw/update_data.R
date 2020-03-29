library(dplyr)
library(readr)
library(usethis)
library(raster)
library(sp)
library(rgdal)

## Get Spanish census data
# Population by age and municipality
# https://www.ine.es/jaxi/Tabla.htm?path=/t20/e244/avance/p02/l0/&file=1mun00.px&L=0
download_url <- 'https://www.ine.es/jaxi/files/_px/es/csv_bdsc/t20/e244/avance/p02/l0/1mun00.csv_bdsc?nocab=1'
download_file <- '1mun00.csv'
if(!download_file %in% dir()){
  download.file(url = download_url,
                destfile = download_file)
}
# Read in
census <- read_delim(download_file, delim = ';')
# Clean up names
names(census) <- c('municipio',
                   'sexo',
                   'edad',
                   'periodio',
                   'total')
census$periodio <- NULL
# Remove unnecessary values
census <- census %>%
  filter(!municipio %in% 'TOTAL NACIONAL',
         !sexo %in% 'Ambos sexos',
         !edad %in% 'Total')
census$total <- ifelse(census$total == '..',
                       0,
                       as.numeric(as.character(gsub('.', '', 
                                                    census$total, fixed = T))))
census$edad <- gsub('años|año', '', census$edad)
census$edad <- gsub(' y más', '', census$edad)
census$edad <- as.numeric(as.character(census$edad))

census$id <- unlist(lapply(strsplit(census$municipio, ' '),
                                  function(x){paste0(x[1], collapse = ' ')}))
census$id <- trimws(census$id)
census$id <- as.numeric(census$id)
census$municipio <- unlist(lapply(strsplit(census$municipio, ' '),
                           function(x){paste0(x[2:length(x)], collapse = ' ')}))
census$municipio <- trimws(census$municipio)
usethis::use_data(census, overwrite = TRUE)

# Get Spain at municipal level
# Must download teh following as 'lineas_limite.zip'
# http://centrodedescargas.cnig.es/CentroDescargas/catalogo.do?Serie=CAANE
# and then unzip
# unzip('lineas_limite.zip')
municipios <- readOGR('recintos_municipales_inspire_peninbal_etrs89',
                      'recintos_municipales_inspire_peninbal_etrs89', encoding="UTF-8")
# Get an ID in municipios
census_ids <- sort(unique(census$id))
census_ids <- sample(census_ids, length(census_ids))
ids <- substr(municipios$INSPIREID, 20, 24)

municipios$id <- as.numeric(ids)
usethis::use_data(municipios, overwrite = TRUE)

# World cities
cities <- read_csv('worldcities.csv')
cities <- cities %>% filter(country == 'Spain')
cities <- cities %>% dplyr::select(city, lat, lng, population)
cities_sp <- cities %>% dplyr::mutate(x = lng, y = lat)
coordinates(cities_sp) <- ~x+y
proj4string(cities_sp) <- proj4string(municipios)
usethis::use_data(cities, overwrite = T)
usethis::use_data(cities_sp, overwrite = T)
