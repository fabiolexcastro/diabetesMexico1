
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, lubridate, fs, sf, readxl, tidyverse, gtools, rgeos, stringr, rgeoda, glue)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

# Load data ---------------------------------------------------------------
tble <- read_excel('./tbl/Diabetes historico.xlsx')
shpf <- st_read('./shp/mex1.shp')

# Summary  ----------------------------------------------------------------

# By each year
colnames(tble)
tble <- mutate(tble, year = year(tble$`Fecha Movimiento`))
tble <- relocate(tble, year, .before = Siniestro...1)
year <- unique(tble$year)

smmr <- purrr::map_dfr(.x = 1:length(year), .f = function(i){
  cat(year[i], '\n')
  yr  <- year[i]
  tbl <- filter(tble, year == yr)
  colnames(tbl)[15] <- 'ENTIDAD'
  colnames(tbl)[2] <- 'Siniestro'
  smm <- tbl %>% 
    group_by(Siniestro, ENTIDAD) %>% 
    summarise(Pagos = sum(Pagos, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(Pagos > 0) %>% 
    setNames(c('gid', 'ENTIDAD', 'Pagos')) %>% 
    mutate(year = yr) %>% 
    relocate(year)
  cat('Done!\n')
  return(smm)
})

sort(unique(shpf$ENTIDAD))
sort(unique(smmr$ENTIDAD))

setdiff(sort(unique(smmr$ENTIDAD)), sort(unique(shpf$ENTIDAD)))

# To group again 
smmr <- smmr %>% 
  group_by(year, ENTIDAD) %>% 
  summarise(count = n(), Pagos = sum(Pagos)) %>% 
  ungroup() %>% 
  mutate(year = glue('y{year}')) 

# To make the join --------------------------------------------------------
fnal <- inner_join(shpf, smmr, by = 'ENTIDAD')
st_write(fnal, './gpkg/shape_data.gpkg')

nrow(smmr)
nrow(fnal)
