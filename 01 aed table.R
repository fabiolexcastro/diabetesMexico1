
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, lubridate, openxlsx, fs, sf, readxl, tidyverse, gtools, rgeos, stringr, rgeoda, glue)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

# Load data ---------------------------------------------------------------
tble <- read_excel('./tbl/Diabetes historico 2.xlsx')
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
  colnames(tbl)[20] <- 'Edad'
  
  smm <- tbl %>%
    filter(Pagos > 0) %>% 
    group_by(Siniestro, ENTIDAD, SEXO) %>% 
    summarise(Pagos = sum(Pagos, na.rm = TRUE), 
              Edad = mean(Edad, na.rm = TRUE)) %>% 
    ungroup() %>% 
    setNames(c('gid', 'ENTIDAD', 'Sexo', 'Pagos', 'Edad')) %>% 
    mutate(year = yr) %>% 
    relocate(year) %>% 
    arrange(ENTIDAD)
  cat('Done!\n')
  return(smm)
})

# Pivot wider
fish_encounters %>% pivot_wider(names_from = station, values_from = seen)
us_rent_income %>% pivot_wider(names_from = variable, values_from = c(estimate, moe))
us_rent_income %>%
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe),
    names_vary = "slowest"
  )
sort(unique(shpf$ENTIDAD))
sort(unique(smmr$ENTIDAD))

setdiff(sort(unique(smmr$ENTIDAD)), sort(unique(shpf$ENTIDAD)))

# To group again 
smmr2 <- smmr %>% 
  group_by(year, ENTIDAD, Sexo) %>% 
  summarise(count = n(), Pagos = sum(Pagos), Edad = mean(Edad)) %>% 
  ungroup() %>% 
  mutate(year = glue('y{year}')) 
smmr2 <- smmr2 %>% pivot_wider(names_from = c(Sexo), values_from = c(Pagos, Edad, count), names_sep = '_') 
smmr2[is.na(smmr2)] <- 0

# Filtering != 2022
smmr2 <- filter(smmr2, year != 'y2022')
write.xlsx(smmr2, './tbl/processed/table_summary_entidad.xlsx')

# To make the join --------------------------------------------------------
length(unique(smmr2$ENTIDAD))
length(unique(smmr2$year))

fnal <- inner_join(shpf, smmr2, by = 'ENTIDAD')
st_write(fnal, './gpkg/shape_data.gpkg', append = FALSE)

nrow(smmr)
nrow(fnal)
