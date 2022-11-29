
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, showtext, spdep, classInt, extrafont, maptools, ggrepel, rgeoda, lubridate, ggspatial, cowplot, ggpubr, cptcity, rnaturalearthdata, rnaturalearth, RColorBrewer, openxlsx, fs, sf, readxl, tidyverse, gtools, rgeos, stringr, rgeoda, glue)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

# Fonts -------------------------------------------------------------------
font_add_google(family = 'Roboto', name = 'Roboto Condensed')
showtext_auto()

# Load data ---------------------------------------------------------------
shpf <- st_read('./gpkg/shape_data_mean.gpkg')
mex1 <- st_read('./shp/destdv1gw/destdv1gw.shp')
wrld <- ne_countries(returnclass = 'sf', scale = 50)

# Convert to shapefile object
shpf <- as(shpf, 'Spatial')
spplot(shpf, 'Pagos_F')

# Process data ------------------------------------------------------------

makeModel <- function(year){
  
  year <- 'y2020'
  
  # Filtramos datos
  shp <- shpf[shpf@data$year == year,]
  spplot(shp, 'Pagos_F')
  
  # Calculamos matrix de pesos espaciales 
  pr.nb <- poly2nb(shp, queen = TRUE)
  wqueen <- nb2listw(pr.nb, style = 'W')
  
  qnwg <- queen_weights(st_as_sf(shp), order = 1, precision_threshold = 0.10)
  qnwg <- rook_weights(st_as_sf(shp), order = 1, precision_threshold = 0.10)
  
  # Centroides
  cent <- coordinates(shp)
  plot(shp, border = 'grey', lwd = 0.5)
  plot(pr.nb, cent, add = T, col = 'darkred')
  
  # Calcular el estadistico de moran 
  
  # Femenino
  # moran_f <- moran.test(shp$Pagos_F, wqueen, randomisation = TRUE, alternative = 'two.sided', na.action = na.exclude)
  moran_f <- local_moran(qnwg, st_drop_geometry(st_as_sf(shp)['Pagos_F']))
  moran_lbls <- lisa_labels(moran_f)
  moran_clrs <- setNames(lisa_colors(moran_f), moran_lbls)
  
  shp <- shp %>% st_as_sf %>% mutate(cluster_num = lisa_clusters(moran_f) + 1, 
                                     cluster = factor(moran_lbls[cluster_num], levels = moran_lbls))
  
  
  ggplot(shp, aes(fill = cluster)) +
    geom_sf(color = "white", size = 0) +
    scale_fill_manual(values = moran_clrs, na.value = "green") +
    theme_dark()
  
  moran.plot(shp$Pagos_F, wqueen, pch = 20)
  
  brks_f <- round(quantile(shp$Pagos_F, probs = seq(0, 1, 0.25)), digits = 2)
  colours <- brewer.pal(4, 'Reds')
  plot(shp, col = colours[findInterval(shp$Pagos_F, brks_f, all.inside = T)], axes = F)
  legend(x = -95, y = 26, legend = leglabs(brks_f), fill = colours, bty = 'n')
  invisible(title(main = 'Pagos femenino'))
  box()
  
  # Masculino
  moran_m <- moran.test(shp$Pagos_M, wqueen, randomisation = TRUE, alternative = 'two.sided', na.action = na.exclude)
  
  
  
  
  
  
  
  
  
}

