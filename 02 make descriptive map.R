
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, showtext, extrafont, lubridate, ggspatial, cowplot, ggpubr, cptcity, rnaturalearthdata, rnaturalearth, RColorBrewer, openxlsx, fs, sf, readxl, tidyverse, gtools, rgeos, stringr, rgeoda, glue)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

# Fonts -------------------------------------------------------------------
font_add_google(family = 'Roboto', name = 'Roboto Condensed')
showtext_auto()

# Load data ---------------------------------------------------------------
shpf <- st_read('./gpkg/shape_data.gpkg')
mex1 <- st_read('./shp/destdv1gw/destdv1gw.shp')
wrld <- ne_countries(returnclass = 'sf', scale = 50)
unique(shpf$ENTIDAD)

# Get the years
years <- sort(unique(shpf$year))
yr <- 'y2008'

# Function to make the map ------------------------------------------------
makeMap_Sexo <- function(yr){
  
  # To filter the data
  cat(yr, '\n')
  shp <- filter(shpf, year == yr)
  
  # To make the map ---------------------------------------------------------
  
  # Femenino
  g_p_f <- ggplot() + 
    geom_sf(data = shp, aes(fill = Pagos_F), col = 'grey60', lwd = 0.5) + 
    scale_fill_gradientn(colors = rev(brewer.pal(n = 9, name = 'RdGy'))) +
    geom_sf(data = mex1, fill = NA, col = 'grey50', lwd = 0.3) + 
    geom_sf(data = wrld, fill = NA, col = 'grey40', lwd = 0.2) +
    geom_sf_text(data = shp, aes(label = ENTIDAD), family = 'Roboto', size = 6.5) +
    coord_sf(xlim = ext(mex1)[1:2], ylim = ext(mex1)[3:4]) +
    labs(x = 'Lon', y = 'Lat', fill = 'Pagos (MXN$)') +
    ggtitle(label = glue('Pagos a asegurados por siniestros de Diabetes'), 
            subtitle = glue('Sexo: Femenino - Año: {gsub("y", "", yr)}')) +
    theme_bw() + 
    theme(legend.position = 'bottom', 
          legend.key.width = unit(4, 'line'), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.title = element_text(family = 'Roboto', size = 50, face = 'bold', hjust = 0.5),
          plot.subtitle = element_text(family = 'Roboto', size = 50, face = 'bold', hjust = 0.5),
          axis.text.y = element_text(angle = 90, hjust = 0.5, family = 'Roboto', size = 30), 
          axis.text.x = element_text(family = 'Roboto', size = 30), 
          axis.title = element_text(family = 'Roboto', size = 40, face = 'bold'),
          legend.title = element_text(family = 'Roboto', face = 'bold', size = 50), 
          legend.text = element_text(family = 'Roboto', size = 40))
    
  ggsave(plot = g_p_f, filename = glue('./png/maps/map_pagosF_{yr}.png'), units = 'in', width = 9, height = 7, dpi = 300)
  
  # Masculino
  g_p_m <- ggplot() + 
    geom_sf(data = shp, aes(fill = Pagos_M), col = 'grey60', lwd = 0.5) + 
    scale_fill_gradientn(colors = rev(brewer.pal(n = 9, name = 'RdGy'))) +
    geom_sf(data = mex1, fill = NA, col = 'grey50', lwd = 0.3) + 
    geom_sf(data = wrld, fill = NA, col = 'grey40', lwd = 0.2) +
    geom_sf_text(data = shp, aes(label = ENTIDAD), family = 'Roboto', size = 6.5) +
    coord_sf(xlim = ext(mex1)[1:2], ylim = ext(mex1)[3:4]) +
    labs(x = 'Lon', y = 'Lat', fill = 'Pagos (MXN$)') +
    ggtitle(label = glue('Pagos a asegurados por siniestros de Diabetes'), 
            subtitle = glue('Sexo: Masculino - Año: {gsub("y", "", yr)}')) +
    theme_bw() + 
    theme(legend.position = 'bottom', 
          legend.key.width = unit(4, 'line'), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.title = element_text(family = 'Roboto', size = 50, face = 'bold', hjust = 0.5),
          plot.subtitle = element_text(family = 'Roboto', size = 50, face = 'bold', hjust = 0.5),
          axis.text.y = element_text(angle = 90, hjust = 0.5, family = 'Roboto', size = 30), 
          axis.text.x = element_text(family = 'Roboto', size = 30), 
          axis.title = element_text(family = 'Roboto', size = 40, face = 'bold'),
          legend.title = element_text(family = 'Roboto', face = 'bold', size = 50), 
          legend.text = element_text(family = 'Roboto', size = 40))
  
  ggsave(plot = g_p_m, filename = glue('./png/maps/map_pagosM_{yr}.png'), units = 'in', width = 9, height = 7, dpi = 300)
  
  g_p <- ggpubr::ggarrange(g_p_f, g_p_m, ncol = 2, nrow = 1, common.legend = T, legend = 'bottom')
  ggsave(plot = g_p, filename = glue('./png/maps/map_pagosFM_{yr}.png'), units = 'in', width = 14, height = 7, dpi = 300)
  cat('Done!\n')
  
}



