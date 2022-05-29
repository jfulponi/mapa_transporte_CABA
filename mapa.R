## Inspiration and base code from https://twitter.com/UrbanDemog/status/1416043653416558597


easypackages::packages("sf",
                       "raster",
                       "stars",
                       "r5r",
                       "aopdata",
                       "ggplot2",
                       "osmdata",
                       "h3jsr",
                       "viridisLite",
                       "ggnewscale",
                       "dplyr",
                       "magrittr",
                       prompt = FALSE
)

#ESTACIONAMIENTOS, AVENIDAS Y AUTOPISTAS, CICLOVÍAS, SUBTES Y FFCC, LINEAS DE BONDIS, POBLACION
barrios <- st_read("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/barrios.geojson")
bbox <- st_bbox(barrios)
metrobus <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/metrobus/recorrido-de-metrobus.geojson")
metrobus_r <-  st_intersection( st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/metrobus/recorrido-de-metrobus.geojson"), barrios)
ffcc_e <- st_intersection(st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/estaciones-de-ferrocarril/estaciones-de-ferrocarril.geojson"), barrios)
ffcc_r <- st_intersection(st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/estaciones-de-ferrocarril/red-de-ferrocarril.geojson"), barrios)
subte_e <- st_intersection(st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/subte-estaciones/subte_estaciones.geojson"), barrios)
subte_r <- st_intersection(st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/subte-estaciones/subte_lineas.geojson"), barrios)
ciclo <- st_intersection(st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/transporte/ciclovias/ciclovias_WGS84.geojson"), barrios)
ecobici <- st_intersection(st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/transporte/estaciones-bicicletas-publicas/nuevas-estaciones-bicicletas-publicas.geojson"), barrios)
bondis_r <- st_intersection(st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/colectivos/recorrido-colectivos.geojson"), barrios)


roads <- opq('Ciudad de Buenos Aires') %>%
  add_osm_feature(key = 'highway',
                  value = c("motorway", "primary","secondary")) %>% osmdata_sf()

roads <- roads$osm_lines

# crop
roads2 <- roads[bbox,]
roads2 <- st_intersection(roads2, barrios)


pob <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/informacion-censal-por-radio/caba_radios_censales.geojson")
plot(pob)

hex <- st_make_grid(pob, cellsize = .008, square = FALSE)
colnames(hex)[1] <- "geometry"
plot(hex)
st_geometry(hex) <- "geometry"
hex <- st_as_sf(hex)
hex$id <- 1:nrow(hex)

hex_pob <- st_intersection(hex, pob)

hex_pob2 <- st_drop_geometry(hex_pob) %>%
  group_by(id) %>%
  summarise(pob = sum(POBLACION))

hex_pob3 <- st_as_sf(merge(hex_pob2, hex))


rotate_data <- function(data, x_add = 0, y_add = 0) {
  
  shear_matrix <- function(){ matrix(c(2, 1.2, 0, 1), 2, 2) }
  
  rotate_matrix <- function(x){ 
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
  }
  data %>% 
    dplyr::mutate(
      geometry = .$geometry * shear_matrix() * rotate_matrix(pi/20) + c(x_add, y_add)
    )
}
library(raster)

rotate_data_geom <- function(data, x_add = 0, y_add = 0) {
  shear_matrix <- function(){ matrix(c(2, 1.2, 0, 1), 2, 2) }
  
  rotate_matrix <- function(x) { 
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
  }
  data %>% 
    dplyr::mutate(
      geom = .$geom * shear_matrix() * rotate_matrix(pi/20) + c(x_add, y_add)
    )
}


### plots

x = -58.43
color = 'gray40'

temp1 <- ggplot() +
  
  # pob
  geom_sf(data = hex_pob3 %>% rotate_data(), aes(fill=pob), color=NA, show.legend = FALSE) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  #annotate("text", label='Población', x=x, y= -34.0, hjust = 0, color=color) +
  labs(caption = "@juanif__")


temp2 <- temp1 +
  
  # ffcc y subtes
  geom_sf(data = ffcc_r %>% rotate_data(y_add = .2), color='darkblue', size=.6, alpha=.8) +
  geom_sf(data = ffcc_e %>% rotate_data(y_add = .2), color='darkblue', size=1.5, alpha=.8) +
  geom_sf(data = subte_r %>% rotate_data(y_add = .2), color='purple', size=.35, alpha=.8) +
  geom_sf(data = subte_e %>% rotate_data(y_add = .2), color='purple', size=1.1, alpha=.8) +
  theme(legend.position = "none")+
  # annotate("text", label='Transporte Ferroviario', x=x, y= -34.8, hjust = 0, color=color) +
  
  # bondis
  geom_sf(data = bondis_r %>% rotate_data(y_add = .3), color='#d5303e', size=.1, alpha=.5) +
  geom_sf(data = metrobus_r %>% rotate_data(y_add = .3), color='green', size=.5, alpha=.5) +
  # annotate("text", label='Red de buses', x=x, y= -34.7, hjust = 0, color=color) +
  
  # OSM
  new_scale_color()+
  geom_sf(data = roads2 %>% rotate_data(y_add = .5), aes(size=as.numeric(lanes), color = highway)) +
  scale_size_continuous(range = c(0.1,0.7))+
  theme(legend.position = "none")+
  new_scale("size")+
  new_scale_color()+
  
  #  annotate("text", label='Avenidas y Autopistas', x=x, y= -34.6, hjust = 0, color=color) +
  
  # estacionamientos
  geom_sf(data = garages %>% rotate_data(y_add = .4),aes(color=sqrt(as.numeric(PISOS_16)), size = as.numeric(PISOS_16))) +
  scale_size_continuous(range = c(0.01, 0.02))+
  scale_color_viridis_c()+
  theme(legend.position = "none")+
  new_scale("size")+
  # annotate("text", label='Estacionamientos', x=x, y= -34.5, hjust = 0, color=color) +
  #color='#0f3c53'
  # sustentables
  geom_sf(data = ciclo %>% rotate_data(y_add = .1), color='darkgreen', size=.2, alpha = .6) +
  geom_sf(data = ecobici %>% rotate_data(y_add = .1), color='darkgreen', size=.7, alpha = .6) +
  theme(legend.position = "none")+
    # annotate("text", label='Red de Ciclovías y Ecobici', x=x, y= -34.5, hjust = 0, color=color)  +
  theme_void() #+
  scale_x_continuous(limits = c(-58.55, -58.2))

temp2


# save plot
ggsave(plot = temp2, filename = 'mapa_transporte.png', 
       dpi=200, width = 15, height = 16, units='cm')


