# https://rforjournalists.com/2020/12/15/how-to-access-open-street-map-in-r/
library(osmdata)
library(tidyverse)
library(sf)
library(ggimage)
# named area method
town <- 'Cambridge'
location <- town |> opq()
wenvoe <- c(lat = 52.18920355562345, long = 0.17807149360630456)
# lat and long box method (good for larger cities)
# 52.18920355562345, 0.17807149360630456
#coords <- matrix(
#  c(
#  0.17807149360630456, 0.171,
#  52.18920355562345, 52.1
#  ),
#  byrow = TRUE,
#  nrow = 2, ncol = 2,
#  dimnames = list(c('x', 'y'), c('min', 'max'))
#)
#location <- coords %>% opq()

# define features
osmdata::available_features()
osmdata::available_tags('building')

main_st <- data.frame(type = c("motorway","trunk","primary","motorway_junction","trunk_link","primary_link","motorway_link"))
st <- data.frame(type = available_tags('highway'))
st <- subset(st, !type %in% main_st$type)
path <- data.frame(type = c("footway","path","steps","cycleway"))
st <- subset(st, !type %in% path$type)
st <- as.character(st$type)
main_st <- as.character(main_st$type)
path <- as.character(path$type)

main_streets <- location |>
  add_osm_feature(key = 'highway', value = main_st) |>
  osmdata_sf()
streets <- location |>
  add_osm_feature(key = 'highway', value = st) |>
  osmdata_sf()
water <- location |>
  add_osm_feature(key = 'natural',
                  value = c('water')) |>
  osmdata_sf()
rail <- location |>
  add_osm_feature(key = 'railway', value = c('rail')) |>
  osmdata_sf()
parks <- location |>
  add_osm_feature(key = 'leisure', value = c('park', 'nature_reserve', "recreation_ground","golf_course","pitch","garden")) |>
  osmdata_sf()
buildings <- location |>
  add_osm_feature(key = 'amenity',
                  value = c('cafe'
                            #,'university'
                            )) |>
  osmdata_sf()

# plot
coords <- data.frame(loc = location[["bbox"]]) |> 
  separate(loc, c('a', 'b', 'c', 'd'), sep = ',')

ggplot() + geom_sf(data = water$osm_multipolygons, fill = 'light blue') + theme_minimal()
ggplot() + 
  # add streets
  geom_sf(data = main_streets$osm_lines, color = '#ff9999', size = 2) + 
  geom_sf(data = streets$osm_lines, size = 1.5, color = '#eedede') +
  # add water
  geom_sf(data = water$osm_polygons, fill = '#c6e1e3') +
  geom_sf(data = water$osm_multipolygons, fill = '#c6e1e3') +
  # rail
  geom_sf(data = rail$osm_lines, color = '#596060', size = 1) +
  # parks
  geom_sf(data = parks$osm_polygons, fill = '#94ba8e') +
  # buildings (cafes)
  geom_sf(data = buildings$osm_points, 
          color = '#40493f', fill = '#40493f', 
          size = 1.1, shape = 15) +
  coord_sf(ylim = c(52.14, 52.25), 
           xlim = c(0.06, 0.20),
           expand = FALSE) +
  geom_point(aes(x = wenvoe[2], y = wenvoe[1]),
             colour = "red", shape = '\u2665', size = 2) +
  theme_void() -> cambridge
cambridge

ggsave("../../../../Desktop/cambridge.png", cambridge, dpi = 320,
       bg = 'white', units = 'px', width = 2500, height = 2500)

# to add hearts
# https://stackoverflow.com/questions/54803891/custom-shape-in-ggplot-geom-point
# https://www.compart.com/en/unicode/search?q=heart#characters

# to do 
# set-up as github repo > then make project in R > then make Tess colab

# add buildings
# make buildings and cafes different colours
# generally adjust colours
# try and functionalise things
