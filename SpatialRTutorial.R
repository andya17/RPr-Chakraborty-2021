install.packages("dplyr")
install.packages("sf")
install.packages("spData")
install.packages("here")
install.packages("ggplot2")
install.packages("tmap")
install.packages("ggspatial")

library(dplyr)
library(sf)
library(spData)
library(here)

list.files(system.file("shapes", package = "spData"))
cycle_hire <- st_read(system.file("shapes/cycle_hire.geojson", package="spData"))

cycle_hire <- cycle_hire %>% 
  mutate(slots = nbikes +  nempty)

data(lnd)

# 4.2

cycle_hire_27700 <- cycle_hire %>%
  st_transform(crs = st_crs(27700))

london_27700 <- lnd %>%
  st_transform(crs = st_crs(27700))

plot(london_27700$geometry) # we just want to plot the geometry column
plot(cycle_hire_27700$geometry, 
     col = "red",  # color
     cex = 0.5,    # size of symbol
     add = TRUE)   # important parameter to create multilayer plots

# 4.3

cycle_hire_27700 %>% inner_join(
  london_27700 %>%
    st_drop_geometry(), # we don't need the geometry here
  by = c( "area" = "NAME")
  
)

cycle_hire_27700 <- cycle_hire_27700 %>% st_join(london_27700 %>% select(GSS_CODE))
cycle_hire_27700 %>% filter(is.na(GSS_CODE))

## ------ 4.4 Aggregation -------

cycle_hire_by_area <- cycle_hire_27700 %>%
  filter(!is.na(GSS_CODE)) %>% # remove NAs
  st_drop_geometry() %>% # let's put geometry aside
  group_by(GSS_CODE) %>%  # group data by GSS_CODE
  tally(name = "count", sort= TRUE) # Aggregate
cycle_hire_by_area

cycle_hire_by_area_sum <- cycle_hire_27700 %>%
  filter(!is.na(GSS_CODE)) %>% # remove NAs
  st_drop_geometry() %>% # let's put geometry aside
  group_by(GSS_CODE) %>%  # group data by GSS_CODE
  summarise(sum = sum(nbikes), count = n()) # Aggregate
cycle_hire_by_area_sum

aggregate(cycle_hire_27700["nbikes"], by = list(cycle_hire_27700$"GSS_CODE"),
          FUN = sum, na.rm = TRUE)

## ----- 4.5 Centroids -----

boroughs_centroids <- london_27700 %>%
  select(NAME, GSS_CODE) %>% # only keep useful columns
  st_centroid()

## ----- 4.6 Geometric binary predicates -----

london_27700 %>% 
  filter(NAME == "Wandsworth") %>% 
  st_contains(cycle_hire_27700)

cycle_hire_27700 %>% filter(id == "614") %>% 
  st_within(london_27700) # borough at index 22

london_27700[unlist(cycle_hire_27700 %>% filter(id == "614") %>% st_within(london_27700)),]

## ----- 4.7 Saving -----

london_27700 %>% left_join(cycle_hire_by_area_sum) %>%
  st_write(
    dsn = here("foss4g_R_workshop.gpkg"),
    layer = "london_boroughs_27700", 
    layer_options = "OVERWRITE=true")

boroughs_centroids %>%
  left_join(cycle_hire_by_area_sum) %>%
  st_write(
    dsn = here("foss4g_R_workshop.gpkg"),
    layer = "boroughs_centroids_27700", 
    layer_options = "OVERWRITE=true")

cycle_hire_27700 %>%
  left_join(cycle_hire_by_area_sum) %>%
  st_write(
    dsn = here("foss4g_R_workshop.gpkg"),
    layer = "cycle_hire_27700",
    layer_options = "OVERWRITE=true")

print(here())
list.files(here())

st_layers(dsn = here("foss4g_R_workshop.gpkg"))

## ----- 5.1 Prepare for Maps -----
library(sf)
library(here)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(tmap)

st_layers(here("foss4g_R_workshop.gpkg"))

cycle_hire_27700 <- st_read(
  dsn = here("foss4g_R_workshop.gpkg"), 
  layer = "cycle_hire_27700")

london_boroughs_27700 <- st_read(
  dsn = here("foss4g_R_workshop.gpkg"), 
  layer = "london_boroughs_27700")

boroughs_centroids_27700<- st_read(
  dsn = here("foss4g_R_workshop.gpkg"), 
  layer = "boroughs_centroids_27700")

## ----- 5.3 Plot -----
london_boroughs_27700 %>% # pipe data to
  ggplot() +                # a ggplot function
  geom_sf(                # precise that it will be a spatial geometry
    aes(                  # provide some aesthetics
      geometry = geom,    # the geometry column (usually auto detected)
      fill = count)       # we want the polygon color to change following the count
  ) -> g # store it in g

g # display g

g <- g +
  scale_fill_viridis_c(
    guide = guide_legend(title = "Hires") # legend title
  )
g

g <- g +
  scale_fill_viridis_c(
    guide = guide_legend(title = "Hires") # legend title
  )
g

ggplot() +  geom_sf(data = london_boroughs_27700) + # add boroughs shape to the map  
  geom_sf(data = boroughs_centroids_27700, # add the boroughs centroids>
          aes(size = boroughs_centroids_27700$count), # fix size of points (by area)
          color = 'red', alpha = 1/5)+ # set points colour and transparency
  ggtitle("Cycle hire points", subtitle = "in London's boroughs") + # set the map title
  theme(legend.position = 'left') + # Legend position
  scale_size_area(name = 'Hires',max_size=10) # 0 value means 0 area + legend title

## ----- 5.4 tmap -----

library(tmap)

tm_shape(london_boroughs_27700) + 
  tm_polygons("count")

tmap_mode("view")

tm_shape(london_boroughs_27700) + 
  tm_polygons("count")

tmap_mode("plot")

tm1 <- tm_shape(london_boroughs_27700) + tm_polygons("count", convert2density = TRUE)
tm2 <- tm_shape(london_boroughs_27700) + tm_bubbles(size = "count")

tmap_arrange(tm1, tm2)

tmap_mode("view")

tm_basemap("Stamen.Watercolor") +
  tm_shape(london_boroughs_27700) + tm_polygons("count", convert2density = TRUE) + tm_bubbles(size = "count", col = "red") +
  tm_tiles("Stamen.TonerLabels")

tmap_mode("plot")

tm_shape(london_boroughs_27700) + 
  tm_polygons("count", convert2density = TRUE) +
  tm_bubbles(size = "count", col = "red") +
  tm_scale_bar(position=c("left", "bottom")) +
  tm_compass(size = 2, position=c("right", "top"))
