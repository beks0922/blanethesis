library(lubridate)
library(sf)
library(terra)
library(tmap)

fall_pleasure <- ais_oct16 %>% 
  # Assign season based on month
  mutate(season = case_when(
    month(BaseDateTime) <= 3 ~ "Winter",
    month(BaseDateTime) <= 6 ~ "Spring",
    month(BaseDateTime) <= 9 ~ "Summer",
    month(BaseDateTime) <= 12 ~ "Fall"
  )) %>% 
  # Convert to Albers California projection
  st_as_sf(coords = c("LON", "LAT"),
           crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>% 
  st_transform("EPSG:3488") %>% 
  # Filter down to "Pleasure" vessels
  filter(ReclassifiedVesselType == "Pleasure")


# Raster template for study area
study_area <- st_read(here::here("data", "studyarea")) %>% 
  st_crop(xmin = -122.6501, ymin = 37.5, xmax = -121, ymax = 38) %>% 
  st_transform("EPSG:3488")
raster_template <- rast(ext(study_area), 
                        resolution = 500,
                        crs = "EPSG:3488")

# Vessel density (Fall 2016 pleasure vessels)
vessel_density <- fall_pleasure %>% 
  rasterize(raster_template, fun = length, ) %>% 
  crop(study_area, mask = TRUE)
names(vessel_density) <- "count"

# Visualize it
sf_use_s2(FALSE)
# Vessel density raster
tm_shape(vessel_density) +
  tm_raster("count") +
  # Study area border
  tm_shape(study_area) +
  tm_borders()
