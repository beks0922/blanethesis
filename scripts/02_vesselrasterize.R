library(lubridate)
library(sf)
library(terra)
library(tmap)
library(tidyverse)
sf_use_s2(FALSE)

ais_seasons_2016 <- readRDS("outputs/ais/ais_reclass_2016.rds") %>% 
  mutate(season = case_when(
    month(BaseDateTime) <= 3 ~ "Winter",
    month(BaseDateTime) <= 6 ~ "Spring",
    month(BaseDateTime) <= 9 ~ "Summer",
    month(BaseDateTime) <= 12 ~ "Fall"
  ))
  
# Create raster template
study_area <- st_read(here::here("data", "studyarea")) %>% 
  st_crop(xmin = -122.6501, ymin = 37.5, xmax = -121, ymax = 38) %>% 
  st_transform("EPSG:3488")
study_area_template <- rast(ext(study_area), 
                            resolution = 500,
                            crs = "EPSG:3488")

# Create vessel raster
create_vessel_raster <- function(vessel_df, season_type, template) {
  # Project lat lon to albers
  vessel_albers <- vessel_df %>% 
    st_as_sf(coords = c("LON", "LAT"),
             crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>% 
    st_transform("EPSG:3488") 
  # Rasterize points
  vessel_raster <- vessel_albers %>% 
    rasterize(template, fun = "count") %>% 
    crop(study_area, mask = TRUE) 
  # Classify raster by quantile
  vessel_quantiles <- quantile(values(vessel_raster), 
                               c(0.33, 0.67, 1), 
                               na.rm = TRUE) 
  # Rename levels
  vessel_ratings <- classify(vessel_raster, 
                             c(0, vessel_quantiles),
                             include.lowest = TRUE)
  
  # Rename levels in raster 
  vessel_levels <- levels(vessel_ratings)[[1]] %>% 
    transmute(value,
              Rating = factor(1:3))
  levels(vessel_ratings) <- vessel_levels

  # Rename raster layer
  names(vessel_ratings) <- paste(season_type,collapse="_")
  
  # Return a raster
  vessel_ratings
}

vessel_density <- ais_seasons_2016 %>% 
  group_by(season, ReclassifiedVesselType) %>% 
  group_map(create_vessel_raster, template = study_area_template) %>% 
  set_names(map_chr(., names))

saveRDS(vessel_density, "outputs/vessel_density.rds")
