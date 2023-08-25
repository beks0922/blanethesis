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

# Create speed ratings
create_speed_ratings <- function(vessel_df, season_type, template) {
  # Project lat lon to albers
  vessel_albers <- vessel_df %>% 
    st_as_sf(coords = c("LON", "LAT"),
             crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>% 
    st_transform("EPSG:3488") 
  # Rasterize points
  speed_raster <- vessel_albers %>% 
    rasterize(template, 
              field = "SOG", 
              fun = \(spd, ...) weighted.mean(spd, spd/60)) %>% 
    crop(study_area, mask = TRUE) 
  # Classify raster by quantile
  speed_quantiles <- quantile(values(speed_raster), 
                              c(0.33, 0.67, 1), 
                              na.rm = TRUE) 
  # Rename levels
  speed_ratings <- classify(speed_raster, 
                            c(0, speed_quantiles),
                            include.lowest = TRUE)
  
  # Rename levels in raster 
  speed_levels <- levels(speed_ratings)[[1]] %>% 
    transmute(value,
              Rating = factor(1:3))
  levels(speed_ratings) <- speed_levels
  
  # Rename raster layer
  names(speed_ratings) <- paste(season_type,collapse="_")
  
  # Return a vector
  as.polygons(speed_ratings)
}

vessel_speed <- ais_seasons_2016 %>% 
  group_by(season, ReclassifiedVesselType) %>% 
  group_map(create_speed_ratings, template = study_area_template) %>% 
  set_names(map_chr(., names))

for (i in seq(length(vessel_speed))) {
  names(vessel_speed[[i]]) <- "Rating"
}

saveRDS(vessel_speed, "outputs/vessel_speed.rds")

#WALK 
walk2(vessel_speed, 
      names(vessel_speed), 
      \(r, n) writeVector(r, 
                          file.path("outputs/speed", str_glue("speed_{n}.shp")), 
                          overwrite = TRUE))



