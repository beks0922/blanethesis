
# =================
# Instructions from Max:
# 1) write a function that:
#    1. takes a year as input
#    2. Find corresponding .rds file (dir(pattern = ""))
#    3. Read the .rds file
#    4. Rasterize that year by season and vessel type (see function in 02_vesselrasterize.R, but don't classify to 1:3)
#    5. Optionally, save rasters
#    6. Return data frame with raster column
#       year | season | vesseltype | raster
#       2016 | fall   | personal   | literally the raster
# 2) Map that function onto your years
#    At this point, you have a data frame with year, season, vesseltype and rasters
# 3) group by season and vesseltype
# 4) combine your rasters within the group (prob need to write a function for that)

###############################################################
library(lubridate)
library(sf)
library(terra)
library(tmap)
library(tidyverse)
sf_use_s2(FALSE)


# Create raster template
study_area <- st_read(here::here("data", "studyarea")) %>% 
  st_crop(xmin = -122.6501, ymin = 37.5, xmax = -121, ymax = 38) %>% 
  st_transform("EPSG:3488")
study_area_template <- rast(ext(study_area), 
                            resolution = 500,
                            crs = "EPSG:3488")

# Create vessel raster
create_vessel_raster <- function(vessel_df, keys, template) {
  # Project lat lon to albers
  vessel_albers <- vessel_df %>% 
    st_as_sf(coords = c("LON", "LAT"),
             crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>% 
    st_transform("EPSG:3488") 
  # Rasterize points
  vessel_raster <- vessel_albers %>% 
    rasterize(template, fun = "count") %>% 
    crop(study_area, mask = TRUE) 
  # Add raster column
  keys %>% 
    mutate(ais_raster = list(vessel_raster))
}

ais_rasterize <- function(year, template){
  ais <- dir("outputs/ais", 
             pattern = str_glue("ais_reclass_{year}.rds"), 
             full.names = TRUE) %>% 
    readRDS() %>% 
    mutate(season = case_when(
      month(BaseDateTime) <= 3 ~ "Winter",
      month(BaseDateTime) <= 6 ~ "Spring",
      month(BaseDateTime) <= 9 ~ "Summer",
      month(BaseDateTime) <= 12 ~ "Fall"
    )) %>% 
    mutate(year = year)
  ais %>%
    group_by(year, season, ReclassifiedVesselType) %>% 
    group_map(create_vessel_raster, template = template) %>% 
    list_rbind()
  }

ais_rasters <- map_df(2016:2019, ais_rasterize, template = study_area_template)
#save.image("outputs/ais/ais_rasters_byyear.RData") # Cannot save SpatRaster as RData object

combine_years <- function(data_list){
  nr_elements <- length(data_list)
  comb <- data_list[[1]]
  if(nr_elements>1){
    for(i in 2:(nr_elements)){
      comb <- merge(comb, data_list[[i]])
    }
  }
  return(comb)
}

vessel_density <- ais_rasters %>%
  group_by(season, ReclassifiedVesselType) %>%
  summarize(ais_raster = list(combine_years(ais_raster)))


save_rasters <- function(raster_list) {
  for (i in 1:nrow(raster_list)) {
    season <- raster_list$season[i]
    vessel_type <- raster_list$ReclassifiedVesselType[i]
    current_raster <- raster_list$ais_raster[[i]]
    writeRaster(current_raster, filename = file.path("outputs/ais/", str_glue("ais_{season}_{vessel_type}.tif")), 
                overwrite = TRUE)
  }
}

# Call the function to save the rasters using the existing vessel_density list
save_rasters(vessel_density)

# Write a function that mutates data into quantiles, adds Rating col, and saves polygons
create_quantile_polys <- function(season_types, vessel_types) {
  for (season_type in season_types) {
    for (vessel_type in vessel_types) {
      # Construct the file path using str_glue
      raster_file <- str_glue("outputs/ais/ais_{season_type}_{vessel_type}.tif")
      
      # Read the saved raster using terra's rast function
      vessel_raster <- rast(raster_file)
      
      # Calculate quantiles and create Ratings column
      vessel_quantiles <- quantile(values(vessel_raster), c(0.33, 0.67, 1), na.rm = TRUE)
      vessel_ratings <- classify(vessel_raster, c(0, vessel_quantiles), include.lowest = TRUE)
      vessel_levels <- levels(vessel_ratings)[[1]] %>%
        transmute(value, Rating = factor(1:3))
      levels(vessel_ratings) <- vessel_levels
      
      # Rename raster layer
      names(vessel_ratings) <- paste(season_type, vessel_type, collapse = "_")
      
      # Convert to polygons
      polygons <- as.polygons(vessel_ratings)
      
      # Convert to an sf object
      sf_polygons <- st_as_sf(polygons)
      
      # Save each polygon separately
      output_filename <- str_glue("outputs/ais/ais_{season_type}_{vessel_type}.shp")
      st_write(sf_polygons, output_filename)
    }
  }
}

# Usage example
season_types <- c("Spring", "Summer", "Fall")
vessel_types <- c("Cargo", "Tanker", "TugTow", "HSF", "Cruise", "OtherPassenger", "Pleasure", "Other")

# Call the function to process and save the quantile polygons individually
create_quantile_polys(season_types, vessel_types)







