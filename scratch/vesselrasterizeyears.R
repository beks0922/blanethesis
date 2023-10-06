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

create_vessel_raster <- function (vessel_df, season_type, year, template) {
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
  
  # Return a vector
  as.polygons(vessel_ratings)
}

for (year in c("2016", "2017", "2018")) {
  #read in ais rds files by year
  ais_year <- list.files("outputs/ais", full.names = TRUE, pattern = str_glue("{year}\\.rds"))
  ais_year_df <- map_df(ais_year, readRDS)
  #create season column
  ais_season_year <- ais_year_df %>% 
    mutate(season = case_when(
      month(BaseDateTime) <= 3 ~ "Winter",
      month(BaseDateTime) <= 6 ~ "Spring",
      month(BaseDateTime) <= 9 ~ "Summer",
      month(BaseDateTime) <= 12 ~ "Fall"))
  
  vessel_density <- ais_season_year %>% 
    group_by(year(BaseDateTime), season, ReclassifiedVesselType) %>% 
    group_map(create_vessel_raster, template = study_area_template) %>% 
    set_names(map_chr(., names))
  
  for (i in seq(length(vessel_density))) 
    names(vessel_density[[i]]) <- "Rating"
}


#WALK 
walk2(vessel_density, 
      names(vessel_density), 
      \(r, n) {
        year_file <- str_glue("vessel_density_{year}.rds")
        readRDS(year_file) %>% 
          extract2(n) %>% 
          writeVector(file.path("outputs/ais", str_glue("ais_{year}_{n}.shp")), 
                      overwrite = TRUE))
      })
saveRDS(vessel_density, "outputs/vessel_density_{year}.rds")
}


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

vessel_class <- function(data, keys) {
  browser()
  summed_vessels <- sum(rast(data$ais_raster), na.rm = TRUE)
  keys %>% 
    mutate(summed_ais_raster = list(summed_vessels))
  }

vessel_density <- ais_rasters %>% 
  group_by(season, ReclassifiedVesselType) %>% 
  group_map(vessel_class)

save_rasters <- function(raster_list) {
  for (i in 1:length(raster_list)) {
    season <- raster_list[[i]]$season
    vessel_type <- raster_list[[i]]$ReclassifiedVesselType
    raster <- raster_list[[i]]$ais_raster
    current_raster <- raster[[1]]
    writeRaster(current_raster, filename = file.path("outputs/ais", str_glue("ais_{season}_{vessel_type}.tif")), 
                overwrite = TRUE)
  }
}

# Call the function to save the rasters using the existing vessel_density list
save_rasters(vessel_density)
