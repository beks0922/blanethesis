library(lubridate)
library(spatstat)
library(terra)
library(tidyverse)
library(tmap)
library(sf)
sf_use_s2(FALSE)

# Load whale data
read_whale <- function(path) {
  read_csv(path) %>%
    select(Year, Mo, Day, Lat_DD, Long_DD) %>% 
    mutate(Long_DD = ifelse(Long_DD > 0, -Long_DD, Long_DD))
}
whales_all <- dir("data/whales", full.names=TRUE) %>% 
  map(read_whale) %>% 
  list_rbind()

# Assign season based on month 
whales_season <- whales_all %>% 
  mutate(season = case_when(
    Mo <= 3 ~ "Winter",
    Mo <= 6 ~ "Spring",
    Mo <= 9 ~ "Summer",
    Mo <= 12 ~ "Fall"
  ))

## Create whale raster function 
create_whale_raster <- function(whale_df, season_year, template, area) {
  # Estimate kernel density
  study_area_owin <- as.owin(area)
  pts <- st_coordinates(whale_df)
  whale_ppp <- ppp(pts[, 1], pts[, 2], window = study_area_owin)
  whale_kde <- resample(rast(density(whale_ppp)),
                        template,
                        method = "sum")
  names(whale_kde) <- "density"
  
  # Classify density by quantiles
  whale_quantiles <- quantile(values(whale_kde), 
                              c(0.33, 0.67, 1), 
                              na.rm = TRUE)
  whale_ratings <- classify(whale_kde, 
                            c(0, whale_quantiles),
                            include.lowest = TRUE)
  names(whale_ratings) <- "rating"
  
  # Rename levels to avoid brackets
  whale_levels <- levels(whale_ratings)[[1]] %>% 
    transmute(value,
              Rating = factor(1:3))
  levels(whale_ratings) <- whale_levels
  
  # Rename raster layer
  names(whale_ratings) <- paste(season_year, collapse="_")
  
  # Return a vector
  as.polygons(whale_ratings)
}

# Group Map
whale_density  <- whales_season %>%
  drop_na() %>%
  st_as_sf(coords = c("Long_DD", "Lat_DD"),
           crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>% 
  st_transform("EPSG:3488")  %>% 
  st_intersection(study_area) %>% 
  group_by(season, Year) %>% 
  group_map(create_whale_raster, 
            template = study_area_template,
            area = study_area) %>% 
  set_names(map_chr(., names))
for (i in seq(length(whale_density))) {
  names(whale_density[[i]]) <- "Rating"
}

saveRDS(whale_density, "outputs/whale_density.rds")

#WALK 
walk2(whale_density, 
      names(whale_density), 
      \(r, n) writeVector(r, file.path("outputs/whales", str_glue("whales_{n}.shp"))))
