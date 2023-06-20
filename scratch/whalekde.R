library(lubridate)
library(spatstat)
library(tidyverse)
library(tmap)

# Functions for working with KDEs
std_dist <- function(x, y) {
  xbar <- mean(x)
  ybar <- mean(y)
  n <- length(x)
  sqrt(sum(x - xbar)^2 / n + sum(y - ybar)^2 / n)
}
# Assuming projected coordinates
default_bandwidth <- function(x, y) {
  xbar <- mean(x)
  ybar <- mean(y)
  dist2center <- sqrt((x - xbar)^2 + (y - ybar)^2)
  dm <- median(dist2center)
  sd <- std_dist(x, y)
  n <- length(x)
  0.9 * min(sd, sqrt(1 / log(2)) * dm) * n^-0.2
}
default_bandwidth_sf <- function(x) {
  xy <- st_coordinates(x)
  default_bandwidth(xy[, 1], xy[, 2])
}

# Raster template for study area
study_area <- st_read(here::here("data", "studyarea")) %>% 
  st_crop(xmin = -122.6501, ymin = 37.5, xmax = -121, ymax = 38) %>% 
  st_transform("EPSG:3488")
raster_template <- rast(ext(study_area), 
                        resolution = 500,
                        crs = "EPSG:3488")

# Load whale data
whales <- read_csv("data/whales/allTMMC_2016Stgs.csv")
whales_sf <- whales %>% 
  # Assign season based on month
  mutate(season = case_when(
    Mo <= 3 ~ "Winter",
    Mo <= 6 ~ "Spring",
    Mo <= 9 ~ "Summer",
    Mo <= 12 ~ "Fall"
  )) %>% 
  # Convert to Albers California projection
  st_as_sf(coords = c("Long_DD", "Lat_DD"),
           crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>% 
  st_transform("EPSG:3488") %>% 
  st_intersection(study_area)

# Estimate kernel density
study_area_owin <- as.owin(study_area)
pts <- st_coordinates(whales_sf)
whale_ppp <- ppp(pts[, 1], pts[, 2], window = study_area_owin)
whale_ds <- resample(rast(density(whale_ppp)),
                     raster_template,
                     method = "sum")
names(whale_ds) <- "density"

# Classify density by quantiles
whale_quantiles <- quantile(values(whale_ds), 
                            c(0.33, 0.67, 1), 
                             na.rm = TRUE)
whale_ratings <- classify(whale_ds, 
                          c(0, whale_quantiles),
                          include.lowest = TRUE)
names(whale_ratings) <- "rating"
# Rename levels to avoid brackets
whale_levels <- levels(whale_ratings)[[1]] %>% 
  mutate(density = 1:3)
levels(whale_ratings) <- whale_levels

# Visualize
# Ratings raster
tm_shape(whale_ratings) +
  tm_raster("density") +
  # Actual points
  tm_shape(whales_sf) +
  tm_dots(col = "red") +
  # Study area border
  tm_shape(study_area) +
  tm_borders()
