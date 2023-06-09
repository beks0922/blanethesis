library(lubridate)
library(SpatialKDE)
library(tmap)

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
  # Run KDE for Fall/Pleasure vessels
  filter(ReclassifiedVesselType == "Pleasure")
kde_bw <- default_bandwidth_sf(fall_pleasure) # 10^-11 is too small!
fall_pleasure_kde <- kde(fall_pleasure,
                         band_width = 5e3,
                         cell_size = 1e3)

tmap_mode("view")
study_area <- st_read(here::here("data", "studyarea"))
sf_use_s2(FALSE)
tm_basemap() +
  tm_shape(mutate(fall_pleasure_kde, kde_value = ifelse(kde_value == 0, NA, kde_value))) +
  tm_fill(col = "kde_value",
          alpha = 0.75) +
  tm_shape(study_area) +
  tm_polygons()
