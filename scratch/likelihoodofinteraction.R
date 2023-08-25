library(sf)
library(terra)
library(tidyverse)
sf_use_s2(FALSE)

#Read vessel density file
vessel_density <- readRDS("outputs/vessel_density.rds")

# Intersects whale shape and vessel shapes
# Creates new Rating field for sum of ratings
lik_of_inter <- function(yr, season, vessel_type) {
  vessel_path <- file.path("outputs/ais", str_glue("ais_{season}_{vessel_type}.shp"))
  whale_path <- file.path("outputs/whales", str_glue("whales_{season}_{yr}.shp"))
  st_intersection(
    st_read(vessel_path),
    st_read(whale_path)
  ) %>% 
    st_collection_extract("POLYGON") %>% 
    mutate(Rating = case_match(as.numeric(Rating) + as.numeric(`Rating.1`),
                               c(2, 3) ~ 1,
                               4       ~ 2,
                               c(5, 6) ~ 3)) %>% 
    select(-`Rating.1`)
}

whale_vessel_loi <- map(str_split(names(vessel_density), "_"),
                        \(s_v) lik_of_inter(2016, s_v[1], s_v[2]))

#WALK 
loi_paths <- map_chr(str_split(names(vessel_density), "_"),
                     \(s_v) file.path("outputs/likelihood", 
                                      str_glue("loi_2016_{s_v[1]}_{s_v[2]}.shp")))
walk2(whale_vessel_loi, 
      loi_paths, 
      \(v, p) write_sf(v, p))
