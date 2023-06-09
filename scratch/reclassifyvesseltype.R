library(googlesheets4)
library(here)
library(sf)
library(tidyverse)

# Download reclassification table
gs4_deauth() # Just so Google doesn't try to authenticate you
vessel_type_reclass <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1yixJkzDKr65gLkf_78YL0DBsRWEssa9q6SXRaM-0bmQ/edit?usp=sharing",
  sheet = "VesselType"
)
passenger_reclass <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1yixJkzDKr65gLkf_78YL0DBsRWEssa9q6SXRaM-0bmQ/edit?usp=sharing",
  sheet = "PassengerType"
) %>% 
  mutate(VesselName = str_to_lower(VesselName))

# Read AIS data
ais_oct16 <- read_csv("data/ais/october2016.csv")

# Reclassify vessel types
ais_oct16 <- ais_oct16 %>% 
  left_join(vessel_type_reclass, by = "VesselType") %>% 
  mutate(VesselName = str_to_lower(VesselName)) %>% 
  left_join(passenger_reclass, by = "VesselName", ) %>% 
  replace_na(list(PassengerType = "OtherPassenger")) %>% 
  mutate(ReclassifiedVesselType = ifelse(ReclassifiedVesselType == "Passenger", 
                                         PassengerType,
                                         ReclassifiedVesselType))
