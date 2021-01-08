# Libraries load
library(haven)
library(here)
library(sf)
library(readr)
library(dplyr)

# Convert CSEW files from .sav to .Rdata and remove haven labels

# Load in SPSS data (see data descriptions on the GitHub README).
csew    <- read_spss(file = here("data","csew_apr11mar12_nvf.sav"))
csew_vf <- read_spss(file = here("data","csew_apr11mar12_vf.sav"))

# Get rid of haven labels.
csew    <- zap_labels(csew)
csew_vf <- zap_labels(csew_vf)

# Save as .Rdata to reduce file size for later (easier) use.
save(csew   , file = here("data","csew_apr11mar12_nvf.Rdata"))
save(csew_vf, file = here("data","csew_apr11mar12_vf.Rdata"))

# Load raw shapefiles (generalised & clipped) obtained from https://census.ukdataservice.ac.uk/get-data/boundary-data.aspx
# apart from 2018 Wards obtained from http://geoportal.statistics.gov.uk/datasets/wards-december-2018-generalised-clipped-boundaries-gb.
ward_sf <- st_read("data/shapefiles/Wards_December_2018_Generalised_Clipped_Boundaries_GB.shp")
msoa_sf <- st_read("data/shapefiles/england_msoa_2011_gen_clipped.shp")
lsoa_sf <- st_read("data/shapefiles/england_lsoa_2011_gen_clipped.shp")
oa_sf   <- st_read("data/shapefiles/england_oa_2011_gen_clipped.shp")

# Load LSOA to Ward look-up table.
LSOA_to_ward <- read_csv(here("data", "Lower_Layer_Super_Output_Area_2011_to_Ward_2018_Lookup_in_England_and_Wales_v3.csv"))

# Filter for Manchester
LSOA_to_ward_manc <- LSOA_to_ward %>% 
  filter(LAD18NM == "Manchester")

# Load in OA look-up table.
OA_to_LAD <- read_csv(here("data","Output_Area_to_Lower_Layer_Super_Output_Area_to_Middle_Layer_Super_Output_Area_to_Local_Authority_District_December_2017_Lookup_in_Great_Britain__Classification_Version_2.csv"))

# Filter for Manchester.
OA_to_LAD <- OA_to_LAD %>% 
  filter(LAD17NM == "Manchester")

# Subset the spatial data using these codes (Ward).
ward_manc_sf <- ward_sf %>% 
  filter(wd18cd %in% LSOA_to_ward_manc$WD18CD)

# Subset the spatial data using these codes (MSOA).
msoa_manc_sf <- msoa_sf %>% 
  filter(code %in% OA_to_LAD$MSOA11CD)

# Subset the spatial data using these codes (LSOA).
lsoa_manc_sf <- lsoa_sf %>% 
  filter(code %in% OA_to_LAD$LSOA11CD)

# Subset the spatial data using these codes (OA).
oa_manc_sf <- oa_sf %>% 
  filter(code %in% OA_to_LAD$OA11CD)

# Save each as shapefiles for later use.
st_write(ward_manc_sf, dsn = "data/shapefiles/ward_manc_sf.shp", driver = "ESRI Shapefile")
st_write(msoa_manc_sf, dsn = "data/shapefiles/msoa_manc_sf.shp", driver = "ESRI Shapefile")
st_write(lsoa_manc_sf, dsn = "data/shapefiles/lsoa_manc_sf.shp", driver = "ESRI Shapefile")
st_write(oa_manc_sf  , dsn = "data/shapefiles/oa_manc_sf.shp"  , driver = "ESRI Shapefile")
