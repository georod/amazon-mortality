#==============================
# Forest landscape metrics
#=============================

## 2025-03-29
## Peter R.

# ## Aim: get landscape metrics from cropped GEE Amazon rasters

# ## Notes
# - Given that GEE raster are in WGS84 these are projected to Brazil mercator so that area is in meters
# - terra is having problems reading portuguese shp. But the 1980-2000 shp only have 1 column with numbers
# - This is a new version of the script where j index is manually entered.  
#   - This is because all raster-year crops are in one folder per shp-period combination



#===============================
# Libraries
#===============================
#install.packages("sf")
library(sf)
#install.packages("terra")
library(terra)
#install.packages("foreach")
library(foreach)
#install.packages("landscapemetrics")
library(landscapemetrics)

library(doParallel)
library(sqldf)

print("libraries loaded")

#===============================
# Directory set up
#===============================


#Darjeeling
#setwd("/home/peterr/projects/lcmor")

## project folder on desktop
#setwd("C:/Users/Peter R/Documents/cghr/data/gis/")

#DRAC
setwd("/home/georod/projects/def-mfortin/georod/scripts/github/amazon-mortality") # here is where my code match code is
#setwd("/home/georod/projects/def-mfortin/georod/data/amazon/") #data


#start.time <- Sys.time()
#start.time


# Darjeeling
#source("./version1/scripts/crop_tool_code_match_table_v1.R")

#DRAC
source("./scripts/crop_tool_code_match_table_v1.R")


#=================================
# Calculate land cover metrics
#=================================

start_time <- Sys.time()

# Darjeeling
#setwd("/home/peterr/projects/lcmor")

# DRAC
setwd("/home/georod/projects/def-mfortin/georod/data/amazon/") # Here is were the cropped raster are


foreach(j=3, .packages=c('doParallel','foreach','terra', 'sf')) %do% {

  
  shp1 <- sf::st_read(rv_match[j, 6])
  if (is.na(sf::st_crs(shp1))) sf::st_set_crs(shp1, 4674) else sf::st_transform(shp1, 4326)

  rcropfiles <- list.files(path=rv_match[j,"rcropf"], pattern='\\.tif$', full.names=TRUE, recursive = TRUE)
  
  path1 <- gsub("mun_rasters2", 'mun_lcover2', rv_match[j,"rcropf"])
  
  if (!dir.exists(path1)) dir.create(path1, recursive = TRUE)
  
  foreach (m=490:length(rcropfiles)) %do% {
    
    rTemp0 <- terra::rast(rcropfiles[m]) 

    mun_code <-  strsplit(strsplit(rcropfiles[m], "/")[[1]][5], "_")[[1]][1]
    
    yr_mun_code <- gsub("\\D", "", strsplit(strsplit(rcropfiles[m], "/")[[1]][5], "_")[[1]][2])
    
    
    mun_poly <- shp1[shp1$"munic"==mun_code,]
    
    mun_polyPj <- sf::st_transform(mun_poly, crs="EPSG:5880") # Brazil mercator as I need meters.
    
  
    rTemp1 <- terra::project(rTemp0, "EPSG:5880", method="near")
    
    temp1 <- landscapemetrics::sample_lsm(rTemp1, mun_polyPj, plot_id=mun_polyPj$"munic",
                       what = c("lsm_c_pland", "lsm_c_ca", "lsm_l_ta")) # "lsm_c_ca"
    
   dim(temp1)

    temp1$year <- yr_mun_code

    saveRDS(temp1, paste0(path1, "/", "lcm_", mun_code,"_", yr_mun_code,".rds"))  # overwrite=TRUE?
    
    print(paste0("lcover metrics done. ", "year: " , yr_mun_code , " state: ", rv_match[j,4], " mun poly: ", mun_code, " m: "))
    
  }
  
}


