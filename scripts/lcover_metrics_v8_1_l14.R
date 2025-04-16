#==============================
# Forest landscape metrics
#=============================

## 2025-02-01
## Peter R.

# ## Aim: get landscape metrics from cropped GEE Amazon rasters

# ## Notes
# -
# - terra is having problems reading portuguese shp



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
#library(sf)
#library(plyr)
#library(reshape2)
#library(rgdal)
#library(rgeos)
library(raster)
library(doParallel)
library(sqldf)

print("libraries loaded")

#===============================
# Directory set up
#===============================

# get current working directory
#getwd()

#setwd("/home/peterr/projects/lcmor")
## project folder on desktop
#setwd("C:/Users/Peter R/Documents/cghr/data/gis/")
#DRAC
setwd("/home/georod/projects/def-mfortin/georod/scripts/github/amazon-mortality")
#setwd("/home/georod/projects/def-mfortin/georod/data/amazon/")


#start.time <- Sys.time()
#start.time


# files

#files10 <- list.files(path='./output1/mun_rasters', pattern='\\.tif$', full.names=TRUE, recursive = TRUE)
# DRAC
#files1 <- list.files(path='/home/georod/projects/def-mfortin/georod/data/amazon/lcover_gee', pattern='\\.tif$', full.names=TRUE)

#files10 <- list.files(path='/home/georod/projects/def-mfortin/georod/data/amazon/output1/mun_rasters/', pattern='\\.tif$', full.names=TRUE, recursive = TRUE)

# raster names for files
#files20 <- list.files(path='./output1/mun_rasters', pattern='\\.tif$', full.names=FALSE, recursive = TRUE)
# DRAC
#files2 <- list.files(path='/home/georod/projects/def-mfortin/georod/data/amazon/lcover_gee', pattern='\\.tif$', full.names=FALSE)
#files20 <- list.files(path='/home/georod/projects/def-mfortin/georod/data/amazon/output1/mun_rasters/', pattern='\\.tif$', full.names=FALSE, recursive = TRUE)

#list.files(path="./data/shp/mesoregions", pattern="\\.shp", full.names=TRUE)
#files3 <- list.files(path='./data/shp/municipalities', pattern='\\.shp$', full.names=TRUE, recursive = TRUE)
# DRAC
#files3 <- list.files(path='/home/georod/projects/def-mfortin/georod/data/amazon/shp/municipalities/', pattern='\\.shp$', full.names=TRUE, recursive = TRUE)

# vector names for files
#files4 <- list.files(path='./data/shp/municipalities', pattern='\\.shp$', full.names=FALSE, recursive = TRUE)
# DRAC
#files4 <- list.files(path='/home/georod/projects/def-mfortin/georod/data/amazon/shp/municipalities/', pattern='\\.shp$', full.names=FALSE, recursive = TRUE)


#prefix1 <- "pcm_" # projected, cropped & masked

#outf10 <- "./output1/mun_lcover/" # Out folder were landscape metric files are saved
# DRAC
#outf <- "/home/georod/projects/def-mfortin/georod/data/amazon/output1/mun_rasters/" # Not needed for lcover metrics as out paths are created dynamically
 


# rf20 <- foreach(i=1:length(files10)) %do% {
#   
#   raster::raster(files10[i])
#   
# }



#----------------------------------------
# Create raster & vector year match table
#----------------------------------------
# This table is needed to ensure raster is cropped with matched vector

#vFilesDf <- cbind.data.frame("vyear"=gsub("\\D", "", (do.call(rbind, strsplit(as.character(files4),'/'))[,1])), "vname"=files3 )
#vFilesDf$rcropf <- paste0(outf, do.call(rbind, strsplit(as.character(files4),'.shp'))[,1])
#rFilesDf <- cbind.data.frame("ryear"=substring(gsub("\\D", "", files2), 3, 6), "rname"=files1)

# vFilesDf <- cbind.data.frame("vyear"=gsub("\\D", "", (do.call(rbind, strsplit(as.character(files4),'/'))[,1])), "vname"=files3 )
# vFilesDf$rcropf <- paste0(outf, do.call(rbind, strsplit(as.character(files4),'.shp'))[,1])
# vFilesDf$shp_folder <- gsub(".shp", "", (paste0(do.call(rbind, strsplit(as.character(files4),'/'))[,2])))
# rFilesDf <- cbind.data.frame("ryear"=substring(gsub("\\D", "", files2), 3, 6), "rname"=files1)
# 
# # recode, raster paired with vector
# #paste(1984:1999, ",", 1991)
# m1 <- matrix(c(
#   2000,2000,
#   2001, 2001,
#   2002, 2001,
#   2003, 2001,
#   2004, 2001,
#   2005, 2005,
#   2006, 2005,
#   2007, 2007,
#   2008, 2007,
#   2009, 2007,
#   2010, 2010,
#   2011, 2010,
#   2012, 2010,
#   2013, 2013,
#   2014, 2014,
#   2015,2015,
#   2016, 2016,
#   2017, 2017,
#   2018, 2018,
#   2019, 2019,
#   2020, 2020,
#   2021, 2021,
#   2022, 2022,
#   2023, 2023
# ), byrow=TRUE, nrow=24, ncol=2)
# 
# 
# rFilesDf$ryear2 <- rFilesDf$ryear
# 
# for (i in 1:nrow(m1)) {
#   
#   rFilesDf$ryear2[rFilesDf$ryear2==as.numeric(m1[i,1])] <- as.numeric(m1[i,2])
#   
# }
# 
# 
# #file match table. This table ensures raster and vectors are match by year
# #rv_match <- sqldf("SELECT * FROM rFilesDf t1 JOIN vFilesDf t2 ON t1.ryear2=t2.vyear")
# rv_match <- fn$sqldf("SELECT *, '$outf'|| 'mun_'|| t1.ryear || '/' || t2.shp_folder AS rcropf2 FROM rFilesDf t1 JOIN vFilesDf t2 ON t1.ryear2=t2.vyear")
# #rv_match <- rv_match[rv_match$ryear %in% c(2000),]
# rv_match <- rv_match[rv_match$ryear %in% c(2007),]

source("./scripts/crop_tool_code_match_table_v1.R")

#============================
# Load mun shp
#============================

polyL2Pj2  <- foreach (i=1:nrow(rv_match)) %do% {
  
  shp1 <- sf::st_read(rv_match[i, 6])
  if (is.na(sf::st_crs(shp1))) sf::st_set_crs(shp1, 4674) else sf::st_transform(shp1, 4326)
  
}





#=================================
# Calculate land cover metrics
#=================================

start_time <- Sys.time()

setwd("/home/georod/projects/def-mfortin/georod/data/amazon/")

#foreach(j=1:length(polyL2Pj)) %do% {
foreach(j=1:nrow(rv_match), .packages=c('doParallel','foreach','terra', 'sf')) %do% {
#foreach(j=1:1, .packages=c('doParallel','foreach','terra', 'sf')) %do% {
  
  rcropfiles <- list.files(path=rv_match[j,"rcropf"], pattern='\\.tif$', full.names=TRUE, recursive = TRUE)
  
  path1 <- gsub("mun_rasters2", 'mun_lcover2', rv_match[j,"rcropf"])
  
  if (!dir.exists(path1)) dir.create(path1, recursive = TRUE)
  
  foreach (m=1:length(rcropfiles)) %do% {
    
    rTemp0 <- terra::rast(rcropfiles[m]) 
    #mun_code <-  strsplit(names(rTemp0), "_")[[1]][1]
    mun_code <-  strsplit(strsplit(rcropfiles[m], "/")[[1]][5], "_")[[1]][1]
    #yr_mun_code <- strsplit(names(rTemp0), "_")[[1]][2]
    yr_mun_code <- gsub("\\D", "", strsplit(strsplit(rcropfiles[m], "/")[[1]][5], "_")[[1]][2])
    
   
    mun_poly <- polyL2Pj2[[j]][polyL2Pj2[[j]]$"munic"==mun_code,]

    mun_polyPj <- sf::st_transform(mun_poly, crs="EPSG:5880") # Brazil mercator
   
    
    #rTemp1 <- terra::project(rTemp0, crs=terra::crs(terra::vect(mun_polyPj)), method="near")
    rTemp1 <- terra::project(rTemp0, "EPSG:5880" , method="near")
    
    temp1 <- sample_lsm(terra::rast(rTemp1), mun_polyPj, plot_id=mun_polyPj$"munic",
                        what = c("lsm_c_pland", "lsm_c_ca", "lsm_l_ta")) # "lsm_c_ca"
    
    temp1$year <- yr_mun_code

    rm(rTemp1)
    
    #dir.create(path=path1, recursive = TRUE)
    
    saveRDS(temp1, paste0(path1, "/", "lcm_", mun_code,"_", yr_mun_code,".rds"))  # overwrite=TRUE?
    
    print(paste0("lcover metrics done. ", "year: " , yr_mun_code , " state: ", rv_match[j,4], " mun poly :", mun_code))
    
    }
  
}



#temp1 %>% print(n=50)

Sys.sleep(5)
#gc()
Sys.sleep(5)
end_time <- Sys.time()

start_time-end_time


#After running the above, collect the lc metrics
# files100 <- list.files(path='./output1/mun_lcover', pattern='\\.rds$', full.names=TRUE, recursive = TRUE)
# 
#  lcM1 <- foreach(i=1:length(files100), .combine =rbind) %do% {
#   
#    temp1 <- readRDS(files100[k])  
#    temp1 <- temp1[temp1$class==3 & temp1$metric=='ca',]
#    temp1
#   
# }

# Extract forest
 # class=3 is forest?
 
