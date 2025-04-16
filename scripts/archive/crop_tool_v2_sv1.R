#==============================
# Crop tool
#=============================

## 2025-01-25
## Peter R.

# ## Aim: Crop GEE Amazon into smaller county-year rasters

# ## Notes
# -
# - terra is having problems reading portuguese shp
# - the code can be run on DRAC



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
#library(landscapemetrics)
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

#start.time <- Sys.time()
#start.time


# files

#files1 <- list.files(path='./data/lcover_gee/amazon', pattern='\\.tif$', full.names=TRUE)
# DRAC
files1 <- list.files(path='/home/georod/projects/def-mfortin/georod/data/amazon/lcover_gee', pattern='\\.tif$', full.names=TRUE)

# raster names for files
#files2 <- list.files(path='./data/lcover_gee/amazon', pattern='\\.tif$', full.names=FALSE)
files2 <- list.files(path='/home/georod/projects/def-mfortin/georod/data/amazon/lcover_gee', pattern='\\.tif$', full.names=FALSE)

#list.files(path="./data/shp/mesoregions", pattern="\\.shp", full.names=TRUE)
#files3 <- list.files(path='./data/shp/municipalities', pattern='\\.shp$', full.names=TRUE, recursive = TRUE)
files3 <- list.files(path='/home/georod/projects/def-mfortin/georod/data/amazon/shp/municipalities', pattern='\\.shp$', full.names=TRUE, recursive = TRUE)

# vector names for files
#files4 <- list.files(path='./data/shp/municipalities', pattern='\\.shp$', full.names=FALSE, recursive = TRUE)
# DRAC
files4 <- list.files(path='/home/georod/projects/def-mfortin/georod/data/amazon/shp/municipalities', pattern='\\.shp$', full.names=FALSE, recursive = TRUE)


#prefix1 <- "pcm_" # projected, cropped & masked

#outf <- "./output1/mun_rasters/" # Out folder were clipped/cropped files are saved 
#DRAC
outf <- "/home/georod/projects/def-mfortin/georod/data/amazon/output1/mun_rasters/" # Out folder were clipped/cropped files are saved


#----------------------------------------
# Create raster & vector year match table
#----------------------------------------
# This table is needed to ensure raster is cropped with matched vector

vFilesDf <- cbind.data.frame("vyear"=gsub("\\D", "", (do.call(rbind, strsplit(as.character(files4),'/'))[,1])), "vname"=files3 )
vFilesDf$rcropf <- paste0(outf, do.call(rbind, strsplit(as.character(files4),'.shp'))[,1])
vFilesDf$shp_folder <- gsub(".shp", "", (paste0(do.call(rbind, strsplit(as.character(files4),'/'))[,2])))
rFilesDf <- cbind.data.frame("ryear"=substring(gsub("\\D", "", files2), 3, 6), "rname"=files1)

# recode, raster paired with vector
#paste(1984:1999, ",", 1991)
m1 <- matrix(c(
  2000,2000,
  2001, 2001,
  2002, 2001,
  2003, 2001,
  2004, 2001,
  2005, 2005,
  2006, 2005,
  2007, 2007,
  2008, 2007,
  2009, 2007,
  2010, 2010,
  2011, 2010,
  2012, 2010,
  2013, 2013,
  2014, 2014,
  2015,2015,
  2016, 2016,
  2017, 2017,
  2018, 2018,
  2019, 2019,
  2020, 2020,
  2021, 2021,
  2022, 2022,
  2023, 2023
), byrow=TRUE, nrow=24, ncol=2)


rFilesDf$ryear2 <- rFilesDf$ryear

for (i in 1:nrow(m1)) {
  
  rFilesDf$ryear2[rFilesDf$ryear2==as.numeric(m1[i,1])] <- as.numeric(m1[i,2])
  
}


#file match table. This table ensures raster and vectors are match by year
#rv_match <- sqldf("SELECT * FROM rFilesDf t1 JOIN vFilesDf t2 ON t1.ryear2=t2.vyear")
rv_match <- fn$sqldf("SELECT *, '$outf'|| 'mun_'|| t1.ryear || '/' || t2.shp_folder AS rcropf2 FROM rFilesDf t1 JOIN vFilesDf t2 ON t1.ryear2=t2.vyear")
#rv_match <- rv_match[rv_match$ryear %in% c(2000),]
#rv_match <- rv_match[rv_match$ryear %in% c(2002:2010),] # It stopped here:"cropped raster: classification_2005 state: 29 mun: 3" 
#rv_match <- rv_match[rv_match$ryear %in% c(2001:2005),] # 2005 did not complete the run
#rv_match <- rv_match[rv_match$ryear %in% c(2011:2014),] # none of the last three worked for some reason but folder were created
#rv_match <- rv_match[rv_match$ryear %in% c(2007:2011),]
#rv_match <- rv_match[rv_match$ryear %in% c(2010:2012),]
#rv_match <- rv_match[rv_match$ryear %in% c(2020:2023),] # 2023 did not complete
rv_match <- rv_match[rv_match$ryear %in% c(2023),]


#===============================
# Load data
#===============================

# terra has issues with Portuguese 
# polyL1 <- foreach (i=1:length(files3)) %do% {
#   
#   terra::vect(files3[i])
#   
# }

# sf has no problems reading Portuguese shp
# polyL2 <- foreach (i=1:length(files3)) %do% {
#   
#   sf::st_read(files3[i])
#   
# }


#------------------------------
# Parallel code
#------------------------------

# terra object's can't be easily sent to parallel loops. Hence, I have to rely on raster

# rf2 <- foreach(i=16) %do% {
#   
#   raster::raster(files1[i])
#   
# }


 rf2 <- foreach(i=1:nrow(rv_match)) %do% {
   
   raster::raster(rv_match[i,2])
   
 }

#print(names(rf2[[1]]))

polyL2 <- foreach (i=1:nrow(rv_match)) %do% {
  
  sf::st_read(rv_match[i, 5])
  
}


polyL2Pj <- foreach (i=1:length(polyL2)) %do% {
  
  if (is.na(sf::st_crs(polyL2[[i]]))) {
    
    st_set_crs(polyL2[[i]], 4674)
    
  } else {
  
  sf::st_transform(polyL2[[i]], 4674)
  }
  
}

# remove object to save space
#rm(polyL2)


#Given that rasters are in WGS84, I need to transform vect to WGS84 first
polyL2Pj2 <- foreach (i=1:length(polyL2Pj)) %do% {
  
     sf::st_transform(polyL2Pj[[i]], st_crs(rf2[[1]]))
  
  }


# remove object to save space
#rm(polyL2Pj)  

print("rasters and vectors loaded & transformed")

Sys.sleep(3)

#start_time <- Sys.time()



#-------------------
#Crop rasters
#-------------------

start_time <- Sys.time()

foreach(j=1:length(polyL2Pj2), .packages=c('doParallel','foreach','raster', 'sf')) %do% {
#foreach(j=1:nrow(rv_match), .packages=c('doParallel','foreach','raster', 'sf')) %do% {
  
  dir.create(path=rv_match[j, "rcropf2"], recursive = TRUE)
  #setwd(rv_match[j, "rcropf"])
  
  rf2r <- raster::raster(rv_match[j,2])
  
  mun_poly <- polyL2Pj2[[j]]
  
  #foreach(i=1:8) %do% {
    #foreach(i=1:length(polyL2Pj2)) %do% {
    
    #foreach(m=1) %do% {
      foreach(m=1:nrow(mun_poly)) %do% {
      
      rTemp1 <- raster::crop(rf2r, mun_poly[m,]) # crop raster to polygon record
      
      if (gsub("\\D", "", names(rf2r)) < 2007) {
      
      raster::writeRaster(rTemp1 , paste0(rv_match[j,"rcropf2"], "/" ,st_drop_geometry(mun_poly[m,"GEOCODIGO"]),"_", (gsub("\\D", "", names(rf2r))), ".tif"), format="GTiff", overwrite=TRUE)
      
      } else if (gsub("\\D", "", names(rf2r)) >= 2007 & gsub("\\D", "", names(rf2r)) < 2010) {
      
      raster::writeRaster(rTemp1 , paste0(rv_match[j,"rcropf2"], "/" ,st_drop_geometry(mun_poly[m,"GEOCODIG_M"]),"_", (gsub("\\D", "", names(rf2r))), ".tif"), format="GTiff", overwrite=TRUE)
      
      } else if (gsub("\\D", "", names(rf2r)) >= 2010 & gsub("\\D", "", names(rf2r)) < 2013) {
      
      raster::writeRaster(rTemp1 , paste0(rv_match[j,"rcropf2"], "/" ,st_drop_geometry(mun_poly[m,"CD_GEOCODM"]),"_", (gsub("\\D", "", names(rf2r))), ".tif"), format="GTiff", overwrite=TRUE)
      
      } else if (gsub("\\D", "", names(rf2r)) >= 2013 & gsub("\\D", "", names(rf2r)) < 2019) {
      
      raster::writeRaster(rTemp1 , paste0(rv_match[j,"rcropf2"], "/" ,st_drop_geometry(mun_poly[m,"CD_GEOCMU"]),"_", (gsub("\\D", "", names(rf2r))), ".tif"), format="GTiff", overwrite=TRUE)
      
      } else if (gsub("\\D", "", names(rf2r)) >= 2019) {
      
      raster::writeRaster(rTemp1 , paste0(rv_match[j,"rcropf2"], "/" ,st_drop_geometry(mun_poly[m,"CD_MUN"]),"_", (gsub("\\D", "", names(rf2r))), ".tif"), format="GTiff", overwrite=TRUE)
      
      } else { 
      
      print("out of range year")
      
      }
      
      print(paste0("cropped raster: ", names(rf2r), " state: ", j, " mun: ", m))
      
      rm(rTemp1)
      
    }
    
  #}
}


Sys.sleep(5)
#gc()
Sys.sleep(5)
end_time <- Sys.time()

start_time-end_time
