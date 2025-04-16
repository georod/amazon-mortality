#==============================
# # Crop tool - Crop GEE rasters
#=============================

## 2025-03-19
## Peter R.

# ## Aim: Crop GEE Amazon into smaller county-year rasters to speed up processing

# ## Notes
# - Para is missing ask Fatima
# - terra is having problems reading Portuguese shp. However, the new shp only have
#   one column with no Portuguese characters.



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

# raster names for files
#files2 <- list.files(path='./data/lcover_gee/amazon', pattern='\\.tif$', full.names=FALSE)


#list.files(path="./data/shp/mesoregions", pattern="\\.shp", full.names=TRUE)
#files3 <- list.files(path='./data/shp/municipalities2', pattern='\\.shp$', full.names=TRUE, recursive = TRUE)

# vector names for files
#files4 <- list.files(path='./data/shp/municipalities2', pattern='\\.shp$', full.names=FALSE, recursive = TRUE)


#prefix1 <- "pcm_" # projected, cropped & masked

#outf <- "./output1/mun_rasters2/" # Out folder were clipped/cropped files are saved 

outf <- "/home/georod/projects/def-mfortin/georod/data/amazon/output1/mun_rasters2/"

##----------------------------------------
## Create raster & vector year match table
##----------------------------------------
## This table is needed to ensure raster is cropped with matched vector
#
## This worked for original file structure. see ./shp/municpalities/
## but for new file structure this can be simpler
##vFilesDf <- cbind.data.frame("vyear"=gsub("\\D", "", (do.call(rbind, strsplit(as.character(files4),'/'))[,1])), "vname"=files3 )
#vFilesDf <- cbind.data.frame("vyear"=gsub("\\D", "", files4), "vname"=files3 )
#vFilesDf$uf <- do.call(rbind, strsplit(as.character(files4),'_'))[,1]
#vFilesDf$rcropf <- paste0(outf, do.call(rbind, strsplit(as.character(files4),'.shp'))[,1])
#rFilesDf <- cbind.data.frame("ryear"=substring(gsub("\\D", "", files2), 3, 6), "rname"=files1)
#
#head(rFilesDf)
## recode, raster year paired with vector year
##paste(1980:1991, ",", 1991)
## m1 <- matrix(c(
##   2000,2000,
##   2001, 2001,
##   2002, 2001,
##   2003, 2001,
##   2004, 2001,
##   2005, 2005,
##   2006, 2005,
##   2007, 2007,
##   2008, 2007,
##   2009, 2007,
##   2010, 2010,
##   2011, 2010,
##   2012, 2010
## ), byrow=TRUE, nrow=13, ncol=2)
#
## I need to add uf/state given than the shp pattern are not the same across all uf/states
#m1 <- foreach(i=1:nrow(vFilesDf), .combine = rbind) %do% {
#  
#cbind.data.frame("year"=substr(vFilesDf$vyear,1,4)[i]:substr(vFilesDf$vyear,5,8)[i], "vyear"=vFilesDf$vyear[i], "uf"=vFilesDf$uf[i])
#  
#}
#

##dim(m1)
#str(m1)
#
##m10 <- m10[!duplicated(m10),]
#
##unique(vFilesDf$vyear)
#
## This for loop matches values before assigning to new object.
## Does not work with new data structure. E.g., 1981-1991
## for (i in 1:nrow(m1)) {
##   
##   rFilesDf$ryear2[rFilesDf$ryear2==as.numeric(m1[i,1])] <- as.numeric(m1[i,2])
##   
## }
#
## For the new folder/file names/structure a SQL is better
## the rastes and object m1 have single year series
## this query will create more records than those in rFilesDf which is expected
## as the we have one Amzon-wide raster to be processes across different shp boundaries
#rFilesDf <- sqldf("SELECT t1.*, t2.vyear AS ryear2, t2.uf FROM rFilesDf t1 JOIN m1 t2 ON t1.ryear=t2.year")
##dim(rFilesDf) # 112 4
#
#
##file match table. This table ensures raster and vectors are match by year and state
##rv_match <- sqldf("SELECT * FROM rFilesDf t1 JOIN vFilesDf t2 ON t1.ryear2=t2.vyear")
#rv_match <- sqldf("SELECT * FROM rFilesDf t1 JOIN vFilesDf t2 ON t1.ryear2=t2.vyear AND t1.uf=t2.uf")
##dim(rv_match) # 112 8
#rv_match <- rv_match[rv_match$ryear %in% c(2000, 2001),] # No data for 2001 as Fatima has not sent yet!
#

source("./scripts/crop_tool_code_match_table_v1.R")

#===============================
# Load data
#===============================

# subset data to do only PA. To remove once done

rv_match <- rv_match[rv_match$uf=='PA' & rv_match$ryear2=="19921996",]

# It seems that terra object's can't be easily sent to parallel loops. 
#  Hence, I have to rely on raster
# Project shp to crop GEE raster which is in proj 4326

#Given that some rasters are in 3857 and other in WGS84, I need to transform vect to WGS84 first

polyL2Pj2  <- foreach (i=1:nrow(rv_match)) %do% {
  
  shp1 <- sf::st_read(rv_match[i, 6])
  if (is.na(sf::st_crs(shp1))) sf::st_set_crs(shp1, 4674) else sf::st_transform(shp1, 4326)
  
}

# polyL2Pj2  <- foreach (i=1:8) %do% {
# 
#   shp1 <- sf::st_read(rv_match[i, 6])
#   if (is.na(sf::st_crs(shp1))) sf::st_set_crs(shp1, 4674) else sf::st_transform(shp1, 4326)
# 
# }




# remove object to save space
#rm(polyL2Pj)  

print("rasters and vectors loaded & transformed")

Sys.sleep(3)

#start_time <- Sys.time()



#-------------------
#Crop rasters
#-------------------

start_time <- Sys.time()


# Darjeeling
#setwd("/home/peterr/projects/lcmor")

# DRAC
setwd("/home/georod/projects/def-mfortin/georod/data/amazon/") # Here is were the cropped raster are


foreach(j=1:nrow(rv_match), .packages=c('doParallel','foreach','terra', 'sf')) %do% {
#foreach(j=1:nrow(rv_match), .packages=c('doParallel','foreach','raster', 'sf')) %do% {
  
  path1 <- rv_match[j,"rcropf"]
  if (!dir.exists(path1)) dir.create(path1, recursive = TRUE)
  
  rf2r <- terra::rast(rv_match[j,2])
  
  mun_poly <- terra::vect(polyL2Pj2[[j]])
  
  foreach(m=1:nrow(mun_poly), .packages=c('terra')) %do% {
      
      rTemp1 <- terra::crop(rf2r, terra::project(mun_poly[m,], terra::crs(rf2r))) # crop raster to polygon record
      
      terra::writeRaster(rTemp1 , filename=paste0(path1, "/" ,
      terra::as.data.frame(mun_poly[m,1]),"_", (gsub("\\D", "", names(rf2r))), ".tif"), overwrite=TRUE)
      
      print(paste0("cropped raster: ", names(rf2r), ", state: ", rv_match[j,4], ", mun: ", m , ", shp: ", rv_match[j,5]))
      
    }
    

}


Sys.sleep(5)
gc()
Sys.sleep(5)
end_time <- Sys.time()

start_time-end_time
