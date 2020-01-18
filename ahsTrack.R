# AHS tracking algorithm. sums pixel state (1=suitable, 0=unsuitable) through time and stores result 
# in new raster (ahsAv), which contains the spatially-distributed relative temporal availability of AHS

library(raster)
library(sp)
library(maptools)
library(rgeos)
library(shapefiles)
library(lattice)
library(rgdal)
library(igraph)


ahsTrack <- function(s.file, patch.path, out.ahsAv)
{
  # grab file names
  patch.files <- list.files(patch.path, pattern=".shp", full.names=TRUE)
  
  # read substrate raster
  subst <- raster(readAsciiGrid(s.file))
  
  # empty raster template, used for rasterizing patch polygons at each time step
  pr <- subst
  
  # emtpy raster to store accumulated pixel duration
  ahsAv <- pr; ahsAv[]=0
  
  # loop through all time steps:
  pb <- txtProgressBar(min=0, max=length(patch.files), width=NA, style=3)
  for(i in 1:length(patch.files))
  {
    setTxtProgressBar(pb, i)
    
    # read patch shape file
    p <- readShapeSpatial(patch.files[i])
    
    # clear template
    pr[]=0
    
    # rasterize polygons and make background = 0
    pr <- rasterize(p, pr, field=1)
    pr[is.na(pr)]=0
    
    # accumulate pixel duration
    ahsAv <- ahsAv + pr
    
  }; close(pb)
  
  # compute relative ahs duration
  ahsAv <- ahsAv/length(patch.files)
  
  # write output ahsAv raster
  writeRaster(ahsAv, out.ahsAv, overwrite=TRUE)
}
