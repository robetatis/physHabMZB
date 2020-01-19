# function reads rasters for water depth, x- and y-velocities (depth-averaged, from shallow water model), 
# and suitable substrates (defined as binary variable, 1=suitable substrate, 0=unsuitable). then runs through all 
# time steps and:
#   1. performs overlay according to tolerance and substrate associations
#   2. builds patches using single-scan connected-component labeling ('flood-fill', column-major linear indexing) 
#      and rasterToPolygons (package 'raster')
#   3. writes resulting polygons to ESRI shapefile for each time step

library(raster)
library(sp)
library(maptools)
library(rgeos)
# library(shapefiles)
# library(lattice)
# library(rgdal)
#library(igraph)

ahsMap <- function(wdCrit, vCrit, vx.path, vy.path, wd.path, s.file, out.path)
{
  flush.console()
  print(paste("starting time: ", Sys.time()))
  
  # make vectors with list of velocity and water depth files
  vx.files <- list.files(vx.path, pattern=".Vx", full.names=TRUE)
  vy.files <- list.files(vy.path, pattern=".Vy", full.names=TRUE)
  wd.files <- list.files(wd.path, pattern=".wd", full.names=TRUE)
  
  # read substrate raster
  subst <- raster(readAsciiGrid(s.file))
  
  # loop through all time steps
  pb <- txtProgressBar(min=0, max=length(vx.files), width=NA, style=3)
  for(i in 1:length(vx.files))
  {
    setTxtProgressBar(pb, i)
    
    # read vx, vy and depth
    vx    <- raster(readAsciiGrid(vx.files[i]))
    vy    <- raster(readAsciiGrid(vy.files[i]))
    wd    <- raster(readAsciiGrid(wd.files[i]))
    
    # make velocity magnitude raster
    wdM  <- as.matrix(wd)
    vxM  <- as.matrix(vx)
    vyM  <- as.matrix(vy)
    vxM  <- 0.5*(vxM[, 1:(ncol(vxM)-1)] + vxM[, 2:ncol(vxM)]) # adjust matrix dimensions, because 
    vyM  <- 0.5*(vyM[1:(nrow(vyM)-1), ] + vyM[2:nrow(vyM), ]) # vx and vy are different in LISFLOOD
    vMag <- sqrt(vxM^2+vyM^2)
    vMag <- raster(vMag, template=wd)
    vMag[wd==0]=NA
    
    # make aquatic habitable space (ahs) raster by overlaying wd, vMag and substrate. 
    # results is binary (1=habitable pixel, 0=unsuitable pixel)
    ahs <- (wd>wdCrit & vMag<vCrit & subst==1)
    ahs[ahs!=1]=NA
    
    
    # build patches using connected component labeling - single-scan, flood-fill:
    
    # if there are no patches, make dummy SpatialPolygonsDataframe with one polygon of zero area
    if(is.null(ahs[!is.na(ahs[])]))
    {
      xmin     <- extent(ahs)@xmin
      ymax     <- extent(ahs)@ymax
      cellsize <- res(ahs)[1]*0
      
      ahsp <- Polygon(cbind(c(xmin, xmin, xmin + cellsize, xmin + cellsize, xmin),
                            c(ymax - cellsize, ymax, ymax, ymax - cellsize, ymax - cellsize)
      )
      )
      ahsp <- SpatialPolygons(list(Polygons(list(ahsp), 1)))		
      ahsp <- SpatialPolygonsDataFrame(ahsp, data=data.frame(1))
      
    } else {
      
      # do one-scan ("flood-fill") connected component labeling using Moore neighborhood 
      # (https://en.wikipedia.org/wiki/Connected-component_labeling, "One-pass version"):
      
      
      ahs[is.na(ahs)]=0      # replace NAs by zero
      
      # extend ahs raster to avoid having to check for boundary and corner cells, and make working matrix xx2
      xx  <- as.matrix(ahs)			
      xx2 <- rbind(rep(0, times=ncol(xx)), xx)
      xx2 <- rbind(xx2, rep(0, times=ncol(xx)))
      xx2 <- cbind(rep(0, times=nrow(xx2)), xx2)
      xx2 <- cbind(xx2, rep(0, times=nrow(xx2)))
      
      # make and initialize objects for connected component labeling
      m             <- nrow(xx2)
      n             <- ncol(xx2)
      connected     <- matrix(0, m, n)    # emtpy patch matrix (will contain cells labeled according to patch 
      #                                     membership, i.e., patch ID)
      offsets       <- c(-1,              # make offsets for Moore neighborhood (linear indexes, column-major)
                         m,           
                         1, 
                         -m,    
                         -(m+1), 
                         -(m-1),
                         (m+1), 
                         (m-1)
      ) 
      
      index         <- numeric() # make emtpy vector to hold (linear) indices of suitable connected pixels 
      
      no_of_objects <- 0         # number of patches found
      
      # run through ahs matrix and build patches
      for(j in 2:(m-1))
      {
        for(k in 2:(n-1))
        {
          if(xx2[j, k]==1) # 1st pixel of patch found?
          {
            no_of_objects    = no_of_objects + 1 # update current patch ID
            
            index            = (k-1)*m + j       # get linear index (column-major) of the patch's 1st pixel
            
            connected[index] = no_of_objects     # label patch's 1st pixel with current patch ID
            
            # do as long as there are suitable connected neighbors:
            while(length(index) > 0)
            {
              xx2[index] = 0                     # make detected connected suitable pixels in working matrix = 0
              #                                    so they are not found again
              
              neighbors <- numeric()             # make emtpy vector of Moore neighbors
              
              # loop through indices of suitable Moore neighbors
              for(l in 1:length(index))
              {
                neighbors <- c(neighbors, index[l] + offsets)          # collect linear indices (column-major) of
                #                                                        all Moore neighbors (cumulatively)
              }
              neighbors        <- unique(neighbors)                    # get rid of repeated indices
              
              index            = neighbors[which(xx2[neighbors] != 0)] # go to Moore neighbors in working matrix,
              #                                                          check which are suitable and update
              #                                                          index with those
              
              connected[index] = no_of_objects                         # label those cells with current patch ID
            }
          }
        }
      }
      ahsc <- connected[2:(nrow(connected)-1), 2:(ncol(connected)-1)] # get rid of ghost cells in patch matrix
      ahsc <- raster(ahsc, template=ahs)
      ahsc[ahsc==0]=NA
      
      
      # make patch polygons by dissolving and unioning pixels
      ahsp <- rasterToPolygons(ahsc, dissolve=TRUE, digits=4)
    }
    
    
    # write polygon ESRI shapefile
    writeOGR(ahsp, dsn=out.path, layer=paste0("ahs", formatC(i, width=nchar(length(vx.files)), 
                                                             flag=0, digits=0, format="f")), 
             driver="ESRI Shapefile", overwrite_layer=TRUE)
  }
  
  close(pb)
  
  flush.console()
  print(paste("end time: ", Sys.time()))
  
}
