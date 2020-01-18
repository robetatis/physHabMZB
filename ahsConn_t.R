# AHS fragmentation analysis using potential connectivity. x* random points within each patch at every
# user-entered time step. passive dispersal journeys follow direction of velocity field. the algorithm
# counts how many such passive journeys end within AHS
# codes in journeyMatrix: 0 = unsucessful journey (does not end in AHS)
#                         1 = sucessful journey (ends in AHS)
#                         2 = unusable journey
# codes in journeyi have different meaning (1=usable, 2=unusable)
# *x is proportional to patch size. also, the total number of seeding points is proportional to 
#  the total AHS in each time step. the density of seeding points (No./AHS) is defined by the user
#  through parameter "seed.dens"

library(raster)
library(sp)
library(maptools)
library(rgeos)
library(shapefiles)
library(lattice)
library(rgdal)
library(igraph)


ahsConn_t <- function(vx.path, vy.path, wd.path, s.file, patch.path,
                      ctresults.file, 
                      dtc, lambda, tend, seed.dens)
{
  flush.console()
  print(paste("starting time: ", Sys.time()))
  
  # matrix to hold results
  cc           <- matrix(ncol=2, nrow=0)
  colnames(cc) <-  c("timeStep", "c")
  
  # make vectors with list of velocity and water depth files
  vx.files    <- list.files(vx.path, pattern=".Vx", full.names=TRUE)
  vy.files    <- list.files(vy.path, pattern=".Vy", full.names=TRUE)
  wd.files    <- list.files(wd.path, pattern=".wd", full.names=TRUE)
  patch.files <- list.files(patch.path, pattern=".shp", full.names=TRUE)
  
  # make domain bounding box based on substrate map; used to determine whether there are journey points
  # outside the domain
  subst <- raster(readAsciiGrid(s.file))
  
  xmin <- extent(subst)@xmin
  xmax <- extent(subst)@xmax
  ymin <- extent(subst)@ymin
  ymax <- extent(subst)@ymax
  
  domain <- SpatialPolygons(list(Polygons(list(Polygon(cbind(c(xmin, xmin, xmax, xmax, xmin),
                                                             c(ymin, ymax, ymax, ymin, ymin)))),
                                          1))
  )
  
  
  # loop through time with user-entered steps (dtc)
  tt <- seq(1, tend, by=dtc)
  
  for(i in tt)
  {
    # emtpy matrix to hold the characteristics of the hypothetical journeys (starting point, 
    # source patch, contact with another patch? (0/1)) in current time step
    journeyMatrix           <- matrix(ncol=4,nrow=0)
    colnames(journeyMatrix) <- c("startX","startY","sourcePatch","endAHS")
    
    # read velocity components and water depth 
    vx    <- raster(readAsciiGrid(vx.files[i]))
    vy    <- raster(readAsciiGrid(vy.files[i]))
    vy[]  <- -vy[] # because y-vel. component is negative upwards in LISFLOOD
    wd    <- raster(readAsciiGrid(wd.files[i]))
    
    # read patch polygons
    p <- readShapeSpatial(patch.files[i])
    
    # get total AHS in current time step and estimate total number of seeding points
    totAHS <- 0
    for(j in 1:length(p)) { totAHS <- totAHS + p@polygons[[j]]@area }
    totSeedN <- ceiling(totAHS*seed.dens)
    
    
    # if no patches in current time step, make c=0 and go to next step.
    # this step relies on what ahsMap.R does when there are no patches: it creates one polygon of 
    # area = zero in the upper-left corner of the SpatialPolygons object. this is the polygon used
    # here to skip to the next step
    if(p@polygons[[1]]@area == 0)
    {
      cc <- rbind(cc, c(i, 0))
      next
    } else {
      
      # make velocity direction and velocity magnitude raster
      wdM  <- as.matrix(wd)
      vxM  <- as.matrix(vx)
      vyM  <- as.matrix(vy)
      vxM  <- 0.5*(vxM[, 1:(ncol(vxM)-1)] + vxM[, 2:ncol(vxM)]) # adjust matrix dimensions, because
      vyM  <- 0.5*(vyM[1:(nrow(vyM)-1), ] + vyM[2:nrow(vyM), ]) # vx and vy are different in LISFLOOD
      vDir <- atan2(vyM, vxM)
      vDir <- raster(vDir, template=wd)
      vMag <- sqrt(vxM^2+vyM^2)
      vMag <- raster(vMag, template=wd)
      
      
      # generate random seeding points (journey starting points) inside all patches in current time step:
      for(j in 1:length(p@polygons))
      {
        # make SpatialPolygons object with current polygon
        polyj <- SpatialPolygons(list(p@polygons[[j]]))
        
        # compute number of points for current patch in proportion to its area
        curSeedN <- ceiling(polyj@polygons[[1]]@area/totAHS*totSeedN)
        
        # make emtpy "seed data frame" to hold resulting seeding points
        sDF2  <-matrix(ncol=2,nrow=0)
        
        # repeat until required number of random seeding points inside current patch is found
        while (nrow(sDF2) < curSeedN)
        {
          # generate random xy coordinates inside bounding box of current patch
          xRand <- runif(1, min=extent(polyj)@xmin, max=extent(polyj)@xmax)
          yRand <- runif(1, min=extent(polyj)@ymin, max=extent(polyj)@ymax)
          
          # does this point fall within the patch? (gives NA if not)
          pOver        <- over(x=SpatialPoints(data.frame(xRand,yRand)), 
                               y=polyj)
          
          # store this point's coordinates and overlay status in data frame
          sDF          <- data.frame(xRand, yRand, pOver)
          
          # drop point if outside polygon
          sDF          <- sDF[which(!is.na(sDF[[3]])), 1:2]
          
          # grow current patch's seeding points matrix
          sDF2         <- rbind(sDF2,sDF)
        }
        
        # format current patch's seeding points matrix
        sDF2[,3]       <- rep(j, nrow(sDF2))
        sDF2[,4]       <- NA
        colnames(sDF2) <- c("startX","startY","sourcePatch","endAHS")
        
        # grow journeyMatrix
        journeyMatrix  <- rbind(journeyMatrix, as.matrix(sDF2)) 
      }
      
      
      # calculate journeys for all seed points and overlay them to patch map
      for(j in 1:nrow(journeyMatrix))
      {
        # draw random distance from dispersal kernel with decay rate = lambda
        journeyMax  <- rexp(1,rate=lambda)
        
        # make matrix to record journey, col.1 = x-coord., col.2 = y-coord., col.3 = journey code
        # journey codes: 1  -> usable (inside domain and displacement at least 1 step)
        #                2  -> unusable (either falls outside domain or starts in dead-water zone)
        journeyi           <- matrix(nrow=1, ncol=3)
        colnames(journeyi) <- c("x", "y", "code")
        
        # make 1st point of journey equal to starting point
        journeyi[1, 1:2] <- journeyMatrix[j, 1:2]
        journeyi[1, 3]   <- 1
        
        # calculate journey:
        journeyLength <- 0; l <- 1
        repeat
        {
          # if point lies outside domain, ignore journey (code=2) and break while loop
          inDomain <- over(x=SpatialPoints(data.frame(x=journeyi[l, 1], y=journeyi[l, 2])), 
                           y=domain)
          
          if(is.na(inDomain))
          {
            journeyi[, "code"] <- 2
            break
            
            # otherwise, continue journey
          } else {
            
            # get local water depth flow direction and velocity
            wdi <- extract(wd,   matrix(c(journeyi[l, 1], journeyi[l, 2]), ncol=2,nrow=1))
            fdi <- extract(vDir, matrix(c(journeyi[l, 1], journeyi[l, 2]), ncol=2,nrow=1))
            vmi <- extract(vMag, matrix(c(journeyi[l, 1], journeyi[l, 2]), ncol=2,nrow=1))

            # cell wet?
            if(wdi > 0)
            {
              # flowing water (vMag > 0.001)?
              if(vmi > 0.001)
              {
                # new point is usable
                journeyi[l, "code"] <- 1
                
                # update journey
                newx <- journeyi[l, 1] + res(wd)[1]*cos(fdi)
                newy <- journeyi[l, 2] + res(wd)[1]*sin(fdi)
                
                journeyi      <- rbind(journeyi, c(newx, newy, NA))
                journeyLength <- journeyLength + sqrt((journeyi[l+1, 1] - journeyi[l, 1])^2 + 
                                                        (journeyi[l+1, 2] - journeyi[l, 2])^2)
                
                l <- l+1
                
                # if journey ends normally (max. length reached without interruption)
                if(journeyMax < journeyLength) 
                {
                  journeyi[l, "code"] <- 1
                  break
                }
                
                # stagnant water ("dead-water zone", v<0.001 m/s)
              } else {
                
                if(l==1) # if journey started in dead-water zone, it is unusable (no displacement)
                {
                  journeyi[, "code"] <- 2
                  break
                  
                } else { # if journey started ok and then ran into stagnant water, it is usable
                  journeyi[l, "code"] <- 1
                  break
                }
              }
              
              # if journey started ok and then ran into shoreline area (dry cell), it is usable
            } else {
              journeyi[l, "code"] <- 11
              break
            }
          }
        }
        
        
        # assess journey:
        # is it usable?
        if(length(which(journeyi[, "code"] == 2)) == 0)
        {
          
          # journeys that ran into dry area are assessed with their second last point (otherwise they
          # would be seen as outside AHS, which is unrealistic since shoreline area is not really
          # bad)
          if(length(which(journeyi[, "code"] == 11)) > 0)
          {
            journeyEnd <- data.frame(x=journeyi[nrow(journeyi) - 1, 1],
                                     y=journeyi[nrow(journeyi) - 1, 2])
          } else
          {
            journeyEnd <- data.frame(x=journeyi[nrow(journeyi), 1],
                                     y=journeyi[nrow(journeyi), 2])
          }
          
          
          # is end point within AHS?
          inAHS                      = over(x=SpatialPoints(journeyEnd), y=p)$layer
          journeyMatrix[j, "endAHS"] = ifelse(!is.na(inAHS), 1, 0)

          # otherwise, it is unusable, set journey code = 2 and go to next journey
        } else {
          journeyMatrix[j, "endAHS"] = 2
          next 
        }
      }
      
      # cc is the fraction of successful usable journeys (end within AHS)
      nusable <- length(journeyMatrix[journeyMatrix[, "endAHS"] != 2, "endAHS"])
      nsucc   <- length(journeyMatrix[journeyMatrix[, "endAHS"] == 1, "endAHS"])
        
      if(nusable == 0) # avoid division by zero
      {
        cc <- rbind(cc, c(i, nusable))
        
      } else {
        cc <- rbind(cc, c(i, nsucc/nusable))
      }
    }
    
    flush.console()
    print(paste("computing connectivity, step ", i, "/", tend))
    
  }
  
  write.table(cc, file=ctresults.file, sep=";", row.names=FALSE, quote=FALSE, append=FALSE)
  
  flush.console()
  print(paste("end time: ", Sys.time()))
  
  
}
