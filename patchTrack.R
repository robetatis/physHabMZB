# patch tracking algorithm based on patch cell signatures.
# new patch appearance...ok
# old patch re-appearance...ok
# patch merging....no!! -> re-appearing larger signature allocated to previous shorter one. other fragments get zero area
#                       -> spurious local disappearances, although total area ok.
#                       -> maybe fractional contribution of daughters to mother patch?
# patch splitting...    -> ??




library(raster)
library(sp)
library(maptools)
library(rgeos)
library(shapefiles)
library(lattice)
library(rgdal)
library(igraph)


s.file     <- "c:\\presentations_papers\\paper2\\cr\\sr.asc"
out.areas  <- "c:\\presentations_papers\\paper2\\cr\\areasMatrix.txt"
patch.path <- "c:\\presentations_papers\\paper2\\cr\\patchMaps\\"

patchTrack <- function(s.file, patch.path, out.areas)
{
  # grab file names
  patch.files <- list.files(patch.path, pattern=".shp", full.names=TRUE)
  
  # read substrate raster
  subst <- raster(readAsciiGrid(s.file))
  
  # create cell IDs raster using substrate raster as template (extent and cell size)
  cellIDrast   <- subst
  cellIDrast[] <- 1:length(cellIDrast[])
  
  # make lists for storing the system's signature structure
  patchSigList     <- list() # list of current signatures
  signatureList    <- list() # final results list: contains patches and their signatures at each time step. 
  # level 1 = time step, level 2 = patches
  
  # store number of signatures in each time step
  numSig <- numeric()
  
  
  # loop through all  time steps:
  for(i in 1:10)#length(patch.files))
  {
    # increase list by one
    signatureList[[i]] <- list()
    
    # read patch shape file
    p <- readShapeSpatial(patch.files[i])
    
    # extract patch signatures
    extr <- extract(cellIDrast, p)
    
    # declare emtpy list to hold current signatures
    patchSigList <- list()
    
    # store current patch signatures in list
    for(j in 1:length(extr)) {patchSigList[[j]] <- extr[[j]]}
    
    # update maximum element length in signatureList
    numSig[i] <- length(patchSigList)
    
    # if time step contains habitat patches
    if(length(patchSigList) != 0)
    {
      # if this is the first time step with patches, simply add these signatures to the list
      if(i == min(which(numSig>0)))
      {
        # start history with these signatures and go to next time step
        signatureList[[i]] <- patchSigList
        
      } else {
        
        # loop through signatures in current time step and look in history for matches
        # possibilities: current signature found in history -> pre-existing patch, signature added 
        #                                                      to corresponding history element
        for(j in 1:length(patchSigList))
        {
          marker <- 0 # this is a flag to indicate whether the current signature was found in the signature history
          
          # loop over previous time steps
          for(k in 1:(i-1))
          {
            # if current past time step is not emtpy, loop through its signatures looking for the current one
            if(length(signatureList[[k]]) > 0)
            {
              # loop over patch signatures of past time step k
              for(l in 1:length(signatureList[[k]]))
              {
                # if current signature is found in history, add it to current location in history, activate flag 
                # (marker) and break out of loop
                if(length(intersect(patchSigList[[j]], signatureList[[k]][[l]])) > 0)
                {
                  
                  # how to check for signature merging and splitting? **********************************
                  
                  signatureList[[i]][[l]] <- patchSigList[[j]]
                  marker <- 1
                  break
                }
              }
              if(marker==1) break  
            }
          }
          
          # if loop over past time steps finishes without finding the current signature (marker=0) -> new patch
          # open new element in history (last position) and put current signature there
          if(marker == 0)
          {
            length(signatureList[[i]])                       <- length(signatureList[[i]]) + 1
            signatureList[[i]][[length(signatureList[[i]])]] <- patchSigList[[j]]
          }
        }
      }
      
      # if current time step is empty (no habitat patches), make current time step an empty list
    } else {
      signatureList[[i]] <- list()
    }
    
    flush.console()
    print(paste("building signature structure, step ", i, "/", length(patch.files)))	
  }
  
  
  # extend length of all list elements to maximum value
  maxLen <- max(as.numeric(lapply(signatureList, length)))
  for(i in 1:length(signatureList)) {length(signatureList[[i]]) <- maxLen}
  
  # loop through signature structure and build areas matrix
  areasMatrix <- matrix(ncol=maxLen, nrow=30)#length(patch.files))
  for(i in 1:length(signatureList))
  {
    for(j in 1:length(signatureList[[i]]))
    {
      areasMatrix[i, j] <- length(signatureList[[i]][[j]]) * res(subst)[1]
    }
  }	
  areasMatrix[is.na(areasMatrix)] <- 0
  
  # write output file
  write.table(areasMatrix, file=out.areas, quote=FALSE, sep=";", row.names=FALSE, col.names=FALSE)
}



plot(areasMatrix[1], type="l")



