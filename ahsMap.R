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
library(shapefiles)
library(lattice)
library(rgdal)
library(igraph)



# feature1










