# physHabMZB
Functions for quantifying temporal habitat dynamics in rivers.

These scripts implement an approach for transient simulation of physical habitat for stream invertebrates. The method and its foundations are described in detail [in this paper](https://onlinelibrary.wiley.com/doi/full/10.1002/eco.2066).

## ahsMap.R
Reads rasters for water depth, x- and y-velocities (depth-averaged, from shallow water model), and suitable substrates (defined as binary variable, 1=suitable substrate, 0=unsuitable). then runs through all  time steps and:
   1. Performs overlay according to tolerance and substrate associations
   2. Builds patches using single-scan connected-component labeling ('flood-fill', column-major linear indexing) and rasterToPolygons (package 'raster')
   3. Writes resulting polygons to ESRI shapefile for each time step

## ahsTrack.R
AHS (Aquatic Habitable Space) tracking algorithm. Sums pixel state (1=suitable, 0=unsuitable) through time and stores result in new raster (ahsAv), which contains the spatially-distributed relative temporal availability of AHS

## patchTrack.R
Attempt at a patch tracking algorithm based on patch cell signatures. So far, the approach correctly handles the appearance of new AHS pathes and the re-appearance of old AHS patches. However, patch merging and splitting are still a problem:
* Re-appearing larger signature allocated to previous shorter one. other fragments get zero area
* Spurious local disappearances, although total area ok.
* Maybe fractional contribution of daughters to mother patch?

## ahsConn_t.R
AHS fragmentation analysis using potential connectivity. n (see below) random points within each patch at every user-entered time step. Passive dispersal journeys follow direction of velocity field. The algorithm counts how many such passive journeys end within AHS.

Codes in journeyMatrix:
0 = unsucessful journey (does not end in AHS)
1 = sucessful journey (ends in AHS)
2 = unusable journey

Codes in journeyi have different meaning (1=usable, 2=unusable).

n is proportional to patch size. Also, the total number of seeding points is proportional to the total AHS in each time step. The density of seeding points (No./AHS) is defined by the user through parameter "seed.dens".

## adjustShear.f
Bottom shear stress adjustment with Bezzola's approach: takes depth-averaged velocity (at 40% water depth) and computes near-bed
velocity and shear stress (tau) based on the thickness of the roughness sublayer, which is calculated based on substrate grain size.
This routine is coded as a stand-alone FORTRAN90 program meant to be executed from the same directory as the input files.
