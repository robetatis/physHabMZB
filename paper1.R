library(shape)
library(dplyr)
library(GISTools)
library(akima)
library(raster)
library(sp)
library(maptools)
library(rgeos)
library(shapefiles)
library(lattice)
library(rgdal)
library(quantreg)
library(TeachingDemos)
library(shape)O
library(GISTools)
library(igraph)
library(rtiff)
library(lmodel2)
library(shape)

# figure 1, study area
{
  bl          <- readShapeSpatial("c:\\doktor\\spatialData\\blaendWGS")
  germany     <- unionSpatialPolygons(bl,rep(1,times=length(bl@data[[4]])))
  hessen      <- list();
  hessen[[1]] <- bl@polygons[[5]]
  hessen      <-SpatialPolygons(hessen)
  lahn        <- readShapeSpatial("c:\\doktor\\spatialData\\lahnWGS")
  wetschaft   <- readShapeSpatial("c:\\doktor\\spatialData\\wetschaftWGS")
  pts         <- read.table("c:\\doktor\\spatialData\\pts.txt",header=TRUE,sep=",")
  
  par(mar=c(5,7,1,1))
  plot(bl,border=NA,xlim=c(8.37-.05,8.87-.05),ylim=c(50.6,50.95),asp=1)
  axis(1,at=round(digits=2,seq(from=extent(germany)@xmin-2,to=extent(germany)@xmax+2,by=.1)),las=1, cex.axis=1.5, padj=.5)
  axis(2,at=round(digits=2,seq(from=extent(germany)@ymin-2,to=extent(germany)@ymax+2,by=.1)), cex.axis=1.5, las=2)
  axis(3,at=round(digits=2,seq(from=extent(germany)@xmin-2,to=extent(germany)@xmax+2,by=.1)),labels=NA,lwd.ticks=0)
  axis(4,at=round(digits=2,seq(from=extent(germany)@ymin-2,to=extent(germany)@ymax+2,by=.1)),labels=NA,lwd.ticks=0)
  lines(lahn,lwd=3)
  lines(wetschaft, lwd=2)
  Arrows(x0=8.5, y0=50.87, x1=8.55, y1=50.85, arr.type="triangle", lwd=2)
  Arrows(x0=8.68, y0=50.95, x1=8.69, y1=50.92, arr.type="triangle", lwd=2)
  points(pts[[4]][4:6],pts[[5]][4:6],pch=22,cex=1.75, col="black", bg="white", lwd=3)
  points(pts[[4]][1:3],pts[[5]][1:3],pch=17,cex=1.75)
  text(x=pts[[4]][4]-0.015,y=pts[[5]][4]+.025,labels="w",cex=2)
  text(x=pts[[4]][5]+0.012,y=pts[[5]][5]+.025,labels="l",cex=2)
  text(x=pts[[4]][6]+.019,y=pts[[5]][6],labels="c",cex=2)
  text(x=pts[[4]][1]-.025,y=pts[[5]][1]-.025,labels="gs",cex=2)
  text(x=pts[[4]][2]+.03,y=pts[[5]][2]+.016,labels="gb",cex=2)
  text(x=pts[[4]][3]+.015,y=pts[[5]][3]+.027,labels="gn",cex=2)
  mtext("Longitude [deg]",1,line=3.5,cex=2)
  mtext("Latitude [deg]",2,line=5.5,cex=2)
  text(x=8.69,y=50.6,labels="Lahn",cex=2,srt=60)
  text(x=8.75, y=51.02, labels="Wetschaft", cex=2)
  points(x=8.34, y=50.8-.05, pch=22, cex=1.75, col="black", bg="white", lwd=3)
  text(x=8.36, y=50.8-.05, labels="Study sites", cex=1.5, adj=0)	
  points(x=8.34, y=50.77-.05, pch=17, cex=1.75)
  text(x=8.36, y=50.77-.05, labels="Stream gauges", cex=1.5, adj=0)
  par(fig=c(0.2093,.4093,.15,.35),mar=c(0,0,0,0),new=TRUE)
  plot(germany,border="grey50",axes=F,asp=1.5,col="white",bg="white")
  box(which="plot")
  plot(hessen,add=TRUE,border="grey50")
  lines(lahn,lwd=2)
}


# figure 2, aerial orthophoto wr and wt with survey basis info for dem
{
  # wr
  setwd("c:\\doktor\\proof_of_concept\\spatial\\")
  wr      <- raster("wrTif.tif")
  wrProf  <- readShapeSpatial("profsutm.shp")
  wrPts   <- readShapeSpatial("wrPtsFinal2.shp")
  wtPts   <- readShapeSpatial("wtPtsFinal2.shp")
  wrGuide <- readShapeSpatial("wrZonesUTM2.shp")
  wtGuide <- readShapeSpatial("wtZonesUTM3.shp")
  
  grayscale_colors <- gray.colors(100,            
                                  start = 0.0,    
                                  end = 0.9,      
                                  gamma = 2.2,
                                  alpha = NULL)   
  
  
  #windows(width=8.5, height=3.5)
  
  png("c:/presentations_papers/paper1/figsRevision/fig2.png",
      width=8.5, height=3.5, units="in", res=2000)
  par(mar=c(3,3,1,1), mfcol=c(1,2))
  plot(wr, col=grayscale_colors, xlim=c(463810, 464200), ylim=c(5641900, 5642160),
       legend=FALSE, axes=FALSE)
  points(wrProf, pch="+", col="white", cex=0.5)
  points(wrPts, pch=20, col="white",cex=0.5)
  plot(wrGuide, add=TRUE, border="white", lwd=1.5)
  box()
  axis(1, at=seq(463850, 464150, length=5), cex.axis=0.7, padj=-1.5)
  axis(2, at=seq(5641900, 5642160, length=4), cex.axis=0.7, padj=1.0)
  mtext("Easting [m]", 1, line=1.75,cex=1)
  mtext("Northing [m]", 2, line=2, cex=1)
  rng <- par("usr")
  text("(A) Wallau, restored", 
       x=rng[1]+(rng[2]-rng[1])*.02, 
       y=rng[4]-(rng[4]-rng[3])*.05, 
       adj=0)
  polygon(x=c(rng[2]-215, rng[2]-215, rng[2], rng[2]),
          y=c(rng[3], rng[3]+55, rng[3]+55, rng[3]), 
          col="white")
  text("Topographic points +", x=rng[2]-200, y=rng[3]+40, adj=0)
  text("In-stream points", x=rng[2]-175, y=rng[3]+15, adj=0)
  points(x=rng[2]-15, y=rng[3]+15, pch=20)
  
  
  # wt
  plot(wr, col=grayscale_colors, xlim=c(463380, 463380+390), 
       ylim=c(5642000-70, 5642000+170+20), legend=FALSE, axes=FALSE)
  points(wrProf, pch="+", col="white", cex=0.5)
  points(wtPts, pch=20, col="white",cex=0.5)
  plot(wtGuide, add=TRUE, border="white", lwd=1.5)
  box()
  axis(1, at=seq(463420+40, 463380+390-50, length=5), cex.axis=0.7, padj=-1.5)
  axis(2, at=seq(5642000-70, 5642000+170+20, length=4), cex.axis=0.7, padj=1.0)
  mtext("Easting [m]", 1, line=1.75, cex=1)
  mtext("Northing [m]", 2, line=2, cex=1)
  rng <- par("usr")
  text("(B) Wallau, trained", 
       x=rng[1]+(rng[2]-rng[1])*.02, 
       y=rng[4]-(rng[4]-rng[3])*.05, 
       adj=0)
  polygon(x=c(rng[2]-215, rng[2]-215, rng[2], rng[2]),
          y=c(rng[3], rng[3]+55, rng[3]+55, rng[3]), 
          col="white")
  text("Topographic points +", x=rng[2]-200, y=rng[3]+40, adj=0)
  text("In-stream points", x=rng[2]-175, y=rng[3]+15, adj=0)
  points(x=rng[2]-15, y=rng[3]+15, pch=20)
  dev.off()
}


# figure 3, Q time series
{
  # plot upstream boundary conditions (q=Q/w)
  setwd("c:\\doktor\\proof_of_concept\\")
  bs             <- read.table("corrBiedSarWithOutliers.txt",header=TRUE)
  bsNoOutl       <- read.table("corrBiedSarWithoutOutliers.txt",header=TRUE)
  outliers       <- read.table("corrBiedSarOutliers.txt",header=TRUE)
  niedNoOut      <- read.table("niederwetter.txt",header=TRUE)
  bsNoOutl[[1]]  <- as.Date(bsNoOutl[[1]])
  niedNoOut[[1]] <- as.Date(niedNoOut[[1]])
  bsNoOutl       <- bsNoOutl[order(bsNoOutl[[1]]),]
  niedNoOut      <- niedNoOut[order(niedNoOut[[1]]),]

  # check frequency of diacharges larger than maximum calibration discharge
  # (cr, 5.03 m3/s)
  fn   <- ecdf(bsNoOutl$sarnau)
  xx   <- knots(fn)
  fnxx <- cbind(x=xx, Fn=fn(xx))
  
  fnxx[fnxx[, 1]==1.59, "Fn"][1]
  
  plot(fn)
  abline(v=1.59, lty=3)
  abline(h=0.3, lty=3)
  
  
  par(mfrow=c(3,1))
  # biedenkopf
  par(mar=c(5, 8, 1, 1))
  plot(bsNoOutl[[1]], bsNoOutl[[2]], type="l", axes=FALSE,xlab="", ylab="", ylim=c(0,150))
  axis(1, pos=0, at=seq(as.Date("2003-11-01"), as.Date("2012-06-26"), by="year"), 
       labels=format(seq(as.Date("2003-11-01"), as.Date("2012-06-26"),by="year"),"%Y"),
       cex.axis=2, padj=.5)
  lines(c(as.Date("2003-11-01"),as.Date("2012-07-30")),c(0,0))
  axis(2,pos=as.Date("2003-11-01"),at=seq(0,150,by=40),cex.axis=2, las=2)
  text(x=14230,y=135,labels="Biedenkopf",cex=3)
  rng <- par("usr")
  text(x=rng[1] + (rng[2]-rng[1])*.1 ,y=rng[4] - (rng[4]-rng[3])*.1, labels="(a)",cex=2)
  mtext(expression(paste("Q [", m^3,s^-1, "]")), 2, line=3.25, cex=2)
  
  # sarnau
  par(mar=c(5, 8, 1, 1))
  plot(bsNoOutl[[1]], bsNoOutl[[3]], type="l", axes=FALSE, xlab="", ylab="", ylim=c(0,150))
  axis(1, pos=0, at=seq(as.Date("2003-11-01"), as.Date("2012-06-26"), by="year"),
       labels=format(seq(as.Date("2003-11-01"), as.Date("2012-06-26"),by="year"),"%Y"), cex.axis=2, padj=.5)
  lines(c(as.Date("2003-11-01"), as.Date("2012-07-30")), c(0,0))
  axis(2, pos=as.Date("2003-11-01"), at=seq(0,150,by=40), cex.axis=2, las=2)
  mtext(expression(paste("Q [", m^3,s^-1, "]")), 2, line=3.25, cex=2)
  text(x=14230,y=135,labels="Sarnau",cex=3)
  rng <- par("usr")
  text(x=rng[1] + (rng[2]-rng[1])*.1 ,y=rng[4] - (rng[4]-rng[3])*.1, labels="(b)",cex=2)
  
  
  # niederwetter
  par(mar=c(5, 8, 1, 1))
  plot(niedNoOut[[1]], niedNoOut[[2]], type="l", axes=FALSE, xlab="", ylab="", ylim=c(0,150))
  axis(1, pos=0, at=seq(as.Date("2003-11-01"),as.Date("2012-06-26"),by="year"),
       labels=format(seq(as.Date("2003-11-01"), as.Date("2012-06-26"),by="year"),"%Y"), cex.axis=2, padj=.5)
  lines(c(as.Date("2003-11-01"), as.Date("2012-07-30")), c(0,0))
  axis(2,pos=as.Date("2003-11-01"),at=seq(0,150,by=40),cex.axis=2, las=2)
  mtext("Years",1,line=4,cex=2)	
  text(x=14230,y=135,labels="Niederwetter",cex=3)
  rng <- par("usr")
  text(x=rng[1] + (rng[2]-rng[1])*.1 ,y=rng[4] - (rng[4]-rng[3])*.1, labels="(c)",cex=2)
  mtext(expression(paste("Q [", m^3,s^-1, "]")), 2, line=3.25, cex=2)
  
}


# remember: rowseladeq: for ct and cr model adequacy, 50 points because ct has 
#                       only 64 in total
#           rowselcal: for all sites, 50 points. the remaining ones are for val

# model adequacy
# compare lisflood-acc and telemac2d with obs. water depth and velocity class,
# compare also telemac2d and lisflood-acc velocities
# quantitative comparisons based on major axis regression
# sites ct and cr and wr
{
  # read and interpolate telemac2d, cr
  {
    setwd("c:\\presentations_papers\\paper1\\crtelemac\\")
    res <- read.table("res.txt", sep="\t", header=TRUE)
    
    pts <- data.frame(x=res$x,
                      y=res$y,
                      z=res$wd
    )
    pInterp <- interp(x=pts[[1]], y=pts[[2]], z=pts[[3]],
                      yo=seq(from=min(pts[[2]]), to=max(pts[[2]]), by=1),
                      xo=seq(from=min(pts[[1]]), to=max(pts[[1]]), by=1),
                      duplicate="mean"
    )
    wdtelr <- raster(pInterp)
    wdtelr[wdtelr<=0]=NA
    
    pts <- data.frame(x=res$x,
                      y=res$y,
                      z=res$vmag
    )
    pInterp <- interp(x=pts[[1]], y=pts[[2]], z=pts[[3]],
                      yo=seq(from=min(pts[[2]]), to=max(pts[[2]]), by=1),
                      xo=seq(from=min(pts[[1]]), to=max(pts[[1]]), by=1),
                      duplicate="mean"
    )
    vtelr <- raster(pInterp)
  }
  
  # read and interpolate telemac2d, ct
  {
    setwd("c:\\presentations_papers\\paper1\\cttelemac\\")
    res <- read.table("res.txt", sep="\t", header=TRUE)
    
    pts <- data.frame(x=res$x,
                      y=res$y,
                      z=res$wd
    )
    pInterp <- interp(x=pts[[1]], y=pts[[2]], z=pts[[3]],
                      yo=seq(from=min(pts[[2]]), to=max(pts[[2]]), by=1),
                      xo=seq(from=min(pts[[1]]), to=max(pts[[1]]), by=1),
                      duplicate="mean"
    )
    wdtelt <- raster(pInterp)
    wdtelt[wdtelt<=0]=NA
    
    pts <- data.frame(x=res$x,
                      y=res$y,
                      z=res$vmag
    )
    pInterp <- interp(x=pts[[1]], y=pts[[2]], z=pts[[3]],
                      yo=seq(from=min(pts[[2]]), to=max(pts[[2]]), by=1),
                      xo=seq(from=min(pts[[1]]), to=max(pts[[1]]), by=1),
                      duplicate="mean"
    )
    vtelt <- raster(pInterp)
  }
  
  # read lisflood-acc, cr
  {
    setwd("c:\\doktor\\proof_of_concept\\mod_cr\\")
    dem   <- raster("dem1.asc")
    
    setwd("c:\\doktor\\proof_of_concept\\modCal\\")
    wdAccr <- raster("cr.wd")
    vxAcc  <- raster("cr.Vx")
    vyAcc  <- raster("cr.Vy")
    
    # compute v = sqrt(vx^2 + vy^2)
    vxAccM <- as.matrix(vxAcc)
    vyAccM <- as.matrix(vyAcc)
    
    vxAccMM <- matrix(nrow=nrow(dem),ncol=0)
    for (j in 1:ncol(dem))
    {
      vxAccMM<-cbind(vxAccMM,0.5*(vxAccM[,j]+vxAccM[,j+1]))
    }
    
    vyAccMM<-matrix(nrow=0, ncol=ncol(dem))
    for (i in 1:nrow(dem))
    {
      vyAccMM<-rbind(vyAccMM,0.5*(vyAccM[i,]+vyAccM[i+1,]))
    }
    
    vAccr <- raster(sqrt(vxAccMM^2 + vyAccMM^2), template = dem)
  }
  
  # read lisflood-acc, ct
  {
    setwd("c:\\doktor\\proof_of_concept\\mod_ct\\")
    dem   <- raster("dem1.asc")
    
    setwd("c:\\doktor\\proof_of_concept\\modCal\\")
    wdAcct <- raster("ct.wd")
    vxAcc  <- raster("ct.Vx")
    vyAcc  <- raster("ct.Vy")
    
    # compute v = sqrt(vx^2 + vy^2)
    vxAccM <- as.matrix(vxAcc)
    vyAccM <- as.matrix(vyAcc)
    
    vxAccMM <- matrix(nrow=nrow(dem),ncol=0)
    for (j in 1:ncol(dem))
    {
      vxAccMM<-cbind(vxAccMM,0.5*(vxAccM[,j]+vxAccM[,j+1]))
    }
    
    vyAccMM<-matrix(nrow=0, ncol=ncol(dem))
    for (i in 1:nrow(dem))
    {
      vyAccMM<-rbind(vyAccMM,0.5*(vyAccM[i,]+vyAccM[i+1,]))
    }
    
    vAcct <- raster(sqrt(vxAccMM^2 + vyAccMM^2), template = dem)
  }
  
  # extract water depths and velocities at observation points, cr
  {
    setwd("c:\\presentations_papers\\paper1")
    rowseladeq <- read.table("rowseladeq.txt", header=TRUE)
    rowselcr   <- rowseladeq$cr # sample(1:100, size=50) # take 50 random points
    
    setwd("c:\\doktor\\proof_of_concept\\modCal\\")
    
    sp      <- read.table("crSP.txt", header=T, sep="\t")
    spSP    <- SpatialPointsDataFrame(coords=cbind(sp[,1], sp[,2]),
                                      data=data.frame(sp[, 3:7]))
    listelr <- sp
    listelr <- cbind(listelr, wdAcc=extract(wdAccr, spSP))
    listelr <- cbind(listelr, wdtel=extract(wdtelr, spSP))
    listelr <- cbind(listelr, vAcc=extract(vAccr, spSP))
    listelr <- cbind(listelr, vtel=extract(vtelr, spSP))
    listelr <- listelr[rowselcr, ]
  }
  
  # extract water depths and velocities at observation points, ct
  {
    setwd("c:\\presentations_papers\\paper1")
    rowseladeq <- read.table("rowseladeq.txt", header=TRUE)
    rowselct   <- rowseladeq$ct
    
    setwd("c:\\doktor\\proof_of_concept\\modCal\\")
    
    sp      <- read.table("ctSP.txt", header=T, sep="\t")
    spSP    <- SpatialPointsDataFrame(coords=cbind(sp[,1], sp[,2]),
                                      data=data.frame(sp[, 3:7]))
    listelt <- sp
    listelt <- cbind(listelt, wdAcc=extract(wdAcct, spSP))
    listelt <- cbind(listelt, wdtel=extract(wdtelt, spSP))
    listelt <- cbind(listelt, vAcc=extract(vAcct, spSP))
    listelt <- cbind(listelt, vtel=extract(vtelt, spSP))
    listelt <- listelt[rowselct, ]
  }
  
  # figure 7, plot results water depth
  {
    windows(width=8.5, height=5.5)
    par(mar=c(3.15, 4, 1, 1), mfrow=c(2,2))
    fsize <- 1.15
    
    # cr telemac2d
    x     <- listelr[, "wdtel"]
    y     <- -listelr[, "h"]
    xyreg <- lmodel2(y~x, nperm=99)
    ee    <- x-y
    xnew  <- 0:2
    ynew  <- xnew*xyreg$regression.results[2,3] + xyreg$regression.results[2,2]
    
    print(c(r2=xyreg$rsquare,
            int=xyreg$regression.results[2, "Intercept"],
            int2.5=xyreg$confidence.intervals[2, "2.5%-Intercept"],
            int97.5=xyreg$confidence.intervals[2, "97.5%-Intercept"],
            slope=xyreg$regression.results[2, "Slope"],
            slope2.5=xyreg$confidence.intervals[2, "2.5%-Slope"],
            slope97.5=xyreg$confidence.intervals[2, "97.5%-Slope"],
            qe10=quantile(ee, probs=0.10, names=FALSE),
            qe90=quantile(ee, probs=0.90, names=FALSE),
            rmse=sqrt(sum(ee^2)/length(ee))))
    
    plot(xyreg, pch="+", xlim=c(0, 1.2), ylim=c(0, 1.2), 
         axes=FALSE, xlab="", ylab="", main="")
    text(x=0, y=1.15, labels="(A) cr, T2D", cex=1.1*fsize, adj=0)
    axis(1, cex.axis=fsize, at=seq(0, 1.2, by=0.2), padj=-0.5)
    axis(2, cex.axis=fsize, at=seq(0, 1.2, by=0.2), las=2, hadj=0.85)
    mtext("Depth [m], mod.", side=1, cex=fsize, line=2)
    mtext("Depth [m], obs.", side=2, cex=fsize, line=2.75)
    lines(-1:2, -1:2, lwd=2)
    legend(x=0.855, y=0.365, legend=c("45º", "MA", "95% CI"),
           lwd=c(2,1,1), col=c("black", "red", "grey"),
           cex=1.05)
    box()
    
    # cr lisflood-acc
    x     <- listelr[, "wdAcc"]
    y     <- -listelr[, "h"]
    xyreg <- lmodel2(y~x, nperm=99)
    ee    <- x-y
    xnew  <- 0:2
    ynew  <- xnew*xyreg$regression.results[2,3] + xyreg$regression.results[2,2]
    
    print(c(r2=xyreg$rsquare,
            int=xyreg$regression.results[2, "Intercept"],
            int2.5=xyreg$confidence.intervals[2, "2.5%-Intercept"],
            int97.5=xyreg$confidence.intervals[2, "97.5%-Intercept"],
            slope=xyreg$regression.results[2, "Slope"],
            slope2.5=xyreg$confidence.intervals[2, "2.5%-Slope"],
            slope97.5=xyreg$confidence.intervals[2, "97.5%-Slope"],
            qe10=quantile(ee, probs=0.10, names=FALSE),
            qe90=quantile(ee, probs=0.90, names=FALSE),
            rmse=sqrt(sum(ee^2)/length(ee))))
    
    plot(xyreg, pch="+", xlim=c(0, 1.2), ylim=c(0, 1.2), 
         axes=FALSE, xlab="", ylab="", main="")
    text(x=0, y=1.15, labels="(B) cr, LF-ACC", cex=1.1*fsize, adj=0)
    axis(1, cex.axis=fsize, at=seq(0, 1.2, by=0.2), padj=-0.5)
    axis(2, cex.axis=fsize, at=seq(0, 1.2, by=0.2), las=2, hadj=0.85)
    mtext("Depth [m], mod.", side=1, cex=fsize, line=2)
    mtext("Depth [m], obs.", side=2, cex=fsize, line=2.75)
    lines(-1:2, -1:2, lwd=2)
    legend(x=0.855, y=0.365, legend=c("45º", "MA", "95% CI"),
           lwd=c(2,1,1), col=c("black", "red", "grey"),
           cex=1.05)
    box()
    
    # ct telemac2d
    x     <- listelt[, "wdtel"]
    y     <- -listelt[, "h"]
    xyreg <- lmodel2(y~x, nperm=99)
    ee    <- x-y
    xnew  <- 0:2
    ynew  <- xnew*xyreg$regression.results[2,3] + xyreg$regression.results[2,2]
    
    print(c(r2=xyreg$rsquare,
            int=xyreg$regression.results[2, "Intercept"],
            int2.5=xyreg$confidence.intervals[2, "2.5%-Intercept"],
            int97.5=xyreg$confidence.intervals[2, "97.5%-Intercept"],
            slope=xyreg$regression.results[2, "Slope"],
            slope2.5=xyreg$confidence.intervals[2, "2.5%-Slope"],
            slope97.5=xyreg$confidence.intervals[2, "97.5%-Slope"],
            qe10=quantile(ee, probs=0.10, names=FALSE),
            qe90=quantile(ee, probs=0.90, names=FALSE),
            rmse=sqrt(sum(ee^2)/length(ee))))
    
    plot(xyreg, pch="+", xlim=c(0, 1.2), ylim=c(0, 1.2), 
         axes=FALSE, xlab="", ylab="", main="")
    text(x=0, y=1.15, labels="(C) ct, T2D", cex=1.1*fsize, adj=0)
    axis(1, cex.axis=fsize, at=seq(0, 1.2, by=0.2), padj=-0.5)
    axis(2, cex.axis=fsize, at=seq(0, 1.2, by=0.2), las=2, hadj=0.85)
    mtext("Depth [m], mod.", side=1, cex=fsize, line=2)
    mtext("Depth [m], obs.", side=2, cex=fsize, line=2.75)
    lines(-1:2, -1:2, lwd=2)
    legend(x=0.855, y=0.365, legend=c("45º", "MA", "95% CI"),
           lwd=c(2,1,1), col=c("black", "red", "grey"),
           cex=1.05)
    box()
    
    # ct lisflood-acc
    x     <- listelt[, "wdAcc"]
    y     <- -listelt[, "h"]
    xyreg <- lmodel2(y~x, nperm=99)
    ee    <- x-y
    xnew  <- 0:2
    ynew  <- xnew*xyreg$regression.results[2,3] + xyreg$regression.results[2,2]
    
    print(c(r2=xyreg$rsquare,
            int=xyreg$regression.results[2, "Intercept"],
            int2.5=xyreg$confidence.intervals[2, "2.5%-Intercept"],
            int97.5=xyreg$confidence.intervals[2, "97.5%-Intercept"],
            slope=xyreg$regression.results[2, "Slope"],
            slope2.5=xyreg$confidence.intervals[2, "2.5%-Slope"],
            slope97.5=xyreg$confidence.intervals[2, "97.5%-Slope"],
            qe10=quantile(ee, probs=0.10, names=FALSE),
            qe90=quantile(ee, probs=0.90, names=FALSE),
            rmse=sqrt(sum(ee^2)/length(ee))))
    
    plot(xyreg, pch="+", xlim=c(0, 1.2), ylim=c(0, 1.2), 
         axes=FALSE, xlab="", ylab="", main="")
    text(x=0, y=1.15, labels="(D) ct, LF-ACC", cex=1.1*fsize, adj=0)
    axis(1, cex.axis=fsize, at=seq(0, 1.2, by=0.2), padj=-0.5)
    axis(2, cex.axis=fsize, at=seq(0, 1.2, by=0.2), las=2, hadj=0.85)
    mtext("Depth [m], mod.", side=1, cex=fsize, line=2)
    mtext("Depth [m], obs.", side=2, cex=fsize, line=2.75)
    lines(-1:2, -1:2, lwd=2)
    legend(x=0.855, y=0.365, legend=c("45º", "MA", "95% CI"),
           lwd=c(2,1,1), col=c("black", "red", "grey"),
           cex=1.05)
    box()
  }
  
  # figure 8,plot comparison velocity telemac2d vs. lisflood-acc
  {
    vlisct <- listelt[, "vAcc"]
    vtelct <- listelt[, "vtel"]
    vliscr <- listelr[, "vAcc"]
    vtelcr <- listelr[, "vtel"]
    
    regt  <- lmodel2(vtelct~vlisct, nperm=99)
    ee    <- vlisct-vtelct
    xnewt <- 0:2
    ynewt <- xnewt*regt$regression.results[2,3] + regt$regression.results[2,2]
    
    print(c(int=regt$regression.results[2, "Intercept"],
            int2.5=regt$confidence.intervals[2, "2.5%-Intercept"],
            int97.5=regt$confidence.intervals[2, "97.5%-Intercept"],
            slope=regt$regression.results[2, "Slope"],
            slope2.5=regt$confidence.intervals[2, "2.5%-Slope"],
            slope97.5=regt$confidence.intervals[2, "97.5%-Slope"],
            qe10=quantile(ee, probs=0.10, names=FALSE),
            qe90=quantile(ee, probs=0.90, names=FALSE),
            rmse=sqrt(sum(ee^2)/length(ee))))


    regr <- lmodel2(vtelcr~vliscr, nperm=99)
    ee   <- vliscr-vtelcr
    xnewr <- 0:2
    ynewr <- xnewr*regr$regression.results[2,3] + regr$regression.results[2,2]
    
    print(c(int=regr$regression.results[2, "Intercept"],
            int2.5=regr$confidence.intervals[2, "2.5%-Intercept"],
            int97.5=regr$confidence.intervals[2, "97.5%-Intercept"],
            slope=regr$regression.results[2, "Slope"],
            slope2.5=regr$confidence.intervals[2, "2.5%-Slope"],
            slope97.5=regr$confidence.intervals[2, "97.5%-Slope"],
            qe10=quantile(ee, probs=0.10, names=FALSE),
            qe90=quantile(ee, probs=0.90, names=FALSE),
            rmse=sqrt(sum(ee^2)/length(ee))))
    

    windows(width=3, height=4)
    fsize <- 0.75
    par(mfcol=c(2,1), mar=c(2.5, 3, 0.5, 1))
    plot(regt, method="MA", pch="+", cex=fsize, main="",
         col=c("red", "black", "black"), 
         xlim=c(0, 1.6), ylim=c(0, 1.6), axes=FALSE, xlab="", ylab="")
    axis(1, at=seq(0, 1.6, by=0.3), cex.axis=fsize, padj=-1.5)
    axis(2, at=seq(0, 1.6, by=0.3), las=2, cex.axis=fsize, hadj=0.5)
    mtext("Vel. [m/s], LF-ACC", side=1, line=1.25, cex=1.25*fsize)
    mtext("Vel. [m/s], T2D", side=2, line=2, cex=1.25*fsize)
    lines(-1:2, -1:2, lwd=2)
    text(x=0, y=1.5, labels="(A) ct", cex=1.1*fsize, adj=0)
    legend(x=1.08, y=0.5, legend=c("45º", "MA", "95% CI"),
           lwd=c(2,1,1), col=c("black", "red", "black"),
           cex=0.8*fsize)
    box()
    
    plot(regr, method="MA", pch="+", cex=fsize, main="",
         col=c("red", "black", "black"), 
         xlim=c(0, 1.6), ylim=c(0, 1.6), axes=FALSE, xlab="", ylab="")
    axis(1, at=seq(0, 1.6, by=0.3), cex.axis=fsize, padj=-1.5)
    axis(2, at=seq(0, 1.6, by=0.3), las=2, cex.axis=fsize, hadj=0.5)
    mtext("Vel. [m/s], LF-ACC", side=1, line=1.25, cex=1.25*fsize)
    mtext("Vel. [m/s], T2D", side=2, line=2, cex=1.25*fsize)
    lines(-1:2, -1:2, lwd=2)
    text(x=0, y=1.5, labels="(B) cr", cex=1.1*fsize, adj=0)
    legend(x=1.08, y=0.5, legend=c("45º", "MA", "95% CI"),
           lwd=c(2,1,1), col=c("black", "red", "black"),
           cex=0.8*fsize)
    box()
  }
  
  # figure 9, plot results velocity class
  {
    # cr
    xcr <- listelr
    vcr <- data.frame(v=c(xcr$vAcc, xcr$vtel),
                      vclass=c(xcr$vClass, xcr$vClass),
                      mod=c(rep("lis", times=nrow(xcr)), 
                            rep("tel", times=nrow(xcr)))
    )
    
    # ct
    xct <- listelt
    vct <- data.frame(v=c(xct$vAcc, xct$vtel),
                      vclass=c(xct$vClass, xct$vClass),
                      mod=c(rep("lis", times=nrow(xct)), 
                            rep("tel", times=nrow(xct)))
    )
    
    # look at differences between velocity classes and models
    aovcr <- aov(v~mod*vclass, data=vcr)
    aovct <- aov(v~mod*vclass, data=vct)
    
    ssm <- summary(aovcr)[[1]][2, 2]
    sst <- sum(summary(aovcr)[[1]][2])
    ssm/sst
    
    windows(width=5, height=3)
    vmax <- 1.75
    par(mar=c(2.25, 3.6, 1, 1), mfrow=c(1,2))
    boxplot(v~mod*vclass, data=vcr, ylim=c(0, vmax), xlim=c(0, 8.5),
            axes=FALSE, col=c("white", "grey"))
    axis(1, at=seq(1.5, 7.5, by=2), cex.axis=0.725, lab=1:4, padj=-1.25)
    axis(2, at=seq(0, vmax, by=0.25), las=2, cex.axis=0.725)
    abline(v=seq(2.5, 7.5, by=2), lty=2)
    mtext("Obs. Vel. Class", side=1, line=1.25, cex=0.9)
    mtext("Modeled vel. [m/s]", side=2, line=2.65, cex=0.9)
    legend(x=-0.5, y=vmax+0.1, legend=c("LF-ACC", "T2D"), 
           fill=c("white", "grey"), adj=0, cex=0.725, 
           bg="white", box.col="black")
    text(x=8.55, y=1.70, label="(A) cr", cex=0.725, adj=1)
    box()
    
    boxplot(v~mod*vclass, data=vct, ylim=c(0, vmax), xlim=c(0, 8.5),
            axes=FALSE, col=c("white", "grey"))
    axis(1, at=seq(1.5, 7.5, by=2), cex.axis=0.725, lab=1:4, padj=-1.25)
    axis(2, at=seq(0, vmax, by=0.25), las=2, cex.axis=0.725)
    abline(v=seq(2.5, 7.5, by=2), lty=2)
    mtext("Obs. Vel. Class", side=1, line=1.25, cex=0.9)
    mtext("Modeled vel. [m/s]", side=2, line=2.65, cex=0.9)
    legend(x=-0.5, y=vmax+0.1, legend=c("LF-ACC", "T2D"), 
           fill=c("white", "grey"), adj=0, cex=0.725, 
           bg="white", box.col="black")
    text(x=8.55, y=1.70, label="(B) ct", cex=0.725, adj=1)
    box()
  }
  
  # compute nash-sutcliffe
  {
    # depth t2d vs obs
    mod <- listelr$wdtel
    obs <- -listelr$h
    nsr <- 1 - sum((mod-obs)^2)/sum((obs-mean(obs))^2)
    
    mod <- listelt$wdtel
    obs <- -listelt$h
    nst <- 1 - sum((mod-obs)^2)/sum((obs-mean(obs))^2)
    
    # depth lf-acc vs obs
    mod <- listelr$wdAcc
    obs <- -listelr$h
    nsr <- 1 - sum((mod-obs)^2)/sum((obs-mean(obs))^2)
    
    mod <- listelt$wdAcc
    obs <- -listelt$h
    nst <- 1 - sum((mod-obs)^2)/sum((obs-mean(obs))^2)
    
    # velocity t2d vs lf-acc
    mod <- listelr$vAcc
    obs <- listelr$vtel
    nsr <- 1 - sum((mod-obs)^2)/sum((obs-mean(obs))^2)
    
    mod <- listelt$vAcc
    obs <- listelt$vtel
    nst <- 1 - sum((mod-obs)^2)/sum((obs-mean(obs))^2)
  }
}


# calibration and validation
{
  # figure 4, 2d-hn calibration, water depth
  {
    setwd("c:\\presentations_papers\\paper1")
    rowselcal <- read.table("rowselcal.txt", header=TRUE, sep=",")
    
    setwd("c:\\doktor\\proof_of_concept\\modCal\\")
    
    plotCal <- function(site, lab, fsize, rowselcal)
    {
      hRast     <- raster(paste0(site, ".wd"))
      sp    	  <- read.table(paste0(site, "SP.txt") ,header=T, sep="\t")
      spSP  	  <- SpatialPoints(cbind(sp$x, sp$y))
      d     	  <- matrix(nrow=nrow(sp), ncol=0)
      d    	    <- extract(hRast,spSP)
      y         <- -sp$h    # observed
      x         <- d        # predicted
      rowsel    <- rowselcal[, site]
      y         <- y[rowsel]
      x         <- x[rowsel]
      y         <- y[!is.na(y)]
      x         <- x[!is.na(x)]
      xyreg     <- lmodel2(y~x, nperm=99)
      ee        <- x-y
      xnew <- 0:2
      ynew <- xnew*xyreg$regression.results[2,3] + xyreg$regression.results[2,2]
      
      print(c(int=xyreg$regression.results[2, "Intercept"],
              int2.5=xyreg$confidence.intervals[2, "2.5%-Intercept"],
              int97.5=xyreg$confidence.intervals[2, "97.5%-Intercept"],
              slope=xyreg$regression.results[2, "Slope"],
              slope2.5=xyreg$confidence.intervals[2, "2.5%-Slope"],
              slope97.5=xyreg$confidence.intervals[2, "97.5%-Slope"],
              qe10=quantile(ee, probs=0.10, names=FALSE),
              qe90=quantile(ee, probs=0.90, names=FALSE),
              rmse=sqrt(sum(ee^2)/length(ee))))
      
      plot(xyreg, pch="+", xlim=c(0, 1.2), ylim=c(0, 1.2), 
           axes=FALSE, xlab="", ylab="", main="")
      text(x=0, y=0.975, labels=lab, cex=1.1*fsize, adj=0)
      axis(1, cex.axis=fsize, at=seq(0, 1.2, by=0.2), padj=-0.5)
      axis(2, cex.axis=fsize, at=seq(0, 1.2, by=0.2), las=2, hadj=0.85)
      mtext("Depth [m], mod.", side=1, cex=0.8*fsize, line=2)
      mtext("Depth [m], obs.", side=2, cex=0.8*fsize, line=2.75)
      lines(-1:2, -1:2, lwd=2)
      rng <- par("usr")
      legend(x=0.71, y=0.42, legend=c("45º", "MA", "95% CI"),
             lwd=c(2,1,1), col=c("black", "red", "grey"),
             cex=1.05)
      box()
    }
    
    
    windows(height=4, width=8)
    par(mar=c(3.5,4.5,1,1), mfrow=c(2,3))
    plotCal("cr", expression(paste("(A) cr,cal\nQ = 4.34 ", m^3~s^-1)), fsize=1.15, rowselcal)
    plotCal("wr", expression(paste("(B) wr,cal\nQ = 1.59 ", m^3~s^-1)), fsize=1.15, rowselcal)
    plotCal("lr", expression(paste("(C) lr,cal\nQ = 1.21 ", m^3~s^-1)), fsize=1.15, rowselcal)
    plotCal("ct", expression(paste("(D) ct,cal\nQ = 5.03 ", m^3~s^-1)), fsize=1.15, rowselcal)
    plotCal("wt", expression(paste("(E) wt,cal\nQ = 1.59 ", m^3~s^-1)), fsize=1.15, rowselcal)
    plotCal("lt", expression(paste("(F) lt,cal\nQ = 1.21 ", m^3~s^-1)), fsize=1.15, rowselcal)
  }
  
  # figure 5, 2d-hn validation, water depth
  {
    setwd("c:\\presentations_papers\\paper1\\")
    rowselcal <- read.table("rowselcal.txt", header=TRUE, sep=",")
    
    plotVal <- function(site, lab, fsize, rowselcal)
    {
      hRast     <- raster(paste0(site, ".wd"))
      sp    	  <- read.table(paste0(site, "SP.txt") ,header=T, sep="\t")
      spSP  	  <- SpatialPoints(cbind(sp$x, sp$y))
      d     	  <- matrix(nrow=nrow(sp), ncol=0)
      d    	    <- extract(hRast,spSP)
      y         <- -sp$h    # observed
      x         <- d        # predicted
      rowselval <- 1:100
      rowsel    <- rowselval[-rowselcal[, site]]
      y         <- y[rowsel]
      x         <- x[rowsel]
      y         <- y[!is.na(y)]
      x         <- x[!is.na(x)]
      xyreg     <- lmodel2(y~x, nperm=99)
      ee        <- x-y
      xnew <- 0:2
      ynew <- xnew*xyreg$regression.results[2,3] + xyreg$regression.results[2,2]
      
      
      print(c(r2=xyreg$rsquare,
              int=xyreg$regression.results[2, "Intercept"],
              int2.5=xyreg$confidence.intervals[2, "2.5%-Intercept"],
              int97.5=xyreg$confidence.intervals[2, "97.5%-Intercept"],
              slope=xyreg$regression.results[2, "Slope"],
              slope2.5=xyreg$confidence.intervals[2, "2.5%-Slope"],
              slope97.5=xyreg$confidence.intervals[2, "97.5%-Slope"],
              qe10=quantile(ee, probs=0.10, names=FALSE),
              qe90=quantile(ee, probs=0.90, names=FALSE),
              rmse=sqrt(sum(ee^2)/length(ee))))
      
      plot(xyreg, pch="+", xlim=c(0, 1.2), ylim=c(0, 1.2), 
           axes=FALSE, xlab="", ylab="", main="")
      text(x=0, y=0.975, labels=lab, cex=1.1*fsize, adj=0)
      axis(1, cex.axis=fsize, at=seq(0, 1.2, by=0.2), padj=-0.5)
      axis(2, cex.axis=fsize, at=seq(0, 1.2, by=0.2), las=2, hadj=0.85)
      mtext("Depth [m], mod.", side=1, cex=0.8*fsize, line=2)
      mtext("Depth [m], obs.", side=2, cex=0.8*fsize, line=2.75)
      lines(-1:2, -1:2, lwd=2)
      rng <- par("usr")
      legend(x=0.71, y=0.42, legend=c("45º", "MA", "95% CI"),
             lwd=c(2,1,1), col=c("black", "red", "grey"),
             cex=1.05)
      box()
    }
    
    setwd("c:\\doktor\\proof_of_concept\\modCal\\")   
    
    windows(height=4, width=8)
    par(mar=c(3.5,4.5,1,1), mfrow=c(2,3))
    plotVal("cr", expression(paste("(A) cr,val\nQ = 4.34 ", m^3~s^-1)), fsize=1.15, rowselcal)
    plotVal("wr", expression(paste("(B) wr,val\nQ = 1.59 ", m^3~s^-1)), fsize=1.15, rowselcal)
    plotVal("lr", expression(paste("(C) lr,val\nQ = 1.21 ", m^3~s^-1)), fsize=1.15, rowselcal)
    plotVal("ct", expression(paste("(D) ct,val\nQ = 5.03 ", m^3~s^-1)), fsize=1.15, rowselcal)
    plotVal("wt", expression(paste("(E) wt,val\nQ = 1.59 ", m^3~s^-1)), fsize=1.15, rowselcal)
    plotVal("lt", expression(paste("(F) lt,val\nQ = 1.21 ", m^3~s^-1)), fsize=1.15, rowselcal)
  }
  
  # figure 6, 2d-hn validation velocity categories
  {
    setwd("c:\\presentations_papers\\paper1\\")
    
    # read rows used for calibration. in plotCal, the rows that are not in this list
    # are used for analysis
    rowselcal <- read.table("rowselcal.txt", header=TRUE, sep=",")
    
    plotCal <- function(site, lab, fsize, rowselcal)
    {
      vmax      <- 1.8
      hRast     <- raster(paste0(site, ".wd"))
      vxRast    <- raster(paste0(site, ".Vx"))
      vyRast    <- raster(paste0(site, ".Vy"))
      
      # put liflood vx and vy on same raster
      vxM   <- as.matrix(vxRast)
      vyM   <- as.matrix(vyRast)
      vxi   <- vxM[, 1:(ncol(vxM)-1)]
      vxip1 <- vxM[, 2:ncol(vxM)]
      vx    <- 0.5*(vxi + vxip1)
      vyj   <- vyM[1:(nrow(vyM)-1),]
      vyjp1 <- vyM[2:nrow(vyM),]
      vy    <- 0.5*(vyj + vyjp1)
      vx    <- raster(vx, template=hRast)
      vy    <- raster(vy, template=hRast)
      vRast <- sqrt(vx^2+vy^2)
      
      # compare observed velocity categories and modeled vel
      sp    	  <- read.table(paste0(site, "SP.txt") ,header=T, sep="\t")
      rowselval <- 1:100
      rowsel    <- rowselval[-rowselcal[, site]]
      sp        <- sp[rowsel, ]
      sp        <- sp[!is.na(sp$x), ]
      spSP  	  <- SpatialPoints(cbind(sp$x, sp$y))
      
      # make data frame with observed velocity classes and computed vMag
      vpred 	  <- matrix(nrow=nrow(sp), ncol=0)
      vpred     <- cbind(vpred, extract(vRast, spSP))
      vClass    <- sp$vClass # observed v class
      vVal      <- data.frame(vpred=vpred, vClass=vClass)
      
      # plot
      boxplot(vpred~vClass, data=vVal, ylim=c(0, vmax), xlim=c(0.5, 6.5),
              axes=FALSE)
      axis(1, at=seq(1, 6, by=1), cex.axis=0.8, lab=1:6, padj=-1.25)
      axis(2, at=seq(0, vmax, by=0.25), las=2, cex.axis=0.8)
      mtext("Obs. Vel. Class", side=1, line=1.25, cex=0.8)
      mtext("Mod. vel. [m/s]", side=2, line=2.65, cex=0.8)
      text(x=0.5, y=0.9*vmax, label=lab, cex=0.8, adj=0)
      box()
    }
    
    windows(width=4, height=4)
    
    setwd("c:\\doktor\\proof_of_concept\\modCal\\")
    
    par(mar=c(2.25, 3.6, 1, 1), mfrow=c(2,2))
    plotCal("cr", expression(paste("(A) cr, val\nQ = 4.34 ", m^3~s^-1)), 
            fsize=1.15, rowselcal)
    plotCal("wr", expression(paste("(B) wr, val\nQ = 1.59 ", m^3~s^-1)), 
            fsize=1.15, rowselcal)
    plotCal("ct", expression(paste("(C) ct, val\nQ = 5.03 ", m^3~s^-1)), 
            fsize=1.15, rowselcal)
    plotCal("wt", expression(paste("(D) wt, val\nQ = 1.59 ", m^3~s^-1)), 
            fsize=1.15, rowselcal)
  }
  
  # figure xxx, comparison T2D vs LF-ACC for site wr
  {
    # read and interpolate T2D
    {  
      setwd("c:\\presentations_papers\\paper1\\wrtelemac\\")
      res <- read.table("res.txt", sep="\t", header=TRUE)
      
      pts <- data.frame(x=res$x,
                        y=res$y,
                        z=res$wd
      )
      pInterp <- interp(x=pts[[1]], y=pts[[2]], z=pts[[3]],
                        yo=seq(from=min(pts[[2]]), to=max(pts[[2]]), by=1),
                        xo=seq(from=min(pts[[1]]), to=max(pts[[1]]), by=1),
                        duplicate="mean"
      )
      wdtel <- raster(pInterp)
      wdtel[wdtel<=0]=NA
      
      pts <- data.frame(x=res$x,
                        y=res$y,
                        z=res$vmag
      )
      pInterp <- interp(x=pts[[1]], y=pts[[2]], z=pts[[3]],
                        yo=seq(from=min(pts[[2]]), to=max(pts[[2]]), by=1),
                        xo=seq(from=min(pts[[1]]), to=max(pts[[1]]), by=1),
                        duplicate="mean"
      )
      vtel <- raster(pInterp)
    }      
    
    # read LF-ACC
    {
      setwd("c:\\doktor\\proof_of_concept\\mod_wr\\")
      dem   <- raster("dem1.asc")
      
      setwd("c:\\doktor\\proof_of_concept\\modCal\\")
      wdAcc <- raster("wr.wd")
      vxAcc <- raster("wr.Vx")
      vyAcc <- raster("wr.Vy")
      
      # compute v = sqrt(vx^2 + vy^2)
      vxAccM <- as.matrix(vxAcc)
      vyAccM <- as.matrix(vyAcc)
      
      vxAccMM <- matrix(nrow=nrow(dem),ncol=0)
      for (j in 1:ncol(dem))
      {
        vxAccMM<-cbind(vxAccMM,0.5*(vxAccM[,j]+vxAccM[,j+1]))
      }
      
      vyAccMM<-matrix(nrow=0, ncol=ncol(dem))
      for (i in 1:nrow(dem))
      {
        vyAccMM<-rbind(vyAccMM,0.5*(vyAccM[i,]+vyAccM[i+1,]))
      }
      
      vAcc <- raster(sqrt(vxAccMM^2 + vyAccMM^2), template = dem)
    }
    
    # extract T2D and LF-ACC
    {
      setwd("c:\\doktor\\proof_of_concept\\modCal\\")
      
      sp      <- read.table("wrSP.txt", header=T, sep="\t")
      rowsel  <- sample(1:100, size=50) # take 50 random points
      sp      <- sp[rowsel, ]
      spSP    <- SpatialPointsDataFrame(coords=cbind(sp[,1], sp[,2]),
                                        data=data.frame(sp[, 3:7]))
      listel <- sp
      listel <- cbind(listel, wdAcc=extract(wdAcc, spSP))
      listel <- cbind(listel, wdtel=extract(wdtel, spSP))
      listel <- cbind(listel, vAcc=extract(vAcc, spSP))
      listel <- cbind(listel, vtel=extract(vtel, spSP))
    }
    
    # plot velocity and water depth comparison
    {
      vlis  <- listel[, "vAcc"]
      vtel  <- listel[, "vtel"]
      wdlis <- listel[, "wdAcc"]
      wdtel <- listel[, "wdtel"]
      wddf  <- cbind(wdlis, wdtel)
      wddf  <- wddf[!is.na(wdtel), ]
      
      regv  <- lmodel2(vlis~vtel, nperm=99)
      ee    <- vlis - vtel
      xnewv <- 0:2
      ynewv <- xnewv*regv$regression.results[2,3] + regv$regression.results[2,2]
      print(c(int=regv$regression.results[2, "Intercept"],
              int2.5=regv$confidence.intervals[2, "2.5%-Intercept"],
              int97.5=regv$confidence.intervals[2, "97.5%-Intercept"],
              slope=regv$regression.results[2, "Slope"],
              slope2.5=regv$confidence.intervals[2, "2.5%-Slope"],
              slope97.5=regv$confidence.intervals[2, "97.5%-Slope"],
              qe10=quantile(ee, probs=0.10, names=FALSE),
              qe90=quantile(ee, probs=0.90, names=FALSE),
              rmse=sqrt(sum(ee^2)/length(ee))))
      
      regwd  <- lmodel2(wddf[, "wdlis"]~wddf[, "wdtel"], nperm=99)
      ee     <- wddf[, "wdlis"] - wddf[, "wdtel"]
      xnewwd <- 0:2
      ynewwd <- xnewwd*regwd$regression.results[2,3] + regwd$regression.results[2,2]
      print(c(int=regwd$regression.results[2, "Intercept"],
              int2.5=regwd$confidence.intervals[2, "2.5%-Intercept"],
              int97.5=regwd$confidence.intervals[2, "97.5%-Intercept"],
              slope=regwd$regression.results[2, "Slope"],
              slope2.5=regwd$confidence.intervals[2, "2.5%-Slope"],
              slope97.5=regwd$confidence.intervals[2, "97.5%-Slope"],
              qe10=quantile(ee, probs=0.10, names=FALSE),
              qe90=quantile(ee, probs=0.90, names=FALSE),
              rmse=sqrt(sum(ee^2)/length(ee))))
      
      
      windows(width=3, height=4)
      fsize <- 0.75
      par(mfcol=c(2,1), mar=c(2.5, 3, 0.5, 1))
      plot(regv, method="MA", pch="+", cex=fsize, main="",
           col=c("red", "black", "black"), 
           xlim=c(0, 1.6), ylim=c(0, 1.6), axes=FALSE, xlab="", ylab="")
      lines(xnewv, ynewv, col="grey", lwd=2)
      axis(1, at=seq(0, 1.6, by=0.3), cex.axis=fsize, padj=-1.5)
      axis(2, at=seq(0, 1.6, by=0.3), las=2, cex.axis=fsize, hadj=0.5)
      mtext("Vel. [m/s], LF-ACC", side=1, line=1.25, cex=1.25*fsize)
      mtext("Vel. [m/s], T2D", side=2, line=2, cex=1.25*fsize)
      lines(-1:2, -1:2, lwd=2)
      text(x=0, y=1.5, labels="(A) wr, velocity", cex=1.1*fsize, adj=0)
      legend(x=1.08, y=0.5, legend=c("45º", "MA reg", "95% CI"),
             lwd=c(2,2,1), col=c("black", "grey", "black"),
             cex=0.8*fsize)
      box()
      
      plot(regwd, method="MA", pch="+", cex=fsize, main="",
           col=c("red", "black", "black"), 
           xlim=c(0, 1.6), ylim=c(0, 1.6), axes=FALSE, xlab="", ylab="")
      lines(xnewwd, ynewwd, col="grey", lwd=2)
      axis(1, at=seq(0, 1.6, by=0.3), cex.axis=fsize, padj=-1.5)
      axis(2, at=seq(0, 1.6, by=0.3), las=2, cex.axis=fsize, hadj=0.5)
      mtext("Depth [m], LF-ACC", side=1, line=1.25, cex=1.25*fsize)
      mtext("Depth [m], T2D", side=2, line=2, cex=1.25*fsize)
      lines(-1:2, -1:2, lwd=2)
      text(x=0, y=1.5, labels="(B) wr, water depth", cex=1.1*fsize, adj=0)
      legend(x=1.08, y=0.5, legend=c("45º", "MA reg", "95% CI"),
             lwd=c(2,2,1), col=c("black", "grey", "black"),
             cex=0.8*fsize)
      box()
    }
  }
}


# figure 10, time series hf and Q for all taxa, with locDens points
{
  # prepare data
  {
    setwd("c:\\presentations_papers\\paper1\\")
    a      <- read.table("hsAv13sp.txt", header=TRUE, sep=";")
    a$date <- as.Date(a$date, format="%m/%d/%Y")
    a      <- a[order(a$resVal),]
    
    setwd("c:\\doktor\\proof_of_concept\\hydromStress\\")
    lf <- list.files(getwd())
    lf <- lf[which(regexpr("out", lf)!=-1)]
    
    taxonlist <- c("Odont_alb",
                   "All_auri",
                   "Ana_nerv", 
                   "Ephem_dan",
                   "Potam_lut",
                   "Hab_lau",    
                   "Ecdy_ven",
                   "Goe_pil",
                   "Caen_luct",
                   "Psych_pus",
                   "Bae_luth",
                   "Bae_fus",
                   "Ase_aqua")
    
    lfuse <- character()
    for(i in lf)
    {
      lfname  <- substr(i, 8, nchar(i))
      lfmatch <- match(taxonlist, lfname)
      if(sum(lfmatch[!is.na(lfmatch)], na.rm=TRUE))
      {
        lfuse <- c(lfuse, i)
      }
    }
    
    tshf <- list()
    count <- 0
    for(i in lfuse)
    {
      count         <- count + 1
      file          <- paste0(getwd(), "/", i, "/frHab.txt")
      hf            <- read.table(file, header=FALSE)
      names(hf)     <- "hf"
      hf2           <- data.frame(tt=seq(as.Date("2003-11-01"), 
                                         length=nrow(hf), by="day"), hf)
      tshf[[count]]        <- hf2
      names(tshf)[[count]] <- substr(i, 5, nchar(i))
    }
    
    # discharge data
    setwd("c:\\doktor\\proof_of_concept\\")
    bs             <- read.table("corrBiedSarWithOutliers.txt",header=TRUE)
    bsNoOutl       <- read.table("corrBiedSarWithoutOutliers.txt",header=TRUE)
    outliers       <- read.table("corrBiedSarOutliers.txt",header=TRUE)
    niedNoOut      <- read.table("niederwetter.txt",header=TRUE)
    bsNoOutl[[1]]  <- as.Date(bsNoOutl[[1]])
    niedNoOut[[1]] <- as.Date(niedNoOut[[1]])
    bsNoOutl       <- bsNoOutl[order(bsNoOutl[[1]]),]
    niedNoOut      <- niedNoOut[order(niedNoOut[[1]]),]
    
    # place all time series in tibble
    hftimeseries <- data.frame()
    
    for(i in 1:length(tshf))
    {
      lename       <- strsplit(names(tshf)[i], "_")[[1]]
      site         <- rep(lename[1], times=nrow(tshf[[i]]))
      state        <- substr(lename[1], 2, 2)
      taxon        <- rep(paste(lename[2], lename[3], sep="_"), 
                          times=nrow(tshf[[i]]))
      hftimeseries <- rbind(hftimeseries,
                            cbind(tshf[[i]], site, taxon, state))
    }
    hftimeseries <- tbl_df(hftimeseries)
  }
  
  # plot
  {
    Qax    <- seq(0.3, 0.5, by=0.1)
    tStart <- as.Date("2003-11-01")
    tEnd   <- as.Date("2012-06-26")
    
    windows(height=8, width=8.5)
    par(mfrow=c(7, 1), mar=c(2.5, 4, 0.5, 4))
    
    # All_auri
    {
      plot(tshf$cr_All_auri, ylim=c(0, 0.5), type="l", lty=1, axes=FALSE, 
           col="grey", xlim=c(tStart, tEnd))
      lines(tshf$lr_All_auri, col="grey")
      lines(tshf$wr_All_auri, col="grey")
      lines(tshf$ct_All_auri)
      lines(tshf$lt_All_auri)
      lines(tshf$wt_All_auri)
      abline(v=unique(a[a$state=="r", "date"]), lty=3, col="black")
      axis(2, las=2)
      axis(1, at=seq(tStart, tEnd, by="year"), 
           labels=format(seq(tStart, tEnd, by="year"), "%d-%b\n%Y"),
           padj=0.3)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=4, line=2.75, at=0.35, cex=0.65)
      axis(4, at=Qax, 
           labels=formatC(seq(-(0.3-0.5)/0.0015, 0, length=length(Qax)), format="d"), 
           las=2, cex.axis=0.8)
      lines(bsNoOutl$Datum, -0.0015*bsNoOutl$sarnau+0.5)
      text(x=as.Date("2012-06-20"), y=0.25, 
           labels="(A) All. aur.", adj=1)
      legend(x=as.Date("2010-07-01"), y=0.4, legend=c("Restored", "Trained"), 
             lty=1, col=c("grey", "black"), bty="n")
      polygon(x=as.Date(c("2010-08-01", "2010-08-01", "2011-09-01", "2011-09-01")),
              y=c(0.14, 0.35, 0.35, 0.14))
      box()
    }
    
    # Ana_nerv
    {    
      plot(tshf$cr_Ana_nerv, ylim=c(0, 0.5), type="l", lty=1, axes=FALSE, 
              col="grey", xlim=c(tStart, tEnd))
      lines(tshf$lr_Ana_nerv, col="grey")
      lines(tshf$wr_Ana_nerv, col="grey")
      lines(tshf$ct_Ana_nerv)
      lines(tshf$lt_Ana_nerv)
      lines(tshf$wt_Ana_nerv)
      abline(v=unique(a[a$state=="r", "date"]), lty=3, col="black")
      axis(2, las=2)
      axis(1, at=seq(tStart, tEnd, by="year"), 
           labels=format(seq(tStart, tEnd, by="year"), "%d-%b\n%Y"),
           padj=0.3)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=4, line=2.75, at=0.35, cex=0.65)
      axis(4, at=Qax, 
           labels=formatC(seq(-(0.3-0.5)/0.0015, 0, length=length(Qax)), format="d"), 
           las=2, cex.axis=0.8)
      lines(bsNoOutl$Datum, -0.0015*bsNoOutl$sarnau+0.5)
      text(x=as.Date("2012-06-20"), y=0.25, 
           labels="(B) Ana. nerv.", adj=1)
      box()
    }    
    
    # Ase_aqua
    {    
      plot(tshf$cr_Ase_aqua, ylim=c(0, 0.5), type="l", lty=1, axes=FALSE, 
           col="grey", xlim=c(tStart, tEnd))
      lines(tshf$lr_Ase_aqua, col="grey")
      lines(tshf$wr_Ase_aqua, col="grey")
      lines(tshf$ct_Ase_aqua)
      lines(tshf$lt_Ase_aqua)
      lines(tshf$wt_Ase_aqua)
      abline(v=unique(a[a$state=="r", "date"]), lty=3, col="black")
      axis(2, las=2)
      axis(1, at=seq(tStart, tEnd, by="year"), 
           labels=format(seq(tStart, tEnd, by="year"), "%d-%b\n%Y"),
           padj=0.3)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=4, line=2.75, at=0.35, cex=0.65)
      axis(4, at=Qax, 
           labels=formatC(seq(-(0.3-0.5)/0.0015, 0, length=length(Qax)), format="d"), 
           las=2, cex.axis=0.8)
      lines(bsNoOutl$Datum, -0.0015*bsNoOutl$sarnau+0.5)
      text(x=as.Date("2012-06-20"), y=0.25, 
           labels="(C) Ase. aqua.", adj=1)
      box()
    }
    
    # Bae_fus
    {    
      plot(tshf$cr_Bae_fus, ylim=c(0, 0.5), type="l", lty=1, axes=FALSE, 
           col="grey", xlim=c(tStart, tEnd))
      lines(tshf$lr_Bae_fus, col="grey")
      lines(tshf$wr_Bae_fus, col="grey")
      lines(tshf$ct_Bae_fus)
      lines(tshf$lt_Bae_fus)
      lines(tshf$wt_Bae_fus)
      abline(v=unique(a[a$state=="r", "date"]), lty=3, col="black")
      axis(2, las=2)
      axis(1, at=seq(tStart, tEnd, by="year"), 
           labels=format(seq(tStart, tEnd, by="year"), "%d-%b\n%Y"),
           padj=0.3)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=4, line=2.75, at=0.35, cex=0.65)
      axis(4, at=Qax, 
           labels=formatC(seq(-(0.3-0.5)/0.0015, 0, length=length(Qax)), format="d"), 
           las=2, cex.axis=0.8)
      lines(bsNoOutl$Datum, -0.0015*bsNoOutl$sarnau+0.5)
      text(x=as.Date("2012-06-20"), y=0.25, 
           labels="(D) Bae. fus.", adj=1)
      box()
    }
    
    # Bae_luth
    {    
      plot(tshf$cr_Bae_luth, ylim=c(0, 0.5), type="l", lty=1, axes=FALSE, 
           col="grey", xlim=c(tStart, tEnd))
      lines(tshf$lr_Bae_luth, col="grey")
      lines(tshf$wr_Bae_luth, col="grey")
      lines(tshf$ct_Bae_luth)
      lines(tshf$lt_Bae_luth)
      lines(tshf$wt_Bae_luth)
      abline(v=unique(a[a$state=="r", "date"]), lty=3, col="black")
      axis(2, las=2)
      axis(1, at=seq(tStart, tEnd, by="year"), 
           labels=format(seq(tStart, tEnd, by="year"), "%d-%b\n%Y"),
           padj=0.3)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=4, line=2.75, at=0.35, cex=0.65)
      axis(4, at=Qax, 
           labels=formatC(seq(-(0.3-0.5)/0.0015, 0, length=length(Qax)), format="d"), 
           las=2, cex.axis=0.8)
      lines(bsNoOutl$Datum, -0.0015*bsNoOutl$sarnau+0.5)
      text(x=as.Date("2012-06-20"), y=0.25, 
           labels="(E) Bae. luth.", adj=1)
      box()
    }
    
    # Caen_luct
    {    
      plot(tshf$cr_Caen_luct, ylim=c(0, 0.5), type="l", lty=1, axes=FALSE, 
           col="grey", xlim=c(tStart, tEnd))
      lines(tshf$lr_Caen_luct, col="grey")
      lines(tshf$wr_Caen_luct, col="grey")
      lines(tshf$ct_Caen_luct)
      lines(tshf$lt_Caen_luct)
      lines(tshf$wt_Caen_luct)
      abline(v=unique(a[a$state=="r", "date"]), lty=3, col="black")
      axis(2, las=2)
      axis(1, at=seq(tStart, tEnd, by="year"), 
           labels=format(seq(tStart, tEnd, by="year"), "%d-%b\n%Y"),
           padj=0.3)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=4, line=2.75, at=0.35, cex=0.65)
      axis(4, at=Qax, 
           labels=formatC(seq(-(0.3-0.5)/0.0015, 0, length=length(Qax)), format="d"), 
           las=2, cex.axis=0.8)
      lines(bsNoOutl$Datum, -0.0015*bsNoOutl$sarnau+0.5)
      text(x=as.Date("2012-06-20"), y=0.25, 
           labels="(F) Caen. luct.", adj=1)
      box()
    }
    
    # Ecdy_ven
    {    
      plot(tshf$cr_Ecdy_ven, ylim=c(0, 0.5), type="l", lty=1, axes=FALSE, 
           col="grey", xlim=c(tStart, tEnd))
      lines(tshf$lr_Ecdy_ven, col="grey")
      lines(tshf$wr_Ecdy_ven, col="grey")
      lines(tshf$ct_Ecdy_ven)
      lines(tshf$lt_Ecdy_ven)
      lines(tshf$wt_Ecdy_ven)
      abline(v=unique(a[a$state=="r", "date"]), lty=3, col="black")
      axis(2, las=2)
      axis(1, at=seq(tStart, tEnd, by="year"), 
           labels=format(seq(tStart, tEnd, by="year"), "%d-%b\n%Y"),
           padj=0.3)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=4, line=2.75, at=0.35, cex=0.65)
      axis(4, at=Qax, 
           labels=formatC(seq(-(0.3-0.5)/0.0015, 0, length=length(Qax)), format="d"), 
           las=2, cex.axis=0.8)
      lines(bsNoOutl$Datum, -0.0015*bsNoOutl$sarnau+0.5)
      text(x=as.Date("2012-06-20"), y=0.25, 
           labels="(G) Ecdy. ven.", adj=1)
      box()
    }
   
    windows(height=9, width=8.5)
    par(mfrow=c(6, 1), mar=c(2.5, 4, 0.5, 4))
    
    # Ephem_dan
    {
      plot(tshf$cr_Ephem_dan, ylim=c(0, 0.5), type="l", lty=1, axes=FALSE, 
           col="grey", xlim=c(tStart, tEnd))
      lines(tshf$lr_Ephem_dan, col="grey")
      lines(tshf$wr_Ephem_dan, col="grey")
      lines(tshf$ct_Ephem_dan)
      lines(tshf$lt_Ephem_dan)
      lines(tshf$wt_Ephem_dan)
      abline(v=unique(a[a$state=="r", "date"]), lty=3, col="black")
      axis(2, las=2)
      axis(1, at=seq(tStart, tEnd, by="year"), 
           labels=format(seq(tStart, tEnd, by="year"), "%d-%b\n%Y"),
           padj=0.3)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=4, line=2.75, at=0.35, cex=0.65)
      axis(4, at=Qax, 
           labels=formatC(seq(-(0.3-0.5)/0.0015, 0, length=length(Qax)), format="d"), 
           las=2, cex.axis=0.8)
      lines(bsNoOutl$Datum, -0.0015*bsNoOutl$sarnau+0.5)
      text(x=as.Date("2012-06-20"), y=0.25, 
           labels="(H) Ephem. dan.", adj=1)
      legend(x=as.Date("2009-11-01"), y=0.4, legend=c("Restored", "Trained"), 
             lty=1, col=c("grey", "black"), bty="n")
      polygon(x=as.Date(c("2009-11-30", "2009-11-30", "2011-02-20", "2011-02-20")),
              y=c(0.19, 0.36, 0.36, 0.19))
      box()
    }
    
    # Goe_pil
    {    
      plot(tshf$cr_Goe_pil, ylim=c(0, 0.5), type="l", lty=1, axes=FALSE, 
           col="grey", xlim=c(tStart, tEnd))
      lines(tshf$lr_Goe_pil, col="grey")
      lines(tshf$wr_Goe_pil, col="grey")
      lines(tshf$ct_Goe_pil)
      lines(tshf$lt_Goe_pil)
      lines(tshf$wt_Goe_pil)
      abline(v=unique(a[a$state=="r", "date"]), lty=3, col="black")
      axis(2, las=2)
      axis(1, at=seq(tStart, tEnd, by="year"), 
           labels=format(seq(tStart, tEnd, by="year"), "%d-%b\n%Y"),
           padj=0.3)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=4, line=2.75, at=0.35, cex=0.65)
      axis(4, at=Qax, 
           labels=formatC(seq(-(0.3-0.5)/0.0015, 0, length=length(Qax)), format="d"), 
           las=2, cex.axis=0.8)
      lines(bsNoOutl$Datum, -0.0015*bsNoOutl$sarnau+0.5)
      text(x=as.Date("2012-06-20"), y=0.25, 
           labels="(I) Goe. pil.", adj=1)
      box()
    }    
    
    # Hab_lau
    {    
      plot(tshf$cr_Hab_lau, ylim=c(0, 0.5), type="l", lty=1, axes=FALSE, 
           col="grey", xlim=c(tStart, tEnd))
      lines(tshf$lr_Hab_lau, col="grey")
      lines(tshf$wr_Hab_lau, col="grey")
      lines(tshf$ct_Hab_lau)
      lines(tshf$lt_Hab_lau)
      lines(tshf$wt_Hab_lau)
      abline(v=unique(a[a$state=="r", "date"]), lty=3, col="black")
      axis(2, las=2)
      axis(1, at=seq(tStart, tEnd, by="year"), 
           labels=format(seq(tStart, tEnd, by="year"), "%d-%b\n%Y"),
           padj=0.3)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=4, line=2.75, at=0.35, cex=0.65)
      axis(4, at=Qax, 
           labels=formatC(seq(-(0.3-0.5)/0.0015, 0, length=length(Qax)), format="d"), 
           las=2, cex.axis=0.8)
      lines(bsNoOutl$Datum, -0.0015*bsNoOutl$sarnau+0.5)
      text(x=as.Date("2012-06-20"), y=0.25, 
           labels="(J) Hab. lau.", adj=1)
      box()
    }
    
    # Odont_alb
    {    
      plot(tshf$cr_Odont_alb, ylim=c(0, 0.5), type="l", lty=1, axes=FALSE, 
           col="grey", xlim=c(tStart, tEnd))
      lines(tshf$lr_Odont_alb, col="grey")
      lines(tshf$wr_Odont_alb, col="grey")
      lines(tshf$ct_Odont_alb)
      lines(tshf$lt_Odont_alb)
      lines(tshf$wt_Odont_alb)
      abline(v=unique(a[a$state=="r", "date"]), lty=3, col="black")
      axis(2, las=2)
      axis(1, at=seq(tStart, tEnd, by="year"), 
           labels=format(seq(tStart, tEnd, by="year"), "%d-%b\n%Y"),
           padj=0.3)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=4, line=2.75, at=0.35, cex=0.65)
      axis(4, at=Qax, 
           labels=formatC(seq(-(0.3-0.5)/0.0015, 0, length=length(Qax)), format="d"), 
           las=2, cex.axis=0.8)
      lines(bsNoOutl$Datum, -0.0015*bsNoOutl$sarnau+0.5)
      text(x=as.Date("2012-06-20"), y=0.25, 
           labels="(K) Odont. alb.", adj=1)
      box()
    }
    
    # Potam_lut
    {    
      plot(tshf$cr_Potam_lut, ylim=c(0, 0.5), type="l", lty=1, axes=FALSE, 
           col="grey", xlim=c(tStart, tEnd))
      lines(tshf$lr_Potam_lut, col="grey")
      lines(tshf$wr_Potam_lut, col="grey")
      lines(tshf$ct_Potam_lut)
      lines(tshf$lt_Potam_lut)
      lines(tshf$wt_Potam_lut)
      abline(v=unique(a[a$state=="r", "date"]), lty=3, col="black")
      axis(2, las=2)
      axis(1, at=seq(tStart, tEnd, by="year"), 
           labels=format(seq(tStart, tEnd, by="year"), "%d-%b\n%Y"),
           padj=0.3)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=4, line=2.75, at=0.35, cex=0.65)
      axis(4, at=Qax, 
           labels=formatC(seq(-(0.3-0.5)/0.0015, 0, length=length(Qax)), format="d"), 
           las=2, cex.axis=0.8)
      lines(bsNoOutl$Datum, -0.0015*bsNoOutl$sarnau+0.5)
      text(x=as.Date("2012-06-20"), y=0.25, 
           labels="(L) Potam. lut.", adj=1)
      box()
    }
    
    # Caen_luct
    {    
      plot(tshf$cr_Psych_pus, ylim=c(0, 0.5), type="l", lty=1, axes=FALSE, 
           col="grey", xlim=c(tStart, tEnd))
      lines(tshf$lr_Psych_pus, col="grey")
      lines(tshf$wr_Psych_pus, col="grey")
      lines(tshf$ct_Psych_pus)
      lines(tshf$lt_Psych_pus)
      lines(tshf$wt_Psych_pus)
      abline(v=unique(a[a$state=="r", "date"]), lty=3, col="black")
      axis(2, las=2)
      axis(1, at=seq(tStart, tEnd, by="year"), 
           labels=format(seq(tStart, tEnd, by="year"), "%d-%b\n%Y"),
           padj=0.3)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=4, line=2.75, at=0.35, cex=0.65)
      axis(4, at=Qax, 
           labels=formatC(seq(-(0.3-0.5)/0.0015, 0, length=length(Qax)), format="d"), 
           las=2, cex.axis=0.8)
      lines(bsNoOutl$Datum, -0.0015*bsNoOutl$sarnau+0.5)
      text(x=as.Date("2012-06-20"), y=0.25, 
           labels="(M) Psych. pus.", adj=1)
      box()
    }
  }
}


# figure 11, hf vs Q
{
  # prepare data
  {
    setwd("c:\\presentations_papers\\paper1\\")
    a      <- read.table("hsAv13sp.txt", header=TRUE, sep=";")
    a$date <- as.Date(a$date, format="%m/%d/%Y")
    a      <- a[order(a$resVal),]
    
    setwd("c:\\doktor\\proof_of_concept\\hydromStress\\")
    lf <- list.files(getwd())
    lf <- lf[which(regexpr("out", lf)!=-1)]
    
    taxonlist <- c("Odont_alb",
                   "All_auri",
                   "Ana_nerv", 
                   "Ephem_dan",
                   "Potam_lut",
                   "Hab_lau",    
                   "Ecdy_ven",
                   "Goe_pil",
                   "Caen_luct",
                   "Psych_pus",
                   "Bae_luth",
                   "Bae_fus",
                   "Ase_aqua")
    
    lfuse <- character()
    for(i in lf)
    {
      lfname  <- substr(i, 8, nchar(i))
      lfmatch <- match(taxonlist, lfname)
      if(sum(lfmatch[!is.na(lfmatch)], na.rm=TRUE))
      {
        lfuse <- c(lfuse, i)
      }
    }
    
    tshf <- list()
    count <- 0
    for(i in lfuse)
    {
      count         <- count + 1
      file          <- paste0(getwd(), "/", i, "/frHab.txt")
      hf            <- read.table(file, header=FALSE)
      names(hf)     <- "hf"
      hf2           <- data.frame(tt=seq(as.Date("2003-11-01"), 
                                         length=nrow(hf), by="day"), hf)
      tshf[[count]]        <- hf2
      names(tshf)[[count]] <- substr(i, 5, nchar(i))
    }
    
    # discharge data
    setwd("c:\\doktor\\proof_of_concept\\")
    bs             <- read.table("corrBiedSarWithOutliers.txt",header=TRUE)
    bsNoOutl       <- read.table("corrBiedSarWithoutOutliers.txt",header=TRUE)
    outliers       <- read.table("corrBiedSarOutliers.txt",header=TRUE)
    niedNoOut      <- read.table("niederwetter.txt",header=TRUE)
    bsNoOutl[[1]]  <- as.Date(bsNoOutl[[1]])
    niedNoOut[[1]] <- as.Date(niedNoOut[[1]])
    bsNoOutl       <- bsNoOutl[order(bsNoOutl[[1]]),]
    niedNoOut      <- niedNoOut[order(niedNoOut[[1]]),]
    
    indices        <- match(bsNoOutl$Datum, niedNoOut$time)
    nied           <- niedNoOut[indices, ]
    biedsa         <- bsNoOutl
    
    Qtable <- tbl_df(data.frame(dateTime=nied$time,
                                Qnied=nied$Q_m3s,
                                Qbied=biedsa$biedenkopf,
                                Qsar=biedsa$sarnau))
    
    hftable <- data.frame(dateTime=Qtable$dateTime)
    for(i in 1:length(tshf))
    {
      indices <- match(Qtable$dateTime, tshf[[i]]$tt)
      hftable <- cbind(hftable, tshf[[i]][indices, "hf" ])
    }
    names(hftable)[2:79] <- names(tshf)
  }
  
  # plot
  {
    windows(height=3, width=8.5)
    par(mar=c(3,3.35,1,1))
    
    layout(matrix(c(1, 1, 2, 2, 3,  3,  4, 4,  5,   5,  6,  6,  7, 7,
                    0, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 0), 
                  2, 14, byrow=TRUE))
    
    # All_auri
    {
      plot(Qtable$Qbied, hftable$cr_All_auri, cex=0.75, pch=20, col="grey",
           ylim=c(0, 0.5), xlim=c(0, 106), axes=FALSE, xlab="", ylab="")
      axis(1, at=seq(0, 100, by=25), padj=-0.75)
      axis(2, at=seq(0, 0.5, by=0.1), las=2, hadj=0.6)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=1, line=2, cex=0.8)
      mtext("hf", side=2, line=2.25, cex=0.8)
      legend(x=30, y=0.35, legend=c("Res.", "Tr."), bty="n",
             pch=20, col=c("grey", "black"))
      points(Qtable$Qbied, hftable$lr_All_auri, cex=0.75, pch=20, col="grey")  
      points(Qtable$Qbied, hftable$wr_All_auri, cex=0.75, pch=20, col="grey")  
      points(Qtable$Qbied, hftable$ct_All_auri, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$lt_All_auri, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$wt_All_auri, cex=0.75, pch=20)
      text(x=105, y=0.45, labels="A.aur.\n(A)", adj=1)
      box()
    }
    
    # Ana_nerv
    {
      plot(Qtable$Qbied, hftable$cr_Ana_nerv, cex=0.75, pch=20, col="grey",
           ylim=c(0, 0.5), xlim=c(0, 106), axes=FALSE, xlab="", ylab="")
      axis(1, at=seq(0, 100, by=25), padj=-0.75)
      axis(2, at=seq(0, 0.5, by=0.1), las=2, hadj=0.6)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=1, line=2, cex=0.8)
      mtext("hf", side=2, line=2.25, cex=0.8)
      points(Qtable$Qbied, hftable$lr_Ana_nerv, cex=0.75, pch=20, col="grey")  
      points(Qtable$Qbied, hftable$wr_Ana_nerv, cex=0.75, pch=20, col="grey")
      points(Qtable$Qbied, hftable$ct_Ana_nerv, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$lt_Ana_nerv, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$wt_Ana_nerv, cex=0.75, pch=20)
      text(x=105, y=0.45, labels="A.ner.\n(B)", adj=1)
      box()
    }
    
    # Ase_aqua
    {
      plot(Qtable$Qbied, hftable$cr_Ase_aqua, cex=0.75, pch=20, col="grey",
           ylim=c(0, 0.5), xlim=c(0, 106), axes=FALSE, xlab="", ylab="")
      axis(1, at=seq(0, 100, by=25), padj=-0.75)
      axis(2, at=seq(0, 0.5, by=0.1), las=2, hadj=0.6)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=1, line=2, cex=0.8)
      mtext("hf", side=2, line=2.25, cex=0.8)
      points(Qtable$Qbied, hftable$lr_Ase_aqua, cex=0.75, pch=20, col="grey")  
      points(Qtable$Qbied, hftable$wr_Ase_aqua, cex=0.75, pch=20, col="grey")
      points(Qtable$Qbied, hftable$ct_Ase_aqua, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$lt_Ase_aqua, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$wt_Ase_aqua, cex=0.75, pch=20)
      text(x=105, y=0.45, labels="A.aq.\n(C)", adj=1)
      box()
    }
    
    # Bae_fus
    {
      plot(Qtable$Qbied, hftable$cr_Bae_fus, cex=0.75, pch=20, col="grey",
           ylim=c(0, 0.5), xlim=c(0, 106), axes=FALSE, xlab="", ylab="")
      axis(1, at=seq(0, 100, by=25), padj=-0.75)
      axis(2, at=seq(0, 0.5, by=0.1), las=2, hadj=0.6)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=1, line=2, cex=0.8)
      mtext("hf", side=2, line=2.25, cex=0.8)
      points(Qtable$Qbied, hftable$lr_Bae_fus, cex=0.75, pch=20, col="grey")
      points(Qtable$Qbied, hftable$wr_Bae_fus, cex=0.75, pch=20, col="grey")
      points(Qtable$Qbied, hftable$ct_Bae_fus, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$lt_Bae_fus, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$wt_Bae_fus, cex=0.75, pch=20)
      text(x=105, y=0.45, labels="B.fus.\n(D)", adj=1)
      box()
    }
    
    # Bae_luth
    {
      plot(Qtable$Qbied, hftable$cr_Bae_luth, cex=0.75, pch=20, col="grey",
           ylim=c(0, 0.5), xlim=c(0, 106), axes=FALSE, xlab="", ylab="")
      axis(1, at=seq(0, 100, by=25), padj=-0.75)
      axis(2, at=seq(0, 0.5, by=0.1), las=2, hadj=0.6)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=1, line=2, cex=0.8)
      mtext("hf", side=2, line=2.25, cex=0.8)
      points(Qtable$Qbied, hftable$lr_Bae_luth, cex=0.75, pch=20, col="grey")
      points(Qtable$Qbied, hftable$wr_Bae_luth, cex=0.75, pch=20, col="grey")
      points(Qtable$Qbied, hftable$ct_Bae_luth, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$lt_Bae_luth, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$wt_Bae_luth, cex=0.75, pch=20)
      text(x=105, y=0.45, labels="B.luth.\n(E)", adj=1)
      box()
    }
    
    # Caen_luct
    {
      plot(Qtable$Qbied, hftable$cr_Caen_luct, cex=0.75, pch=20, col="grey",
           ylim=c(0, 0.5), xlim=c(0, 106), axes=FALSE, xlab="", ylab="")
      axis(1, at=seq(0, 100, by=25), padj=-0.75)
      axis(2, at=seq(0, 0.5, by=0.1), las=2, hadj=0.6)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=1, line=2, cex=0.8)
      mtext("hf", side=2, line=2.25, cex=0.8)
      points(Qtable$Qbied, hftable$lr_Caen_luct, cex=0.75, pch=20, col="grey")
      points(Qtable$Qbied, hftable$wr_Caen_luct, cex=0.75, pch=20, col="grey")
      points(Qtable$Qbied, hftable$ct_Caen_luct, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$lt_Caen_luct, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$wt_Caen_luct, cex=0.75, pch=20)
      text(x=105, y=0.45, labels="C.luct.\n(F)", adj=1)
      box()
    }
    
    # Ecdy_ven
    {
      plot(Qtable$Qbied, hftable$cr_Ecdy_ven, cex=0.75, pch=20, col="grey",
           ylim=c(0, 0.5), xlim=c(0, 106), axes=FALSE, xlab="", ylab="")
      axis(1, at=seq(0, 100, by=25), padj=-0.75)
      axis(2, at=seq(0, 0.5, by=0.1), las=2, hadj=0.6)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=1, line=2, cex=0.8)
      mtext("hf", side=2, line=2.25, cex=0.8)
      points(Qtable$Qbied, hftable$lr_Ecdy_ven, cex=0.75, pch=20, col="grey")
      points(Qtable$Qbied, hftable$wr_Ecdy_ven, cex=0.75, pch=20, col="grey")
      points(Qtable$Qbied, hftable$ct_Ecdy_ven, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$lt_Ecdy_ven, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$wt_Ecdy_ven, cex=0.75, pch=20)
      text(x=105, y=0.45, labels="E.ven.\n(G)", adj=1)
      box()
    }
    
    # Ephem_dan
    {
      plot(Qtable$Qbied, hftable$cr_Ephem_dan, cex=0.75, pch=20, col="grey",
           ylim=c(0, 0.5), xlim=c(0, 106), axes=FALSE, xlab="", ylab="")
      axis(1, at=seq(0, 100, by=25), padj=-0.75)
      axis(2, at=seq(0, 0.5, by=0.1), las=2, hadj=0.6)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=1, line=2, cex=0.8)
      mtext("hf", side=2, line=2.25, cex=0.8)
      points(Qtable$Qbied, hftable$lr_Ephem_dan, cex=0.75, pch=20, col="grey")
      points(Qtable$Qbied, hftable$wr_Ephem_dan, cex=0.75, pch=20, col="grey")
      points(Qtable$Qbied, hftable$ct_Ephem_dan, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$lt_Ephem_dan, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$wt_Ephem_dan, cex=0.75, pch=20)
      text(x=105, y=0.45, labels="E.dan.\n(H)", adj=1)
      box()
    }
    
    # Goe_pil
    {
      plot(Qtable$Qbied, hftable$cr_Goe_pil, cex=0.75, pch=20, col="grey",
           ylim=c(0, 0.5), xlim=c(0, 106), axes=FALSE, xlab="", ylab="")
      axis(1, at=seq(0, 100, by=25), padj=-0.75)
      axis(2, at=seq(0, 0.5, by=0.1), las=2, hadj=0.6)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=1, line=2, cex=0.8)
      mtext("hf", side=2, line=2.25, cex=0.8)
      points(Qtable$Qbied, hftable$lr_Goe_pil, cex=0.75, pch=20, col="grey")
      points(Qtable$Qbied, hftable$wr_Goe_pil, cex=0.75, pch=20, col="grey")
      points(Qtable$Qbied, hftable$ct_Goe_pil, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$lt_Goe_pil, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$wt_Goe_pil, cex=0.75, pch=20)
      text(x=105, y=0.45, labels="G.pil.\n(I)", adj=1)
      box()
    }
    
    # Hab_lau
    {
      plot(Qtable$Qbied, hftable$cr_Hab_lau, cex=0.75, pch=20, col="grey",
           ylim=c(0, 0.5), xlim=c(0, 106), axes=FALSE, xlab="", ylab="")
      axis(1, at=seq(0, 100, by=25), padj=-0.75)
      axis(2, at=seq(0, 0.5, by=0.1), las=2, hadj=0.6)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=1, line=2, cex=0.8)
      mtext("hf", side=2, line=2.25, cex=0.8)
      points(Qtable$Qbied, hftable$lr_Hab_lau, cex=0.75, pch=20, col="grey")
      points(Qtable$Qbied, hftable$wr_Hab_lau, cex=0.75, pch=20, col="grey")
      points(Qtable$Qbied, hftable$ct_Hab_lau, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$lt_Hab_lau, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$wt_Hab_lau, cex=0.75, pch=20)
      text(x=105, y=0.45, labels="H.lau.\n(J)", adj=1)
      box()
    }
    
    # Odont_alb
    {
      plot(Qtable$Qbied, hftable$cr_Odont_alb, cex=0.75, pch=20, col="grey",
           ylim=c(0, 0.5), xlim=c(0, 106), axes=FALSE, xlab="", ylab="")
      axis(1, at=seq(0, 100, by=25), padj=-0.75)
      axis(2, at=seq(0, 0.5, by=0.1), las=2, hadj=0.6)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=1, line=2, cex=0.8)
      mtext("hf", side=2, line=2.25, cex=0.8)
      points(Qtable$Qbied, hftable$lr_Odont_alb, cex=0.75, pch=20, col="grey")
      points(Qtable$Qbied, hftable$wr_Odont_alb, cex=0.75, pch=20, col="grey")
      points(Qtable$Qbied, hftable$ct_Odont_alb, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$lt_Odont_alb, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$wt_Odont_alb, cex=0.75, pch=20)
      text(x=105, y=0.45, labels="O.alb.\n(K)", adj=1)
      box()
    }
    
    # Potam_lut
    {
      plot(Qtable$Qbied, hftable$cr_Potam_lut, cex=0.75, pch=20, col="grey",
           ylim=c(0, 0.5), xlim=c(0, 106), axes=FALSE, xlab="", ylab="")
      axis(1, at=seq(0, 100, by=25), padj=-0.75)
      axis(2, at=seq(0, 0.5, by=0.1), las=2, hadj=0.6)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=1, line=2, cex=0.8)
      mtext("hf", side=2, line=2.25, cex=0.8)
      points(Qtable$Qbied, hftable$lr_Potam_lut, cex=0.75, pch=20, col="grey")
      points(Qtable$Qbied, hftable$wr_Potam_lut, cex=0.75, pch=20, col="grey")
      points(Qtable$Qbied, hftable$ct_Potam_lut, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$lt_Potam_lut, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$wt_Potam_lut, cex=0.75, pch=20)
      text(x=105, y=0.45, labels="P.luth.\n(L)", adj=1)
      box()
    }
    
    # Psych_pus
    {
      plot(Qtable$Qbied, hftable$cr_Psych_pus, cex=0.75, pch=20, col="grey",
           ylim=c(0, 0.5), xlim=c(0, 106), axes=FALSE, xlab="", ylab="")
      axis(1, at=seq(0, 100, by=25), padj=-0.75)
      axis(2, at=seq(0, 0.5, by=0.1), las=2, hadj=0.6)
      mtext(expression(paste("Q [", m^3,s^-1, "]")), side=1, line=2, cex=0.8)
      mtext("hf", side=2, line=2.25, cex=0.8)
      points(Qtable$Qbied, hftable$lr_Psych_pus, cex=0.75, pch=20, col="grey")
      points(Qtable$Qbied, hftable$wr_Psych_pus, cex=0.75, pch=20, col="grey")
      points(Qtable$Qbied, hftable$ct_Psych_pus, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$lt_Psych_pus, cex=0.75, pch=20)
      points(Qtable$Qbied, hftable$wt_Psych_pus, cex=0.75, pch=20)
      text(x=105, y=0.45, labels="P.pus.\n(M)", adj=1)
      box()
    }
  }
}


# figure 12, hsAv for all taxa
{
  # prepare data
  {
    setwd("c:\\presentations_papers\\paper1\\")
    a      <- read.table("hsAv13sp.txt", header=TRUE, sep=";")
    a$date <- as.Date(a$date, format="%m/%d/%Y")
    a      <- a[order(a$resVal),]
    
    setwd("c:\\doktor\\proof_of_concept\\hydromStress\\")
    lf <- list.files(getwd())
    lf <- lf[which(regexpr("out", lf)!=-1)]
    
    taxonlist <- c("Odont_alb",
                   "All_auri",
                   "Ana_nerv", 
                   "Ephem_dan",
                   "Potam_lut",
                   "Hab_lau",    
                   "Ecdy_ven",
                   "Goe_pil",
                   "Caen_luct",
                   "Psych_pus",
                   "Bae_luth",
                   "Bae_fus",
                   "Ase_aqua")
    
    lfuse <- character()
    for(i in lf)
    {
      lfname  <- substr(i, 8, nchar(i))
      lfmatch <- match(taxonlist, lfname)
      if(sum(lfmatch[!is.na(lfmatch)], na.rm=TRUE))
      {
        lfuse <- c(lfuse, i)
      }
    }
    
    tshf <- list()
    count <- 0
    for(i in lfuse)
    {
      count         <- count + 1
      file          <- paste0(getwd(), "/", i, "/frHab.txt")
      hf            <- read.table(file, header=FALSE)
      names(hf)     <- "hf"
      hf2           <- data.frame(tt=seq(as.Date("2003-11-01"), 
                                         length=nrow(hf), by="day"), hf)
      tshf[[count]]        <- hf2
      names(tshf)[[count]] <- substr(i, 5, nchar(i))
    }
    
  }
  
  # plot
  {
    windows(height=6, width=8.5)
    par(mar=c(2,3.25,0.5,1))
    layout(matrix(c(1,  1,  2,  2,  3,  3,  4, 4,   
                    0,  5,  5,  6,  6,  7,  7, 0,
                    0,  8,  8,  9,  9, 10, 10, 0,
                    0, 11, 11, 12, 12, 13, 13, 0), 
                  4, 8, byrow=TRUE))

    # All_auri
    {
      plot(knots(ecdf(tshf$cr_All_auri[, 2])),
           ecdf(tshf$cr_All_auri[, 2])(knots(ecdf(tshf$cr_All_auri[, 2]))),
           type="l", xlim=c(0,0.3), ylim=c(0,1), axes=FALSE, xlab="", ylab="",
           col="grey", lwd=2)
      lines(knots(ecdf(tshf$lr_All_auri[, 2])), lty=2, lwd=2,
            ecdf(tshf$lr_All_auri[, 2])(knots(ecdf(tshf$lr_All_auri[, 2]))), col="grey")
      lines(knots(ecdf(tshf$wr_All_auri[, 2])), lty=3, lwd=2,
            ecdf(tshf$wr_All_auri[, 2])(knots(ecdf(tshf$wr_All_auri[, 2]))), col="grey")
      lines(knots(ecdf(tshf$ct_All_auri[, 2])), lwd=2,
            ecdf(tshf$ct_All_auri[, 2])(knots(ecdf(tshf$ct_All_auri[, 2]))))
      lines(knots(ecdf(tshf$lt_All_auri[, 2])), lty=2, lwd=2,
            ecdf(tshf$lt_All_auri[, 2])(knots(ecdf(tshf$lt_All_auri[, 2]))))
      lines(knots(ecdf(tshf$wt_All_auri[, 2])), lty=3, lwd=2,
            ecdf(tshf$wt_All_auri[, 2])(knots(ecdf(tshf$wt_All_auri[, 2]))))
      axis(1, pos=0, at=seq(0, 0.5, by=0.1), padj=-0.75)
      axis(2, pos=0, at=seq(0, 1, by=0.2), las=2, hadj=0.6)
      mtext(side=1, "hf", line=1.2)
      mtext(side=2, expression(F[n](hf)), line=1.7)
      lines(x=c(0, 0.3), y=c(1, 1))
      lines(x=c(0.3, 0.3), y=c(1, 0))
      legend(x=0.1, y=0.44, legend=c("cr", "lr", "wr", "ct", "lt", "wt"), bty="n",
             col=c("grey", "grey", "grey", "black", "black", "black"), 
             lty=c(1, 2, 3, 1, 2, 3), cex=0.85, ncol=2, lwd=2)
      shadowtext(x=0.295, y=0.8, labels="A.aur.\n(A)", adj=1, col="black", bg="white")
    }
    
    # Ana_nerv
    {
      plot(knots(ecdf(tshf$cr_Ana_nerv[, 2])),
           ecdf(tshf$cr_Ana_nerv[, 2])(knots(ecdf(tshf$cr_Ana_nerv[, 2]))),
           type="l", xlim=c(0,0.3), ylim=c(0,1), axes=FALSE, xlab="", ylab="",
           col="grey", lwd=2)
      lines(knots(ecdf(tshf$lr_Ana_nerv[, 2])), lty=2, lwd=2,
            ecdf(tshf$lr_Ana_nerv[, 2])(knots(ecdf(tshf$lr_Ana_nerv[, 2]))), col="grey")
      lines(knots(ecdf(tshf$wr_Ana_nerv[, 2])), lty=3, lwd=2,
            ecdf(tshf$wr_Ana_nerv[, 2])(knots(ecdf(tshf$wr_Ana_nerv[, 2]))), col="grey")
      lines(knots(ecdf(tshf$ct_Ana_nerv[, 2])), lwd=2,
            ecdf(tshf$ct_Ana_nerv[, 2])(knots(ecdf(tshf$ct_Ana_nerv[, 2]))))
      lines(knots(ecdf(tshf$lt_Ana_nerv[, 2])), lty=2, lwd=2,
            ecdf(tshf$lt_Ana_nerv[, 2])(knots(ecdf(tshf$lt_Ana_nerv[, 2]))))
      lines(knots(ecdf(tshf$wt_Ana_nerv[, 2])), lty=3, lwd=2,
            ecdf(tshf$wt_Ana_nerv[, 2])(knots(ecdf(tshf$wt_Ana_nerv[, 2]))))
      axis(1, pos=0, at=seq(0, 0.5, by=0.1), padj=-0.75)
      axis(2, pos=0, at=seq(0, 1, by=0.2), las=2, hadj=0.6)
      mtext(side=1, "hf", line=1.2)
      mtext(side=2, expression(F[n](hf)), line=1.7)
      lines(x=c(0, 0.3), y=c(1, 1))
      lines(x=c(0.3, 0.3), y=c(1, 0))
      shadowtext(x=0.295, y=0.15, labels="A.nerv.\n(B)", adj=1, col="black", bg="white")
    }
    
    # Ase_aqua
    {
      plot(knots(ecdf(tshf$cr_Ase_aqua[, 2])),
           ecdf(tshf$cr_Ase_aqua[, 2])(knots(ecdf(tshf$cr_Ase_aqua[, 2]))),
           type="l", xlim=c(0,0.3), ylim=c(0,1), axes=FALSE, xlab="", ylab="",
           col="grey", lwd=2)
      lines(knots(ecdf(tshf$lr_Ase_aqua[, 2])), lty=2, lwd=2,
            ecdf(tshf$lr_Ase_aqua[, 2])(knots(ecdf(tshf$lr_Ase_aqua[, 2]))), col="grey")
      lines(knots(ecdf(tshf$wr_Ase_aqua[, 2])), lty=3, lwd=2,
            ecdf(tshf$wr_Ase_aqua[, 2])(knots(ecdf(tshf$wr_Ase_aqua[, 2]))), col="grey")
      lines(knots(ecdf(tshf$ct_Ase_aqua[, 2])), lwd=2,
            ecdf(tshf$ct_Ase_aqua[, 2])(knots(ecdf(tshf$ct_Ase_aqua[, 2]))))
      lines(knots(ecdf(tshf$lt_Ase_aqua[, 2])), lty=2, lwd=2,
            ecdf(tshf$lt_Ase_aqua[, 2])(knots(ecdf(tshf$lt_Ase_aqua[, 2]))))
      lines(knots(ecdf(tshf$wt_Ase_aqua[, 2])), lty=3, lwd=2,
            ecdf(tshf$wt_Ase_aqua[, 2])(knots(ecdf(tshf$wt_Ase_aqua[, 2]))))
      axis(1, pos=0, at=seq(0, 0.5, by=0.1), padj=-0.75)
      axis(2, pos=0, at=seq(0, 1, by=0.2), las=2, hadj=0.6)
      mtext(side=1, "hf", line=1.2)
      mtext(side=2, expression(F[n](hf)), line=1.7)
      lines(x=c(0, 0.3), y=c(1, 1))
      lines(x=c(0.3, 0.3), y=c(1, 0))
      shadowtext(x=0.295, y=0.15, labels="A.aq.\n(C)", adj=1, col="black", bg="white")
    }
    
    # Bae_fus
    {
      plot(knots(ecdf(tshf$cr_Bae_fus[, 2])),
           ecdf(tshf$cr_Bae_fus[, 2])(knots(ecdf(tshf$cr_Bae_fus[, 2]))),
           type="l", xlim=c(0,0.3), ylim=c(0,1), axes=FALSE, xlab="", ylab="",
           col="grey", lwd=2)
      lines(knots(ecdf(tshf$lr_Bae_fus[, 2])), lty=2, lwd=2,
            ecdf(tshf$lr_Bae_fus[, 2])(knots(ecdf(tshf$lr_Bae_fus[, 2]))), col="grey")
      lines(knots(ecdf(tshf$wr_Bae_fus[, 2])), lty=3, lwd=2,
            ecdf(tshf$wr_Bae_fus[, 2])(knots(ecdf(tshf$wr_Bae_fus[, 2]))), col="grey")
      lines(knots(ecdf(tshf$ct_Bae_fus[, 2])), lwd=2,
            ecdf(tshf$ct_Bae_fus[, 2])(knots(ecdf(tshf$ct_Bae_fus[, 2]))))
      lines(knots(ecdf(tshf$lt_Bae_fus[, 2])), lty=2, lwd=2,
            ecdf(tshf$lt_Bae_fus[, 2])(knots(ecdf(tshf$lt_Bae_fus[, 2]))))
      lines(knots(ecdf(tshf$wt_Bae_fus[, 2])), lty=3, lwd=2,
            ecdf(tshf$wt_Bae_fus[, 2])(knots(ecdf(tshf$wt_Bae_fus[, 2]))))
      axis(1, pos=0, at=seq(0, 0.5, by=0.1), padj=-0.75)
      axis(2, pos=0, at=seq(0, 1, by=0.2), las=2, hadj=0.6)
      mtext(side=1, "hf", line=1.2)
      mtext(side=2, expression(F[n](hf)), line=1.7)
      lines(x=c(0, 0.3), y=c(1, 1))
      lines(x=c(0.3, 0.3), y=c(1, 0))
      shadowtext(x=0.295, y=0.15, labels="B.fus.\n(D)", adj=1, col="black", bg="white")
    }
    
    # Bae_luth
    {
      plot(knots(ecdf(tshf$cr_Bae_luth[, 2])),
           ecdf(tshf$cr_Bae_luth[, 2])(knots(ecdf(tshf$cr_Bae_luth[, 2]))),
           type="l", xlim=c(0,0.3), ylim=c(0,1), axes=FALSE, xlab="", ylab="",
           col="grey", lwd=2)
      lines(knots(ecdf(tshf$lr_Bae_luth[, 2])), lty=2, lwd=2,
            ecdf(tshf$lr_Bae_luth[, 2])(knots(ecdf(tshf$lr_Bae_luth[, 2]))), col="grey")
      lines(knots(ecdf(tshf$wr_Bae_luth[, 2])), lty=3, lwd=2,
            ecdf(tshf$wr_Bae_luth[, 2])(knots(ecdf(tshf$wr_Bae_luth[, 2]))), col="grey")
      lines(knots(ecdf(tshf$ct_Bae_luth[, 2])), lwd=2,
            ecdf(tshf$ct_Bae_luth[, 2])(knots(ecdf(tshf$ct_Bae_luth[, 2]))))
      lines(knots(ecdf(tshf$lt_Bae_luth[, 2])), lty=2, lwd=2,
            ecdf(tshf$lt_Bae_luth[, 2])(knots(ecdf(tshf$lt_Bae_luth[, 2]))))
      lines(knots(ecdf(tshf$wt_Bae_luth[, 2])), lty=3, lwd=2,
            ecdf(tshf$wt_Bae_luth[, 2])(knots(ecdf(tshf$wt_Bae_luth[, 2]))))
      axis(1, pos=0, at=seq(0, 0.5, by=0.1), padj=-0.75)
      axis(2, pos=0, at=seq(0, 1, by=0.2), las=2, hadj=0.6)
      mtext(side=1, "hf", line=1.2)
      mtext(side=2, expression(F[n](hf)), line=1.7)
      lines(x=c(0, 0.3), y=c(1, 1))
      lines(x=c(0.3, 0.3), y=c(1, 0))
      shadowtext(x=0.295, y=0.15, labels="B.luth.\n(E)", adj=1, col="black", bg="white")
    }
    
    # Caen_luct
    {
      plot(knots(ecdf(tshf$cr_Caen_luct[, 2])),
           ecdf(tshf$cr_Caen_luct[, 2])(knots(ecdf(tshf$cr_Caen_luct[, 2]))),
           type="l", xlim=c(0,0.3), ylim=c(0,1), axes=FALSE, xlab="", ylab="",
           col="grey", lwd=2)
      lines(knots(ecdf(tshf$lr_Caen_luct[, 2])), lty=2, lwd=2,
            ecdf(tshf$lr_Caen_luct[, 2])(knots(ecdf(tshf$lr_Caen_luct[, 2]))), col="grey")
      lines(knots(ecdf(tshf$wr_Caen_luct[, 2])), lty=3, lwd=2,
            ecdf(tshf$wr_Caen_luct[, 2])(knots(ecdf(tshf$wr_Caen_luct[, 2]))), col="grey")
      lines(knots(ecdf(tshf$ct_Caen_luct[, 2])), lwd=2,
            ecdf(tshf$ct_Caen_luct[, 2])(knots(ecdf(tshf$ct_Caen_luct[, 2]))))
      lines(knots(ecdf(tshf$lt_Caen_luct[, 2])), lty=2, lwd=2,
            ecdf(tshf$lt_Caen_luct[, 2])(knots(ecdf(tshf$lt_Caen_luct[, 2]))))
      lines(knots(ecdf(tshf$wt_Caen_luct[, 2])), lty=3, lwd=2,
            ecdf(tshf$wt_Caen_luct[, 2])(knots(ecdf(tshf$wt_Caen_luct[, 2]))))
      axis(1, pos=0, at=seq(0, 0.5, by=0.1), padj=-0.75)
      axis(2, pos=0, at=seq(0, 1, by=0.2), las=2, hadj=0.6)
      mtext(side=1, "hf", line=1.2)
      mtext(side=2, expression(F[n](hf)), line=1.7)
      lines(x=c(0, 0.3), y=c(1, 1))
      lines(x=c(0.3, 0.3), y=c(1, 0))
      shadowtext(x=0.295, y=0.15, labels="C.luct.\n(F)", adj=1, col="black", bg="white")
    }
    
    # Ecdy_ven
    {
      plot(knots(ecdf(tshf$cr_Ecdy_ven[, 2])),
           ecdf(tshf$cr_Ecdy_ven[, 2])(knots(ecdf(tshf$cr_Ecdy_ven[, 2]))),
           type="l", xlim=c(0,0.3), ylim=c(0,1), axes=FALSE, xlab="", ylab="",
           col="grey", lwd=2)
      lines(knots(ecdf(tshf$lr_Ecdy_ven[, 2])), lty=2, lwd=2,
            ecdf(tshf$lr_Ecdy_ven[, 2])(knots(ecdf(tshf$lr_Ecdy_ven[, 2]))), col="grey")
      lines(knots(ecdf(tshf$wr_Ecdy_ven[, 2])), lty=3, lwd=2,
            ecdf(tshf$wr_Ecdy_ven[, 2])(knots(ecdf(tshf$wr_Ecdy_ven[, 2]))), col="grey")
      lines(knots(ecdf(tshf$ct_Ecdy_ven[, 2])), lwd=2,
            ecdf(tshf$ct_Ecdy_ven[, 2])(knots(ecdf(tshf$ct_Ecdy_ven[, 2]))))
      lines(knots(ecdf(tshf$lt_Ecdy_ven[, 2])), lty=2, lwd=2,
            ecdf(tshf$lt_Ecdy_ven[, 2])(knots(ecdf(tshf$lt_Ecdy_ven[, 2]))))
      lines(knots(ecdf(tshf$wt_Ecdy_ven[, 2])), lty=3, lwd=2,
            ecdf(tshf$wt_Ecdy_ven[, 2])(knots(ecdf(tshf$wt_Ecdy_ven[, 2]))))
      axis(1, pos=0, at=seq(0, 0.5, by=0.1), padj=-0.75)
      axis(2, pos=0, at=seq(0, 1, by=0.2), las=2, hadj=0.6)
      mtext(side=1, "hf", line=1.2)
      mtext(side=2, expression(F[n](hf)), line=1.7)
      lines(x=c(0, 0.3), y=c(1, 1))
      lines(x=c(0.3, 0.3), y=c(1, 0))
      shadowtext(x=0.295, y=0.15, labels="E.ven.\n(G)", adj=1, col="black", bg="white")
    }
    
    # Ephem_dan
    {
      plot(knots(ecdf(tshf$cr_Ephem_dan[, 2])),
           ecdf(tshf$cr_Ephem_dan[, 2])(knots(ecdf(tshf$cr_Ephem_dan[, 2]))),
           type="l", xlim=c(0,0.3), ylim=c(0,1), axes=FALSE, xlab="", ylab="",
           col="grey", lwd=2)
      lines(knots(ecdf(tshf$lr_Ephem_dan[, 2])), lty=2, lwd=2,
            ecdf(tshf$lr_Ephem_dan[, 2])(knots(ecdf(tshf$lr_Ephem_dan[, 2]))), col="grey")
      lines(knots(ecdf(tshf$wr_Ephem_dan[, 2])), lty=3, lwd=2,
            ecdf(tshf$wr_Ephem_dan[, 2])(knots(ecdf(tshf$wr_Ephem_dan[, 2]))), col="grey")
      lines(knots(ecdf(tshf$ct_Ephem_dan[, 2])), lwd=2,
            ecdf(tshf$ct_Ephem_dan[, 2])(knots(ecdf(tshf$ct_Ephem_dan[, 2]))))
      lines(knots(ecdf(tshf$lt_Ephem_dan[, 2])), lty=2, lwd=2,
            ecdf(tshf$lt_Ephem_dan[, 2])(knots(ecdf(tshf$lt_Ephem_dan[, 2]))))
      lines(knots(ecdf(tshf$wt_Ephem_dan[, 2])), lty=3, lwd=2,
            ecdf(tshf$wt_Ephem_dan[, 2])(knots(ecdf(tshf$wt_Ephem_dan[, 2]))))
      axis(1, pos=0, at=seq(0, 0.5, by=0.1), padj=-0.75)
      axis(2, pos=0, at=seq(0, 1, by=0.2), las=2, hadj=0.6)
      mtext(side=1, "hf", line=1.2)
      mtext(side=2, expression(F[n](hf)), line=1.7)
      lines(x=c(0, 0.3), y=c(1, 1))
      lines(x=c(0.3, 0.3), y=c(1, 0))
      shadowtext(x=0.295, y=0.15, labels="E.dan.\n(H)", adj=1, col="black", bg="white")
    }
    
    # Goe_pil
    {
      plot(knots(ecdf(tshf$cr_Goe_pil[, 2])),
           ecdf(tshf$cr_Goe_pil[, 2])(knots(ecdf(tshf$cr_Goe_pil[, 2]))),
           type="l", xlim=c(0,0.3), ylim=c(0,1), axes=FALSE, xlab="", ylab="",
           col="grey", lwd=2)
      lines(knots(ecdf(tshf$lr_Goe_pil[, 2])), lty=2, lwd=2,
            ecdf(tshf$lr_Goe_pil[, 2])(knots(ecdf(tshf$lr_Goe_pil[, 2]))), col="grey")
      lines(knots(ecdf(tshf$wr_Goe_pil[, 2])), lty=3, lwd=2,
            ecdf(tshf$wr_Goe_pil[, 2])(knots(ecdf(tshf$wr_Goe_pil[, 2]))), col="grey")
      lines(knots(ecdf(tshf$ct_Goe_pil[, 2])), lwd=2,
            ecdf(tshf$ct_Goe_pil[, 2])(knots(ecdf(tshf$ct_Goe_pil[, 2]))))
      lines(knots(ecdf(tshf$lt_Goe_pil[, 2])), lty=2, lwd=2,
            ecdf(tshf$lt_Goe_pil[, 2])(knots(ecdf(tshf$lt_Goe_pil[, 2]))))
      lines(knots(ecdf(tshf$wt_Goe_pil[, 2])), lty=3, lwd=2,
            ecdf(tshf$wt_Goe_pil[, 2])(knots(ecdf(tshf$wt_Goe_pil[, 2]))))
      axis(1, pos=0, at=seq(0, 0.5, by=0.1), padj=-0.75)
      axis(2, pos=0, at=seq(0, 1, by=0.2), las=2, hadj=0.6)
      mtext(side=1, "hf", line=1.2)
      mtext(side=2, expression(F[n](hf)), line=1.7)
      lines(x=c(0, 0.3), y=c(1, 1))
      lines(x=c(0.3, 0.3), y=c(1, 0))
      shadowtext(x=0.295, y=0.15, labels="G.pil.\n(I)", adj=1, col="black", bg="white")
    }
    
    # Hab_lau
    {
      plot(knots(ecdf(tshf$cr_Hab_lau[, 2])),
           ecdf(tshf$cr_Hab_lau[, 2])(knots(ecdf(tshf$cr_Hab_lau[, 2]))),
           type="l", xlim=c(0,0.3), ylim=c(0,1), axes=FALSE, xlab="", ylab="",
           col="grey", lwd=2)
      lines(knots(ecdf(tshf$lr_Hab_lau[, 2])), lty=2, lwd=2,
            ecdf(tshf$lr_Hab_lau[, 2])(knots(ecdf(tshf$lr_Hab_lau[, 2]))), col="grey")
      lines(knots(ecdf(tshf$wr_Hab_lau[, 2])), lty=3, lwd=2,
            ecdf(tshf$wr_Hab_lau[, 2])(knots(ecdf(tshf$wr_Hab_lau[, 2]))), col="grey")
      lines(knots(ecdf(tshf$ct_Hab_lau[, 2])), lwd=2,
            ecdf(tshf$ct_Hab_lau[, 2])(knots(ecdf(tshf$ct_Hab_lau[, 2]))))
      lines(knots(ecdf(tshf$lt_Hab_lau[, 2])), lty=2, lwd=2,
            ecdf(tshf$lt_Hab_lau[, 2])(knots(ecdf(tshf$lt_Hab_lau[, 2]))))
      lines(knots(ecdf(tshf$wt_Hab_lau[, 2])), lty=3, lwd=2,
            ecdf(tshf$wt_Hab_lau[, 2])(knots(ecdf(tshf$wt_Hab_lau[, 2]))))
      axis(1, pos=0, at=seq(0, 0.5, by=0.1), padj=-0.75)
      axis(2, pos=0, at=seq(0, 1, by=0.2), las=2, hadj=0.6)
      mtext(side=1, "hf", line=1.2)
      mtext(side=2, expression(F[n](hf)), line=1.7)
      lines(x=c(0, 0.3), y=c(1, 1))
      lines(x=c(0.3, 0.3), y=c(1, 0))
      shadowtext(x=0.295, y=0.15, labels="H.lau.\n(J)", adj=1, col="black", bg="white")
    }
    
    # Odont_alb
    {
      plot(knots(ecdf(tshf$cr_Odont_alb[, 2])),
           ecdf(tshf$cr_Odont_alb[, 2])(knots(ecdf(tshf$cr_Odont_alb[, 2]))),
           type="l", xlim=c(0,0.3), ylim=c(0,1), axes=FALSE, xlab="", ylab="",
           col="grey", lwd=2)
      lines(knots(ecdf(tshf$lr_Odont_alb[, 2])), lty=2, lwd=2,
            ecdf(tshf$lr_Odont_alb[, 2])(knots(ecdf(tshf$lr_Odont_alb[, 2]))), col="grey")
      lines(knots(ecdf(tshf$wr_Odont_alb[, 2])), lty=3, lwd=2,
            ecdf(tshf$wr_Odont_alb[, 2])(knots(ecdf(tshf$wr_Odont_alb[, 2]))), col="grey")
      lines(knots(ecdf(tshf$ct_Odont_alb[, 2])), lwd=2,
            ecdf(tshf$ct_Odont_alb[, 2])(knots(ecdf(tshf$ct_Odont_alb[, 2]))))
      lines(knots(ecdf(tshf$lt_Odont_alb[, 2])), lty=2, lwd=2,
            ecdf(tshf$lt_Odont_alb[, 2])(knots(ecdf(tshf$lt_Odont_alb[, 2]))))
      lines(knots(ecdf(tshf$wt_Odont_alb[, 2])), lty=3, lwd=2,
            ecdf(tshf$wt_Odont_alb[, 2])(knots(ecdf(tshf$wt_Odont_alb[, 2]))))
      axis(1, pos=0, at=seq(0, 0.5, by=0.1), padj=-0.75)
      axis(2, pos=0, at=seq(0, 1, by=0.2), las=2, hadj=0.6)
      mtext(side=1, "hf", line=1.2)
      mtext(side=2, expression(F[n](hf)), line=1.7)
      lines(x=c(0, 0.3), y=c(1, 1))
      lines(x=c(0.3, 0.3), y=c(1, 0))
      shadowtext(x=0.295, y=0.15, labels="O.alb.\n(K)", adj=1, col="black", bg="white")
    }
    
    # Potam_lut
    {
      plot(knots(ecdf(tshf$cr_Potam_lut[, 2])),
           ecdf(tshf$cr_Potam_lut[, 2])(knots(ecdf(tshf$cr_Potam_lut[, 2]))),
           type="l", xlim=c(0,0.3), ylim=c(0,1), axes=FALSE, xlab="", ylab="",
           col="grey", lwd=2)
      lines(knots(ecdf(tshf$lr_Potam_lut[, 2])), lty=2, lwd=2,
            ecdf(tshf$lr_Potam_lut[, 2])(knots(ecdf(tshf$lr_Potam_lut[, 2]))), col="grey")
      lines(knots(ecdf(tshf$wr_Potam_lut[, 2])), lty=3, lwd=2,
            ecdf(tshf$wr_Potam_lut[, 2])(knots(ecdf(tshf$wr_Potam_lut[, 2]))), col="grey")
      lines(knots(ecdf(tshf$ct_Potam_lut[, 2])), lwd=2,
            ecdf(tshf$ct_Potam_lut[, 2])(knots(ecdf(tshf$ct_Potam_lut[, 2]))))
      lines(knots(ecdf(tshf$lt_Potam_lut[, 2])), lty=2, lwd=2,
            ecdf(tshf$lt_Potam_lut[, 2])(knots(ecdf(tshf$lt_Potam_lut[, 2]))))
      lines(knots(ecdf(tshf$wt_Potam_lut[, 2])), lty=3, lwd=2,
            ecdf(tshf$wt_Potam_lut[, 2])(knots(ecdf(tshf$wt_Potam_lut[, 2]))))
      axis(1, pos=0, at=seq(0, 0.5, by=0.1), padj=-0.75)
      axis(2, pos=0, at=seq(0, 1, by=0.2), las=2, hadj=0.6)
      mtext(side=1, "hf", line=1.2)
      mtext(side=2, expression(F[n](hf)), line=1.7)
      lines(x=c(0, 0.3), y=c(1, 1))
      lines(x=c(0.3, 0.3), y=c(1, 0))
      shadowtext(x=0.295, y=0.15, labels="P.lut.\n(L)", adj=1, col="black", bg="white")
    }
    
    # Psych_pus
    {
      plot(knots(ecdf(tshf$cr_Psych_pus[, 2])),
           ecdf(tshf$cr_Psych_pus[, 2])(knots(ecdf(tshf$cr_Psych_pus[, 2]))),
           type="l", xlim=c(0,0.3), ylim=c(0,1), axes=FALSE, xlab="", ylab="",
           col="grey", lwd=2)
      lines(knots(ecdf(tshf$lr_Psych_pus[, 2])), lty=2, lwd=2,
            ecdf(tshf$lr_Psych_pus[, 2])(knots(ecdf(tshf$lr_Psych_pus[, 2]))), col="grey")
      lines(knots(ecdf(tshf$wr_Psych_pus[, 2])), lty=3, lwd=2,
            ecdf(tshf$wr_Psych_pus[, 2])(knots(ecdf(tshf$wr_Psych_pus[, 2]))), col="grey")
      lines(knots(ecdf(tshf$ct_Psych_pus[, 2])), lwd=2,
            ecdf(tshf$ct_Psych_pus[, 2])(knots(ecdf(tshf$ct_Psych_pus[, 2]))))
      lines(knots(ecdf(tshf$lt_Psych_pus[, 2])), lty=2, lwd=2,
            ecdf(tshf$lt_Psych_pus[, 2])(knots(ecdf(tshf$lt_Psych_pus[, 2]))))
      lines(knots(ecdf(tshf$wt_Psych_pus[, 2])), lty=3, lwd=2,
            ecdf(tshf$wt_Psych_pus[, 2])(knots(ecdf(tshf$wt_Psych_pus[, 2]))))
      axis(1, pos=0, at=seq(0, 0.5, by=0.1), padj=-0.75)
      axis(2, pos=0, at=seq(0, 1, by=0.2), las=2, hadj=0.6)
      mtext(side=1, "hf", line=1.2)
      mtext(side=2, expression(F[n](hf)), line=1.7)
      lines(x=c(0, 0.3), y=c(1, 1))
      lines(x=c(0.3, 0.3), y=c(1, 0))
      shadowtext(x=0.295, y=0.15, labels="P.pus.\n(M)", adj=1, col="black", bg="white")
    }
  }
}


# figure 13, hsAv trained vs. restored for all taxa
{
  # get and prepare data
  setwd("c:\\presentations_papers\\paper1\\")
  a        <- read.table("hsAv13sp.txt", header=TRUE, sep=";", colClasses=c("character"))
  a$date   <- as.Date(a$date, format="%m/%d/%Y")
  a$ind_m2 <- as.numeric(a$ind_m2)
  a$resVal <- as.numeric(a$resVal)
  a$hsAv   <- as.numeric(a$hsAv)
  a        <- tbl_df(a)

  # plot
  taxa <- as.character(unique(a$taxon))
  taxa <- taxa[order(taxa)]
  
  windows(height=3, width=4)
  par(mar=c(3.5,3.5,1,1))
  dat <- filter(a, taxon==taxa[1] & state=="r")
  plot(rep(0.75, times=nrow(dat)), dat$hsAv, ylim=c(0, 0.3), xlim=c(0.75, 13.25),
       axes=FALSE, xlab="", ylab="")
  dat <- filter(a, taxon==taxa[1] & state=="t")
  points(rep(1.25, times=nrow(dat)), dat$hsAv, pch=16)
  for(i in 2:13)
  {
    dat <- filter(a, taxon==taxa[i] & state=="r")
    points(rep(i-0.25, times=nrow(dat)), dat$hsAv)
    
    dat <- filter(a, taxon==taxa[i] & state=="t")
    points(rep(i+0.25, times=nrow(dat)), dat$hsAv, pch=16)
  }
  abline(v=seq(1.5, 12.5, by=1), col="grey")
  axis(1, at=1:13, labels=c("A. aur.", "A. nerv.", "A. aq.", "B. fus.", "B. luth.", 
                            "C. luct.", "E. ven.", "E. dan.", "G. pil.", "H. lau.", 
                            "O. alb.", "P. lut.", "P. pus."),
       las=2,
       hadj=0.85
  )
  axis(2, at=seq(0, 0.25, by=0.05), las=2, hadj=0.8)
  mtext("hsAv", side=2, line=2.65)
  legend(x=0.25, y=0.325, legend=c("Restored", "Trained"), pch=c(1,16),
         bg="white")
  box()
}


# figure 14, coplot avgN~hsAv|resVal
{
  # compute hsAv up to sampling dates, do not average taxon abundance!
  {
    setwd("c:\\presentations_papers\\paper1\\")
    a      <- read.table("locDens_hsAv_13sp.txt",header=TRUE, sep=";")
    a$date <- as.Date(a$date, format="%Y-%m-%d")
    a      <- a[order(a$resVal),]
    a$tag  <- as.character(a$tag)
    a$hsAv <- NA
    
    # list of output folders containing the time series of hf for each species*site
    spSites <- scan("lista.txt", what=character())
    
    # start of all simulations
    tStart <- as.Date("2003-11-01")
    
    for(i in 1:nrow(a))
    {
      # scan full hf time series
      hf <- scan(paste0("c:\\doktor\\proof_of_concept\\hydromStress\\", 
                        a[i,"tag"], "\\frHab.txt"))
      
      # make time vector up to sampling date of current observation
      tt <- seq(from=tStart, to=a[i, "date"], by="day") 
      
      # cut hf time series
      hf <- hf[1:length(tt)]	 			   
      
      # compute hsAv up to j
      x    <- knots(ecdf(hf))
      Fn   <- ecdf(x)
      y    <- Fn(x)
      xy   <- cbind(x=c(0, x, 0, 0),
                    y=c(0, y, 1, 0))
      
      # store hsAv for the corresponding observation
      a[i,"hsAv"] <- Polygon(xy)@area
    }
    # write.table(a, file="locDens_hsAv_13sp.txt", sep=";", 
    #             row.names=FALSE, quote=FALSE)
  }
  
  # make quantile regressions, tau=0.90
  {
    # get data
    setwd("c:\\presentations_papers\\paper1\\")
    a      <- read.table("locDens_hsAv_13sp.txt", header=TRUE, sep=";")
    a$date <- as.Date(a$date, format="%m/%d/%Y")
    a      <- a[order(a$resVal),]
    
    # define x interval
    xMin  <- 0.000
    xMax  <- 0.250
    
    # make data frame for storing results
    qReg <- data.frame(taxon=NA, 
                       n=NA, 
                       slope=NA, 
                       seslope=NA, 
                       pslope=NA, 
                       intercept=NA, 
                       seintercept=NA, 
                       pintercept=NA)
    
    # loop through all 13 species
    pb <- txtProgressBar(min=0, max=13, initial=1, char = "=", width=30, style=3)
    
    for(i in 1:13)
    {	
      setTxtProgressBar(pb, i)
      
      # grab data	
      dat  <- a[a$resVal==unique(a$resVal)[i] & a$hsAv<=xMax, ]		
      
      # put taxon and n in output data frame
      qReg[i, "taxon"]       <- as.character(dat$taxon[1])
      qReg[i, "n"]           <- nrow(dat)
      
      # make quantile regression
      qR   <- rq(ind_m2 ~ hsAv, data=dat, tau=0.90)
      
      # bootstraping
      summqR         <- summary(qR, se="boot", bsmethod ="xy", R=500)
      
      # store results in output data frame
      qReg[i, "slope"]       <- summqR$coefficients[2, "Value"]
      qReg[i, "seslope"]     <- summqR$coefficients[2, "Std. Error"]
      qReg[i, "pslope"]      <- summqR$coefficients[2, "Pr(>|t|)"]
      qReg[i, "intercept"]   <- summqR$coefficients[1, "Value"]
      qReg[i, "seintercept"] <- summqR$coefficients[1, "Std. Error"]
      qReg[i, "pintercept"]  <- summqR$coefficients[1, "Pr(>|t|)"]
    }; close(pb)
    
    #write.table(qReg, file="qReg.txt", quote=FALSE, row.names=FALSE, sep="\t")
  }
  
  # plot
  {
    # get data
    setwd("c:\\presentations_papers\\paper1\\")
    a       <- read.table("locDens_hsAv_13sp.txt", header=TRUE, sep=";")
    a$date  <- as.Date(a$date, format="%d/%m/%Y")
    a$taxon <- as.character(a$taxon)
    a       <- a[order(a$resVal),]
    qReg2   <- read.table("qReg2.txt", header=TRUE, sep="\t", 
                          colClasses=c("character", 
                                       rep("numeric", times=10)))
    # specifications for hsAv axis
    xMin  <- 0.000
    xMax  <- 0.250
    xBy   <- 0.105
    xSeq  <- seq(xMin, xMax, by=xBy)
    
    # make vector of resVal
    rV    <- unique(a$resVal)
    
    # start plot
    windows(width=8.5, height=4)
    par(mfrow=c(1,13), mar=c(7,0,5,0), oma=c(0,6.5,0,.25))
    
    # loop through all 13 taxa
    for(i in 1:13)
    {
      # plot density~hsAv
      plot(x=a[a$resVal==unique(a$resVal)[i] & a$hsAv<=xMax, "hsAv"],
           y=a[a$resVal==unique(a$resVal)[i] & a$hsAv<=xMax, "ind_m2"],
           pch=20, xlim=c(xMin,xMax), ylim=c(0,400), cex=2, col="black",
           xaxt="n", yaxt="n", xlab="", ylab="", axes=FALSE)
      
      # draw quantile regression line, tau=0.9
      xAxqReg <- seq(xMin, xMax, by=0.001)
      lines(xAxqReg, qReg2[i, "intercept"] + qReg2[i, "slope"]*xAxqReg, lwd=1)
      
      # get plot box dimensions for placing axes and labels
      rng <- par("usr")
      
      # axes
      box()
      if(i%%2 != 0) 
      {
        axis(1, cex.axis=1.75, las=3, at=xSeq, 
             labels=formatC(xSeq, width=5, digits=3, format="f", flag=0))
      } else {
        axis(3, cex.axis=1.75, las=3, at=xSeq, 
             labels=formatC(xSeq, width=5, digits=3, format="f", flag=0))
      }
      if(i==1) {axis(2, cex.axis=1.5, las=1)}
      
      # build short taxa names
      gen       <- strsplit(as.character(unique(a$taxon)[i]), split=" ")[[1]][1]
      sp        <- strsplit(as.character(unique(a$taxon)[i]), split=" ")[[1]][2]
      shortname <- paste(substr(gen, 1, 1), substr(sp, 1,2), sep=".")
      
      # plot short taxa names
      text(x=rng[2]-0.10*(rng[2]-rng[1]),
           y=rng[4]-0.05*(rng[4]-rng[3]),
           labels=shortname, cex=1.5, adj=1)
      
      # plot resVal
      text(x=rng[2]-0.1*(rng[2]-rng[1]),
           y=rng[4]-0.125*(rng[4]-rng[3]),
           labels=formatC(rV[i], width=5, digits=3, format="f"),
           cex=1.5, adj=1)
    }
    mtext(side=1, "hsAv", line=-1.2, cex=1.5, outer=TRUE)
    mtext(side=2, expression(paste("locDens [Ind. ",m^-2,"]")), line=3.75, cex=1.5, outer=TRUE)
  }
}


# figure 15, relationship between intercept from quantile regressions and resVal
{
  # grab and prpare data
  setwd("c:\\presentations_papers\\paper1\\")
  qReg2 <- read.table("qReg2.txt", header=TRUE, sep="\t", 
                      colClasses=c("character", 
                                   rep("numeric", times=10)))

  # make upper and lower error bounds
  uint   <- qReg2$intercept + qReg2$seintercept
  lint   <- qReg2$intercept - qReg2$seintercept
  
  # do 1000 least squares regressions using random draws from intercept +/- se(intercept)
  intrV <- data.frame(slope=rep(NA, 1000), 
                      pslope=rep(NA, 1000), 
                      intercept=rep(NA, 1000))
  for(i in 1:1000)
  {	
    summ               <- summary(lm(runif(13, lint, uint)~qReg2$resVal))
    intrV$slope[i]     <- summ$coefficients[2,1]
    intrV$pslope[i]    <- summ$coefficients[2,4]
    intrV$intercept[i] <- summ$coefficients[1,1]
  }
  
  # compute proportion of significant positive slopes
  nrow(intrV[intrV$pslope<=0.05 & intrV$slope>0,])/1000
  nrow(intrV[intrV$pslope<=0.1 & intrV$slope>0,])/1000
  
  # build uncertinty area for plot
  xx    <- seq(0,1,by=0.01)
  yy    <- numeric(0)
  yyMin <- numeric(0)
  yyMax <- numeric(0)
  for(j in 1:length(xx))
  {
    for(i in 1:1000)
    {
      yy[i] <- intrV$intercept[i] + xx[j]*intrV$slope[i]
    }
    yyMin[j] <- min(yy, na.rm=TRUE)
    yyMax[j] <- max(yy, na.rm=TRUE)
  }	
  
  # plot mean relationship and uncertainty band
  windows(height=2.5, width=3)
  par(mar=c(2.25, 2.8, 0.5, 0.5))
  plot(intercept~resVal, data=qReg2, pch=NA, ylim=c(-100,300), xlim=c(0.45, 0.705),
       xlab="", ylab="", axes=FALSE)
  polygon(x=c(xx, xx[length(xx):1]), 
          y=c(yyMin, yyMax[length(yyMax):1]), 
          col="grey80", border="grey80")
  points(intercept~resVal, data=qReg2, pch=20)
  for(i in 1:13) 
    lines(x=c(qReg2$resVal[i], qReg2$resVal[i]), y=c(uint[i], lint[i]))
  abline(h=0, lty=3)
  abline(lm(intercept~resVal, data=qReg2), lty=2)
  box()
  axis(1, at=seq(0.4, 0.8, by=0.05), padj=-1.25, cex.axis=0.7)
  axis(2, las=2, at=seq(-100, 300, by=50), hadj=0.65, cex.axis=0.7)
  mtext(side=1, "resVal", line=1.25, cex=0.7)
  mtext(side=2, expression(paste("y-Int. [Ind. ",m^-2,"]")), line=1.85, cex=0.7)
  legend(x=0.45, y=300, legend=c("y-Int", "se(y-Int.)", "OLS line", "OLS area"),
         lty=c(NA, 1, 2, NA), pch=c(20, NA, NA, 15), pt.cex=c(1, NA, NA, 2), 
         col=c("black", "black", "black", "grey80"),
         border=c(NA, NA, NA, "grey80"), cex=0.7, adj=0)
}


# look at substrate composition through time
{
  plot(qReg2$resVal, qReg2$slope, ylim=c(-1000, 2000))
  for(i in 1:nrow(qReg2))
    lines(x=c(qReg2$resVal[i], qReg2$resVal[i]),
          y=c(qReg2$slope[i] - qReg2$seslope[i], 
              qReg2$slope[i] + qReg2$seslope[i]))

  
    
  setwd("c:\\presentations_papers\\paper1\\")
  sub <- read.table("substrChange.txt", 
                    header=TRUE, 
                    colClasses=c("factor",
                                 "factor",
                                 "factor",
                                 "factor",
                                 "factor",
                                 "factor",
                                 "factor",
                                 "factor",
                                 "character",
                                 "factor")
  )
  sub$year <- paste0("20", substr(as.character(sub$date), 8, 9))

  with(sub, table(year, Substrat, site, renat))
  
  
  setwd("c:\\presentations_papers\\paper1\\")
  sub2 <- read.table("substrChange2.txt", header=TRUE)
  
  subNames <- c("Ak", "Arg", "CPOM", "FPOM", "LPOTP", 
                ">Macro.", "Meso.", "Micro.", 
                "Pel.", "Psamm.", "subM",
                "Techno.", "Xyl.")
  
  site <- "ct"
  lab  <- "(A) ct"
  plotSub <- function(site, lab)
  {
    dat <- sub2[sub2$site==site,]
    barplot(as.matrix(dat[, 2:14]), beside=TRUE, las=2, ylim=c(0, 100), xaxt="n",
            legend.text=c("2005", "2007", "2009", "2012"),
            args.legend=list(x=65, y=95, bg="white", ncol=1),
            ylab="% coverage", cex.lab=1.125)
    axis(1, at=seq(3, 63, by=5), labels=subNames, las=2, hadj=0.85)
    #abline(v=seq(5.5, 70, by=5), lty=3)
    text(x=5, y=92, label=lab)
    box()
  }
  
  windows(height=4, width=8.5)
  par(mfrow=c(2,3), mar=c(4, 4, 1, 1))
  plotSub(site="cr", lab="(A) cr")
  plotSub(site="wr", lab="(B) wr")
  plotSub(site="lr", lab="(C) lr")
  plotSub(site="ct", lab="(D) ct")
  plotSub(site="wt", lab="(E) wt")
  plotSub(site="lt", lab="(F) lt")
}


# summarize 2dhn quality
{
  setwd("c:\\presentations_papers\\paper1")
  hnqual <- read.table("2dhnQual.txt", header=TRUE, sep="\t")

  xx <- hnqual$qe90
  plot(sort(xx))
  length(xx[xx<=0.10])/length(xx)
  

  par(mfcol=c(1,2))
  hist(hnqual$int, xlim=c(-0.3, 0.3))
  hist(hnqual$sl, xlim=c(0.5, 1.5))
}


# look at bed shear-discharge relation
{
  # grab and format discharge data cr (sarnau & niederwetter)
  {
    setwd("c:\\presentations_papers\\paper1\\")
    Qsn <- tbl_df(read.table("QsarNied.txt", header=TRUE,
                             colClasses=c("character", 
                                          "numeric", 
                                          "numeric")))
    Qsn$date <- as.Date(Qsn$date, format="%d-%b-%y")
  }
  

  # grab tau0 data
  # remove 1st two rows (come from warmup in lisflood simulations)
  {
    setwd("c:/doktor/proof_of_concept/hydromStress/")
    tau0cr <- tbl_df(read.table(paste0(getwd(), "/input_cr/tau.txt")))
    tau0ct <- tbl_df(read.table(paste0(getwd(), "/input_ct/tau.txt")))
    tau0cr <- tau0cr[3:nrow(tau0cr), ]
    tau0ct <- tau0ct[3:nrow(tau0ct), ]
    
  }
  
  
  # grab depth-averaged velocities and estimated v at 0.25*yr (yr = height of)
  # roughness sublayer
  # remove 1st two rows (come from warmup in lisflood simulations)
  {
    setwd("c:/doktor/proof_of_concept/hydromStress/")
    vMagcr   <- tbl_df(read.table(paste0(getwd(), "/input_cr/vMag.txt")))
    vMagct   <- tbl_df(read.table(paste0(getwd(), "/input_ct/vMag.txt")))
    v025yrcr <- tbl_df(read.table(paste0(getwd(), "/input_cr/v025yr.txt")))
    v025yrct <- tbl_df(read.table(paste0(getwd(), "/input_ct/v025yr.txt")))
    
    vMagcr   <- vMagcr[3:nrow(vMagcr), ]
    vMagct   <- vMagct[3:nrow(vMagct), ]
    v025yrcr <- v025yrcr[3:nrow(v025yrcr), ]
    v025yrct <- v025yrct[3:nrow(v025yrct), ]
  }
 
  
  # % area with tau0 below maximal MZB tau0crit (1.35 N/m2) 
  {
    perclowtau0 <- numeric()
    for(i in 1:nrow(tau0cr))
    {
      tau0cr_t       <- unlist(slice(tau0cr, i))
      perclowtau0[i] <- length(tau0cr_t[tau0cr_t <= 1.35])/100
    }
    
    plot(Qsn$Qsarnau + Qsn$Qniederwetter, perclowtau0, 
         ylim=c(0,1), xlim=c(0,10))
    abline(v=4.34)
    abline(v=5.03)
    abline(h=0.30)
    
    
  }

  # make tau and v statistics in reach  
  {
    tau0cr_q30 <- apply(X=tau0cr, MARGIN=1, FUN=quantile, probs=0.3)
    tau0ct_q30 <- apply(X=tau0ct, MARGIN=1, FUN=quantile, probs=0.3)
  }


  par(mfcol=c(1,2), mar=c(3,5,1,1))
  plot(Qsn$Qsarnau + Qsn$Qniederwetter, tau0cr_q30, 
       ylim=c(0, 10), xlim=c(0, 6),
       pch=20)
  abline(v=4.34)
  abline(h=1.35)
  plot(Qsn$Qsarnau + Qsn$Qniederwetter, tau0ct_q30, 
       ylim=c(0, 10), xlim=c(0, 6))
  abline(v=5.03)
  abline(h=1.35)
  
}


# look at variability in hf in trained vs. restored across taxa
{
  # prepare data
  {
    setwd("c:\\presentations_papers\\paper1\\")
    a      <- read.table("hsAv13sp.txt", header=TRUE, sep=";")
    a$date <- as.Date(a$date, format="%m/%d/%Y")
    a      <- a[order(a$resVal),]
    
    setwd("c:\\doktor\\proof_of_concept\\hydromStress\\")
    lf <- list.files(getwd())
    lf <- lf[which(regexpr("out", lf)!=-1)]
    
    taxonlist <- c("Odont_alb",
                   "All_auri",
                   "Ana_nerv", 
                   "Ephem_dan",
                   "Potam_lut",
                   "Hab_lau",    
                   "Ecdy_ven",
                   "Goe_pil",
                   "Caen_luct",
                   "Psych_pus",
                   "Bae_luth",
                   "Bae_fus",
                   "Ase_aqua")
    
    lfuse <- character()
    for(i in lf)
    {
      lfname  <- substr(i, 8, nchar(i))
      lfmatch <- match(taxonlist, lfname)
      if(sum(lfmatch[!is.na(lfmatch)], na.rm=TRUE))
      {
        lfuse <- c(lfuse, i)
      }
    }
    
    tshf <- list()
    count <- 0
    for(i in lfuse)
    {
      count         <- count + 1
      file          <- paste0(getwd(), "/", i, "/frHab.txt")
      hf            <- read.table(file, header=FALSE)
      names(hf)     <- "hf"
      hf2           <- data.frame(tt=seq(as.Date("2003-11-01"), 
                                         length=nrow(hf), by="day"), hf)
      tshf[[count]]        <- hf2
      names(tshf)[[count]] <- substr(i, 5, nchar(i))
    }
    
    # discharge data
    setwd("c:\\doktor\\proof_of_concept\\")
    bs             <- read.table("corrBiedSarWithOutliers.txt",header=TRUE)
    bsNoOutl       <- read.table("corrBiedSarWithoutOutliers.txt",header=TRUE)
    outliers       <- read.table("corrBiedSarOutliers.txt",header=TRUE)
    niedNoOut      <- read.table("niederwetter.txt",header=TRUE)
    bsNoOutl[[1]]  <- as.Date(bsNoOutl[[1]])
    niedNoOut[[1]] <- as.Date(niedNoOut[[1]])
    bsNoOutl       <- bsNoOutl[order(bsNoOutl[[1]]),]
    niedNoOut      <- niedNoOut[order(niedNoOut[[1]]),]
    
    indices        <- match(bsNoOutl$Datum, niedNoOut$time)
    nied           <- niedNoOut[indices, ]
    biedsa         <- bsNoOutl
    
    Qtable <- tbl_df(data.frame(dateTime=nied$time,
                                Qnied=nied$Q_m3s,
                                Qbied=biedsa$biedenkopf,
                                Qsar=biedsa$sarnau))
    
    hftable <- data.frame(dateTime=Qtable$dateTime)
    for(i in 1:length(tshf))
    {
      indices <- match(Qtable$dateTime, tshf[[i]]$tt)
      hftable <- cbind(hftable, tshf[[i]][indices, "hf" ])
    }
    names(hftable)[2:79] <- names(tshf)
  }

  # compute sd of hf time series in trained and restored sites
  {
    hftable <- tbl_df(hftable) 
    
    hfcr    <- dplyr::select(hftable, contains("cr_"))
    hfwr    <- dplyr::select(hftable, contains("wr_"))
    hflr    <- dplyr::select(hftable, contains("lr_"))
    hfrest  <- tbl_df(cbind(hfcr, hfwr, hflr))
    
    hfct    <- dplyr::select(hftable, contains("ct_"))
    hfwt    <- dplyr::select(hftable, contains("wt_"))
    hflt    <- dplyr::select(hftable, contains("lt_"))
    hftrain <- tbl_df(cbind(hfct, hfwt, hflt))
    
    sdrest  <- apply(as.matrix(hfrest), 2, sd, na.rm=TRUE)
    sdtrain <- apply(as.matrix(hftrain), 2, sd, na.rm=TRUE)
    
    meanrest  <- apply(as.matrix(hfrest), 2, mean, na.rm=TRUE)
    meantrain <- apply(as.matrix(hftrain), 2, mean, na.rm=TRUE)
    
    sdhf <- tbl_df(data.frame(sd=c(sdrest, sdtrain), 
                              state=c(rep("rest", times=length(sdrest)),
                                      rep("train", times=length(sdtrain)))))
    
    meanhf <- tbl_df(data.frame(mean=c(meanrest, meantrain), 
                              state=c(rep("rest", times=length(meanrest)),
                                      rep("train", times=length(meantrain)))))
    
    
    par(mfcol=c(1,2))
    boxplot(mean~state, data=meanhf, yaxt="n", ylab="mean(hf)")
    axis(2, las=2)
    boxplot(sd~state, data=sdhf, yaxt="n", ylab="sd(hf)")
    axis(2, las=2)
  
    tapply(meanhf$mean, meanhf$state, mean)
    tapply(sdhf$sd, sdhf$state, mean)
  }
}


# how much do hf time series for different taxa correlate?
{
  # prepare data
  {
    # discharge data
    setwd("c:\\doktor\\proof_of_concept\\")
    bs             <- read.table("corrBiedSarWithOutliers.txt",header=TRUE)
    bsNoOutl       <- read.table("corrBiedSarWithoutOutliers.txt",header=TRUE)
    outliers       <- read.table("corrBiedSarOutliers.txt",header=TRUE)
    niedNoOut      <- read.table("niederwetter.txt",header=TRUE)
    bsNoOutl[[1]]  <- as.Date(bsNoOutl[[1]])
    niedNoOut[[1]] <- as.Date(niedNoOut[[1]])
    bsNoOutl       <- bsNoOutl[order(bsNoOutl[[1]]),]
    niedNoOut      <- niedNoOut[order(niedNoOut[[1]]),]
    
    indices        <- match(bsNoOutl$Datum, niedNoOut$time)
    nied           <- niedNoOut[indices, ]
    biedsa         <- bsNoOutl
    
    Qtable <- tbl_df(data.frame(dateTime=nied$time,
                                Qnied=nied$Q_m3s,
                                Qbied=biedsa$biedenkopf,
                                Qsar=biedsa$sarnau))
    
    setwd("c:\\presentations_papers\\paper1\\")
    a      <- read.table("hsAv13sp.txt", header=TRUE, sep=";")
    a$date <- as.Date(a$date, format="%m/%d/%Y")
    a      <- a[order(a$resVal),]
    
    setwd("c:\\doktor\\proof_of_concept\\hydromStress\\")
    lf <- list.files(getwd())
    lf <- lf[which(regexpr("out", lf)!=-1)]
    
    taxonlist <- c("Odont_alb",
                   "All_auri",
                   "Ana_nerv", 
                   "Ephem_dan",
                   "Potam_lut",
                   "Hab_lau",    
                   "Ecdy_ven",
                   "Goe_pil",
                   "Caen_luct",
                   "Psych_pus",
                   "Bae_luth",
                   "Bae_fus",
                   "Ase_aqua")
    
    lfuse <- character()
    for(i in lf)
    {
      lfname  <- substr(i, 8, nchar(i))
      lfmatch <- match(taxonlist, lfname)
      if(sum(lfmatch[!is.na(lfmatch)], na.rm=TRUE))
      {
        lfuse <- c(lfuse, i)
      }
    }
    
    tshf <- list()
    count <- 0
    for(i in lfuse)
    {
      count         <- count + 1
      file          <- paste0(getwd(), "/", i, "/frHab.txt")
      hf            <- read.table(file, header=FALSE)
      names(hf)     <- "hf"
      hf2           <- data.frame(tt=seq(as.Date("2003-11-01"), 
                                         length=nrow(hf), by="day"), hf)
      tshf[[count]]        <- hf2
      names(tshf)[[count]] <- substr(i, 5, nchar(i))
      
      hftable <- data.frame(dateTime=Qtable$dateTime)
    }
    
    
    for(i in 1:length(tshf))
    {
      indices <- match(Qtable$dateTime, tshf[[i]]$tt)
      hftable <- cbind(hftable, tshf[[i]][indices, "hf" ])
    }
    names(hftable)[2:79] <- names(tshf)
  }
  
  # make correlations
  {
    hftable2 <- hftable[complete.cases(hftable), 2:ncol(hftable)]
    
    # separate trained and restored!
    hftable_ct <- dplyr::select(hftable2, contains("ct_"))
    hftable_wt <- dplyr::select(hftable2, contains("wt_"))
    hftable_lt <- dplyr::select(hftable2, contains("lt_"))
    hftable_t  <- tbl_df(cbind(hftable_ct, hftable_wt, hftable_lt))
    
    hftable_cr <- dplyr::select(hftable2, contains("cr_"))
    hftable_wr <- dplyr::select(hftable2, contains("wr_"))
    hftable_lr <- dplyr::select(hftable2, contains("lr_"))
    hftable_r  <- tbl_df(cbind(hftable_cr, hftable_wr, hftable_lr))
    
    # compute correlation coefficients
    corMat_t   <- cor(hftable_t)
    corMat_r   <- cor(hftable_r)
    
    corMatDF_t <- data.frame(row=rownames(corMat_t)[row(corMat_t)[upper.tri(corMat_t)]], 
                             col=colnames(corMat_t)[col(corMat_t)[upper.tri(corMat_t)]], 
                             corr=corMat_t[upper.tri(corMat_t)])
  
    corMatDF_r <- data.frame(row=rownames(corMat_r)[row(corMat_r)[upper.tri(corMat_r)]], 
                             col=colnames(corMat_r)[col(corMat_r)[upper.tri(corMat_r)]], 
                             corr=corMat_r[upper.tri(corMat_r)])
    
     # write.table(corMatDF_t,
     #             file="c:\\presentations_papers\\paper1\\corTableHF_t.txt",
     #             row.names = FALSE,
     #             quote = FALSE,
     #             sep="\t")
     # write.table(corMatDF_r,
     #             file="c:\\presentations_papers\\paper1\\corTableHF_r.txt",
     #             row.names = FALSE,
     #             quote = FALSE,
     #             sep="\t")

    
    par(mfcol=c(1,2))
    hist(corMatDF_t$corr, xlim=c(-1,1))
    hist(corMatDF_r$corr, xlim=c(-1,1))
    
    # mutually exclusive -> r < -0.5
    # unrelated          -> -0.5 < r < 0.5
    # co-existing        -> r > 0.5
    
    coexR  <- nrow(corMatDF_r[corMatDF_r$corr > 0.5, ])/nrow(corMatDF_r)
    mutexR <- nrow(corMatDF_r[corMatDF_r$corr < -0.5, ])/nrow(corMatDF_r)
    unrelR <- nrow(corMatDF_r[corMatDF_r$corr > -0.5 &
                                corMatDF_r$corr < 0.5, ])/nrow(corMatDF_r)

    coexT  <- nrow(corMatDF_t[corMatDF_t$corr > 0.5, ])/nrow(corMatDF_t)
    mutexT <- nrow(corMatDF_t[corMatDF_t$corr < -0.5, ])/nrow(corMatDF_t)
    unrelT <- nrow(corMatDF_t[corMatDF_t$corr > -0.5 &
                                corMatDF_t$corr < 0.5, ])/nrow(corMatDF_t)
    
    habAs <- data.frame(restored=c(coexR,
                                   mutexR,
                                   unrelR),
                        trained=c(coexT,
                                  mutexT,
                                  unrelT))
    rownames(habAs) <- c("coex", "mutex", "unrel")
  }
}


# analyze froude numbers
{
  Frlr <- read.table("c:\\presentations_papers\\paper1\\Frgr05lr.txt")[[1]]
  Frlt <- read.table("c:\\presentations_papers\\paper1\\Frgr05lt.txt")[[1]]
  Frwt <- read.table("c:\\presentations_papers\\paper1\\Frgr05wt.txt")[[1]]
  Frwr <- read.table("c:\\presentations_papers\\paper1\\Frgr05wr.txt")[[1]]
  Frct <- read.table("c:\\presentations_papers\\paper1\\Frgr05ct.txt")[[1]]
  Frcr <- read.table("c:\\presentations_papers\\paper1\\Frgr05cr.txt")[[1]]
  
  plot(Frlr, type="l" , ylim=c(0,20))
  lines(Frlt)
  lines(Frct)
  lines(Frcr)
  lines(Frwr)
  lines(Frwt)
}


# calibration Froude numbers
{
  # read lisflood-acc, cr
  {
    setwd("c:\\doktor\\proof_of_concept\\mod_cr\\")
    dem   <- raster("dem1.asc")
    
    setwd("c:\\doktor\\proof_of_concept\\modCal\\")
    wdAccr <- raster("cr.wd")
    vxAcc  <- raster("cr.Vx")
    vyAcc  <- raster("cr.Vy")
    
    # compute v = sqrt(vx^2 + vy^2)
    vxAccM <- as.matrix(vxAcc)
    vyAccM <- as.matrix(vyAcc)
    
    vxAccMM <- matrix(nrow=nrow(dem),ncol=0)
    for (j in 1:ncol(dem))
    {
      vxAccMM<-cbind(vxAccMM,0.5*(vxAccM[,j]+vxAccM[,j+1]))
    }
    
    vyAccMM<-matrix(nrow=0, ncol=ncol(dem))
    for (i in 1:nrow(dem))
    {
      vyAccMM<-rbind(vyAccMM,0.5*(vyAccM[i,]+vyAccM[i+1,]))
    }
    
    vAccr <- raster(sqrt(vxAccMM^2 + vyAccMM^2), template = dem)
  }
  
  # read lisflood-acc, ct
  {
    setwd("c:\\doktor\\proof_of_concept\\mod_ct\\")
    dem   <- raster("dem1.asc")
    
    setwd("c:\\doktor\\proof_of_concept\\modCal\\")
    wdAcct <- raster("ct.wd")
    vxAcc  <- raster("ct.Vx")
    vyAcc  <- raster("ct.Vy")
    
    # compute v = sqrt(vx^2 + vy^2)
    vxAccM <- as.matrix(vxAcc)
    vyAccM <- as.matrix(vyAcc)
    
    vxAccMM <- matrix(nrow=nrow(dem),ncol=0)
    for (j in 1:ncol(dem))
    {
      vxAccMM<-cbind(vxAccMM,0.5*(vxAccM[,j]+vxAccM[,j+1]))
    }
    
    vyAccMM<-matrix(nrow=0, ncol=ncol(dem))
    for (i in 1:nrow(dem))
    {
      vyAccMM<-rbind(vyAccMM,0.5*(vyAccM[i,]+vyAccM[i+1,]))
    }
    
    vAcct <- raster(sqrt(vxAccMM^2 + vyAccMM^2), template = dem)
  }
  
  # make histograms
  {
    FrAccr <- vAccr/sqrt(9.81*wdAccr)
    FrAcct <- vAcct/sqrt(9.81*wdAcct)
    FrAccr <- FrAccr[][!is.na(FrAccr[])]
    FrAcct <- FrAcct[][!is.na(FrAcct[])]
    
    length(FrAccr[FrAccr>0.4])/length(FrAccr)
    length(FrAcct[FrAcct>0.4])/length(FrAcct)
    
    par(mfcol=c(1,2))
    plot(ecdf(FrAccr), xlim=c(0, 1), main="cr")
    plot(ecdf(FrAcct), xlim=c(0, 1), main="ct")
    
    

    
    plot(ecdf())
    cbind(histr$counts, histr$mids)
    
    
  }
}

# dems and points
{
  dem    <- raster("c:\\doktor\\proof_of_concept\\mod_cr\\dem1.asc")
  sp     <- read.table("c:\\doktor\\proof_of_concept\\modCal\\crSP.txt", header=T,sep="\t")
  
  
  contour(dem, asp=1)
  
  # cr
  {
    # grab dem and calibration points
    dem    <- raster("c:\\doktor\\proof_of_concept\\mod_cr\\dem1.asc")
    sp     <- read.table("c:\\doktor\\proof_of_concept\\modCal\\crSP.txt", header=T,sep="\t")
    
    # make matrices for persp
    demMat   <- as.matrix(dem)
    x        <- seq(0, dim(dem)[3]*nrow(demMat), length=nrow(demMat))
    y        <- seq(0, dim(dem)[3]*ncol(demMat), length=ncol(demMat))
    nrz      <- nrow(demMat)
    ncz      <- ncol(demMat)
    
    # plot dem and calibration points
    expand   <- 0.075
    phi	     <- 40
    theta    <- 0
    zlim     <- c(-2,2)
    xlim     <- c(260, 372)
    zlim     <- c(-2, 2)
    
    pmat <- persp(dem, col="grey90", theta=theta, phi=phi, shade=.5, ticktype="detailed", zlim=zlim, expand=expand, border=NA,
                  axes=TRUE, box=FALSE, xlim=xlim)
    pts  <- trans3d(x=sp[[1]],y=sp[[2]],z=sp[[3]],pmat=pmat)
    points(pts,pch=20,cex=0.5)
    
    # define x,y and z axes
    min.x  <- extent(dem)@xmin
    max.x  <- extent(dem)@xmax + 10
    x.axis <- seq(min.x, max.x, by=10)
    
    min.y  <- extent(dem)@ymin
    max.y  <- extent(dem)@ymax
    y.axis <- seq(min.y, max.y, by=20)
    
    min.z  <- zlim[1]
    max.z  <- zlim[2]
    z.axis <- seq(min.z, max.z, by=4)
    
    # plot axis lines
    lines(trans3d(x.axis, min.y, min.z, pmat))
    lines(trans3d(min.x, y.axis, max.z, pmat))
    lines(trans3d(min.x, min.y, z.axis, pmat))
    
    # define and plot tick marks
    x.tick     <- seq(290, 370, by=20)
    tick.start <- trans3d(x.tick, min.y, min.z, pmat)
    tick.end   <- trans3d(x.tick, (min.y - 5), min.z, pmat)
    segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
    
    y.tick     <- seq(680, 900, by=40)
    tick.start <- trans3d(min.x, y.tick, max.z, pmat)
    tick.end   <- trans3d(min.x - 2, y.tick, max.z, pmat)
    segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
    
    z.tick     <- seq(-2, 2, by=4)
    tick.start <- trans3d(min.x, min.y, z.axis, pmat)
    tick.end   <- trans3d(min.x, (min.y - 2), z.axis, pmat)
    segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
    
    # define and plot tick mark labels
    labels    <- x.tick
    label.pos <- trans3d(x.tick, min.y, min.z-5, pmat)
    text(label.pos$x, label.pos$y, labels=labels, adj=0.5, cex=1)
    
    labels    <- y.tick
    label.pos <- trans3d((min.x - 12), y.tick, max.z, pmat)
    text(label.pos$x, label.pos$y, labels=labels, adj=0, cex=1)
    
    labels    <- z.tick
    label.pos <- trans3d(min.x-4, min.y-5, z.tick, pmat)
    text(label.pos$x, label.pos$y, labels=labels, adj=0, cex=1)
    
    # define and plot axis labels
    x.lab     <- "x [m]"
    label.pos <- trans3d(mean(x.axis), min.y-30, min.z, pmat)
    text(label.pos$x, label.pos$y, labels=x.lab, adj=1, cex=1)
    
    y.lab     <- "y [m]"
    label.pos <- trans3d(min.x-20, mean(y.axis), min.z, pmat)
    text(label.pos$x, label.pos$y, labels=y.lab, adj=1, cex=1)
    
    z.lab     <- "z [m]"
    label.pos <- trans3d(min.x-10, min.y-5, mean(z.axis), pmat)
    text(label.pos$x, label.pos$y, labels=z.lab, adj=1, cex=1)
    
    # label
    lab       <- "(A) cr"
    lab.pos <- trans3d(min.x, max.y, max.z+10, pmat)
    text(lab.pos$x, lab.pos$y, labels=lab, adj=1, cex=1.25)
  }
  
  # ct
  {
    dem <- raster(readAsciiGrid("c:\\doktor\\proof_of_concept\\mod_ct\\dem1.asc"))
    sp  <- read.table("c:\\doktor\\proof_of_concept\\modCal\\ctPts100.txt", header=TRUE, sep="\t")
    
    demMat   <- as.matrix(dem)
    x        <- seq(0, dim(dem)[3]*nrow(demMat), length=nrow(demMat))
    y        <- seq(0, dim(dem)[3]*ncol(demMat), length=ncol(demMat))
    nrz      <- nrow(demMat)
    ncz      <- ncol(demMat)
    
    expand <- 2.5
    phi	   <- 30
    theta  <- 90
    zlim   <- c(-1.5,2.5)
    pmat <- persp(dem, col="grey90", theta=theta, phi=phi, shade=.5, ticktype="detailed", zlim=zlim, expand=expand, border=NA,
                  axes=FALSE, box=FALSE, ylim=c(70,235), scale=FALSE)
    pts  <- trans3d(x=sp[[1]],y=sp[[2]],z=sp[[3]],pmat=pmat)
    points(pts,pch=20,cex=0.5)
    
    # define x,y and z axes
    x.axis <- seq(extent(dem)@xmin, extent(dem)@xmax, by=1)
    min.x  <- extent(dem)@xmin
    max.x  <- extent(dem)@xmax
    y.axis <- seq(extent(dem)@ymin, extent(dem)@ymax, by=1)
    min.y  <- extent(dem)@ymin
    max.y  <- extent(dem)@ymax
    z.axis <- seq(-1.5, 2.5, by=4)
    min.z  <- -1.5
    max.z  <- 2.5
    
    # plot axis lines
    lines(trans3d(x.axis, min.y, max.z, pmat))
    lines(trans3d(max.x, y.axis, min.z, pmat))
    lines(trans3d(max.x, min.y, z.axis, pmat))
    
    # define and plot tick marks
    x.tick     <- seq(940, 1110, by=40)
    tick.start <- trans3d(x.tick, min.y, max.z, pmat)
    tick.end   <- trans3d(x.tick, (min.y - 3), max.z, pmat)
    segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
    
    y.tick     <- seq(100, 230, by=20)
    tick.start <- trans3d(max.x, y.tick, min.z, pmat)
    tick.end   <- trans3d(max.x+5, y.tick, min.z, pmat)
    segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
    
    z.tick     <- seq(-1, 2, by=3)
    tick.start <- trans3d(max.x, min.y, z.axis, pmat)
    tick.end   <- trans3d(max.x, (min.y - 2), z.axis, pmat)
    segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
    
    # define and plot tick mark labels
    labels    <- x.tick
    label.pos <- trans3d(x.tick, min.y-5, max.z, pmat)
    text(label.pos$x, label.pos$y, labels=labels, adj=1, cex=1)
    
    labels    <- y.tick
    label.pos <- trans3d(max.x+10, y.tick, min.z, pmat)
    text(label.pos$x, label.pos$y, labels=labels, adj=0.5, cex=1)
    
    labels    <- z.tick
    label.pos <- trans3d(max.x, min.y-3, z.tick, pmat)
    text(label.pos$x, label.pos$y, labels=labels, adj=1, cex=1)
    
    # define and plot axis labels
    x.lab     <- "x [m]"
    label.pos <- trans3d(mean(x.axis), min.y - 30, mean(z.axis), pmat)
    text(label.pos$x, label.pos$y, labels=x.lab, adj=1, cex=1)
    
    y.lab     <- "y [m]"
    label.pos <- trans3d(max.x+25, mean(y.axis), min.z, pmat)
    text(label.pos$x, label.pos$y, labels=y.lab, adj=1, cex=1)
    
    # label
    lab       <- "(B) ct"
    lab.pos <- trans3d(min.x, min.y, max.z+10, pmat)
    text(lab.pos$x, lab.pos$y, labels=lab, adj=1, cex=1.25)
  }
  
  # lr
  {
    dem <- raster(readAsciiGrid("c:\\doktor\\proof_of_concept\\mod_lr\\dem1.asc"))
    sp  <- read.table("c:\\doktor\\proof_of_concept\\modCal\\lrsP.txt", header=TRUE, sep="\t")
    
    dem <- aggregate(dem, fact=3)
    
    demMat   <- as.matrix(dem)
    x        <- seq(0, dim(dem)[3]*nrow(demMat), length=nrow(demMat))
    y        <- seq(0, dim(dem)[3]*ncol(demMat), length=ncol(demMat))
    nrz      <- nrow(demMat)
    ncz      <- ncol(demMat)
    demFacet <- (demMat[-1, -1] + demMat[-1, -ncz] + demMat[-nrz, -1] + demMat[-nrz, -ncz])/4
    
    expand <- 0.075
    phi	   <- 40
    theta  <- 90
    zlim   <- c(-1.5,2.0)
    pmat <- persp(dem, col="grey90", theta=theta, phi=phi, shade=.5, ticktype="detailed", zlim=zlim, expand=expand, border=NA,
                  axes=FALSE, box=FALSE, ylim=c(220,400))
    pts  <- trans3d(x=sp[[1]],y=sp[[2]],z=sp[[3]],pmat=pmat)
    points(pts,pch=20,cex=0.5)
    
    # define x,y and z axes
    x.axis <- seq(extent(dem)@xmin, extent(dem)@xmax, by=1)
    min.x  <- extent(dem)@xmin
    max.x  <- extent(dem)@xmax
    y.axis <- seq(extent(dem)@ymin, extent(dem)@ymax, by=1)
    min.y  <- extent(dem)@ymin
    max.y  <- extent(dem)@ymax
    z.axis <- seq(-1.5, 2.0, by=0.1)
    min.z  <- -1.5
    max.z  <- 2.0
    
    # plot axis lines
    lines(trans3d(x.axis, min.y, max.z, pmat))
    lines(trans3d(max.x, y.axis, min.z, pmat))
    lines(trans3d(max.x, min.y, z.axis, pmat))
    
    # define and plot tick marks
    x.tick     <- seq(590, 890, by=40)
    tick.start <- trans3d(x.tick, min.y, max.z, pmat)
    tick.end   <- trans3d(x.tick, (min.y - 3), max.z, pmat)
    segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
    
    y.tick     <- seq(260, 380, by=20)
    tick.start <- trans3d(max.x, y.tick, min.z, pmat)
    tick.end   <- trans3d(max.x+5, y.tick, min.z, pmat)
    segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
    
    z.tick     <- seq(-1.5, 2, by=3)
    tick.start <- trans3d(max.x, min.y, z.tick, pmat)
    tick.end   <- trans3d(max.x, min.y-2, z.tick, pmat)
    segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
    
    # define and plot tick mark labels
    labels    <- x.tick
    label.pos <- trans3d(x.tick, min.y-5, max.z, pmat)
    text(label.pos$x, label.pos$y, labels=labels, adj=1, cex=1)
    
    labels    <- y.tick
    label.pos <- trans3d(max.x+12, y.tick, min.z, pmat)
    text(label.pos$x, label.pos$y, labels=labels, adj=0.5, cex=1)
    
    labels    <- z.tick
    label.pos <- trans3d(max.x, min.y-3, z.tick, pmat)
    text(label.pos$x, label.pos$y, labels=labels, adj=1, cex=1)
    
    # define and plot axis labels
    x.lab     <- "x [m]"
    label.pos <- trans3d(mean(x.axis), min.y - 30, mean(z.axis), pmat)
    text(label.pos$x, label.pos$y, labels=x.lab, adj=1, cex=1)
    
    y.lab     <- "y [m]"
    label.pos <- trans3d(max.x+30, mean(y.axis), min.z, pmat)
    text(label.pos$x, label.pos$y, labels=y.lab, adj=1, cex=1)
    
    # label
    lab       <- "(C) lr"
    lab.pos <- trans3d(min.x, min.y, max.z+10, pmat)
    text(lab.pos$x, lab.pos$y, labels=lab, adj=1, cex=1.25)			
  }
  
  # lt
  {
    dem <- raster(readAsciiGrid("c:\\doktor\\proof_of_concept\\mod_lt\\dem1.asc"))
    sp  <- read.table("c:\\doktor\\proof_of_concept\\modCal\\ltsP100.txt", header=TRUE, sep="\t")
    
    demMat   <- as.matrix(dem)
    x        <- seq(0, dim(dem)[3]*nrow(demMat), length=nrow(demMat))
    y        <- seq(0, dim(dem)[3]*ncol(demMat), length=ncol(demMat))
    nrz      <- nrow(demMat)
    ncz      <- ncol(demMat)
    
    expand <- 1.5
    phi	   <- 20
    theta  <- 70
    zlim   <- c(-1,2)
    pmat <- persp(dem, col="grey90", theta=theta, phi=phi, shade=.5, ticktype="detailed", zlim=zlim, expand=expand, border=NA,
                  axes=FALSE, box=FALSE, ylim=c(6510, 6560), xlim=c(-19060, -18800), scale=FALSE)
    pts  <- trans3d(x=sp[[1]],y=sp[[2]],z=sp[[3]],pmat=pmat)
    points(pts,pch=20,cex=0.5)
    
    # define x,y and z axes
    x.axis <- seq(extent(dem)@xmin, extent(dem)@xmax, by=1)
    min.x  <- extent(dem)@xmin
    max.x  <- extent(dem)@xmax
    y.axis <- seq(extent(dem)@ymin, extent(dem)@ymax, by=1)
    min.y  <- extent(dem)@ymin
    max.y  <- extent(dem)@ymax
    z.axis <- seq(-1.5, 2.0, by=0.1)
    min.z  <- -1.5
    max.z  <- 2
    
    # plot axis lines
    lines(trans3d(x.axis, min.y, max.z, pmat))
    lines(trans3d(max.x, y.axis, min.z, pmat))
    lines(trans3d(max.x, min.y, z.axis, pmat))
    
    # define and plot tick marks
    x.tick     <- seq(-19050, -18850, by=50)
    tick.start <- trans3d(x.tick, min.y, max.z, pmat)
    tick.end   <- trans3d(x.tick, (min.y - 3), max.z, pmat)
    segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
    
    y.tick     <- seq(6530, 6550, by=10)
    tick.start <- trans3d(max.x, y.tick, min.z, pmat)
    tick.end   <- trans3d(max.x+5, y.tick, min.z, pmat)
    segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
    
    z.tick     <- seq(-1.5, 2, by=3)
    tick.start <- trans3d(max.x, min.y, z.tick, pmat)
    tick.end   <- trans3d(max.x, min.y-2, z.tick, pmat)
    segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
    
    # define and plot tick mark labels
    labels    <- x.tick
    label.pos <- trans3d(x.tick, min.y-4, max.z, pmat)
    text(label.pos$x, label.pos$y, labels=labels, adj=1, cex=1)
    
    labels    <- y.tick
    label.pos <- trans3d(max.x+12, y.tick, min.z, pmat)
    text(label.pos$x, label.pos$y, labels=labels, adj=0.5, cex=1)
    
    labels    <- z.tick
    label.pos <- trans3d(max.x, min.y-3, z.tick, pmat)
    text(label.pos$x, label.pos$y, labels=labels, adj=1, cex=1)
    
    # define and plot axis labels
    x.lab     <- "x [m]"
    label.pos <- trans3d(mean(x.axis), min.y - 25, mean(z.axis), pmat)
    text(label.pos$x, label.pos$y, labels=x.lab, adj=1, cex=1.25)
    
    y.lab     <- "y [m]"
    label.pos <- trans3d(max.x+25, mean(y.axis), min.z-3, pmat)
    text(label.pos$x, label.pos$y, labels=y.lab, adj=1, cex=1)
    
    #z.lab     <- "z [m]"
    #label.pos <- trans3d(max.x, min.y-12, mean(z.axis), pmat)
    #text(label.pos$x, label.pos$y, labels=z.lab, adj=1, cex=1)
    
    # label
    lab       <- "(D) lt"
    lab.pos <- trans3d(min.x, min.y, max.z+10, pmat)
    text(lab.pos$x, lab.pos$y, labels=lab, adj=1, cex=1.5)
  }
  
  # wr
  {
    dem <- raster(readAsciiGrid("c:\\doktor\\proof_of_concept\\mod_wr\\dem1.asc"))
    sp  <- read.table("c:\\doktor\\proof_of_concept\\modCal\\wrsP.txt", header=TRUE, sep="\t")
    
    demMat   <- as.matrix(dem)
    x        <- seq(0, dim(dem)[3]*nrow(demMat), length=nrow(demMat))
    y        <- seq(0, dim(dem)[3]*ncol(demMat), length=ncol(demMat))
    nrz      <- nrow(demMat)
    ncz      <- ncol(demMat)
    
    expand <- 0.075
    phi	   <- 40
    theta  <- 90
    zlim   <- c(-1,2)
    pmat <- persp(dem, col="grey90", theta=theta, phi=phi, shade=.5, ticktype="detailed", zlim=zlim, expand=expand, border=NA,
                  axes=FALSE, box=FALSE, ylim=c(400, 580))
    pts  <- trans3d(x=sp[[1]],y=sp[[2]],z=sp[[3]],pmat=pmat)
    points(pts,pch=20,cex=0.5)
    
    # define x,y and z axes
    x.axis <- seq(extent(dem)@xmin, extent(dem)@xmax, by=1)
    min.x  <- extent(dem)@xmin
    max.x  <- extent(dem)@xmax
    y.axis <- seq(extent(dem)@ymin, extent(dem)@ymax, by=1)
    min.y  <- extent(dem)@ymin
    max.y  <- extent(dem)@ymax
    z.axis <- seq(-1.5, 2.0, by=0.1)
    min.z  <- -1.5
    max.z  <- 2
    
    # plot axis lines
    lines(trans3d(x.axis, min.y, max.z, pmat))
    lines(trans3d(max.x, y.axis, min.z, pmat))
    lines(trans3d(max.x, min.y, z.axis, pmat))
    
    # define and plot tick marks
    x.tick     <- seq(800, 1040, by=40)
    tick.start <- trans3d(x.tick, min.y, max.z, pmat)
    tick.end   <- trans3d(x.tick, (min.y - 3), max.z, pmat)
    segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
    
    y.tick     <- seq(450, 570, by=20)
    tick.start <- trans3d(max.x, y.tick, min.z, pmat)
    tick.end   <- trans3d(max.x+5, y.tick, min.z, pmat)
    segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
    
    z.tick     <- seq(-1.5, 2, by=3)
    tick.start <- trans3d(max.x, min.y, z.tick, pmat)
    tick.end   <- trans3d(max.x, min.y-2, z.tick, pmat)
    segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
    
    # define and plot tick mark labels
    labels    <- x.tick
    label.pos <- trans3d(x.tick, min.y-4, max.z, pmat)
    text(label.pos$x, label.pos$y, labels=labels, adj=1, cex=1)
    
    labels    <- y.tick
    label.pos <- trans3d(max.x+12, y.tick, min.z, pmat)
    text(label.pos$x, label.pos$y, labels=labels, adj=0.5, cex=1)
    
    labels    <- z.tick
    label.pos <- trans3d(max.x, min.y-3, z.tick, pmat)
    text(label.pos$x, label.pos$y, labels=labels, adj=1, cex=1)
    
    # define and plot axis labels
    x.lab     <- "x [m]"
    label.pos <- trans3d(mean(x.axis), min.y - 30, mean(z.axis), pmat)
    text(label.pos$x, label.pos$y, labels=x.lab, adj=1, cex=1)
    
    y.lab     <- "y [m]"
    label.pos <- trans3d(max.x+35, mean(y.axis), min.z, pmat)
    text(label.pos$x, label.pos$y, labels=y.lab, adj=1, cex=1)
    
    # label
    lab       <- "(E) wr"
    lab.pos <- trans3d(min.x, min.y, max.z+10, pmat)
    text(lab.pos$x, lab.pos$y, labels=lab, adj=1, cex=1.25)
  }
  
  # wt
  {
    dem <- raster(readAsciiGrid("c:\\doktor\\proof_of_concept\\mod_wt\\dem1.asc"))
    sp  <- read.table("c:\\doktor\\proof_of_concept\\modCal\\wtsP.txt", header=TRUE, sep="\t")
    
    demMat   <- as.matrix(dem)
    x        <- seq(0, dim(dem)[3]*nrow(demMat), length=nrow(demMat))
    y        <- seq(0, dim(dem)[3]*ncol(demMat), length=ncol(demMat))
    nrz      <- nrow(demMat)
    ncz      <- ncol(demMat)
    
    expand <- 0.075
    phi	   <- 40
    theta  <- 90
    zlim   <- c(-1,2)
    pmat <- persp(dem, col="grey90", theta=theta, phi=phi, shade=.5, ticktype="detailed", zlim=zlim, expand=expand, border=NA,
                  axes=FALSE, box=FALSE, ylim=c(6750, 6880))
    pts  <- trans3d(x=sp[[1]],y=sp[[2]],z=sp[[3]],pmat=pmat)
    points(pts,pch=20,cex=0.5)
    
    # define x,y and z axes
    x.axis <- seq(extent(dem)@xmin, extent(dem)@xmax, by=1)
    min.x  <- extent(dem)@xmin
    max.x  <- extent(dem)@xmax
    y.axis <- seq(extent(dem)@ymin, extent(dem)@ymax-15, by=1)
    min.y  <- extent(dem)@ymin
    max.y  <- extent(dem)@ymax
    z.axis <- seq(-1.5, 2.0, by=0.1)
    min.z  <- -1.5
    max.z  <- 2
    
    # plot axis lines
    lines(trans3d(x.axis, min.y, max.z, pmat))
    lines(trans3d(max.x, y.axis, min.z, pmat))
    lines(trans3d(max.x, min.y, z.axis, pmat))
    
    # define and plot tick marks
    x.tick     <- seq(-20714, -20482, by=40)
    tick.start <- trans3d(x.tick, min.y, max.z, pmat)
    tick.end   <- trans3d(x.tick, (min.y - 3), max.z, pmat)
    segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
    
    y.tick     <- seq(6790, 6850, by=20)
    tick.start <- trans3d(max.x, y.tick, min.z, pmat)
    tick.end   <- trans3d(max.x+5, y.tick, min.z, pmat)
    segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
    
    z.tick     <- seq(-1.5, 2, by=3)
    tick.start <- trans3d(max.x, min.y, z.tick, pmat)
    tick.end   <- trans3d(max.x, min.y-2, z.tick, pmat)
    segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
    
    # define and plot tick mark labels
    labels    <- x.tick
    label.pos <- trans3d(x.tick, min.y-4, max.z, pmat)
    text(label.pos$x, label.pos$y, labels=labels, adj=1, cex=1)
    
    labels    <- y.tick
    label.pos <- trans3d(max.x+12, y.tick, min.z, pmat)
    text(label.pos$x, label.pos$y, labels=labels, adj=0.5, cex=1)
    
    labels    <- z.tick
    label.pos <- trans3d(max.x, min.y-3, z.tick, pmat)
    text(label.pos$x, label.pos$y, labels=labels, adj=1, cex=1)
    
    # define and plot axis labels
    x.lab     <- "x [m]"
    label.pos <- trans3d(mean(x.axis), min.y - 30, mean(z.axis), pmat)
    text(label.pos$x, label.pos$y, labels=x.lab, adj=1, cex=1)
    
    y.lab     <- "y [m]"
    label.pos <- trans3d(max.x+30, mean(y.axis), min.z, pmat)
    text(label.pos$x, label.pos$y, labels=y.lab, adj=1, cex=1)
    
    # label
    lab       <- "(F) wt"
    lab.pos <- trans3d(min.x, min.y, max.z+10, pmat)
    text(lab.pos$x, lab.pos$y, labels=lab, adj=1, cex=1.25)
  }
  
  dev.off()
}


# table dóledec et al. (2007)
# Normalised ln-densities (as number of individuals by dm2) of taxa across hemisphere numbers (noted f0 to f19) from model no. 2 (sum across hemisphere numbers
# 1), i.e. average preference models for each taxon across surveys. R2TAXno. 2 and RTAXno. 3 are R2TAX values from model no. 2 and model no. 3, respectively; AVGFST: preferred
# hemisphere number (from model no. 2). Coleoptera adults and larvae are abbreviated (a) and (l), respectively; *stands for ‘early instars of’
{
  td <- read.table("c:\\presentations_papers\\paper1\\doledecModels.txt", header=TRUE, sep=";")
  td$Taxa <- as.character(td$Taxa)
  
  windows(height=4, width=7)
  par(mfcol=c(3,5), mar=c(3,4,3,1))
  for(i in 1:nrow(td))
  {
    plot(1:20,td[i,6:25], type="o", ylim=c(0,0.2), pch=20, axes=FALSE, xlab="", ylab="")
    axis(1)
    axis(2, las=2)
    box()
    abline(v=td[i,"AVGFST"], col="red")
    rng    <- par("usr")
    labGen <- substr(strsplit(td[i,"Taxa"], split=" ")[[1]][1], 1,1)
    labSp  <- strsplit(td[i,"Taxa"], split=" ")[[1]][2]
    title(main=paste0(labGen, ".", labSp), adj=1)
  }
  
}


# work with telemac model
{
  # make domain bound
  setwd("c:\\presentations_papers\\paper1\\wrtelemac\\")
  dem    <- raster("dem1.asc")
  
  xmin <- extent(dem)@xmin + 1
  xmax <- extent(dem)@xmax - 1
  ymin <- extent(dem)@ymin + 1
  ymax <- extent(dem)@ymax - 1
  
  xy <- cbind(c(xmin, xmin, xmax, xmax, xmin),
              c(ymin, ymax, ymax, ymin, ymin))
  
  bound <- SpatialPolygons(list(Polygons(list(Polygon(xy)), 1)))
  #shapefile(bound, file="bound.shp", overwrite=TRUE)
}


# point maps O. alb. wt, wr.
{
  setwd("c:\\doktor\\proof_of_concept\\hydromStress\\")
  
  ptswt <- readShapeSpatial(paste0(getwd(),"\\out_wt_Odont_alb\\pointMaps\\points300.shp"))
  ptswr <- readShapeSpatial(paste0(getwd(),"\\out_wr_Odont_alb\\pointMaps\\points300.shp"))
  
  colVectwt <- ifelse(ptswt@data==1,"grey60","white")
  colVectwr <- ifelse(ptswr@data==1,"grey60","white")
  
  windows(width=8.5, height=5)
  par(mfcol=c(2,3), oma=c(0,0,0,0))
  
  # points wt
  par(mar=c(4.5,4.5,1,1))
  plot(ptswt, pch=21, bg=colVectwt, col="black", cex=1, axes=FALSE, asp=4, ylim=c(6810,6850), lwd=1.5)
  box()
  axis(1, at=seq(-20670, -20470, by=70), cex.axis=1.25)
  axis(2, at=seq(6815, 6850, by=15), cex.axis=1.25)
  mtext(side=1, "x [m]", line=2.6)
  mtext(side=2, "y [m]", line=3)
  rng <- par("usr")
  text(labels="(a)", x=rng[2]-0.04*(rng[2]-rng[1]), y=rng[4]-0.07*(rng[4]-rng[3]), adj=1, cex=1.25)
  
  # points wr
  par(mar=c(4.5,4.5,1,1))
  plot(ptswr, pch=21, bg=colVectwr, col="black", cex=1, axes=FALSE, asp=2.5, lwd=1.5)
  box()
  axis(1, at=seq(800, 1070, by=70), cex.axis=1.25)
  axis(2, at=seq(460, 540, by=40), cex.axis=1.25)
  mtext(side=1, "x [m]", line=2.6)
  mtext(side=2, "y [m]", line=3)
  rng <- par("usr")
  text(labels="(d)", x=rng[2]-0.04*(rng[2]-rng[1]), y=rng[4]-0.07*(rng[4]-rng[3]), adj=1, cex=1.25)
}


# figure x. point duration, O.albicorne
{
  setwd("c:\\doktor\\defense")
  uiT <- read.table("uiSpatialWt.txt",header=TRUE)
  uiR <- read.table("uiSpatialWr.txt",header=TRUE)
  
  # symbol sizes
  uMax  <- 0.1 							   # symbol size full duration
  uMin  <- 0.01 							   # symbol size zero duration
  uMaxT <- max(sqrt((uiT$totalDur/2060)/pi)) # max. relative symbol size trained
  uMaxR <- max(sqrt((uiR$totalDur/2060)/pi)) # max. relative symbol size restored
  uMinT <- min(sqrt((uiT$totalDur/2060)/pi)) # min. relative symbol size trained
  uMinR <- min(sqrt((uiR$totalDur/2060)/pi)) # min. relative symbol size restored
  uT    <- sqrt((uiT$totalDur/2060)/pi)	   # relative symbol sizes trained
  uR    <- sqrt((uiR$totalDur/2060)/pi)	   # relative symbol sizes restored
  
  windows(width=4, height=5)
  par(mar=c(3.35,3.75,1,0.5), mfcol=c(2,1))
  
  # wt
  with(uiT, symbols(x=x, y=y, circles=uT, inches=uMaxT*uMax, bg="black", fg=NULL, 
                    xlab="", ylab="", xaxt="n", yaxt="n", ylim=c(6810,6845))
  )
  box()
  axis(1, at=seq(-20670, -20470, by=70))
  axis(2, at=seq(6812, 6845, by=15))
  mtext(side=1, "x [m]", line=2.25)
  mtext(side=2, "y [m]", line=2.75)
  rng <- par("usr")
  text(labels="(a)", x=rng[2]-0.03*(rng[2]-rng[1]), y=rng[4]-0.09*(rng[4]-rng[3]), adj=1)
  symbols(x=rng[1] + 0.08*(rng[2]-rng[1]), y=rng[3] + 0.3*(rng[4]-rng[3]), 
          circles=uMin, inches=uMin, bg="black", fg=NULL, add=TRUE)
  text(x=rng[1] + 0.14*(rng[2]-rng[1]), y=rng[3] + 0.3*(rng[4]-rng[3]), labels="u = 0", adj=0)
  symbols(x=rng[1] + 0.08*(rng[2]-rng[1]), y=rng[3] + 0.19*(rng[4]-rng[3]), 
          circles=uMax, inches=uMax, bg="black", fg=NULL, add=TRUE)
  text(x=rng[1] + 0.14*(rng[2]-rng[1]), y=rng[3] + 0.19*(rng[4]-rng[3]), labels="u = 1", adj=0)
  polygon(x=c(-20717,-20717,-20650,-20650), y=c(6812,6822,6822,6812))
  
  # wr
  with(uiR, symbols(x=x, y=y, circles=uR, inches=uMaxR*uMax, bg="black", fg=NULL, 
                    xlab="", ylab="", xaxt="n", yaxt="n")
  )
  box()
  axis(1, at=seq(800, 1070, by=70))
  axis(2, at=seq(460, 540, by=40))
  mtext(side=1, "x [m]", line=2.25)
  mtext(side=2, "y [m]", line=2.75)
  rng <- par("usr")
  text(labels="(b)", x=rng[2]-0.03*(rng[2]-rng[1]), y=rng[4]-0.09*(rng[4]-rng[3]), adj=1)
}


