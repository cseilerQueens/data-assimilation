.libPaths(c("/home/cseiler/daisy/renv", "/home/cseiler/AMBER/renv/library/R-4.3/x86_64-pc-linux-gnu"))
library(raster)
library(amber)

rm(list = ls())
setwd("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/simulations")

# fractional land area
#flnd <-
#  raster::stack(
#    "simulations/transient_CRUJRAv2.4.5_2022/netcdf_files/rsFile_modified.nc",
#    subds = "FLND"
#  )

cru.def <- raster::stack("transient_CRUJRAv2.4.5_2022/netcdf_files/nbp_annually.nc")
cru.opt <- raster::stack("transient_CRUJRAv2.4.5-opt-20250106/netcdf_files/nbp_annually.nc")

# hist.def <- raster::stack("transient_ISIMIP3b.CanESM5/netcdf_files/nbp_annually.nc")
# hist.opt <- raster::stack("transient_ISIMIP3b.CanESM5-opt/netcdf_files/nbp_annually.nc")

ssp.def <- raster::stack("ssp585_ISIMIP3b.CanESM5/netcdf_files/nbp_annually.nc")
ssp.opt <- raster::stack("ssp585_ISIMIP3b.CanESM5-opt/netcdf_files/nbp_annually.nc")

setwd("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation")

# Subset: 
# cru: 1960-2022
# hist: 1960-2022
# ssp:  2015-2100

# CRU: 1995-2014
cru.def <- subset(cru.def, 260:322)
cru.opt <- subset(cru.opt, 260:322)

# Sum and convert kg m-2 s-1 to kg m-2 y-1
cru.def <- rotate(sum(cru.def)) * 86400 * 365.24
cru.opt <- rotate(sum(cru.opt)) * 86400 * 365.24
cru.delta <- cru.opt - cru.def

# CanESM5-SSP: 2015-2100
last <- nlayers(ssp.def)
first <- last-85
ssp.def <- subset(ssp.def, first:last)
ssp.opt <- subset(ssp.opt, first:last)

# Sum and convert kg m-2 s-1 to kg m-2 y-1
ssp.def <- rotate(sum(ssp.def)) * 86400 * 365.24
ssp.opt <- rotate(sum(ssp.opt)) * 86400 * 365.24
ssp.delta <- ssp.opt - ssp.def

# Get values
cru.def.values <- values(cru.def)
cru.opt.values <- values(cru.opt)
ssp.def.values <- values(ssp.def)
ssp.opt.values <- values(ssp.opt)

# panel 01
variable <- cru.def
my.max <- cellStats(abs(variable), stat = "max")
min.max.int <- c(-5, 5, 1)
my.title <- "(a) \n Default culumlative NBP (1960-2022, CRUJRAv2 forcing) "
legend.bar.text <- latex2exp::TeX("kgC m$^{-2}$ year$^{-1}$")
panel01 <- list(variable, min.max.int, my.title, legend.bar.text)

# panel 02
variable <- ssp.def
my.max <- cellStats(abs(variable), stat = "max")
min.max.int <- c(-10, 10, 2)
my.title <- "(b) \n Default culumlative NBP (2015-2100, CanESM5 forcing) "
legend.bar.text <- latex2exp::TeX("kgC m$^{-2}$ year$^{-1}$")
panel02 <- list(variable, min.max.int, my.title, legend.bar.text)

# panel 03
variable <- cru.opt
my.max <- cellStats(abs(variable), stat = "max")
min.max.int <- c(-5, 5, 1)
my.title <- "(c) \n Optimized cumulative NBP (1960-2022, CRUJRAv2 forcing) "
legend.bar.text <- latex2exp::TeX("kgC m$^{-2}$ year$^{-1}$")
panel03 <- list(variable, min.max.int, my.title, legend.bar.text)

# panel 04
variable <- ssp.opt
my.max <- cellStats(abs(variable), stat = "max")
min.max.int <- c(-10, 10, 2)
my.title <- "(d) \n Optimized cumulative NBP (2015-2100, CanESM5 forcing) "
legend.bar.text <- latex2exp::TeX("kgC m$^{-2}$ year$^{-1}$")
panel04 <- list(variable, min.max.int, my.title, legend.bar.text)


# panel 05
variable <- cru.delta
my.max <- cellStats(abs(variable), stat = "max")
min.max.int <- c(-2, 2, 0.5)
my.title <- "(e) \n Optimized minus default NBP (c-a, CRUJRAv2 forcing) "
legend.bar.text <- latex2exp::TeX("kgC m$^{-2}$ year$^{-1}$")
panel05 <- list(variable, min.max.int, my.title, legend.bar.text)

# panel 06
variable <- ssp.delta
my.max <- cellStats(abs(variable), stat = "max")
min.max.int <- c(-5, 5, 1)
my.title <- "(f) \n Optimized minus default NBP (d-b, CanESM5 forcing) "
legend.bar.text <- latex2exp::TeX("kgC m$^{-2}$ year$^{-1}$")
panel06 <- list(variable, min.max.int, my.title, legend.bar.text)

panels <- list(panel01, panel02, panel03, panel04, panel05, panel06)

# Make Figure (1)

my.xlim <- c(-180, 180)
my.ylim <- c(-60, 90)

plot.width <- 12
plot.height <- 12 # 9

my.projection <- "+proj=longlat +ellps=WGS84"
shp.filename <- system.file("extdata/ne_110m_land/ne_110m_land.shp", package = "amber")
land <- intFun.coast(my.xlim, my.ylim, my.projection, shp.filename)  # reproject coastline

shp.ocean <- "/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/AMBER/summary/ne_110m_ocean/ne_110m_ocean.shp"
ocean <- intFun.coast(my.xlim, my.ylim, my.projection, shp.ocean)

my.filename = "NBP_maps"

outputDir <- "/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/analysis/figures"

grDevices::png(paste(outputDir, "/", my.filename, ".png", sep = ""), width = plot.width,
               height = plot.height, units = "in", res = 300)

graphics::par(mfrow = c(4, 2), font.main = 1, mar = c(3, 3, 3, 6.5), oma = c(0.5,0,0.5,0), lwd = 1, cex = 1)

for (i in 1:length(panels)) {
  
  # get the inputs for each panel
  
  data <- panels[[i]][[1]]
  min.max.int <- panels[[i]][[2]]
  my.title <- panels[[i]][[3]]
  legend.bar.text <- panels[[i]][[4]]
  
  # for legend
  min <- min.max.int[1]
  max <- min.max.int[2]
  interval <- min.max.int[3]
  
  my.breaks <- round(seq(min - interval, max + interval, interval), 3)  # location of color breaks
  my.labels <- round(seq(min, max, interval), 3)  # location of labels
  my.col <- scico::scico(n = length(my.breaks), palette = 'bam')
  if(i>4){
    my.col <- rev(scico::scico(n = length(my.breaks), palette = 'roma')) 
  }
  
  
  # Change values of outliers for the purpose of the legend
  data[data > max] <- max + interval/2
  data[data < min] <- min - interval/2
  
  # Make a mask, where (1) implies bias reduction and NA means bias increase
  mask <- data
  mask[mask>0] <- NA
  mask <- mask - mask + 1
  # mask <- rasterToPolygons(mask, dissolve = TRUE)
  mask <- raster::rasterToPoints(mask)
  
  my.axis.args <- list(at = my.labels, labels = my.labels, cex.axis = 1.0)
  my.legend.args <- list(text = legend.bar.text, side = 2, font = 1, line = 0.5,
                         cex = 1.0)
  
  dummy <- stats::runif(360 * 180, min = min, max = max)
  dummy <- matrix(dummy, nrow = 180)
  dummy <- raster::raster(dummy)
  
  myExtent <- c(-180, 180, -90, 90)
  
  raster::extent(dummy) <- myExtent
  
  my.xlim <- c(raster::extent(land)[1], raster::extent(land)[2])
  my.ylim <- c(raster::extent(land)[3], raster::extent(land)[4])
  
  raster::plot(dummy, col = NA, legend = FALSE, xlim = my.xlim, ylim = my.ylim,
               main = NA, axes = FALSE)
  
  raster::plot(land, col = "grey", border = NA, add = TRUE)
  raster::plot(data, col = my.col, breaks = my.breaks, legend = FALSE, add = TRUE)
  raster::plot(ocean, add = TRUE, col = "white", border = NA)
  raster::plot(land, add = TRUE)
  # points(mask, pch = 16, cex = 0.2)
  #  raster::plot(mask,
  #  density = 15,
  #  add = TRUE,
  #  col = "black",
  #  border = NA,
  #  angle = -45)
  
  graphics::mtext(my.title, side = 3, line = 0.5, cex = 1.00)
  
  # ticks
  
  graphics::axis(1, labels = TRUE, tcl = 0.5, cex.axis = 1.0)
  graphics::axis(2, labels = TRUE, tcl = 0.5, las = 2, cex.axis = 1.0)
  
  raster::plot(data, legend.only = TRUE, col = my.col, breaks = my.breaks, axis.args = my.axis.args,
               legend.args = my.legend.args, legend.width = 2.0, legend.shrink = 1, font = 1.5, horiz = FALSE, cex = 1.5)
  
}

par(mar = c(5, 5, 3, 6.5))
plot(x = cru.def.values, y = cru.opt.values,
     xlab = latex2exp::TeX("Default cum. NBP (kgC m$^{-2}$ y$^{-1}$)"),
     ylab = latex2exp::TeX("Optimized cum. NBP (kgC m$^{-2}$ y$^{-1}$)"),
     cex.lab = 1.25,
     cex.axis = 1.25)
abline(0,1, col = "red")
abline(v=0, col = "red")
abline(h=0, col = "red")
legend("topleft", "CRUJRAv2 forcing", bty = "n", cex = 1.25)
graphics::mtext("(g)", side = 3, line = 0.5, cex = 1.00)


plot(x = ssp.def.values, y = ssp.opt.values,
     xlab = latex2exp::TeX("Default cum. NBP (kgC m$^{-2}$ y$^{-1}$)"),
     ylab = latex2exp::TeX("Optimized cum. NBP (kgC m$^{-2}$ y$^{-1}$)"),
     cex.lab = 1.25,
     cex.axis = 1.25)
abline(0,1, col = "red")
abline(v=0, col = "red")
abline(h=0, col = "red")
legend("topleft", "CanESM5 forcing", bty = "n", cex = 1.25)
graphics::mtext("(h)", side = 3, line = 0.5, cex = 1.00)
dev.off()



