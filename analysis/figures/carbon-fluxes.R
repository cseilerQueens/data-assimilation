.libPaths(c("/home/cseiler/daisy/renv"))
library(terra)
# library(viridis)

rm(list = ls())
setwd("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/simulations")

# fractional land area
# fractional land area
flnd <-
  terra::rast(
    "simulations/transient_CRUJRAv2.4.5_2022/netcdf_files/rsFile_modified.nc",
    subds = "FLND"
  )

cru.def <- raster::stack("transient_CRUJRAv2.4.5_2022/netcdf_files/nbp_annually.nc")
cru.opt <- raster::stack("transient_CRUJRAv2.4.5-opt-20250106/netcdf_files/nbp_annually.nc")

hist.def <- raster::stack("transient_ISIMIP3b.CanESM5/netcdf_files/nbp_annually.nc")
hist.opt <- raster::stack("transient_ISIMIP3b.CanESM5-opt/netcdf_files/nbp_annually.nc")

ssp.def <- raster::stack("ssp585_ISIMIP3b.CanESM5/netcdf_files/nbp_annually.nc")
ssp.opt <- raster::stack("ssp585_ISIMIP3b.CanESM5-opt/netcdf_files/nbp_annually.nc")

setwd("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation")

# Subset: 
# cru: 2013-2022
# hist: 1985-2014
# ssp:  2081-2100

# CRU: 1995-2014
cru.def <- subset(cru.def, 313:322)
cru.opt <- subset(cru.opt, 313:322)

# CanESM5-hist: 1985-2014
last <- nlyr(hist.def)
first <- last-29
hist.def <- subset(hist.def, first:last)
hist.opt <- subset(hist.opt, first:last)


# CanESM5-SSP: 2071-2100
last <- nlayers(ssp.def)
first <- last-29
ssp.def <- subset(ssp.def, first:last)
ssp.opt <- subset(ssp.opt, first:last)

sim <- c(cru.def, cru.opt, hist.def, hist.opt, ssp.def, ssp.opt)

df <- data.frame()

for (i in sim) {

data <- i
area <- raster::area(data) * 1000 * 1000  # area of grid cell in m^2
data <- data * 86400 * 365.25 * 1000 * 10^(-15) # convert from kgC m-2 s-1 to PgC m-2 yr-1
dataXarea <- data * area
dataXarea <- dataXarea * flnd
globalTotals <- raster::cellStats(dataXarea, stat = "sum", na.rm = TRUE)  # spatial sum
# quantiles <- quantile(globalTotals, probs = c(0.05, 0.25, 0.50, 0.75, 0.95))
data.mean <- mean(globalTotals)
data.sd <- sd(globalTotals)
ms <- cbind(data.mean, data.sd)
df <- rbind(df, ms)
}

df <- t(df)
colnames(df) <- c("CRUJRAv2.def", "CRUJRAv2.opt", "CanESM5-hist-def", "CanESM5-hist-opt", "CanESM5-SSP585-def", "CanESM5-SSP585-opt")

plot(df[1,], pch = 16, ylim = c(-4, 8))
x0 <- 1:ncol(df)
y0 <- df[1,]
x1 <- x0
ub <- y0 + df[2,]
lb <- y0 - df[2,]
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = ub, angle = 90)
arrows(x0 = x0, y0 = y0, x1 = x1, y1 = lb, angle = 90)



# Note: GCB 2023 (2001-2023): NBP = 2.1±1.1 PgC yr–1
# GCB: 1.2 ± 0.8

# 2013–2022, O2-based: 1.2 +/- 0.8
nbp.min <- 1.2 - 0.8
nbp.max <- 1.2 + 0.8

abline(h=1.2, col = "grey", lty = 2)
abline(h=nbp.min, col = "grey", lty = 2)
abline(h=nbp.max, col = "grey", lty = 2)

abline(v=2.5)
abline(v=4.5)


  
  




