.libPaths("/home/cseiler/daisy/renv")
library(terra)

rm(list = ls())
setwd("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation")

cru.def <- "simulations/transient_CRUJRAv2.4.5_2022/netcdf_files/nbp_annually.nc"
cru.opt <- "simulations/transient_CRUJRAv2.4.5-opt-20250106/netcdf_files/nbp_annually.nc"

hist.def <- "simulations/transient_ISIMIP3b.CanESM5/netcdf_files/nbp_annually.nc"
hist.opt <- "simulations/transient_ISIMIP3b.CanESM5-opt/netcdf_files/nbp_annually.nc"

ssp.def <- "simulations/ssp585_ISIMIP3b.CanESM5/netcdf_files/nbp_annually.nc"
ssp.opt <- "simulations/ssp585_ISIMIP3b.CanESM5-opt/netcdf_files/nbp_annually.nc"


sim <- list(cru.def, cru.opt, hist.def, hist.opt, ssp.def, ssp.opt)
id <- list("CRU.def", "CRU.opt", "Hist.def", "Hist.opt", "SSP.def", "SSP.opt")
n <- length(sim)
result <- list()

for (i in 1:n) {
  
  # Time Axis (I am not using terra's time function as the resulting years are wrong)
  nc <- sim[[i]]
  nc <- ncdf4::nc_open(nc)
  time <- ncdf4::ncvar_get(nc, "time")
  nTime <- length(time)
  tunits <- ncdf4::ncatt_get(nc, "time", attname = "units")
  tunits <- strsplit(tunits$value, " ")
  origin <- unlist(tunits)[3]
  start.date <- as.Date(origin) + time[1]
  start.date <- format(as.Date(start.date), "%Y-%m-15")  # center of month
  dates <- seq(as.Date(start.date), by = "year", length = nTime)
  dates <- format(dates, "%Y")
  ncdf4::nc_close(nc)
  
  # fractional land area
  flnd.cru <-
    terra::rast(
      "simulations/transient_CRUJRAv2.4.5_2022/netcdf_files/rsFile_modified.nc",
      subds = "FLND"
    )
  terra::crs(flnd.cru) = "+proj=longlat +datum=WGS84"
  
  # fractional land area
  flnd.isimip <-
    terra::rast(
      "simulations/transient_ISIMIP3b.CanESM5-opt/netcdf_files/rsFile_modified.nc",
      subds = "FLND"
    )
  terra::crs(flnd.isimip) = "+proj=longlat +datum=WGS84"
  
  if (i < 3) {flnd <- flnd.cru} 
  if (i > 2) {flnd <- flnd.isimip} 

  # Values
  nc <- sim[[i]]
  data <- terra::rast(nc)
  crs(data) = "+proj=longlat +datum=WGS84"
  crs(flnd) = "+proj=longlat +datum=WGS84"
  
 flnd <- resample(flnd, data, method = "bilinear")

  flnd <- mask(x = flnd, mask = data)    
  flnd <- crop(x = flnd, y = data)

  
  area <- terra::cellSize(data, unit = "m") # area of grid cell in m^2
  data <-
    data * 86400 * 365.25 * 1000 * 10 ^ (-15) # convert from kgC m-2 s-1 to PgC m-2 yr-1
  dataXarea <- data * area
  dataXarea <- dataXarea * flnd
  globalTotals <-
    terra::global(x = dataXarea, fun = "sum", na.rm = TRUE)  # spatial sum
  
  # Get the moving average
  movingAverageFun <- function(x, n = 20) {
    stats::filter(x, rep(1/n, n), sides = 2)
  }
  movingAverage <- movingAverageFun(globalTotals)
  df <- data.frame(globalTotals, movingAverage)
  rownames(df) <- dates
  
  result[[i]] <- df
}

cru.def <- result[[1]]
cru.opt <- result[[2]]

cru.def <- cru.def[as.numeric(rownames(cru.def)) >= 1900, ]
cru.opt <- cru.opt[as.numeric(rownames(cru.opt)) >= 1900, ]



hist.def <- result[[3]]
hist.opt <- result[[4]]

ssp.def <- result[[5]]
ssp.opt <- result[[6]]

my.ylab <- "NBP (PgC per year)"

png("analysis/figures/NBP-CRUJRAv2.png", width = 6, height = 4, units = "in", res = 300)
x <- rownames(cru.def)
plot(x = x, y = cru.def$sum, col = "black", lwd = 0.5, type = "l", 
     main = "Forcing Data: CRUJRAv2", xlab = NA, ylab = my.ylab)
lines(x = x, y = cru.def$movingAverage, col = "black", lwd = 2, type = "l")

lines(x = x, y = cru.opt$sum, col = "red", lwd = 0.5, type = "l")
lines(x = x, y = cru.opt$movingAverage, col = "red", lwd = 2, type = "l")

abline(h=0, lty = 2)

# GCB 2024 Table 5
# 2013–2022, O2-based: 1.2 +/- 0.8
# 1990s 2000s 2013–2022
# Inversions: 0.9 [0.6,1.3] (3) 1.3 [0.7,2] (4) 1.6 [0.5,2.3] (8)
# O2:  1.3 ± 0.8 1.1 ± 0.7 1.2 ± 0.8

timeAxis <- 1990:2022
nbp.mean <- c(rep(1.3, 10), rep(1.1, 10), rep(1.2, 13))
nbp.sd <- c(rep(0.8, 10), rep(0.7, 10), rep(0.8, 13))
nbp.min <- nbp.mean + nbp.sd
nbp.max <- nbp.mean - nbp.sd

lines(x = timeAxis, y = nbp.max, lty = 1, col = "blue")
lines(x = timeAxis, y = nbp.min, lty = 1, col = "blue")

legend("topleft", pch = 16, c("Default", "Optimized", "GCB 2023, Table 5 (Atm. O2)"), 
       col = c("black", "red", "blue"), bty = "n")

dev.off()


# CanESM5

png("analysis/figures/NBP-CanESM5.png", width = 6, height = 4, units = "in", res = 300)
x <- rownames(ssp.def)
plot(x = x, y = ssp.def$sum, col = "black", lwd = 0.5, type = "l", 
     main = "Forcing Data: Bias-Adjusted CanESM5 (SSP5-8.5)",
     xlab = NA, ylab = my.ylab)
lines(x = x, y = ssp.def$movingAverage, col = "black", lwd = 2, type = "l")

lines(x = x, y = ssp.opt$sum, col = "red", lwd = 0.5, type = "l")
lines(x = x, y = ssp.opt$movingAverage, col = "red", lwd = 2, type = "l")

abline(h=0, lty = 2)

legend("topleft", pch = 16, c("Default", "Optimized"), col = c("black", "red"), bty = "n")

dev.off()



