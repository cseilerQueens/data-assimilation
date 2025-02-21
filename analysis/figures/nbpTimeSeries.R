
.libPaths("/home/cseiler/daisy/renv")
library(terra)

rm(list = ls())
setwd("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation")

# Global Carbon Budget 2024
# NBP = E_FOS - S_CEMENT - S_OCEAN - G_ATM
gcb <- read.table("analysis/figures/GCB2024", head = TRUE)

x <- gcb$Year
y_max <- gcb$NBP + gcb$NBP_unc
y_min <- gcb$NBP - gcb$NBP_unc
x_polygon <- c(x, rev(x))    # Combine x and reversed x for closed polygon
y_polygon <- c(y_max, rev(y_min))  # Combine upper and lower bounds



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

capture.output(print(result), file = "analysis/figures/result.txt")

cru.def <- result[[1]]
cru.opt <- result[[2]]

cru.def <- cru.def[as.numeric(rownames(cru.def)) >= 1900, ]
cru.opt <- cru.opt[as.numeric(rownames(cru.opt)) >= 1900, ]



hist.def <- result[[3]]
hist.opt <- result[[4]]

ssp.def <- result[[5]]
ssp.opt <- result[[6]]

my.ylab <- "NBP (PgC per year)"

png("analysis/figures/NBP-CRUJRAv2.png", width = 6, height = 6, units = "in", res = 300)
x <- rownames(cru.def)
plot(x = x, y = cru.def$sum, col = NA, lwd = 0.5, type = "l",
     main = "Forcing Data: CRUJRAv2", xlab = NA, ylab = my.ylab,
xlim = c(1960, 2022),
ylim = c(-4, 4))

polygon(x_polygon, y_polygon, col = "grey", border = "grey")

lines(x = x, y = cru.def$sum, col = "black", lwd = 2, type = "l")
lines(x = x, y = cru.opt$sum, col = "red", lwd = 2, type = "l")

abline(h=0, lty = 2)

legend("topleft", pch = 16, c("Default", "Optimized", "GCB 2024"),
       col = c("black", "red", "grey"), bty = "n")

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

