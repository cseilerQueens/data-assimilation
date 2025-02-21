.libPaths("/home/cseiler/AMBER/renv/library/R-4.3/x86_64-pc-linux-gnu")

rm(list=ls())

userDirectory <- "/home/cseiler/projects/def-cseiler-ab/cseiler"
outputDir <- file.path(userDirectory, "data-assimilation/analysis/figures")
setwd(outputDir)

library(amber)
library(classInt)
library(doParallel)
library(foreach)
library(Hmisc)
library(latex2exp)
library(ncdf4)
library(ncdf4.helpers)
library(parallel)
library(raster)
# library(rgdal)
# library(rgeos)
library(scico)
library(sp)
library(stats)
library(utils)
library(viridis)
library(xtable)

source("globalTimeSeries.R")

#-----------------------------------
# historical period
#-----------------------------------

modIDs <- c(
  "CLASSIC-CRUJRA-def",
  "CLASSIC-CRUJRA-opt")

variables <- c("NBP", "GPP", "RA", "RH", "fFire", "cVeg", "cSoil")

# restart file
rs.path <- "/home/cseiler/projects/def-cseiler-ab/cseiler/CLASSIC_CanESM/links/CRUJRAv2"

mod.path <- "/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/analysis/figures/links"

# CLASSIC-CRUJRA-def  CLASSIC-CRUJRA-opt  CLASSIC-CanESM5-def  CLASSIC-CanESM5-opt


nc.list <- foreach(i = modIDs) %do%{
    dir <- file.path(mod.path, i)
nc.01 <- file.path(dir, "nbp_annually.nc")
nc.02 <- file.path(dir, "gpp_annually.nc")
nc.03 <- file.path(dir, "ra_annually.nc")
nc.04 <- file.path(dir, "rh_annually.nc")
nc.05 <- file.path(dir, "fFire_annually.nc")
nc.06 <- file.path(dir, "cVeg_annually.nc")
nc.07 <- file.path(dir, "cSoil_annually.nc")

# nc.ii <- file.path(dir, "fDeforestToAtmos_annually.nc")
# nc.ii <- file.path(dir, "fProductDecomp_annually.nc")


mod.path.x <- c(nc.01, nc.02, nc.03, nc.04, nc.05, nc.06, nc.07)

    assign(paste("mod.path.", i, sep=".") , mod.path.x)
}

period <- c('1960', '2022-12')

globalSum = TRUE
nv <- length(variables)

conversionFactor.flux <- rep(86400 * 365.25 * 10^(-12), 5)
conversionFactor.stock <- rep(10^(-12), 2)
conversionFactors <- c(conversionFactor.flux, conversionFactor.stock)
units.flux <- rep('PgC yr$^{-1}$', 5)
units.stock <- rep("PgC", 2)
units <- c(units.flux, units.stock)

myLevel <- 1

globalValues <- "globalSum"
monthlyValues <- FALSE # because inputs are annual files

myFilename <- "globalTimeSeries-CRUJRAv2"
plot.width <- 6 # 7
plot.height <- length(variables) * 1.2

lastYearsForTrend <- 50

globalTimeSeries(nc.list, modIDs, variables, units, period, conversionFactors,
    outputDir=outputDir, plot.width, plot.height, myLevel = 1, myFilename, globalValues, rs.path, monthlyValues, lastYearsForTrend, ratios = FALSE)

#-----------------------------------
# SSP585
#-----------------------------------

modIDs <- c(
  "CLASSIC-CanESM5-def",
  "CLASSIC-CanESM5-opt")

variables <- c("NBP", "GPP", "RA", "RH", "fFire", "cVeg", "cSoil")

nc.list <- foreach(i = modIDs) %do%{
  dir <- file.path(mod.path, i)
  nc.01 <- file.path(dir, "nbp_annually.nc")
  nc.02 <- file.path(dir, "gpp_annually.nc")
  nc.03 <- file.path(dir, "ra_annually.nc")
  nc.04 <- file.path(dir, "rh_annually.nc")
  nc.05 <- file.path(dir, "fFire_annually.nc")
  nc.06 <- file.path(dir, "cVeg_annually.nc")
  nc.07 <- file.path(dir, "cSoil_annually.nc")

mod.path.x <- c(nc.01, nc.02, nc.03, nc.04, nc.05, nc.06, nc.07)

    assign(paste("mod.path.", i, sep=".") , mod.path.x)
}

period <- c('2015-01', '2099-12')

globalSum = TRUE
nv <- length(variables)

conversionFactor.flux <- rep(86400 * 365.25 * 10^(-12), 5)
conversionFactor.stock <- rep(10^(-12), 2)
conversionFactors <- c(conversionFactor.flux, conversionFactor.stock)
units.flux <- rep('PgC yr$^{-1}$', 5)
units.stock <- rep("PgC", 2)
units <- c(units.flux, units.stock)

myLevel <- 1

globalValues <- "globalSum"
monthlyValues <- FALSE # because inputs are annual files

myFilename <- "globalTimeSeries-CanESM5"

lastYearsForTrend <- 50

globalTimeSeries(nc.list, modIDs, variables, units, period, conversionFactors,
    outputDir=outputDir, plot.width, plot.height, myLevel = 1, myFilename, globalValues, rs.path = FALSE, monthlyValues, lastYearsForTrend, ratios = FALSE)


# Additional analysis

CLASSIC.CRUJRAv2.def <- c(1.34 , 133.07 , 59.05 , 69.91 , 2.14 , 469.40 , 1163.89 ) 
CLASSIC.CRUJRAv2.opt <- c(0.86 , 129.50 , 60.93 , 65.77 , 1.43 , 394.56 , 1026.27 ) 
CLASSIC.CanESM5.def <- c(4.30 , 235.44 , 107.44 , 120.25 , 3.34 , 781.98 , 1349.98 ) 
CLASSIC.CanESM5.opt <- c(2.64 , 224.17 , 108.69 , 110.34 , 2.42 , 638.73 , 1169.43 ) 

dCRU <- (CLASSIC.CRUJRAv2.opt - CLASSIC.CRUJRAv2.def)
print(round(dCRU,2))

dCRU <- (CLASSIC.CRUJRAv2.opt - CLASSIC.CRUJRAv2.def) / CLASSIC.CRUJRAv2.def * 100
print(round(dCRU,0))

dCanESM5 <- (CLASSIC.CanESM5.opt - CLASSIC.CanESM5.def)
print(round(dCanESM5,2))

dCanESM5 <- (CLASSIC.CanESM5.opt - CLASSIC.CanESM5.def) / CLASSIC.CanESM5.def * 100
print(round(dCanESM5,0))




