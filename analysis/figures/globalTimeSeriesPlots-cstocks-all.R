rm(list=ls())

userDirectory <- "/home/cseiler/projects/def-cseiler-ab/cseiler"

outputDir <- file.path(userDirectory, "CLASSIC_CanESM/globalTimeSeries")
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
library(rgdal)
library(rgeos)
library(scico)
library(sp)
library(stats)
library(utils)
library(viridis)
library(xtable)

modIDs <- c(
    "CRUJRAv2",
    "CRUJRAv2-CO2fixed",
    "CRUJRAv2-NCycle",
    "CRUJRAv2-NCycleV2",
    "GSWP3-W5E5",
    "CanESM5-hist",
    "CanESM5-CO2fixed-hist",
    "CanESM5-NCycle-hist",
    "CanESM5-NCycleV2-hist",
    "CanESM5-ISIMIP3b-hist",
    "CanESM5-ISIMIP3b-NCycle-hist",
    "CanESM5-ISIMIP3b-NCycleV2-hist",
    "CanESM5-CRUJRAv2.RSDS-hist",
    "CanESM5-CRUJRAv2.RLDS-hist",  
    "CanESM5-CRUJRAv2.TAS-hist",  
    "CanESM5-CRUJRAv2.PRE-hist",  
    "CanESM5-CRUJRAv2.HUSS-hist",  
    "CanESM5-CRUJRAv2.WIND-hist",
    "CanESM5-CRUJRAv2.PS-hist")

variables <- c('cSoil', 'cVeg', 'cStem', 'cRoot', 'cLeaf')

rs.path <- "/home/cseiler/projects/def-cseiler-ab/cseiler/CLASSIC_CanESM/links/CRUJRAv2"

mod.path <- "/home/cseiler/projects/def-cseiler-ab/cseiler/CLASSIC_CanESM/links"

nc.list <- foreach(i = modIDs) %do%{
    dir <- file.path(mod.path, i)
nc.01 <- file.path(dir, "cSoil_annually.nc")
nc.02 <- file.path(dir, "cVeg_annually.nc")
nc.03 <- file.path(dir, "cStem_annually.nc")
nc.04 <- file.path(dir, "cRoot_annually.nc")
nc.05 <- file.path(dir, "cLeaf_annually.nc")
nc.06 <- file.path(dir, "cProduct_annually.nc")
mod.path.x <- c(nc.01, nc.02, nc.03, nc.04, nc.05, nc.06)

    assign(paste("mod.path.", i, sep=".") , mod.path.x)
}

period <- c('1850-01', '2014-12')

globalSum = TRUE
nv <- length(variables)
conversionFactors <- rep(10^(-12), nv)
units <- rep('PgC', nv)

myLevel <- 1

globalValues <- "globalSum"
monthlyValues <- FALSE # because inputs are annual files
outputDir <- "/home/cseiler/projects/def-cseiler-ab/cseiler/CLASSIC_CanESM/globalTimeSeries"

myFilename <- "globalTimeSeries-cstocks-all"
plot.width <- 10
plot.height <- length(variables) * 2

lastYearsForTrend <- 50

globalTimeSeries(nc.list, modIDs, variables, units, period, conversionFactors,
    outputDir, plot.width, plot.height, myLevel = 1, myFilename, globalValues, rs.path, monthlyValues, lastYearsForTrend, ratios = FALSE)

#-----------------------------------
# SSP585
#-----------------------------------

modIDs <- c(
    "CanESM5-SSP585",
    "CanESM5-CO2fixed-SSP585",
    "CanESM5-NCycle-SSP585",
    "CanESM5-NCycleV2-SSP585",
    "CanESM5-ISIMIP3b-SSP585",
    "CanESM5-ISIMIP3b-NCycle-SSP585",
    "CanESM5-ISIMIP3b-NCycleV2-SSP585")

variables <- c('cSoil', 'cVeg', 'cStem', 'cRoot', 'cLeaf')

rs.path <- "/home/cseiler/projects/def-cseiler-ab/cseiler/CLASSIC_CanESM/links/CRUJRAv2"

mod.path <- "/home/cseiler/projects/def-cseiler-ab/cseiler/CLASSIC_CanESM/links"

nc.list <- foreach(i = modIDs) %do%{
    dir <- file.path(mod.path, i)
nc.01 <- file.path(dir, "cSoil_annually.nc")
nc.02 <- file.path(dir, "cVeg_annually.nc")
nc.03 <- file.path(dir, "cStem_annually.nc")
nc.04 <- file.path(dir, "cRoot_annually.nc")
nc.05 <- file.path(dir, "cLeaf_annually.nc")
nc.06 <- file.path(dir, "cProduct_annually.nc")
mod.path.x <- c(nc.01, nc.02, nc.03, nc.04, nc.05, nc.06)

    assign(paste("mod.path.", i, sep=".") , mod.path.x)
}

period <- c('2014-01', '2099-12')

globalSum = TRUE
nv <- length(variables)
conversionFactors <- rep(10^(-12), nv)
units <- rep('PgC', nv)

myLevel <- 1

globalValues <- "globalSum"
monthlyValues <- FALSE # because inputs are annual files

myFilename <- "globalTimeSeries-cstocks-ssp585"
plot.width <- 10
plot.height <- length(variables) * 2

lastYearsForTrend <- 50

globalTimeSeries(nc.list, modIDs, variables, units, period, conversionFactors,
    outputDir=outputDir, plot.width, plot.height, myLevel = 1, myFilename, globalValues, rs.path, monthlyValues, lastYearsForTrend, ratios = FALSE)
