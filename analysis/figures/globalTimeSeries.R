################################################################################
#' Timeseries plot of multiple variables
#' @description This function produces a Figure that shows the the time series of multiple variables for multiple models
#' nc.list, modIDs, variables, units, period, conversionFactors, outputDir, plot.width = 8, plot.height = 8, myLevel = 1, fractionalLand = FALSE
#' @param nc.list A list with paths for each model and variable (see example below)
#' @param modIDs An R object with the different model run IDs
#' @param variables Variable names, e.g. c('gpp', 'ra', 'rh')
#' @param units Vector of units of each variable in LaTeX format, e.g. c('W m$^{-2}$', 'W m$^{-2}$')
#' @param period The time period you would like to consider, e.g. c('1850-01', '2014-12')
#' @param conversionFactors Vector of factors that convert the unit of each variable into the desired output unit
#' @param outputDir A string that gives the output directory, e.g. '/home/project/study'. The output table in LaTeX format will only be written if the user specifies an output directory.
#' @param plot.width Number that gives the plot width, e.g. 6
#' @param plot.height Number that gives the plot height, e.g. 4
#' @param my.filename Output file name, e.g. 'globalTimeSeries-metForcing'
#' @param myLevel A number that determines what level of the output netCDF file to use.
#' @param globalValues Either 'globalMean' or 'globalSum'. If set to 'globalMean',
#' values are averaged across all grid cells. If set to 'globalSum', values are
#' summed up. The sum is weighted by grid cell area.
#' @param rs.path A string that gives the path to the CLASSIC restart file rsFile_modified.nc. If the restart file contains fractional land cover, then it will be used for computing global sums.
#' @param monthlyValues Logical. In the input netCDF file has monthly time steps choose TRUE. For annual time steps choose FALSE.
#' @param lastYearsForTrend A number that gives the last number of years that should be considered for compouting the trend, e.g. 100
#' @param ratios Logical, whether to plot absolute or relative values, where the first variable will serve as the denominator
#' @return A time series plot for global mean or globally summed values
#'
#' @examples
#' \donttest{
#' library(amber)
#' library(classInt)
#' library(doParallel)
#' library(foreach)
#' library(Hmisc)
#' library(latex2exp)
#' library(ncdf4)
#' library(ncdf4.helpers)
#' library(parallel)
#' library(raster)
#' library(scico)
#' library(sp)
#' library(stats)
#' library(utils)
#' library(viridis)
#' library(xtable)
#'
#'
#' modIDs <- c('CRUJRAv2', 'CanESM5-hist')
#' variables <- c('RSDS', 'RLDS', 'TAS', 'PR', 'HUSS', 'WIND', 'PS')
#' # CRUJRAv2
#' dirCRUJRAv2 <- '/space/hall5/sitestore/eccc/crd/ccrp/users/csr001/projects/data/metForcing/T63/CRUJRAv2'
#' nc.01 <- file.path(dirCRUJRAv2, 'dswrf_CRUJRAv2_MLY.nc')
#' nc.02 <- file.path(dirCRUJRAv2, 'dlwrf_CRUJRAv2_MLY.nc')
#' nc.03 <- file.path(dirCRUJRAv2, 'tmp_CRUJRAv2_MLY.nc')
#' nc.04 <- file.path(dirCRUJRAv2, 'pre_CRUJRAv2_MLY.nc')
#' nc.05 <- file.path(dirCRUJRAv2, 'spfh_CRUJRAv2_MLY.nc')
#' nc.06 <- file.path(dirCRUJRAv2, 'wind_CRUJRAv2_MLY.nc')
#' nc.07 <- file.path(dirCRUJRAv2, 'pres_CRUJRAv2_MLY.nc')

#' nc.mod01 <- c(nc.01, nc.02, nc.03, nc.04, nc.05, nc.06, nc.07)

#' # CanESM5
#' dirCanESM5 <- '/space/hall5/sitestore/eccc/crd/ccrp/users/csr001/projects/data/metForcing/T63/CanESM5/historical/r1i1p1f1/monthlyMeans'
#' nc.01 <- file.path(dirCanESM5, 'rsds_6hr_CanESM5_historical_r1i1p1f1_gn_18500101-20141231_MLY.nc')
#' nc.02 <- file.path(dirCanESM5, 'rlds_6hr_CanESM5_historical_r1i1p1f1_gn_18500101-20141231_MLY.nc')
#' nc.03 <- file.path(dirCanESM5, 'tas_6hr_CanESM5_historical_r1i1p1f1_gn_18500101-20141231_MLY.nc')
#' nc.04 <- file.path(dirCanESM5, 'pr_6hr_CanESM5_historical_r1i1p1f1_gn_18500101-20141231_MLY.nc')
#' nc.05 <- file.path(dirCanESM5, 'huss_6hr_CanESM5_historical_r1i1p1f1_gn_18500101-20141231_MLY.nc')
#' nc.06 <- file.path(dirCanESM5, 'sfcWind_6hr_CanESM5_historical_r1i1p1f1_gn_18500101-20141231_MLY.nc')
#' nc.07 <- file.path(dirCanESM5, 'ps_6hr_CanESM5_historical_r1i1p1f1_gn_18500101-20141231_MLY.nc')

#' nc.mod02 <- c(nc.01, nc.02, nc.03, nc.04, nc.05, nc.06, nc.07)

#' nc.list <- list(nc.mod01, nc.mod02)

#' period <- c('1850-01', '2014-12')

#' globalSum = FALSE
#' conversionFactors <- c(1, 1, 1, 86400 * 365.25/12, 1000, 1, 1/100)
#' units <- c('W m$^{-2}$', 'W m$^{-2}$', 'Centigrade', 'mm month$^{-1}$', '10$^{-3}$ kg kg$^{-1}$',
#'    'm s$^{-1}$', 'hPa')

#' rs.path <- FALSE
#' globalValues <- 'globalMean'
#' outputDir <- '/fs/site5/eccc/crd/ccrp/users/csr001/projects/CLASSIC_CanESM/compareForcings'

#' myFilename <- 'globalTimeSeries-metForcing.png'
#' plot.width <- 8
#' plot.height <- 8

#' globalTimeSeries(nc.list, modIDs, variables, units, period, conversionFactors, outputDir = FALSE,
#'     plot.width = 8, plot.height = 8, myLevel = 1, globalValues)
#' } #donttest
#' @export
globalTimeSeries <- function(nc.list, modIDs, variables, units, period, conversionFactors,
    outputDir = FALSE, plot.width = 8, plot.height = 8, myLevel = 1, myFilename = "globalTimeSeries",
    globalValues = "globalMean", rs.path = FALSE, monthlyValues = TRUE, lastYearsForTrend = 50,
    ratios = FALSE) {

    nmod <- length(modIDs)
    nvar <- length(variables)

    # create a mask
    mod.list <- foreach::foreach(i = 1:nmod) %do% {
        mod <- raster::brick(nc.list[[i]][1], level = myLevel)
        return(mod)
    }

    mod.mean <- lapply(mod.list, raster::mean)  # mean of each model
    multi.mod.mean <- raster::stack(mod.mean)  # stack of mean of each model
    multi.mod.mean <- raster::mean(multi.mod.mean)  # mean of each model mean

    mask <- multi.mod.mean
    mask <- mask - mask + 1
    suppressWarnings(mask <- raster::rotate(mask))

    # Nested loop: each model and each variable

    eachMod <- foreach::foreach(m = 1:nmod) %do% {

        eachVar <- foreach::foreach(v = 1:nvar) %do% {

            # subset time period
            nc.mod <- nc.list[[m]][v]
            mod <- raster::brick(nc.mod, level = myLevel)
            dates.mod <- intFun.getZ(nc.mod)
            mod <- raster::setZ(mod, dates.mod)
            names(mod) <- dates.mod

            start.date <- period[1]
            end.date <- period[2]

            mod <- mod[[which(format(as.Date(raster::getZ(mod)), "%Y-%m") >= start.date &
                format(as.Date(raster::getZ(mod)), "%Y-%m") <= end.date)]]
            suppressWarnings(mod <- raster::rotate(mod))
            mod.names <- names(mod)
            mod <- mod * conversionFactors[v]

            data <- mod * mask

            # Convert temperature from K to C if required
            cellMax <- max(raster::cellStats(data, stat = "max", na.rm = TRUE), na.rm = TRUE)

            if (variables[v] == "TAS" && cellMax > 100) {
                data <- data - 273.15
            }

            names(data) <- mod.names

            date <- as.Date(names(data), format = "X%Y.%m.%d")
            index <- format(as.Date(names(data), format = "X%Y.%m.%d"), format = "%m")
            month <- as.numeric(index)

            # time series
            if (globalValues == "globalMean") {
                time.series <- raster::cellStats(data, stat = "mean", na.rm = TRUE)  # spatial mean
                time.series <- data.frame(date, month, time.series)
                colnames(time.series) <- c("date", "month", "data")
                year <- format(as.Date(time.series$date), "%Y")
                time.series <- data.frame(year, time.series)
                annualValues <- tapply(time.series$data, time.series$year, mean,
                  na.rm = TRUE)
                annualValues <- data.frame(annualValues)
                year <- rownames(annualValues)
                year <- as.numeric(year)
                annualValues <- data.frame(year, annualValues)
                time.series <- merge(time.series, annualValues, by = "year")

                if (monthlyValues == TRUE) {
                  time.series[time.series$month != 1, "annualValues"] <- NA
                  time.series.yr <- subset(time.series, month == 1)
                  time.series.yr <- subset(time.series.yr, select = -c(data, month))
                }
                if (monthlyValues == FALSE) {
                  time.series.yr <- subset(time.series, select = -c(data, month))
                }
            }

            if (globalValues == "globalSum") {
                area <- raster::area(data) * 1000 * 1000  # area of grid cell in m^2
                dataXarea <- data * area

                # use fractional land cover if available
                if (rs.path != FALSE) {
                  try(flnd <- raster::raster(file.path(rs.path, "rsFile_modified.nc"),
                    v = "FLND"))
                  suppressWarnings(flnd <- raster::rotate(flnd))
                  try(dataXarea <- dataXarea * flnd)
                  rm(flnd)
                }

                time.series <- raster::cellStats(dataXarea, stat = "sum", na.rm = TRUE)  # spatial sum
                time.series <- data.frame(date, month, time.series)
                colnames(time.series) <- c("date", "month", "data")
                year <- format(as.Date(time.series$date), "%Y")
                time.series <- data.frame(year, time.series)
                annualValues <- tapply(time.series$data, time.series$year, mean,
                  na.rm = TRUE)
                annualValues <- data.frame(annualValues)
                year <- rownames(annualValues)
                year <- as.numeric(year)
                annualValues <- data.frame(year, annualValues)
                time.series <- merge(time.series, annualValues, by = "year")

                if (monthlyValues == TRUE) {
                  time.series[time.series$month != 1, "annualValues"] <- NA
                  time.series.yr <- subset(time.series, month == 1)
                  time.series.yr <- subset(time.series.yr, select = -c(data, month))
                }
                if (monthlyValues == FALSE) {
                  time.series.yr <- subset(time.series, select = -c(data, month))
                }
            }

            if (ratios == TRUE && v == 1) {
                denominator <- time.series.yr
            }

            if (ratios == TRUE) {
                annualValues <- time.series.yr$annualValues/denominator$annualValues
                time.series.yr$annualValues <- annualValues
            }

            # Get the moving average
            movingAverageFun <- function(x, n = 10) {
                stats::filter(x, rep(1/n, n), sides = 2)
            }
            movingAverage <- movingAverageFun(time.series.yr$annualValues)
            time.series.yr <- data.frame(time.series.yr, movingAverage)

            # Get the annual mean for last 20 years
            maxYear <- as.numeric(max(time.series.yr$year))
            lastTwoDecades <- subset(time.series.yr, year > maxYear - 20)
            avg <- mean(lastTwoDecades$annualValues, na.rm = TRUE)
            avg <- round(avg, 2)
            avg <- rep(avg, nrow(time.series.yr))

            time.series.yr <- data.frame(time.series.yr, avg)

            # Get the slope

            # subset data set for which you want to compute the trend
            maxYear <- as.numeric(max(time.series.yr$year))
            time.series.yr.forTrend <- subset(time.series.yr, year > maxYear - lastYearsForTrend)
            y <- time.series.yr.forTrend$annualValues
            x <- as.numeric(time.series.yr.forTrend$year)
            fit <- lm(y ~ x)
            slope <- fit$coefficients[2]
            if (ratios == TRUE) {
                slope <- slope * 10^3
            }
            slope <- round(slope, 3)
            slope <- rep(slope, nrow(time.series.yr))

            time.series.yr <- data.frame(time.series.yr, slope)

            return(time.series.yr)
        }
    }

    # Note that eachMod is a nested list: eachMod[[number of models]][[number
    # of variables]]

    time.series.list <- eachMod

    # colors
    myPalette <- "viridis"
    my.col <- viridis::viridis(n = nmod, begin = 0, end = 0.8, option = myPalette)

    oldpar <- graphics::par(mfrow = c(1, 2))
    on.exit(graphics::par(oldpar))

    # Creat an empty object that will store the mean and slope values:
    allMeans <- numeric(0)
    allSlopes <- numeric(0)


    if (outputDir != FALSE) {
        grDevices::png(paste(outputDir, "/", myFilename, ".png", sep = ""), width = plot.width,
            height = plot.height, units = "in", res = 300)
    }

    ny <- nvar
    if (ratios == TRUE) {
        ny <- nvar - 1
    }


    graphics::par(mfrow = c(ny, 1), font.main = 1, mar = c(0, 0, 0, 0), oma = c(2,
        5, 1, 5), lwd = 1, cex = 1, xpd = NA, tck = 0.03, las = 1)

    # loop for each variable
    first <- 1
    if (ratios == TRUE) {
        first <- 2  # because the 1. variable is used as denominator
    }

    for (v in first:length(variables)) {

        # function that checks whether integer is even or odd this will be used
        # for deciding where to add labels
        is.even <- function(x) x%%2 == 0
        my.unit <- latex2exp::TeX(units[v])
        my.var <- variables[v]

        # Get min and max value for y-axis limits

        time.series.list <- lapply(eachMod, "[[", v)

        xlim.min <- min(unlist(lapply(time.series.list, function(x) min(x$year, na.rm = TRUE))),
            na.rm = TRUE)
        xlim.max <- max(unlist(lapply(time.series.list, function(x) max(x$year, na.rm = TRUE))),
            na.rm = TRUE)
        my.xlim <- c(xlim.min, xlim.max)
        my.xlim <- as.numeric(my.xlim)

        ylim.min <- min(unlist(lapply(time.series.list, function(x) min(x$annualValues,
            na.rm = TRUE))), na.rm = TRUE)
        ylim.max <- max(unlist(lapply(time.series.list, function(x) max(x$annualValues,
            na.rm = TRUE))), na.rm = TRUE)
        my.ylim <- c(ylim.min, ylim.max)

        plot(time.series.list[[1]]$year, time.series.list[[1]]$annualValues, col = NA,
            ylim = my.ylim, xlim = my.xlim, xlab = NA, ylab = NA, main = NA, xaxt = "n",
            yaxt = "n")

        for (i in 1:nmod) {
            lines(time.series.list[[i]]$year, time.series.list[[i]]$annualValues,
                col = my.col[i], lwd = 0.25)

            lines(time.series.list[[i]]$year, time.series.list[[i]]$movingAverage,
                col = my.col[i], lwd = 1.5)

            allMeans <- rbind(allMeans, time.series.list[[i]]$avg[1])
            allSlopes <- rbind(allSlopes, time.series.list[[i]]$slope[1])

        }

        legendContent <- paste(modIDs, " (mean = ", allMeans, ", trend = ", allSlopes,
            ")", sep = "")

        if (ratios == TRUE) {

            legendContent <- paste(modIDs, " (mean = ", allMeans, ", trend = ", allSlopes,
                " x 10-3)", sep = "")
        }

        allMeans <- numeric(0)
        allSlopes <- numeric(0)

        graphics::legend("topleft", legendContent, col = my.col, pch = 16, bty = "n",
            horiz = FALSE, cex = 0.75)


  #      if (v == first) {
  #          myText <- paste("Mean values and trends correspond to the last 20 and",
  #              lastYearsForTrend, "years, respectively")
  #          mtext(myText, side = 3, line = 1, outer = TRUE)
  #      }

        #-----------------------------------------------------------------------
        # ticks and labels
        #-----------------------------------------------------------------------

        years <- as.numeric(time.series.list[[1]]$year)
        mmi <- intFun.min.max.int(years)
        start <- mmi[1] + mmi[3]
        end <- mmi[2] - mmi[3]
        timeAxis <- seq(start, end, mmi[3])

        graphics::axis(1, at = timeAxis, labels = FALSE, tcl = 0.3)

        # for subplots when i is odd:
        if (is.even(v) == FALSE) {
            graphics::axis(2, labels = TRUE, tcl = 0.3)
            graphics::axis(4, labels = FALSE, tcl = 0.3)
            graphics::mtext(my.unit, 2, line = 3.5, las = 3)
            graphics::mtext(my.var, 4, line = 0.5, las = 3)
        }
        # for subplots when i is even:
        if (is.even(v) == TRUE) {
            graphics::axis(2, labels = FALSE, tcl = 0.3)
            graphics::axis(4, labels = TRUE, tcl = 0.3)
            graphics::mtext(my.unit, 4, line = 3.5, las = 3)
            graphics::mtext(my.var, 2, line = 0.5, las = 3)
        }

        if (v == nvar) {
            graphics::axis(1, at = timeAxis, labels = TRUE, tcl = 0.3)
        }
    }


    if (outputDir != FALSE) {
        grDevices::dev.off()
    }

    # Make a table for mean values and for slope

    # 1: simulation (1-2) length(modIDs) 2: flux (1-6) length(variables)
    time.series.list <- eachMod

    # Mean values
    eachMod <- foreach::foreach(m = 1:nmod) %do% {
        eachVar <- foreach::foreach(v = 1:nvar) %do% {
            avg <- time.series.list[[m]][[v]]$avg[1]
            return(avg)
        }
    }
    avg <- matrix(unlist(eachMod), ncol = v, byrow = TRUE)
    avg <- data.frame(avg)
    rownames(avg) <- modIDs
    colnames(avg) <- variables
    unit <- units[1]
    unit <- rep(unit, m)
    avg <- data.frame(avg, unit)
    # convert to LaTeX
    data <- xtable::xtable(avg, type = "latex", file = paste(myFilename, "avg.tex",
        sep = "_"))
    xtable::caption(data) <- "avg"
    if (outputDir != FALSE) {
        xtable::print.xtable(data, include.rownames = TRUE, label = "tab:avg", type = "latex",
            file = paste(myFilename, "avg.tex", sep = "_"), caption.placement = "top",
            sanitize.text.function = function(x) {
                x
            })
    }

    # Slope

    eachMod <- foreach::foreach(m = 1:nmod) %do% {

        eachVar <- foreach::foreach(v = 1:nvar) %do% {
            slope <- time.series.list[[m]][[v]]$slope[1]
            return(slope)
        }
    }

    slope <- matrix(unlist(eachMod), ncol = v, byrow = TRUE)
    slope <- data.frame(slope)
    rownames(slope) <- modIDs
    colnames(slope) <- variables
    unit <- paste(units[1], "yr$^{-1}$")
    unit <- rep(unit, m)
    slope <- data.frame(slope, unit)
    # convert to LaTeX
    data <- xtable::xtable(slope, digits = 3, type = "latex", file = paste(myFilename, "slope.tex",
        sep = "_"))
    xtable::caption(data) <- "Slope"
    if (outputDir != FALSE) {
        xtable::print.xtable(data, include.rownames = TRUE, label = "tab:slope",
            type = "latex", file = paste(myFilename, "slope.tex", sep = "_"), caption.placement = "top",
            sanitize.text.function = function(x) {
                x
            })
    }


}
