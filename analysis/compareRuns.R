.libPaths(c("/home/cseiler/daisy/renv", "/home/cseiler/AMBER/renv/library/R-4.3/x86_64-pc-linux-gnu"))
library(daisy)
library(viridis)

rm(list = ls())
setwd("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/analysis")

library(GA)

# Optimizing against CLASSIC
P1R1N20G60Mod <- readRDS("GPP-CLASSIC/objectFinal.rds")
P2R1N20G60Mod <- readRDS("GPP-CLASSIC-vmax-kn/objectFinal.rds")
P2R6N20G60Mod <- readRDS("ALBS-GPP-HFLS-HFSS-LAI-TS-CLASSIC/object_iteration_99.rds")
P2R6N50G60Mod <- readRDS("ALBS-GPP-HFLS-HFSS-LAI-TS-P50-CLASSIC/objectFinal.rds")

# Optimizing against GEOs
P2R6N20G60Geo <- readRDS("ALBS-GPP-HFLS-HFSS-LAI-TS-SUG/objectFinal.rds")
P28R6N20G60Geo <- readRDS("ALBS-GPP-HFLS-HFSS-LAI-TS-SUG-28par/objectFinal.rds")
P28R6N20G200Geo <- readRDS("ALBS-GPP-HFLS-HFSS-LAI-TS-SUG-28par-200GC/objectFinal.rds")
P28R6N20G60GeoI1901 <- readRDS("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation02/object.rds")
# P28R6N20G200GeoI1901 <- readRDS("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation02/objectFinal.rds")

png("ScoreVsGen-CLASSIC.png", width = 8, height = 4.3, units = "in", res = 300)
par(mfrow = c(1,2), mar = c(4,4,2,1), lwd=1, cex=1)

object <- P1R1N20G60Mod
df <- data.frame(object@summary)
#lower_bound <- df$q1
#upper_bound <- df$q3
lower_bound <- df$min
upper_bound <- df$max

# Plot
x <- 1:length(df$median)
y <- df$median

plot(x, y, 
     ylim = c(0.8, 1),
     type = "l",
     xlab = "Generation",
     ylab = "AMBER Score (-)",
     main = "(a)")

# Plot IQR
arrows(x, lower_bound, x, upper_bound, angle = 90, code = 3, length = 0)

# P2R1N20G60Mod
object <- P2R1N20G60Mod
df <- data.frame(object@summary)
#lower_bound <- df$q1
#upper_bound <- df$q3
lower_bound <- df$min
upper_bound <- df$max
x <- 1:length(df$median)
y <- df$median

lines(x, y, type = "l", col = "red")

# Plot IQR
arrows(x, lower_bound, x, upper_bound, angle = 90, code = 3, length = 0, col = "red")

legend("bottomright", c("P1R1N20G60Mod", "P2R1N20G60Mod"), pch = 16, col = c("black", "red"))

# (b) 

object <- P2R6N20G60Mod
df <- data.frame(object@summary)
#lower_bound <- df$q1
#upper_bound <- df$q3
lower_bound <- df$min
upper_bound <- df$max

# Plot
x <- 1:length(df$median)
y <- df$median

plot(x, y, 
     ylim = c(0.8, 1),
     type = "l",
     xlab = "Generation",
     ylab = "AMBER Score (-)",
     main = "(b)")

# Plot IQR
arrows(x, lower_bound, x, upper_bound, angle = 90, code = 3, length = 0)

# P2R6N50G60Mod
object <- P2R6N50G60Mod
df <- data.frame(object@summary)
#lower_bound <- df$q1
#upper_bound <- df$q3
lower_bound <- df$min
upper_bound <- df$max
x <- 1:length(df$median)
y <- df$median

lines(x, y, type = "l", col = "red")

# Plot IQR
arrows(x, lower_bound, x, upper_bound, angle = 90, code = 3, length = 0, col = "red")

legend("bottomright", c("P2R6N20G60Mod", "P2R6N50G60Mod"), pch = 16, col = c("black", "red"))

dev.off()

#-------------------------------------------------------------------------------
# Compare runs against Global Earth Observations
#-------------------------------------------------------------------------------

# Scores that you get when using default parameter values
defaultScoreG60 <- 0.659637798602352 # 60 Grid Cells
defaultScoreG200 <- 0.664981976460085 # 200 Grid Cells
defaultScoreI1901 <- 0.659508924814264 # simulation starts in 1901

# G60 <- data.frame(P28R6N20G60Geo@summary)$median
# G200 <- data.frame(P28R6N20G200Geo@summary)$median
# I1901 <- data.frame(P28R6N20G60GeoI1901@summary)$median

G60 <- data.frame(P28R6N20G60Geo@summary)$max
G200 <- data.frame(P28R6N20G200Geo@summary)$max
I1901 <- data.frame(P28R6N20G60GeoI1901@summary)$max

dG60 <- G60 - defaultScoreG60
dG200 <- G200 - defaultScoreG200
dI1901 <- I1901 - defaultScoreI1901

nsim <- 3 # number of simulations

myPalette <- "plasma"
my.col <- viridis::viridis(n = nsim, begin = 0, end = 0.9, direction = -1, option = myPalette)

png("ScoreVsGen-GEO.png", width = 4, height = 4.3, units = "in", res = 300)
par(mfrow = c(1,1), mar = c(4,4,2,1), lwd=1, cex=1)

my.ylab <- latex2exp::TeX("$Delta$ AMBER Score (-)")
plot(dG60,
     ylim = c(0,0.01),
#     ylim = c(-0.02,0.01),
     type = "l",
     xlab = "Generation",
     ylab = my.ylab,
     col = my.col[1])
lines(dG200, col = my.col[2])
lines(dI1901, col = my.col[3])

# abline(h = 0, col = "grey", lty = 2)

legend("bottomright", legend = c("P28R6N20G60Geo", "P28R6N20G200Geo", "P28R6N20G60GeoI1901"), text.col = my.col, bty = "n")

dev.off()

#-------------------------------------------------------------------------------
# Compare runs against Global Earth Observations V2
#-------------------------------------------------------------------------------

# Scores that you get when using default parameter values
defaultScoreG60 <- 0.659637798602352 # 60 Grid Cells
defaultScoreG200 <- 0.664981976460085 # 200 Grid Cells
defaultScoreI1901 <- 0.659508924814264 # simulation starts in 1901

G60.median <- data.frame(P28R6N20G60Geo@summary)$median - defaultScoreG60
G200.median <- data.frame(P28R6N20G200Geo@summary)$median - defaultScoreG200
I1901.median <- data.frame(P28R6N20G60GeoI1901@summary)$median - defaultScoreI1901
# I1901.median <- I1901.median[1:17]

G60.max <- data.frame(P28R6N20G60Geo@summary)$max - defaultScoreG60
G200.max <- data.frame(P28R6N20G200Geo@summary)$max - defaultScoreG200
I1901.max <- data.frame(P28R6N20G60GeoI1901@summary)$max - defaultScoreI1901
# I1901.max <- I1901.max[1:17]

G60.min <- data.frame(P28R6N20G60Geo@summary)$min - defaultScoreG60
G200.min <- data.frame(P28R6N20G200Geo@summary)$min - defaultScoreG200
I1901.min <- data.frame(P28R6N20G60GeoI1901@summary)$min - defaultScoreI1901
# I1901.min <- I1901.min[1:17]

x <- 1:length(G60.median)
x_polygon <- c(x, rev(x))

nsim <- 3 # number of simulations

myPalette <- "plasma"
my.col <- viridis::viridis(n = nsim, begin = 0, end = 0.9, direction = -1, option = myPalette)
my.col.pol <- viridis::viridis(n = nsim, begin = 0, end = 0.9, direction = -1, alpha = 0.25, option = myPalette)

png("ScoreVsGen-GEO.png", width = 4, height = 4.3, units = "in", res = 300)
par(mfrow = c(1,1), mar = c(4,4,2,1), lwd=1, cex=1)

my.ylab <- latex2exp::TeX("$Delta$ AMBER Score (-)")
plot(x = x, y = G60.median,
     ylim = c(-0.01,0.01),
     type = "l",
     xlab = "Generation",
     ylab = my.ylab,
     col = my.col[1])

y_polygon <- c(G60.max, rev(G60.min))
polygon(x_polygon, y_polygon, col = my.col.pol[1], border = NA)

lines(x = x, y = G200.median, col = my.col[2])
y_polygon <- c(G200.max, rev(G200.min))
polygon(x_polygon, y_polygon, col = my.col.pol[2], border = NA)


x <- 1:25
x_polygon <- c(x, rev(x))
lines(x = x, y = I1901.median, col = my.col[3])
y_polygon <- c(I1901.max, rev(I1901.min))
polygon(x_polygon, y_polygon, col = my.col.pol[3], border = NA)

abline(h = 0, col = "grey", lty = 2)

legend("bottomright", legend = c("P28R6N20G60Geo", "P28R6N20G200Geo", "P28R6N20G60GeoI1901"), text.col = my.col, bty = "n")

dev.off()

# Check change in mean
data <- readRDS("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation02/object.rds")
plot(data.frame(data@summary)$median, type = "l")

#-------------------------------------------------------------------------------
# Show how best parameter values change from generation to generation
#-------------------------------------------------------------------------------
# data <- P28R6N20G200Geo
data <- P28R6N20G60GeoI1901 
l <- data@bestSol

# Create column names
nP <- length(l[[1]]) # number of Parameters
my.colnames <-paste("P", 1:nP, sep = "")

# Initialize an empty data frame
df <- data.frame()

# Loop through each matrix and each element to create the data frame
for (i in 1:length(l)) {
  mat <- l[[i]]
  values <- c(mat)
  df <- rbind(df, values)
}
colnames(df) <- my.colnames


png("parValuesVsGen-GEO.png", width = 6, height = 4, units = "in", res = 300)
par(mfrow = c(1,1), mar = c(4,4,1,1), lwd=1, cex=1)

myPalette <- "plasma"
my.col.vmax <- viridis::viridis(n = 9, begin = 0, end = 1, direction = 1, option = myPalette)

my.col <- rgb(0.5, 0.5, 0.5, alpha = 0.2)  # Semi-transparent blue color

plot(x = 1:nrow(df), y = unlist(df[1]), xlim = c(1,70), ylim = c(0,1), col = NA,
     xlab = "Generation",
     ylab = "Normalized Parameter Values (-)")

my.legend <- c(
  "NDL-EVG",
  "NDL-DCD",
  "BDL-EVG",
  "BDL-DCD-CLD",
  "BDL-DCD-DRY",
  "GRASS-C3",
  "GRASS-C4",
  "CROP-C3",
  "CROP-C4")

legend("right", legend = my.legend, text.col = my.col.vmax, horiz = FALSE, bty = "n")

for (i in 1:ncol(df)) 
{
  lines(unlist(df[i]), col = my.col)
  lines(unlist(df[1]), col = my.col.vmax[1])
  lines(unlist(df[5]), col = my.col.vmax[2])
  lines(unlist(df[2]), col = my.col.vmax[3])
  lines(unlist(df[6]), col = my.col.vmax[4])
  lines(unlist(df[9]), col = my.col.vmax[5])
  lines(unlist(df[4]), col = my.col.vmax[6])
  lines(unlist(df[8]), col = my.col.vmax[7])
  lines(unlist(df[3]), col = my.col.vmax[8])
  lines(unlist(df[7]), col = my.col.vmax[9])
}

dev.off()


#-------------------------------------------------------------------------------
# Scatterplot that compares optimized against default normalized parameter values
#-------------------------------------------------------------------------------

data <- P28R6N20G60GeoI1901 
# data <- data@bestSol
# optimized <- c(data[[13]]) # Change number to most recent run
optimized <- c(data@solution)

# Read normalized default files produced in run_daisy.R
df <- read.table("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation02/normParameterValues")
default <- df$x
data <- data.frame(default, optimized)
vmax <- data[1:9,]

my.col.grey <- rgb(0.5, 0.5, 0.5, alpha = 0.2)  # Semi-transparent blue color

png("parameterValuesScatterPlot.png", width = 4, height = 4, units = "in", res = 300)
par(mfrow = c(1,1), mar = c(4,4,1,1), lwd=1, cex=1)
plot(x = data$default, y = data$optimized,
     xlab = "Normalized Default Parameter Values (-)",
     ylab = "Normalized Optimized Parameter Values (-)",
     xlim = c(0,1), ylim = c(0,1), asp=1, pch = 16, col = my.col.grey)
points(vmax$default[1], vmax$optimized[1], col = my.col.vmax[1], pch = 0, cex = 1.5)
points(vmax$default[5], vmax$optimized[5], col = my.col.vmax[2], pch = 1, cex = 1.5)
points(vmax$default[2], vmax$optimized[2], col = my.col.vmax[3], pch = 2, cex = 1.5)
points(vmax$default[6], vmax$optimized[6], col = my.col.vmax[4], pch = 3, cex = 1.5)
points(vmax$default[9], vmax$optimized[9], col = my.col.vmax[5], pch = 4, cex = 1.5)
points(vmax$default[4], vmax$optimized[4], col = my.col.vmax[6], pch = 5, cex = 1.5)
points(vmax$default[8], vmax$optimized[8], col = my.col.vmax[7], pch = 6, cex = 1.5)
points(vmax$default[3], vmax$optimized[3], col = my.col.vmax[8], pch = 7, cex = 1.5)
points(vmax$default[7], vmax$optimized[7], col = my.col.vmax[9], pch = 8, cex = 1.5)

abline(0,1)

my.legend <- c(
  "NDL-EVG",
  "NDL-DCD",
  "BDL-EVG",
  "BDL-DCD-CLD",
  "BDL-DCD-DRY",
  "GRASS-C3",
  "GRASS-C4",
  "CROP-C3",
  "CROP-C4")

legend("bottomright", legend = my.legend, text.col = my.col.vmax, horiz = FALSE, bty = "n", cex = 0.75,
       pch = c(0:8), col = my.col.vmax)

dev.off()


