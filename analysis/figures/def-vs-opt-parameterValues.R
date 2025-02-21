.libPaths(c("/home/cseiler/daisy/renv", "/home/cseiler/AMBER/renv/library/R-4.3/x86_64-pc-linux-gnu"))
library(daisy)
library(viridis)

rm(list = ls())
setwd("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/analysis")

library(GA)

#-------------------------------------------------------------------------------
# Scatterplot that compares optimized against default normalized parameter values
#-------------------------------------------------------------------------------

data <- readRDS("ALBS-GPP-HFLS-HFSS-LAI-TS-PAR28-GC160-P100-I1701/objectFinal.rds")
# data <- data@bestSol
# optimized <- c(data[[13]]) # Change number to most recent run
optimized <- c(data@solution)

# Read normalized default files produced in run_daisy.R
df <- read.table("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/normParameterValues")
default <- df$x
data <- data.frame(default, optimized)
vmax <- data[1:9,]

my.col.grey <- rgb(0.5, 0.5, 0.5, alpha = 0.2)  # Semi-transparent color

myPalette <- "plasma"
my.col.vmax <- viridis::viridis(n = 9, begin = 0, end = 1, direction = 1, option = myPalette)

png("figures/def-vs-opt-parameterValues.png", width = 3.8, height = 8, units = "in", res = 300)
par(mfrow = c(2,1), mar = c(4,4,2,1), lwd=1, cex=1)
plot(x = data$default, y = data$optimized,
     main = "(a)",
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

# dev.off()


#-------------------------------------------------------------------------------
# Show how best parameter values change from generation to generation
#-------------------------------------------------------------------------------
data <- readRDS("ALBS-GPP-HFLS-HFSS-LAI-TS-PAR28-GC160-P100-I1701/objectFinal.rds")
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


# png("figures/evolution.png", width = 6, height = 4, units = "in", res = 300)
# par(mfrow = c(1,1), mar = c(4,4,1,1), lwd=1, cex=1)

myPalette <- "plasma"
my.col.vmax <- viridis::viridis(n = 9, begin = 0, end = 1, direction = 1, option = myPalette)

my.col <- rgb(0.5, 0.5, 0.5, alpha = 0.2)  # Semi-transparent blue color

plot(x = 1:nrow(df), y = unlist(df[1]), xlim = c(1,26), ylim = c(0,1), col = NA,
     main = "(b)",
     xlab = "Generation",
     ylab = "Normalized Parameter Values (-)")

# my.legend <- c(
#  "NDL-EVG",
#  "NDL-DCD",
#  "BDL-EVG",
#  "BDL-DCD-CLD",
#  "BDL-DCD-DRY",
#  "GRASS-C3",
#  "GRASS-C4",
#  "CROP-C3",
#  "CROP-C4")

# legend("right", legend = my.legend, text.col = my.col.vmax, horiz = FALSE, bty = "n")

for (i in 1:ncol(df)) 
{
  lines(unlist(df[i]), col = my.col)
}
  lines(unlist(df[1]), col = my.col.vmax[1])
  lines(unlist(df[5]), col = my.col.vmax[2])
  lines(unlist(df[2]), col = my.col.vmax[3])
  lines(unlist(df[6]), col = my.col.vmax[4])
  lines(unlist(df[9]), col = my.col.vmax[5])
  lines(unlist(df[4]), col = my.col.vmax[6])
  lines(unlist(df[8]), col = my.col.vmax[7])
  lines(unlist(df[3]), col = my.col.vmax[8])
  lines(unlist(df[7]), col = my.col.vmax[9])


  points(26, unlist(df[1])[25], col = my.col.vmax[1], pch = 0)
  points(26, unlist(df[5])[25], col = my.col.vmax[2], pch = 1)
  points(26, unlist(df[2])[25], col = my.col.vmax[3], pch = 2)
  points(26, unlist(df[6])[25], col = my.col.vmax[4], pch = 3)
  points(26, unlist(df[9])[25], col = my.col.vmax[5], pch = 4)
  points(26, unlist(df[4])[25], col = my.col.vmax[6], pch = 5)
  points(26, unlist(df[8])[25], col = my.col.vmax[7], pch = 6)
  points(26, unlist(df[3])[25], col = my.col.vmax[8], pch = 7)
  points(26, unlist(df[7])[25], col = my.col.vmax[9], pch = 8)


dev.off()


