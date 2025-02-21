.libPaths(c("/home/cseiler/daisy/renv", "/home/cseiler/AMBER/renv/library/R-4.3/x86_64-pc-linux-gnu"))
library(daisy)
library(viridis)

rm(list = ls())
setwd("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/analysis")

library(GA)

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


png("figures/evolution.png", width = 6, height = 4, units = "in", res = 300)
par(mfrow = c(1,1), mar = c(4,4,1,1), lwd=1, cex=1)

myPalette <- "plasma"
my.col.vmax <- viridis::viridis(n = 9, begin = 0, end = 1, direction = 1, option = myPalette)

my.col <- rgb(0.5, 0.5, 0.5, alpha = 0.2)  # Semi-transparent blue color

plot(x = 1:nrow(df), y = unlist(df[1]), xlim = c(1,35), ylim = c(0,1), col = NA,
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
