.libPaths(c("/home/cseiler/daisy/renv", "/home/cseiler/AMBER/renv/library/R-4.3/x86_64-pc-linux-gnu"))
library(daisy)
library(viridis)

rm(list = ls())
setwd("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/analysis")

library(GA)

# Optimizing against CLASSIC
dir <- "/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation-tests"

experiments <- c(
  "S1C1M1", 
  "S1C1M2", 
  "S1C1M3", 
  "S1C1M4", 
  "S1C2M1", 
  "S1C3M1", 
  "S1C4M1", 
  "S1C5M1", 
  "S2C1M1", 
  "S3C1M1", 
  "S4C1M1", 
  "S5C1M1", 
  "S6C1M1")

experiment <- list()

for (i in experiments) {
  
data  <- readRDS(file.path(dir, i, "object.rds"))
experiment[[i]] <- data
}

png("ScoreVsGen-SelCroMut.png", width = 8, height = 4.3, units = "in", res = 300)
par(mfrow = c(1,1), mar = c(4,4,2,1), lwd=1, cex=1)

myPalette <- "plasma"
my.col <- viridis::viridis(n = length(experiments), begin = 0, end = 1, direction = 1, option = myPalette)

object <- experiment[[1]]
df <- data.frame(object@summary)
y <- df$median

plot(y, type = "l", col = NA, ylim = c(0.64, 0.67),
     ylab = "AMBER Score (-)")


for (i in 1:length(experiment)) 
{
  object <- experiment[[i]]
  df <- data.frame(object@summary)
  y <- df$median
  y <- df$max
  lines(y, col = my.col[i])
  points(y, pch = i, col = my.col[i])
  print(y)
}

legend("right", experiments,  text.col = my.col, pch = 1:length(experiment), col = my.col, bty = "n")
dev.off()
