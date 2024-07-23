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
  "S6C1M1",
  "S3C5M2")

experiment <- list()

for (i in experiments) {

  myFile <- file.path(dir, i, "objectFinal.rds")
  if (file.exists(myFile)) {
    data  <- readRDS(myFile)
  } else {
    data  <- readRDS(file.path(dir, i, "object.rds"))
  }
  
experiment[[i]] <- data
}

png("ScoreVsGen-SelCroMut.png", width = 8, height = 4.3, units = "in", res = 300)
par(mfrow = c(1,1), mar = c(4,4,2,1), lwd=1, cex=1)

myPalette <- "plasma"
my.col <- viridis::viridis(n = length(experiments), begin = 0, end = 1, direction = 1, option = myPalette)

object <- experiment[[1]]
df <- data.frame(object@summary)
y <- df$median

plot(y, type = "l", col = NA, 
     ylim = c(0.64, 0.67),
     xlim = c(1,23),
     xlab = "Generation",
     ylab = "Median AMBER Score (-)")


for (i in 1:length(experiment)) 
{
  object <- experiment[[i]]
  df <- data.frame(object@summary)
  # y <- df$median
  y <- df$max
  lines(y, col = my.col[i])
  points(y, pch = i, col = my.col[i])
  print(y)
}

legend("right", experiments,  text.col = my.col, pch = 1:length(experiment), col = my.col, bty = "n")
legend("topleft", c("Population Size: 100", "Gridcells: 40", "Period: 2001-2010"), bty = "n")
dev.off()

# Get maximum value of each run and then rank 

max.score <- numeric()

for (i in 1:length(experiment)) 
{
  object <- experiment[[i]]
  df <- data.frame(object@summary)
  y <- df$max
  y.max <- max(y, na.rm = TRUE)
  max.score[i] <-  y.max
  
}

df <- data.frame(experiments, max.score)
df <- df[order(df$max.score, decreasing = TRUE), ]
print(df)

plot(x = 1: length(df$max.score), 
     y = df$max.score,
     )

# Best candidates so far:
# S = 3
# C = 5
# M = 2

# difference:
delta <- max(df$max.score) - min(df$max.score)

# default
type <- "real-valued"

gaControl(type)$selection # -> S = 5
# [1] "gareal_lsSelection"

gaControl(type)$crossover # -> C = 3
# [1] "gareal_laCrossover" 

gaControl(type)$mutation # -> M = 1
# [1] "gareal_raMutation"

selection <- c(
  "gareal_lrSelection",
  "gareal_nlrSelection", 
  "gareal_rwSelection",
  "gareal_tourSelection", 
  "gareal_lsSelection", 
  "gareal_sigmaSelection")

crossover <- c(
  "gareal_spCrossover",
  "gareal_waCrossover",
  "gareal_laCrossover",
  "gareal_blxCrossover",
  "gareal_laplaceCrossover")

mutation <- c(
  "gareal_raMutation",
  "gareal_nraMutation",
  "gareal_rsMutation",
  "gareal_powMutation")

# S=5 already yields one of the best results, so I think that changing this to S3C5M3 will not cause a big difference
# But the larger population seems to make a big difference, so that is good
