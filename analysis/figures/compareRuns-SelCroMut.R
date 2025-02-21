.libPaths(
  c(
    "/home/cseiler/daisy/renv",
    "/home/cseiler/AMBER/renv/library/R-4.3/x86_64-pc-linux-gnu"
  )
)
library(daisy)
library(viridis)

rm(list = ls())
setwd("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/analysis")

library(GA)

# Optimizing against CLASSIC
dir <-
  "/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation-tests"

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
  "S3C5M2",
  
  "S2C1M2",
  "S3C1M2",
  "S4C1M2",
  "S5C1M2",
  "S6C1M2",
  
  "S5C2M2",
  "S5C3M2",
  "S5C4M2",
  "S5C5M2"
)

S5C1M2 <- readRDS(file.path(dir, "S5C1M2", "object.rds"))
S1C1M1 <- readRDS(file.path(dir, "S1C1M1", "object.rds"))

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

my.col <- rgb(0.5, 0.5, 0.5, alpha = 0.5)  # Semi-transparent color

png(
  "figures/SelCroMut.png",
  width = 10,
  height = 10,
  units = "cm",
  res = 500
)
par(
  mfrow = c(1, 1),
  mar = c(4, 4, 1, 1),
  lwd = 1,
  cex = 1
)

object <- experiment[[1]]
df <- data.frame(object@summary)
y <- df$median

plot(
  y,
  type = "l",
  col = NA,
  ylim = c(0.655, 0.675),
  xlim = c(1, 20),
  xlab = "Generation",
  ylab = "Maximum AMBER Score (-)")


for (i in 1:length(experiment))
{
  object <- experiment[[i]]
  df <- data.frame(object@summary)
  # y <- df$median
  y <- df$max
  lines(y, col = my.col)
}

object <- S5C1M2
df <- data.frame(object@summary)
# y <- df$median
y <- df$max
lines(y, col = "black", lty = 2)

object <- S1C1M1
df <- data.frame(object@summary)
# y <- df$median
y <- df$max
lines(y, col = "red")



#legend(
#  "right",
#  experiments,
#  text.col = my.col,
#  pch = 1:length(experiment),
#  col = my.col,
#  bty = "n",
#  cex = 0.7
#)

legend("topleft",
       c("Population Size: 100", "Gridcells: 40", "Period: 2001-2010"),
       bty = "n")
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
df <- df[order(df$max.score, decreasing = TRUE),]
print(df)

plot(x = 1:length(df$max.score),
     y = df$max.score,)

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
  "gareal_sigmaSelection"
)

crossover <- c(
  "gareal_spCrossover",
  "gareal_waCrossover",
  "gareal_laCrossover",
  "gareal_blxCrossover",
  "gareal_laplaceCrossover"
)

mutation <- c(
  "gareal_raMutation",
  "gareal_nraMutation",
  "gareal_rsMutation",
  "gareal_powMutation"
)

# Chosen combination: S5C1M2
