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

# dir <-"/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/CLASSIC_meta_ARCHIVE"
dir <-"/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/analysis/ALBS-GPP-HFLS-HFSS-LAI-TS-PAR28-GC160-P100-I1701"

  myFile <- file.path(dir, "objectFinal.rds")
  
png(
  "FinalRun.png",
  width = 10,
  height = 10,
  units = "cm",
  res = 500
)
par(
  mfrow = c(1, 1),
  mar = c(4, 4, 2, 1),
  lwd = 1,
  cex = 1
)



object <- readRDS(myFile)
df <- data.frame(object@summary)
y <- df$median
q1 <- df$q1
q3 <- df$q3

plot(
  y,
  type = "l",
  col = "black",
  ylim = c(0.63, 0.67),
  xlim = c(1, 25),
  xlab = "Generation",
  ylab = "Median AMBER Score (-)")

lines(q1, col = "grey")
lines(q3, col = "grey")

legend("bottomright",
       c("Population Size: 100", "Gridcells: 160", "Period: 1701-2020"),
       bty = "n")
dev.off()
