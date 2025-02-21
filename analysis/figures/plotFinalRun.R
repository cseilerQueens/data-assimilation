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


# Get default score:

files <- c(
"daisyOutput_ALBS-CERES", 
"daisyOutput_ALBS-MODIS", 
"daisyOutput_GPP-GOSIF", 
"daisyOutput_HFLS-FLUXCOM",
"daisyOutput_HFSS-FLUXCOM",  
"daisyOutput_LAI-Copernicus",  
"daisyOutput_TS-MODIS",
"daisyOutput_ALBS-GEWEXSRB",  
"daisyOutput_GPP-FLUXCOM",  
"daisyOutput_HFLS-CLASSr",  
"daisyOutput_HFSS-CLASSr",   
"daisyOutput_LAI-AVHRR",     
"daisyOutput_LAI-MODIS")

score <- numeric(0)

for (i in files) {

  scores <- read.table(paste0("../", i))
  score <- c(score, scores$V1)
}

print(score)

S.default <- mean(score)
print(S.default)

# S.default <-  0.6593508 # in case those files get overwritten


# dir <-"/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/CLASSIC_meta_ARCHIVE"
dir <-"/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/analysis/ALBS-GPP-HFLS-HFSS-LAI-TS-PAR28-GC160-P100-I1701"

  myFile <- file.path(dir, "objectFinal.rds")
  
png(
  "figures/FinalRun.png",
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



object <- readRDS(myFile)
df <- data.frame(object@summary)
y <- df$median
q1 <- df$q1
q3 <- df$q3
max.score <- df$max


# Make an IQR polygon
n <- length(q1)
x_coords <- c(1:n, n:1)
y_coords <- c(q1, rev(q3))

# Plot Results

plot(
  y,
  type = "l",
  col = NA,
  ylim = c(0.63, 0.67),
  xlim = c(1, 25),
  xlab = "Generation",
  ylab = "AMBER Score (-)")

# Draw the shaded polygon
polygon(x_coords, y_coords, col = "grey", border = "grey")

lines(y)
lines(max.score, col = "black", lty = 2)

abline(S.default, 0, col = "red")

legend("bottomright",
       c("Population Size: 100", "Gridcells: 160", "Period: 1701-2020"),
       bty = "n")

legend("right",
       c(
         "Maximum Score",
         "Interquartile Range",
         "Median Score",
         "Default Score"),
       lty = c(2, 1, 1, 1),
       col = c("black", "grey", "black", "red"),
       lwd = 2,
       bty = "n")


dev.off()
