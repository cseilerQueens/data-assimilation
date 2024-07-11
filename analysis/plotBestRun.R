.libPaths("/home/cseiler/daisy/renv")
library(daisy)

rm(list = ls())
setwd("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/analysis")

# order
# modelpft assigns the PFTs to their position in the kk sized matrix for CLASS and CTEM:
# modelpft(1:3)  = 1,     1,     0,      ! CLASS PFT 1 NDL (NdlTr)
#               EVG    DCD
# modelpft(4:6) = 1,     1,     1,       ! CLASS PFT 2 BDL (BdlTr)
#               EVG  DCD-CLD DCD-DRY
# modelpft(7:9)= 1,     1,     0,        ! CLASS PFT 3 CROP (Crop)
#              C3      C4
# modelpft(10:12)= 1,     1,   0,   ! CLASS PFT 4 GRASS (Grass)
#             C3      C4

# default values

# vmax(1:3)   = 42.0e-06, 47.0e-06, 0.00e-06,
# vmax(4:6)   = 35.0e-06, 57.0e-06, 40.0e-06,
# vmax(7:9)   = 55.0e-06, 40.0e-06, 0.00e-06,
# vmax(10:12) = 55.0e-06, 15.0e-06, 0.00e-06,

# kn(1:3)   = 0.50, 0.50, 0.00,
# kn(4:6)   = 0.50, 0.50, 0.50,
# kn(7:9)   = 0.40, 0.48, 0.00,
# kn(10:12) = 0.46, 0.44, 0.00,


# vmax: 4.2e-05 3.5e-05 5.5e-05 5.5e-05 4.7e-05 5.7e-05 4.0e-05 1.5e-05 4.0e-05
# kn: 0.50 0.50 0.40 0.46 0.50 0.50 0.48 0.44 0.50
# "NDL-EVG", "BDL-EVG", "CROP-C3", "GRASS-C3", "NDL-DCD", "BDL-DCD-CLD", "CROP-C4", "GRASS-C4", "BDL-DCD-DRY"

# default values
vmax <- c(4.2e-05, 3.5e-05, 5.5e-05, 5.5e-05, 4.7e-05, 5.7e-05, 4.0e-05, 1.5e-05, 4.0e-05)
kn <- c(0.50, 0.50, 0.40, 0.46, 0.50, 0.50, 0.48, 0.44, 0.50)
default.list <- list(vmax, kn)

# uncertainty ranges
vcmax.min <- read.csv("../vcmax.min.csv", header = TRUE)  * 10^(-6)
vcmax.max <- read.csv("../vcmax.max.csv", header = TRUE)  * 10^(-6)
kn.min <- read.csv("../kn.min.csv", header = TRUE)
kn.max <- read.csv("../kn.max.csv", header = TRUE)
upperBound.list <- list(vcmax.max, kn.max)
lowerBound.list <- list(vcmax.min, kn.min)

parameterName.list <- list("vmax", "kn")

ylab.vmax <- latex2exp::TeX("Maximum Carboxylation Rate ($\\mu$mol CO$_2$ m$^{-2}$ s$^{-1}$)")
ylab.kn <- "Canopy Extinction Coefficient (-)"
ylab.list <- list(ylab.vmax, ylab.kn)

factor.list <- list(10^6, 1) # unit conversion factor

legLoc.list <- list("bottomleft", "topleft")

PFTs <- c("NDL-EVG", "BDL-EVG", "CROP-C3", "GRASS-C3", "NDL-DCD", "BDL-DCD-CLD", "CROP-C4", "GRASS-C4", "BDL-DCD-DRY")

# Get results and omit duplicates that originate because runs have been restarted
data <- read.table("ALBS-GPP-HFLS-HFSS-LAI-TS-CLASSIC/daisyOutput_GPP-CLASSIC")

duplicated(data)
data <- data[!duplicated(data), ]
duplicated(data)

# The first 6 columns present the scores:
# "S", "S_bias", "S_rmse", "S_phase", "S_iav", "S_dist", 
# The next columns present the PFT-specific parameter values, with 9 columns per parameter
# "NDL-EVG", "BDL-EVG", "CROP-C3", "GRASS-C3", "NDL-DCD", "BDL-DCD-CLD", "CROP-C4", "GRASS-C4", "BDL-DCD-DRY"
# The last column gives the date when those values were written to the file

# Find run with highest mean score so far
id <- which.max(data[[1]])
best <- data[id,]
best.vmax <- best[7:15]
best.kn <- best[16:24]
best.list <- list(best.vmax, best.kn)

colnames(best.vmax) <- PFTs
colnames(best.kn) <- PFTs

write.table(best.vmax, "best_vmax.txt")
write.table(best.kn, "best_kn.txt")

all.vmax <- data[7:15]
all.kn <- data[16:24]
all.list <- list(all.vmax, all.kn)

# Create boxplots showing sampled parameter values, default parameter values, and optimized parameter value
for (i in 1:length(best.list)) {
  
  parameterName <- parameterName.list[[i]]
  best <- best.list[[i]]
  default <- default.list[[i]]
  all <- all.list[[i]]
  my.ylab <- ylab.list[[i]]
  factor <- factor.list[[i]]
  upperBound <- unlist(upperBound.list[[i]])
  lowerBound <- unlist(lowerBound.list[[i]])
  legLoc <- legLoc.list[[i]]
  
  bda <- rbind(best, default, upperBound, lowerBound, all) # bda = best, default, all
  colnames(bda) <- PFTs
  
  # Make a more intuitive order
  bda <- data.frame(
    bda["NDL-EVG"],
    bda["NDL-DCD"],
    bda["BDL-EVG"],
    bda["BDL-DCD-CLD"],
    bda["BDL-DCD-DRY"],
    bda["GRASS-C3"],
    bda["GRASS-C4"],
    bda["CROP-C3"],
    bda["CROP-C4"])
  
  best <- as.vector(unlist(best))
  
  # parameter values
  pv <- bda[5:nrow(bda),]
  
  qmax <- apply(X = pv, MARGIN = 2, FUN = max)
  q75 <- apply(X = pv, MARGIN = 2, FUN = function(x) {quantile(x,probs = c(.75),na.rm=TRUE)})
  q25 <- apply(X = pv, MARGIN = 2, FUN = function(x) {quantile(x,probs = c(.25),na.rm=TRUE)})
  qmin <- apply(X = pv, MARGIN = 2, FUN = min)
  
  quantiles <- rbind(qmax, q75, q25, qmin)
  
  best <- unlist(bda[1,])
  default <- unlist(bda[2,])
  upperBound <- unlist(bda[3,])
  lowerBound <- unlist(bda[4,])
  
  my.ylim <- c(min(lowerBound), max(upperBound)) * factor
  
  main.list <- list("(a) Reference Data: CLASSIC", "(b) Reference Data: CLASSIC")
  
  my.main <- main.list[[i]]
  
  png(paste("parameterValues_", parameterName, ".png", sep = ""), width = 6, height = 6, units = "in", res = 300)
  par(mar = c(8,5,2,1))
  
  boxplot(quantiles * factor, lty = 1, las = 3, ylab = my.ylab, ylim = my.ylim, main = my.main)
  points(default * factor, pch = 17, col = "blue", cex = 2)
  points(best * factor, pch = 16, col = "red", cex = 2)
  points(upperBound * factor, pch = 1, cex = 1)
  points(lowerBound * factor, pch = 1, cex = 1)
  legend(legLoc, pch = c(17, 16, 1), col = c("blue", "red", "black"), c("Default", "Optimized", "Range"), bty = "n")
  
  dev.off()
  
}

# Compare the relative difference of vmax versus kn: Is an overestimation of vmax compensated with an overestimation of kn?

