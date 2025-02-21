
rm(list = ls())
setwd("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/analysis/figures")


# Part I

# default values
default <- matrix(c(4.2e-05, 3.5e-05, 5.5e-05, 5.5e-05, 4.7e-05, 5.7e-05, 4.0e-05, 1.5e-05, 4.0e-05), ncol = 9)

# uncertainty ranges
lowerBound <- read.csv("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/vmax.min.csv", header = TRUE) #  * 10^(-6)
upperBound <- read.csv("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/vmax.max.csv", header = TRUE) #  * 10^(-6)
parameterName <- "vmax"

lowerBound <- matrix(lowerBound[lowerBound!=0], ncol = 9) 
upperBound <- matrix(upperBound[upperBound!=0], ncol = 9)

ylab <- latex2exp::TeX("Maximum Carboxylation Rate ($\\mu$mol CO$_2$ m$^{-2}$ s$^{-1}$)")
factor <- 10^6 # unit conversion factor
PFTs <- c("NDL-EVG", "BDL-EVG", "CROP-C3", "GRASS-C3", "NDL-DCD", "BDL-DCD-CLD", "CROP-C4", "GRASS-C4", "BDL-DCD-DRY")

# GPP-CLASSIC
# Get results and omit duplicates that originate because runs have been restarted
data <- read.table("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/analysis/GPP-CLASSIC/daisyOutput_GPP-CLASSIC")
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
best <- best[7:15]
all <- data[7:15]

colnames(best) <- PFTs
colnames(default) <- PFTs
colnames(upperBound) <- PFTs
colnames(lowerBound) <- PFTs
colnames(all) <- PFTs

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
  
  q975 <- apply(X = pv, MARGIN = 2, FUN = function(x) {quantile(x,probs = c(.975),na.rm=TRUE)})
  q75 <- apply(X = pv, MARGIN = 2, FUN = function(x) {quantile(x,probs = c(.75),na.rm=TRUE)})
  q25 <- apply(X = pv, MARGIN = 2, FUN = function(x) {quantile(x,probs = c(.25),na.rm=TRUE)})
  q025 <- apply(X = pv, MARGIN = 2, FUN = function(x) {quantile(x,probs = c(.025),na.rm=TRUE)})
  
  quantiles <- rbind(q975, q75, q25, q025)
  best <- unlist(bda[1,])
  default <- unlist(bda[2,])
  upperBound <- unlist(bda[3,])
  lowerBound <- unlist(bda[4,])
  
  data01 <- list(best, default, quantiles)
  
  # Part II
  
  # default values
  default <- matrix(c(4.2e-05, 3.5e-05, 5.5e-05, 5.5e-05, 4.7e-05, 5.7e-05, 4.0e-05, 1.5e-05, 4.0e-05), ncol = 9)
  
  # uncertainty ranges
  lowerBound <- read.csv("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/vmax.min.csv", header = TRUE) #  * 10^(-6)
  upperBound <- read.csv("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/vmax.max.csv", header = TRUE) #  * 10^(-6)
  parameterName <- "vmax"
  
  lowerBound <- matrix(lowerBound[lowerBound!=0], ncol = 9) 
  upperBound <- matrix(upperBound[upperBound!=0], ncol = 9)
  
  ylab <- latex2exp::TeX("Maximum Carboxylation Rate ($\\mu$mol CO$_2$ m$^{-2}$ s$^{-1}$)")
  factor <- 10^6 # unit conversion factor
  PFTs <- c("NDL-EVG", "BDL-EVG", "CROP-C3", "GRASS-C3", "NDL-DCD", "BDL-DCD-CLD", "CROP-C4", "GRASS-C4", "BDL-DCD-DRY")
  
  # GPP-CLASSIC
  # Get results and omit duplicates that originate because runs have been restarted
  data <- read.table("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/analysis/ALBS-GPP-HFLS-HFSS-LAI-TS-P50-CLASSIC/daisyOutput_GPP-CLASSIC")
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
  best <- best[7:15]
  all <- data[7:15]
  
  colnames(best) <- PFTs
  colnames(default) <- PFTs
  colnames(upperBound) <- PFTs
  colnames(lowerBound) <- PFTs
  colnames(all) <- PFTs
  
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
  
  q975 <- apply(X = pv, MARGIN = 2, FUN = function(x) {quantile(x,probs = c(.975),na.rm=TRUE)})
  q75 <- apply(X = pv, MARGIN = 2, FUN = function(x) {quantile(x,probs = c(.75),na.rm=TRUE)})
  q25 <- apply(X = pv, MARGIN = 2, FUN = function(x) {quantile(x,probs = c(.25),na.rm=TRUE)})
  q025 <- apply(X = pv, MARGIN = 2, FUN = function(x) {quantile(x,probs = c(.025),na.rm=TRUE)})
  
  quantiles <- rbind(q975, q75, q25, q025)
  best <- unlist(bda[1,])
  default <- unlist(bda[2,])
  upperBound <- unlist(bda[3,])
  lowerBound <- unlist(bda[4,])
  
  data02 <- list(best, default, quantiles)
  

  
  my.ylim <- c(min(lowerBound), max(upperBound)) * factor
  my.ylab <- latex2exp::TeX("Maximum Carboxylation Rate ($\\mu$mol CO$_2$ m$^{-2}$ s$^{-1}$)")
  
  png("syntheticDataTest.png", width = 12, height = 6, units = "in", res = 300)
  par(mfrow = c(1, 2), mar = c(8,4,1,1), pty="s")
  
  # (a) 
  best <- data01[[1]]
  default <- data01[[2]]
  quantiles <- data01[[3]]
  
  boxplot(quantiles * factor, lty = 1, las = 3, ylab = my.ylab, ylim = my.ylim)
  points(default * factor, pch = 17, col = "blue", cex = 2)
  points(best * factor, pch = 16, col = "red", cex = 2)
  points(upperBound * factor, pch = 1, cex = 1)
  points(lowerBound * factor, pch = 1, cex = 1)
  legend("bottomleft", pch = c(17, 16, 1), col = c("blue", "red", "black"), c("Default", "Optimized", "Range"), bty = "n")
  legend("topleft", "(a)", bty = "n")
  
  # (b) 
  best <- data02[[1]]
  default <- data02[[2]]
  quantiles <- data02[[3]]
  
  boxplot(quantiles * factor, lty = 1, las = 3, ylab = my.ylab, ylim = my.ylim)
  points(default * factor, pch = 17, col = "blue", cex = 2)
  points(best * factor, pch = 16, col = "red", cex = 2)
  points(upperBound * factor, pch = 1, cex = 1)
  points(lowerBound * factor, pch = 1, cex = 1)
  legend("bottomleft", pch = c(17, 16, 1), col = c("blue", "red", "black"), c("Default", "Optimized", "Range"), bty = "n")
  legend("topleft", "(b)", bty = "n")
  
  
  dev.off()
  
