rm(list=ls())

outputDir <- "/home/cseiler/projects/def-cseiler-ab/cseiler/CLASSIC_CanESM/summaries"
setwd(outputDir)

data <- read.table("scoreMatrix")

def <- read.table("../")

# baseline
# title <- "(a) Quasi-observed forcing"
# x <- data$CRUJRAv2
# y <- data$GSWP3.W5E5
# x.lab <- "CRUJRAv2"
# y.lab <- "GSWP3-W5E5"
# a <- list(title, x, y, x.lab, y.lab)


# bias
title <- "(a) Impact of bias (CRUJRAv2)"
x <- data$CRUJRAv2
y <- data$CanESM5.hist
x.lab <- "CRUJRAv2"
y.lab <- "CanESM5-hist"
a <- list(title, x, y, x.lab, y.lab)

# bias
title <- "(b) Impact of bias (GSWP3-W5E5)"
x <- data$GSWP3.W5E5
y <- data$CanESM5.hist
x.lab <- "GSWP3-W5E5"
y.lab <- "CanESM5-hist"
b <- list(title, x, y, x.lab, y.lab)

# bias adjustment
title <- "(c) Impact of bias adjustment"
x <- data$CanESM5.hist
y <- data$CanESM5.ISIMIP3b.hist
x.lab <- "CanESM5-hist"
y.lab <- "CanESM5biasAdj-hist"
c <- list(title, x, y, x.lab, y.lab)

# variable that deteriorates most
title <- "(d) Impact of precipitation"
x <- data$CanESM5.hist
y <- data$CanESM5.CRUJRAv2.PRE.hist
x.lab <- "CanESM5-hist"
y.lab <- "CanESM5-CRUJRAv2.PRE-hist"
d <- list(title, x, y, x.lab, y.lab)

# N-cycle: CRUJRAv2
title <- "(e) Impact of N cycle for CRUJRAv2"
x <- data$CRUJRAv2
y <- data$CRUJRAv2.NCycleV2
x.lab <- "CRUJRAv2"
y.lab <- "CRUJRAv2-NCycle"
e <- list(title, x, y, x.lab, y.lab)

# N-cycle: CanESM5
title <- "(f) Impact of N cycle for CanESM5-hist"
x <- data$CanESM5.hist
y <- data$CanESM5.NCycleV2.hist
x.lab <- "CanESM5-hist"
y.lab <- "CanESM5-NCycle-hist"
f <- list(title, x, y, x.lab, y.lab)

panels <- list(a, b, c, d, e, f)


my.filename <- "scoreMatrixHist"
plot.width <- 14
plot.height <- 10
grDevices::png(paste(outputDir, "/", my.filename, ".png", sep = ""), width = plot.width,
               height = plot.height, units = "in", res = 300)
graphics::par(mfrow = c(2, 3), font.main = 1, mar = c(5, 4, 3, 2), oma = c(1,1,1,1), lwd = 1, cex = 1)


for (i in 1:length(panels)) {
  
  data <- panels[[i]]
  myTitle <- data[[1]]
  x <- data[[2]]
  y <- data[[3]]
  x.lab <- data[[4]]
  y.lab <- data[[5]]
  
  delta <- y - x
  
  frequency.pos <- length(delta[delta > 0])
  frequency.neg <- length(delta[delta < 0])
  
  
  hist(
    delta,
    main = myTitle,
    xlim = c(-0.1, 0.1),
    ylim = c(0, 15),
    breaks = seq(-1, 1, 0.02),
    las = 1,
    xlab = paste(y.lab, "minus", x.lab, sep = " ")
  )
  
  legend("topleft", "Negative values:", frequency.neg, bty = 'n')
  legend("topright", "Positive values:", frequency.pos, bty = 'n')
  
  abline(v = 0, lwd = 1, lty = 1)
  
  box()
  
  # graphics::axis(3, labels = FALSE, tcl = 0.5, cex.axis = 1.5)
  # graphics::axis(4, labels = FALSE, tcl = 0.5, las = 2, cex.axis = 1.5)
  
  
}
dev.off()


# CRUJRAv2
# CRUJRAv2.NCycleV2
# GSWP3.W5E5 
# CanESM5.hist 
# CanESM5.NCycleV2.hist 
# CanESM5.CRUJRAv2.RSDS.hist 
# CanESM5.CRUJRAv2.RLDS.hist 
# CanESM5.CRUJRAv2.TAS.hist 
# CanESM5.CRUJRAv2.PRE.hist 
# CanESM5.CRUJRAv2.HUSS.hist 
# CanESM5.CRUJRAv2.WIND.hist 
# CanESM5.CRUJRAv2.PS.hist 
# CanESM5.ISIMIP3b.hist 
# CanESM5.ISIMIP3b.NCycleV2.hist
