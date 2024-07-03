# To redirect the output to a textfile, do:
# Rscript run_daisy.R > output.txt

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

# vmax(1:3)   = 42.0e-06, 47.0e-06, 0.00e-06,
# vmax(4:6)   = 35.0e-06, 57.0e-06, 40.0e-06,
# vmax(7:9)   = 55.0e-06, 40.0e-06, 0.00e-06,
# vmax(10:12) = 55.0e-06, 15.0e-06, 0.00e-06,


.libPaths("/home/cseiler/daisy/renv")
library(daisy)
rm(list = ls())

setwd('/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation')

# Copy default parameter run_parameters.txt file to current work directory
# This file will be overwritten with new values before CLASSIC is run

system('cp /home/cseiler/classic/classic_code/classic/configurationFiles/default_run_parameters.txt run_parameters.txt')

# Select parameters and obtain their parameter values
parameterFile <- '/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/run_parameters.txt'

parameterNames <- list("vmax", "kn", "thrprcnt", "alpha_phtsyn", "lfespany", "grescoef", 
                "avertmas", "mxrtdpth", "sn", "XLEAF", "maxage", "ZOLNG", 
                "coldthrs", "alpha", "gamma_w", "beta2", "kappa", "minslai", 
                "coldlmt", "roothrsh", "abar", "vpd0", "albnir", "TCSAND", 
                "TCCLAY", "ZOLNS", "minlvfr", "omega")

# parameterNames <- list("vmax", "kn")

parameterValues <- list()

for (i in parameterNames) {
  parameters <- getParameterValues(parameterFile = parameterFile, parameterName = i)
  parameterValues[[i]] <- parameters
}

# CLASSIC has 12 PFTs, however, the default configuration only uses 9 PFTs.
# The PFTs that are not being used have parameter values that are equal to zero.
# Those values need to be excluded, otherwise the optimization will not work


# For single parameter
# non_zero_indices <- which(parameterValues != 0, arr.ind = TRUE)
# parameterValues <- parameterValues[non_zero_indices]

# Apply which() function to each element of the list
# non_zero_indices <- lapply(parameterValues, function(x) which(x != 0))

# Extract non-zero values using the obtained indices
# non_zero_values <- lapply(seq_along(parameterValues), function(i) parameterValues[[i]][non_zero_indices[[i]]])

# Print the result
# print(non_zero_values)

# non_zero_indices <- lapply(parameterValues, function(x) which(x != 0, arr.ind = TRUE))
# non_zero_values <- lapply(seq_along(parameterValues), function(i) parameterValues[[i]][non_zero_indices[[i]]])

# parameterValues <- non_zero_values

# uncertainty ranges
min.csv <- paste(parameterNames, "min.csv", sep = ".")
max.csv <- paste(parameterNames, "max.csv", sep = ".")

# Read all CSV files into a list of data frames
min.csv.list <- lapply(min.csv, read.csv)
max.csv.list <- lapply(max.csv, read.csv)

# Check if number of parameter values per parameter is consistent between min and max values:
length.min <- length(unlist(min.csv.list))
length.max <- length(unlist(max.csv.list))

if (length.max != length.min) {
  stop("Error: The lengths of the minimum and maximum parameter values are not equal. Script stopped.")
}

upperBound <- max.csv.list
lowerBound <- min.csv.list

# Convert 0 to NA

 upperBound <- lapply(upperBound, function(x) {
  if(length(x) == 12){
   x[x == 0] <- NA
  }
  return(x)
})

lowerBound <- lapply(lowerBound, function(x) {
  if(length(x) == 12){
    x[x == 0] <- NA
  }
  return(x)
})

normalization <- list()

for (p in 1:length(parameterValues)) {
  df <- parameterValues[p] # default parameter values
  ub <- upperBound[p] # upper bounds per parameter
  lb <- lowerBound[p] # lower bounds per parameter
  
  # Check whether l <= df <= u
  condition_satisfied <- mapply(function(lb, df, ub) lb <= df & df <= ub, lb, df, ub)
  
  if (all(condition_satisfied, na.rm = TRUE)) {
    print(parameterNames[[p]])
    print("low < def < up satisfied")
  } else {
    print(parameterNames[[p]])
    print("low < def < up not satisfied")
  }
  
  result <- mapply(intFun.normalize, df, ub, lb)
  result <- unlist(result)
  result <- result[!is.na(result)]
  normalization[[p]] <- result
}

parameterValueLength <- sapply(normalization, length)
normParameterValues <- unlist(normalization)
upperBound <- unlist(upperBound)
lowerBound <- unlist(lowerBound) 

# Drop all NaNs
normParameterValues <- normParameterValues[!is.na(normParameterValues)]
upperBound <- upperBound[!is.na(upperBound)]
lowerBound <- lowerBound[!is.na(lowerBound)]

upper <- upperBound-upperBound + 1
lower <- lowerBound-lowerBound

# Omit column names
names(normParameterValues)  <- NULL
names(upperBound) <- NULL
names(lowerBound) <- NULL
names(upper) <- NULL
names(lower) <- NULL

write.table(x = normParameterValues, file = "normParameterValues")

run_classic_file <- "/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/run_classic.sh"
# run_classic_file <- "/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/test.sh"

# rm(normParameterValues)

# gpp_monthly.nc
# lai_monthly.nc  
# hfls_monthly.nc  
# hfss_monthly.nc  
# ts_monthly.nc
# albs_monthly.nc

dir.mod <- "/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/simulations/daisyRun/netcdf_files"
dir.ref <- "/home/cseiler/projects/def-cseiler-ab/cseiler/reference_T63"

nc.mod01 <- file.path(dir.mod, "gpp_monthly.nc")
nc.mod02 <- file.path(dir.mod, "lai_monthly.nc")
nc.mod03 <- file.path(dir.mod, "hfls_monthly.nc")
nc.mod04 <- file.path(dir.mod, "hfss_monthly.nc")
nc.mod05 <- file.path(dir.mod, "ts_monthly.nc")
nc.mod06 <- file.path(dir.mod, "albs_monthly.nc")

# GPP
nc.ref01a <- file.path(dir.ref, "gpp_GOSIF_128x64.nc")
nc.ref01b <- file.path(dir.ref, "GPP.ensembleMedian.CRUNCEPv6.monthly_128x64.nc")

# LAI
nc.ref02a <- file.path(dir.ref, "lai_AVHRR_128x64.nc")
nc.ref02b <- file.path(dir.ref, "lai_copernicus_128x64.nc")
nc.ref02c <- file.path(dir.ref, "lai_MODIS_128x64_LiboWang.nc")

# HFLS
nc.ref03a <- file.path(dir.ref, "CLASS_v1.1_hfls.nc")
nc.ref03b <- file.path(dir.ref, "LE.RS_METEO.EBC-ALL.MLM-ALL.METEO-ALL.720_360.monthly_128x64.nc")

# HFSS
nc.ref04a <- file.path(dir.ref, "CLASS_v1.1_hfss.nc")
nc.ref04b <- file.path(dir.ref, "H.RS_METEO.EBC-ALL.MLM-ALL.METEO-ALL.720_360.monthly_128x64.nc")

# LST
nc.ref05a <- file.path(dir.ref, "MOYD11C3_128x64.nc")

# ALBS
nc.ref06a <- file.path(dir.ref, "albs_CERES_128x64.nc")
nc.ref06b <- file.path(dir.ref, "albs_GEWEXSRB_128x64.nc")
nc.ref06c <- file.path(dir.ref, "albedo_MODIS_128x64.nc")




mod.list <- list(
  nc.mod01, nc.mod01,
  nc.mod02, nc.mod02, nc.mod02,
  nc.mod03, nc.mod03,
  nc.mod04, nc.mod04,
  nc.mod05,
  nc.mod06, nc.mod06, nc.mod06
)

ref.list <- list(
  nc.ref01a, nc.ref01b,
  nc.ref02a, nc.ref02b, nc.ref02c,
  nc.ref03a, nc.ref03b,
  nc.ref04a, nc.ref04b,
  nc.ref05a,
  nc.ref06a, nc.ref06b, nc.ref06c
)

# Unit conversion factors for reference data

ref.unit.conv.list <- list(
  1/86400000, 1/86400000,
  1,1,1,
  1, (10^6/86400),
  1, (10^6/86400),
  1,
  1,1,1
)


ref.id.list <- list("GPP-GOSIF", "GPP-FLUXCOM",
                    "LAI-AVHRR", "LAI-Copernicus", "LAI-MODIS",
                    "HFLS-CLASSr", "HFLS-FLUXCOM",
                    "HFSS-CLASSr", "HFSS-FLUXCOM",
                    "TS-MODIS",
                    "ALBS-CERES", "ALBS-GEWEXSRB", "ALBS-MODIS")

modelOutputFolder <- "/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/simulations/daisyRun"

library("GA")

source("ga_daisy.R")

setwd("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation")

# Different options for selection, crossover, and mutation:

# Selection:
# gareal_lrSelection: Linear-rank selection (default)
# gareal_nlrSelection: Nonlinear-rank selection
# gareal_rwSelection: roulette wheel selection
# gareal_tourSelection: (Unbiased) tournament selection
# gareal_lsSelection: Fitness proportional selection with fitness linear scaling
# gareal_sigmaSelection Fitness proportional selection with Goldberg’s sigma truncation scaling

# Crossover
# gareal_spCrossover: Single-point crossover
# gareal_uCrossover: Uniform crossover
# gareal_waCrossover: Whole arithmetic crossover
# gareal_laCrossover: Local arithmetic crossover (default)
# gareal_blxCrossover: Blend crossover

# Mutation
# gareal_raMutation: Uniform random mutation (default)
# gareal_nraMutation: Nonuniform random mutation
# gareal_rsMutation: Random mutation around the solution



selection <- c(
  "gareal_lrSelection",
  "gareal_nlrSelection",
  "gareal_rwSelection",
  "gareal_tourSelection",
  "gareal_lsSelection",
  "gareal_sigmaSelection")

crossover <- c(
  "gareal_spCrossover",
  "gareal_uCrossover",
  "gareal_waCrossover",
  "gareal_laCrossover",
  "gareal_blxCrossover")

mutation <- c(
  "gareal_raMutation",
  "gareal_nraMutation",
  "gareal_rsMutation")

# default: selection = 1, crossover = 4, mutation = 1 (S1C4M1)
# options S1-6, C1-5, M1-3

# default
# S1C4M1

# selection
# S2C4M1
# S3C4M1
# S4C4M1
# S5C4M1
# S6C4M1

# crossover
# S1C1M1
# S1C2M1
# S1C3M1
# S1C5M1

# mutation
# S1C4M2
# S1C4M3


my.selection <- selection[1]
my.crossover <- selection[1]
my.mutation <- selection[1]

result <- ga_daisy(
    type = "real-valued",
    fitness = cost.fun,
    lowerBound = lowerBound,
    upperBound = upperBound,
    parameterValueLength = parameterValueLength,
    parameterNames = parameterNames, 
    parameterFile = parameterFile,
    mod.list = mod.list,
    ref.list = ref.list,
    ref.id.list = ref.id.list,
    ref.unit.conv.list = ref.unit.conv.list,
    run_classic_file = run_classic_file,
    modelOutputFolder = modelOutputFolder,
    lower = lower,
    upper = upper,
    population = "gareal_Population",
    selection = "gareal_tourSelection",
    crossover = "gareal_spCrossover",
    mutation = "gareal_raMutation",
    popSize = 100,
    elitism = 4,
    maxiter = 20,
    run = 20,
    maxFitness = 1,
    suggestions = normParameterValues,
    keepBest = TRUE,
    seed = 1)
saveRDS(result, "result.rds")
