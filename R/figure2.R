
## Load libraries
library(here)
library(dplyr)
library(tidyr)
library(naniar) ## for missing data 
library(gt)
library(gtExtras)
library(dendextend)

# Load the first iteration of the intermediate IDA ready data sets
ADSL <- readRDS(here::here("data", "IDA", "ADSL_01.rds"))
ADLB <- readRDS(here::here("data", "IDA", "ADLB_01.rds"))

# source functions 
source(here("R", "M1-predictor-missing.R"))
source(here("R", "M2-complete-cases.R"))
source(here("R", "M3-complete-cases-sets.R"))
source(here("R", "M3-dendogram.R"))


tiff(filename = "fig2.tiff", units="mm", width=170, height=225, compression = 'lzw', res=300)

m3_dendogram(ADLB)

dev.off()
