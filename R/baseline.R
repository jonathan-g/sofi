# calculate the SoFI baseline scenario
library(tidyverse)
library(dplyr)
library(stats)
library(GPArotation)
library(sf)
library(stringr)
library(BBmisc)
library(paran)
library(rprojroot)

proj_root <- find_root(is_rstudio_project)

source(paste0(proj_root, "/R/SoFI_funcs.R"))

# calculate the baseline scenario SoFI
# indicator transformation: averaged by area, "2"
# indicator normalization: z-score normalization, "5"
# pca selection: Kaiser selection, "7"
# pca rotation: Varimax rotation, "11"
# weighting scheme: equal weight sum, "16"
sofi_baseline <- sofi_calc(variable, indicator_transformation = '2', indicator_normalization = '5', pca_selection =  '7', pca_rotation =  '11', weight_scheme = '16')

#saveRDS(sofi_baseline, file = (paste0(proj_root, "/data/gen/sofi_baseline.rds")))