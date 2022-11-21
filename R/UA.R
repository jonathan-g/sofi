# Uncertainty Analysis
library(tidyverse)
library(dplyr)
library(stats)
library(GPArotation)
library(sf)
library(stringr)
library(BBmisc)
library(paran)

proj_root <- find_root(is_rstudio_project)

source(paste0(proj_root, "/R/SoFI_funcs.R"))

indicator_transformation_option = c("1", "2", "3")
indicator_normalization_option = c("4", "5", "6")
pca_selection_option = c("7", "8", "9") 
pca_rotation_option = c("10", "11", "12", "13", "14", "15")
weight_scheme_option = c("16", "17", "18")

sample_calc <- function(input, sample_number) {
  sample_results <- vector(mode = "list", length = 0)
  indicator_transformation_list <- vector(length = 0)
  indicator_normalization_list <- vector(length = 0)
  pca_selection_list <- vector(length = 0)
  pca_rotation_list <- vector(length = 0)
  weight_scheme_list <-vector(length = 0)
  for (i in 1:sample_number) {
    indicator_transformation <- sample(indicator_transformation_option, size = 1)
    indicator_normalization <- sample(indicator_normalization_option, size = 1)
    pca_selection <- sample(pca_selection_option, size = 1)
    pca_rotation <- sample(pca_rotation_option, size = 1)
    weight_scheme <- sample(weight_scheme_option , size = 1)
    
    sfi <- sf_rank_calculation(input, indicator_transformation, indicator_normalization, pca_selection, pca_rotation,  weight_scheme)
    
    sample_results <- append(sample_results, list(sfi))
    indicator_transformation_list <- append(indicator_transformation_list, indicator_transformation)
    indicator_normalization_list <- append(indicator_normalization_list, indicator_normalization)
    pca_selection_list <- append(pca_selection_list, pca_selection)
    pca_rotation_list <- append(pca_rotation_list, pca_rotation)
    weight_scheme_list <- append(weight_scheme_list, weight_scheme)
  }
  samples <- list(sample_results = sample_results, 
                  indicator_transformation_list = indicator_transformation_list, 
                  indicator_normalization_list = indicator_normalization_list,
                  pca_selection_list = pca_selection_list, 
                  pca_rotation_list = pca_rotation_list, 
                  weight_scheme_list = weight_scheme_list ) 
}

UA_samples <- sample_calc(variable, 300)
saveRDS(UA_samples, file = (paste0(proj_root, "/data/gen/UA_samples.rds")))