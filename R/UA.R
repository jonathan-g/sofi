# Uncertainty Analysis
library(tidyverse)
library(dplyr)
library(stats)
library(GPArotation)
library(sf)
library(stringr)
library(Rmisc)
library(BBmisc)
library(paran)
library(rprojroot)

proj_root <- find_root(is_rstudio_project)

source(paste0(proj_root, "/R/SoFI_funcs.R"))
source(paste0(proj_root, "/R/baseline.R"))
# sofi_indicators_df <- readRDS(paste0(proj_root, '/data/gen/sofi_indicators_df.rds'))
# 
# sofi_indicators_df <- sofi_indicators_df %>% 
#   replace(is.na(.), 0)
# 
# # remove all 0 variables.
# variable <- sofi_indicators_df %>% 
#   st_drop_geometry() %>% 
#   dplyr::select(-'GEOID', -'NAME') %>%
#   dplyr::select_if(colSums(.) != 0) %>%
#   bind_cols(sofi_indicators_df %>% 
#               st_drop_geometry() %>%
#               dplyr::select('NAME', 'GEOID')) %>%
#   relocate('NAME', .before = 'barber') %>%
#   relocate('GEOID', .before = 'barber') %>%
#   relocate('area', .after = 'ethnic_equity')
# 
# saveRDS(variable, file = (paste0(proj_root, "/data/gen/variable.rds")))

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
    
    sofi <- sofi_calc(input, indicator_transformation, indicator_normalization, pca_selection, pca_rotation,  weight_scheme)
    
    sample_results <- append(sample_results, list(sofi))
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

set.seed(1)
UA_samples <- sample_calc(variable, 3584)
saveRDS(UA_samples, file = (paste0(proj_root, "/data/gen/UA_samples.rds")))

# create UA sample results data frame
samples_df <- UA_samples$sample_results[[1]]$sofi_rank %>% dplyr::select('NAME')
for (i in 1:length(UA_samples$sample_results)) {
  samples_df <- samples_df %>%
    left_join(UA_samples$sample_results[[i]]$sofi_rank %>%
                dplyr::select('NAME','rank') %>%
                dplyr::rename(!!paste('rank', i) := rank) %>% st_drop_geometry(), by = 'NAME')
}

# Confidence Interval (CI)
CI <- as.data.frame(apply(as.matrix(samples_df %>% dplyr::select(-'NAME') %>% st_drop_geometry()), 1, function(x) CI(x)) %>%
                      t(.)) %>%
  cbind(samples_df %>% dplyr::select('NAME')) %>%
  mutate(interval = upper - lower)

# Median
Median <- as.data.frame(apply(as.matrix(samples_df %>% dplyr::select(-'NAME') %>% st_drop_geometry()), 1, function(x) median(x)))
colnames(Median) <- 'Median'
Median <- Median %>%
  cbind(samples_df %>% dplyr::select('NAME'))

# Coefficient of Variation (CV)
CV <- as.data.frame(apply(as.matrix(samples_df %>% dplyr::select(-'NAME') %>% st_drop_geometry()), 1, function(x) cv(x)))
colnames(CV) <- 'CV'
CV <- CV %>%
  cbind(samples_df %>% dplyr::select('NAME'))

# Mode
getmode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

Mode <- as.data.frame(apply(as.matrix(samples_df %>% dplyr::select(-'NAME') %>% st_drop_geometry()), 1, function(x) getmode(x)))
colnames(Mode) <- 'Mode'
Mode <- Mode %>%
  cbind(samples_df %>% dplyr::select('NAME'))

# Mean rank
Mean <- as.data.frame(apply(as.matrix(samples_df %>% dplyr::select(-'NAME') %>% st_drop_geometry()), 1, function(x) mean(x)))
colnames(Mean) <- 'Mean'
Mean <- Mean %>%
  cbind(samples_df %>% dplyr::select('NAME'))

# baseline
Baseline <- sofi_baseline$sofi_rank %>% 
  dplyr::select('NAME', 'baseline' = 'rank')

# Generate Uncertainty Results Data frame
UA_df <- CI %>%
  left_join(Median %>% dplyr::select ('NAME', 'Median'), by = 'NAME') %>%
  left_join(CV %>% dplyr::select ('NAME', 'CV') , by = 'NAME') %>%
  left_join(Mode %>% dplyr::select ('NAME', 'Mode'), by = 'NAME') %>%
  left_join(Mean %>% dplyr::select ('NAME', 'Mean'), by = 'NAME') %>%
  left_join(Baseline %>% dplyr::select('NAME', 'baseline'), by = 'NAME') %>%
  dplyr::select(-'Mean') %>%
  relocate('NAME', .before = 'upper')

saveRDS(UA_df, file = (paste0(proj_root, "/data/gen/UA_df.rds")))
