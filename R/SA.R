# Sensitivity Analysis
library(tidyverse)
library(dplyr)
library(stats)
library(GPArotation)
library(sf)
library(stringr)
library(BBmisc)
library(paran)
library(rprojroot)
library(sensobol)
library(data.table)
library(ggplot2)
proj_root <- find_root(is_rstudio_project)

source(paste0(proj_root, "/R/SoFI_funcs.R"))

N <- 2^9
k <- 5
params <- c('Indicator transformation', 'Indicator normalization', 'PCA selection', 'PCA rotation', 'Weight scheme')
R <- 10^3
type <- "norm"
conf <- 0.95

# construct SA sampling matrix
mat <- sobol_matrices(N = N, params = params)
mat[, 1] <- floor(3*mat[, 1]) + 1
mat[, 2] <- floor(3*mat[, 2]) + 4
mat[, 3] <- floor(3*mat[, 3]) + 7
mat[, 4] <- floor(6*mat[, 4]) + 10
mat[, 5] <- floor(3*mat[, 5]) + 16

# SoFI SA function
sofi_sa_fun <- function(input, mat){
  output_number <- nrow(input)
  sample_size <- nrow(mat)
  
  score <- matrix(0, nrow = sample_size, ncol = output_number)
  colnames(score) <- input$NAME
  
  rank <- matrix(0, nrow = sample_size, ncol = output_number)
  colnames(rank) <- input$NAME
  
  for (i in 1:sample_size) {
    indicator_transformation <- mat[i, 1]
    indicator_normalization <- mat[i, 2]
    pca_selection <- mat[i, 3]
    pca_rotation <- mat[i, 4]
    weight_scheme <- mat[i, 5] 
    
    sofi_rank <- sofi_calc(input, indicator_transformation, indicator_normalization, pca_selection, pca_rotation,  weight_scheme) %$% 
      sofi_rank %>%
      dplyr::select('NAME', 'score', 'rank') %>% 
      st_drop_geometry()
    
    score[i, ] <- as.vector(t(sovi_rank %>% dplyr::select('score')))
    rank[i, ] <- as.vector(t(sovi_rank %>% dplyr::select('rank')))
  }
  y <- list(score = score, rank = rank)
  return(y)
}

# Calc the SA model output
y <- sofi_sa_fun(input = variable, mat)

# Mean Total order SA index calculation function
mean_Ti_calc <- function(y, N, params){
  y_score <- y$score
  y_rank <- y$rank
  ind_score_results_df <- NULL
  ind_rank_results_df <- NULL
  
  for (i in 1:ncol(y_score)){
    ind_score <- sobol_indices(Y = y_score[,i], N = N, params = params)
    ind_score_results <- ind_score$results %>%
      filter(sensitivity == 'Ti') %>%
      mutate(CT = i)
    
    ind_score_results_df <- bind_rows(ind_score_results_df, ind_score_results)%>%
      dplyr::group_by(parameters) %>%
      dplyr::summarise(mean_Ti = mean(original, na.rm = TRUE))
    
    ind_rank <- sobol_indices(Y = y_rank[,i], N = N, params = params)
    ind_rank_results <- ind_rank$results %>%
      filter(sensitivity == 'Ti') %>%
      mutate(CT = i)
    
    ind_rank_results_df <- bind_rows(ind_rank_results_df, ind_rank_results)%>%
      dplyr::group_by(parameters) %>%
      dplyr::summarise(mean_Ti = mean(original, na.rm = TRUE))
  }
  mean_Ti <- list(mean_Ti_score = ind_score_results_df, mean_Ti_rank = ind_rank_results_df)
}


