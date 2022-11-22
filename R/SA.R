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

# function to determine the dominant input function
dominate_input_determination <- function(y, N, params){
  y_score <- y$score
  y_rank <- y$rank
  
  dominate_input_score_df <- matrix(nrow = 1, ncol = ncol(y_score))
  colnames(dominate_input_score_df) <- colnames(y_score)
  
  dominate_input_rank_df <- matrix(nrow = 1, ncol = ncol(y_rank))
  colnames(dominate_input_rank_df) <- colnames(y_rank)
  
  for (i in 1:ncol(y_score)) {
    sobol_ind_score  <- sobol_indices(Y = y_score[,i], N = N, params = params)
    sobol_ind_score_df <- sobol_ind_score$results
    dominate_input_score <- sobol_ind_score_df$parameters[sobol_ind_score_df$original == max(sobol_ind_score_df$original[sobol_ind_score_df$sensitivity == "Si"])&sobol_ind_score_df$sensitivity == "Si"]
    dominate_input_score_df[1,i] <- dominate_input_score
    
    sobol_ind_rank  <- sobol_indices(Y = y_rank[,i], N = N, params = params)
    sobol_ind_rank_df <- sobol_ind_rank$results
    dominate_input_rank <- sobol_ind_rank_df$parameters[sobol_ind_rank_df$original == max(sobol_ind_rank_df$original[sobol_ind_rank_df$sensitivity == "Si"])&sobol_ind_rank_df$sensitivity == "Si"]
    dominate_input_rank_df[1,i] <- dominate_input_rank
  }
  dominate_input_fcts <- list(dominate_input_score = dominate_input_score_df, dominate_input_rank = dominate_input_rank_df)
}

# Dominate input factors mapping function (map dominant input fcts to each census tract)
dominate_input_map <- function(dominate_input_fcts, input){
  dominate_input_rank <- dominate_input_fcts$dominate_input_rank
  dominate_input_score <- dominate_input_fcts$dominate_input_score
  
  dominate_input_score_df <- as.data.frame(dominate_input_score) %>%
    pivot_longer(everything(), names_to = 'NAME', values_to = 'dominate_input_fcts') %>%
    left_join(input %>% dplyr::select('NAME'), by = "NAME")
  
  dominate_input_rank_df   <- as.data.frame(dominate_input_rank) %>%
    pivot_longer(everything(), names_to = 'NAME', values_to = 'dominate_input_fcts') %>%
    left_join(input %>% dplyr::select('NAME'), by = "NAME")
  
  dominate_input_df <- list(dominate_input_score_df = dominate_input_score_df, dominate_input_rank_df = dominate_input_rank_df)
}















