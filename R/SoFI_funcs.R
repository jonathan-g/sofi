# Social Fabric Model Index Construction Functions
library(tidyverse)
library(dplyr)
library(stats)
library(GPArotation)
library(sf)
library(stringr)
library(BBmisc)
library(paran)
library(rprojroot)

#-------------------read in input data--------------------------------
variable <- readRDS(file.path(proj_root, "data", '/gen/sofi_indicators_df.rds')) %>%
  dplyr::select(-GEOID)

# Transformation func-------------------------------------------
no_transformation <- function(variable){
  indicator_transformed <- variable
}

area_transformation <- function(variable) {
  for (i in 1:ncol(variable)) {
    if (!colnames(variable[,i])[1] %in% c('NAME', "area", "geometry")) {
      variable[,i] <- variable[,i] %>% 
        st_drop_geometry()/as.numeric(variable$area * 3.86102e-7)
    }
  }
  indicator_transformed <- variable
}

population_transformation <- function(variable) {
  variable$pop[variable$pop == 0] <- sum(variable$pop) 
  for (i in 1:ncol(variable)) {
    if (!colnames(variable[,i])[1] %in% c('NAME', "area", "geometry")) {
      variable[,i] <- variable[,i]/as.numeric(variable$pop)
    }
  }
  indicator_transformed <- variable %>% 
    dplyr::select(-'pop')
}

# Normalization/Scale func----------------------------------------
no_normalization <- function(indicator_transformed) {
  indicator_normalized <- indicator_transformed
}

z_normalization <- function(indicator_transformed) {
  for (i in 1:ncol(indicator_transformed)) {
    if (!colnames(indicator_transformed[, i])[1] %in% c('NAME', "area", "geometry")) {
      indicator_transformed [, i] <- normalize(indicator_transformed[,i] %>% 
                                                 st_drop_geometry(), method = "standardize", range = c(0, 1), margin = 2L, on.constant = "quiet")
    }
  }
  indicator_normalized <- indicator_transformed
}

min_max_normalization <- function(indicator_transformed) {
  for (i in 1:ncol(indicator_transformed)) {
    if (!colnames(indicator_transformed[, i])[1] %in% c('NAME', "area", "geometry")) {
      indicator_transformed[, i] <- (indicator_transformed[,i] %>% 
                                       st_drop_geometry() - min(indicator_transformed[,i] %>% 
                                                                  st_drop_geometry()))/(max(indicator_transformed[,i] %>%
                                                                                              st_drop_geometry())-min(indicator_transformed[,i] %>% 
                                                                                                                        st_drop_geometry()))
    }
  }
  indicator_normalized <- indicator_transformed
}

# PCA conduction/analysis func-----------------------------------
pca_process <- function(indicator_normalized) {
  pca <-  prcomp(indicator_normalized %>% dplyr::select(-"NAME", -"area") %>% 
                   st_drop_geometry(), center = TRUE, scale. = TRUE)  
  CT_tab <- indicator_normalized %>% 
    dplyr::select('NAME', 'area')
  indicator_normalized <- indicator_normalized
  pca <- list(pca = pca, CT_tab = CT_tab, indicator_normalized = indicator_normalized)
}

# PCA selection func-----------------------------------------
Kaiser_selection <- function(pca){
  pca.var <- pca$pca$sdev ^ 2
  pca.pvar <- pca.var/sum(pca.var)
  
  pca_tab <- as.data.frame(pca$pca$x[,pca$pca$sdev^2 > 1, drop=FALSE])
  pca_rotation_selected <- as.data.frame(pca$pca$rotation[, pca$pca$sdev^2 > 1, drop=FALSE])
  pca_pvar <- pca.pvar[pca$pca$sdev^2 > 1]
  CT_tab <- pca$CT_tab
  pca_selected <- list(pca_df_selected = pca_tab, pca_rotation_selected = pca_rotation_selected, pca_pvar = pca_pvar, CT_tab = CT_tab)
}

variances_explained_selection <- function(pca){
  pca.var <- pca$pca$sdev ^ 2
  pca.pvar <- pca.var/sum(pca.var)
  pca.cvar <- cumsum(pca.pvar)
  pca_tab <- as.data.frame(pca$pca$x[, 1: min(which(pca.cvar > 0.8)), drop = FALSE])
  pca_rotation_selected <- as.data.frame(pca$pca$rotation[, 1: min(which((pca.cvar) > 0.8)), drop = FALSE])
  
  pca_pvar <- pca.pvar[1: min(which(pca.cvar > 0.8))]
  CT_tab <- pca$CT_tab
  pca_selected <- list(pca_df_selected = pca_tab, pca_rotation_selected = pca_rotation_selected, pca_pvar = pca_pvar, CT_tab = CT_tab)
}

horn_parallel_selection <- function(pca){
  parallel_pca <- paran(pca$indicator_normalized %>% dplyr::select(-"NAME", -"area") %>% st_drop_geometry(), iterations=5000)
  
  pca.var <- pca$pca$sdev ^ 2
  pca.pvar <- pca.var/sum(pca.var)
  pca.cvar <- cumsum(pca.pvar)
  pca_tab <- as.data.frame(pca$pca$x[, 1: parallel_pca$Retained, drop = FALSE])
  pca_rotation_selected <- as.data.frame(pca$pca$rotation[, 1: parallel_pca$Retained, drop = FALSE])
  pca_pvar <- pca.pvar[1: parallel_pca$Retained]
  CT_tab <- pca$CT_tab
  pca_selected <- list(pca_df_selected = pca_tab, pca_rotation_selected = pca_rotation_selected, pca_pvar = pca_pvar, CT_tab = CT_tab)
}

# PCA rotation func------------------------------------------
unrotate <- function(pca_selected){
  pca_rotated_rotation <- pca_selected$pca_rotation_selected
  pca_df_selected <- pca_selected$pca_df_selected
  pca_pvar <- pca_selected$pca_pvar
  CT_tab <- pca_selected$CT_tab
  pca_rotated <- list(pca_df_selected = pca_df_selected, pca_rotated_rotation = pca_rotated_rotation, pca_pvar = pca_pvar, CT_tab = CT_tab)
}

varimax_process <- function(pca_selected) {
  pca_rotated_rotation <- varimax(data.matrix(pca_selected$pca_rotation_selected))
  pca_df_selected <- pca_selected$pca_df_selected
  pca_pvar <- pca_selected$pca_pvar
  CT_tab <- pca_selected$CT_tab
  pca_rotated <- list(pca_df_selected = pca_df_selected, pca_rotated_rotation = pca_rotated_rotation$loadings, pca_pvar = pca_pvar, CT_tab = CT_tab)
}

quartimax_process <- function(pca_selected){
  pca_rotated_rotation <- quartimax(data.matrix(pca_selected$pca_rotation_selected),  Tmat=diag(ncol(data.matrix(pca_selected$pca_rotation_selected))))
  pca_df_selected <- pca_selected$pca_df_selected
  pca_pvar <- pca_selected$pca_pvar
  CT_tab <- pca_selected$CT_tab
  pca_rotated <- list(pca_df_selected = pca_df_selected, pca_rotated_rotation = pca_rotated_rotation$loadings, pca_pvar = pca_pvar, CT_tab = CT_tab)
}

promax_2 <- function(pca_selected){
  pca_rotated_rotation <- promax(data.matrix(pca_selected$pca_rotation_selected), m = 2)
  pca_df_selected <- pca_selected$pca_df_selected
  pca_pvar <- pca_selected$pca_pvar
  CT_tab <- pca_selected$CT_tab
  pca_rotated <- list(pca_df_selected = pca_df_selected, pca_rotated_rotation = pca_rotated_rotation$loadings, pca_pvar = pca_pvar, CT_tab = CT_tab)
}

promax_3 <- function(pca_selected){
  pca_rotated_rotation <- promax(data.matrix(pca_selected$pca_rotation_selected), m = 3)
  pca_df_selected <- pca_selected$pca_df_selected
  pca_pvar <- pca_selected$pca_pvar
  CT_tab <- pca_selected$CT_tab
  pca_rotated <- list(pca_df_selected = pca_df_selected, pca_rotated_rotation = pca_rotated_rotation$loadings, pca_pvar = pca_pvar, CT_tab = CT_tab)
}

promax_4 <- function(pca_selected){
  pca_rotated_rotation <- promax(data.matrix(pca_selected$pca_rotation_selected), m = 4)
  pca_df_selected <- pca_selected$pca_df_selected
  pca_pvar <- pca_selected$pca_pvar
  CT_tab <- pca_selected$CT_tab
  pca_rotated <- list(pca_df_selected = pca_df_selected, pca_rotated_rotation = pca_rotated_rotation$loadings, pca_pvar = pca_pvar, CT_tab = CT_tab)
}

# Cardinal assignment process func-----------------------------------
cardinal_assign <- function(pca_rotated){
  if (class(pca_rotated$pca_rotated_rotation) == 'loadings'){
    pca_rotated_rotation <- data.frame(matrix(as.numeric(pca_rotated$pca_rotated_rotation), attributes(pca_rotated$pca_rotated_rotation)$dim, dimnames=attributes(pca_rotated$pca_rotated_rotation)$dimnames))
  }else {
    pca_rotated_rotation <- pca_rotated$pca_rotated_rotation
  }
  
  pca_pvar <- pca_rotated$pca_pvar
  
  for (i in 1:ncol(pca_rotated_rotation)) {
    dominant_variable <- rownames(pca_rotated_rotation)[which.max(abs(pca_rotated_rotation[,i]))]
    if (str_detect(dominant_variable, "single_person_hshd|single_parent_family|poverty|unemployment|limt_english|crime|limt_edu")){
      if (pca_rotated_rotation[dominant_variable, i] > 0) {
        pca_rotated$pca_df_selected[, i] <- pca_rotated$pca_df_selected[, i] * (-1)
      }
    }
    
    else if (!str_detect(dominant_variable, "single_person_hshd|single_parent_family|poverty|unemployment|limt_english|crime|limt_edu")){
      if (pca_rotated_rotation[dominant_variable, i] < 0) {
        pca_rotated$pca_df_selected[, i] <- pca_rotated$pca_df_selected[, i] * (-1)
      }
    }
  }
  CT_tab <- pca_rotated$CT_tab
  pca_cardinal_assigned <- list(pca_df =  pca_rotated$pca_df_selected, pca_pvar = pca_pvar, CT_tab = CT_tab) 
}

# Weight scheme process func--------------------------------------------
equal_sum <- function(pca_cardinal_assigned){
  score <- pca_cardinal_assigned$pca_df %>%
    mutate(score = rowSums(.))
  CT_tab<- pca_cardinal_assigned$CT_tab
  sofi_score <- list(score = score, CT_tab = CT_tab)
}

first_component <- function(pca_cardinal_assigned) {
  score <- pca_cardinal_assigned$pca_df %>% dplyr::select(PC1) %>%
    mutate(score = PC1)
  CT_tab<- pca_cardinal_assigned$CT_tab
  sofi_score <- list(score = score, CT_tab = CT_tab)
}

weighted_sum <- function(pca_cardinal_assigned){
  score <- as.data.frame(t(t(pca_cardinal_assigned$pca_df) * pca_cardinal_assigned$pca_pvar)) %>%
    mutate(score = rowSums(.))
  CT_tab<- pca_cardinal_assigned$CT_tab
  sofi_score <- list(score = score, CT_tab = CT_tab)
}

# Calculate the ranking, save the results---------------------------------------
rank_process <- function(sofi_score){
  sofi_rank <- cbind(sofi_score$CT_tab, sofi_score$score) %>%
    mutate (rank = rank(.$score, ties.method = 'random'))
}


# social fabric score/rank calculation function
sofi_calc <- function(input, 
                      indicator_transformation = c("1", "2", "3"), 
                      indicator_normalization = c("4", "5", "6"), 
                      pca_selection =  c("7", "8", "9"), 
                      pca_rotation = c("10", "11", "12", "13", "14", "15"), 
                      weight_scheme = c("16", "17", "18")){
  if(indicator_transformation == 1) {
    indicator_transformed <- no_transformation(input)
  }else if (indicator_transformation == 2) {
    indicator_transformed <- area_transformation(input)
  }else if (indicator_transformation == 3) {
    indicator_transformed <- population_transformation(input)
  }
  
  if(indicator_normalization == 4){
    indicator_normalized <- no_normalization(indicator_transformed)
  }else if(indicator_normalization == 5){
    indicator_normalized <- z_normalization(indicator_transformed)
  }else if(indicator_normalization == 6){
    indicator_normalized <- min_max_normalization(indicator_transformed)
  }
  
  pca <- pca_process(indicator_normalized) 
  
  if(pca_selection == 7){
    pca_selected <- Kaiser_selection(pca)
  }else if(pca_selection == 8){
    pca_selected <- variances_explained_selection(pca)
  }else if(pca_selection == 9){
    pca_selected <- horn_parallel_selection(pca)
  }
  
  if(pca_rotation == 10){
    pca_rotated <- unrotate(pca_selected)
  }else if(pca_rotation == 11){
    pca_rotated <- varimax_process(pca_selected)
  }else if(pca_rotation == 12){
    pca_rotated <- quartimax_process(pca_selected)
  }else if(pca_rotation == 13){
    pca_rotated <- promax_2(pca_selected)
  }else if(pca_rotation == 14){
    pca_rotated <- promax_3(pca_selected)
  }else if(pca_rotation == 15){
    pca_rotated <- promax_4(pca_selected)
  }
  
  pca_cardinal_assigned <- cardinal_assign(pca_rotated)
  
  if(weight_scheme == 16){
    sofi_score <- equal_sum(pca_cardinal_assigned)
  }else if(weight_scheme == 17){
    sofi_score <- first_component(pca_cardinal_assigned)
  }else if(weight_scheme == 18)
    sofi_score <- weighted_sum(pca_cardinal_assigned)
  
  sofi_rank <- rank_process(sofi_score)
  
  results = list(sofi_score = sofi_score, sofi_rank = sofi_rank)
  
  return(results)
}

