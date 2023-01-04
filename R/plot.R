# plot functions for UA and SA analysis
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
source(paste0(proj_root, "/R/SA.R"))
source(paste0(proj_root, "/R/UA.R"))

# UA plot functions
# Median rank vs CV
p1 <- ggplot(UA_df, aes(x=Median, y=CV*100)) +
  geom_point(size=2, shape=23, color="blue") +
  geom_smooth(color="blue", fill="blue")+
  ggtitle("SoFI ranking vs Coefficient of Variance (CV)") + xlab("Median rank of Social Fabric Index (SoFI)") + ylab("Coefficient of Variance (%)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5), axis.title.x = element_text(size=18, face="bold" ), axis.title.y = element_text(size=18, face="bold"), axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18))
p1

# Median rank vs CI interval
p2 <- ggplot(UA_df, aes(x=Median, y=interval)) +
  geom_point(size=2, shape=23, color="red") +
  geom_smooth(color="red", fill="red") +
  ggtitle("SoFI ranking vs Confidence Interval (CI)") + xlab("Median rank of Social Fabric Index (SoFI)") + ylab("Confidence Inverval (95%)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5), axis.title.x = element_text(size=18, face="bold" ), axis.title.y = element_text(size=18, face="bold"), axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18))
p2

# Baseline rank vs CV
p3 <- ggplot(UA_df, aes(x=baseline, y=CV*100)) +
  geom_point(size=2, shape=23, color="blue") +
  geom_smooth(color="blue", fill="blue")+
  ggtitle("SoFI ranking vs Coefficient of Variance (CV)") + xlab("Baseline rank of Social Fabric Index (SoFI)") + ylab("Coefficient of Variance (%)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5), axis.title.x = element_text(size=18, face="bold" ), axis.title.y = element_text(size=18, face="bold"), axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18))
p3

# Baseline rank vs CI interval
p4 <- ggplot(UA_df, aes(x=baseline, y=interval)) +
  geom_point(size=2, shape=23, color="red") +
  geom_smooth(color="red", fill="red") +
  ggtitle("SoFI ranking vs Confidence Interval (CI)") + xlab("Baseline rank of Social Fabric Index (SoFI)") + ylab("Confidence Inverval (95%)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5), axis.title.x = element_text(size=18, face="bold" ), axis.title.y = element_text(size=18, face="bold"), axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18))
p4

# SA plot functions
mean_Ti <- mean_Ti_calc(y, N, params)

# plot the mean total order effect sensitivity analysis index (score) across all the tracts.
p_mean_Ti_score <- ggplot(mean_Ti$mean_Ti_score, aes(x=parameters, y=mean_Ti)) + 
  geom_bar(stat = "identity") +
  ylab("mean S_ti (total order effect)") + xlab("Input factors") +
  theme_classic() +
  theme(axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 15), axis.title.x = element_text(size = 20, face="bold"), axis.text.x = element_text(size = 15)) + 
  coord_flip()

# plot the mean total order effect sensitivity analysis index (rank) across all the tracts.
p_mean_Ti_rank <- ggplot(mean_Ti$mean_Ti_rank, aes(x=parameters, y=mean_Ti)) + 
  geom_bar(stat = "identity") +
  ylab("mean S_ti (total order effect)") + xlab("Input factors") +
  theme_classic() +
  theme(axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 15), axis.title.x = element_text(size = 20, face="bold"), axis.text.x = element_text(size = 15)) + 
  coord_flip()

