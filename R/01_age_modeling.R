# load required packages ------------------------------------------------------
library(tidyverse)
library(modifiedBChron)
library(RColorBrewer)
library(ggridges)
library(cowplot)
source('./R/format_model.R')
# read in the data ------------------------------------------------------------
geochron_all <- read.csv(file = './data/geochron.csv')

geochron_top <- read.csv(file = './data/geochron.csv') |> 
  filter(position_m < 272.66) |> 
  filter(is.finite(age_new))

geochron_bottom <- read.csv(file = './data/geochron.csv') |> 
  filter(position_m > 272.66) |> 
  filter(is.finite(age_new))

isotopes <- read.csv(file = './data/isotopes.csv')

# filter out NA for the old ages 
geochron_old <- geochron_all |> 
  filter(is.finite(age_old))



# make age-depth models -------------------------------------------------------
# model above the hiatus
top_model <- ageModel(ages = geochron_top$age_new,
                      ageSds = geochron_top$age_sd_new,
                      positions = -geochron_top$position_m,
                      positionThicknesses = geochron_top$thickness_m,
                      ids = geochron_top$sample,
                      predictPositions = seq(0, -272.66, by = -0.01),
                      MC = 10000, 
                      burn = 1000,
                      truncateDown = 431.44)

bottom_model <- ageModel(ages = geochron_bottom$age_new,
                      ageSds = geochron_bottom$age_sd_new,
                      positions = -geochron_bottom$position_m,
                      positionThicknesses = geochron_bottom$thickness_m,
                      ids = geochron_bottom$sample,
                      predictPositions = seq(-272.66, -330, by = -0.01),
                      MC = 10000, 
                      burn = 1000,
                      truncateUp = 430.81,
                      truncateDown = 437.7) # Cramer et al., 2012


old_model <- ageModel(ages = geochron_old$age_old,
                      ageSds = geochron_old$age_sd_old,
                      positions = -geochron_old$position_m,
                      positionThicknesses = geochron_old$thickness_m,
                      ids = geochron_old$sample,
                      predictPositions = seq(0, -330, by = -0.01),
                      MC = 10000, 
                      burn = 1000,
                      truncateDown = 437.7)

# format the models and save the results 
top_model <- format_model(top_model)
bottom_model <- format_model(bottom_model)
old_model <- format_model(old_model)

old_model |> write_rds(file = './results/old_model.rds')
top_model |> write_rds(file = './results/top_model.rds')
bottom_model |> write_rds(file = './results/bottom_model.rds')
