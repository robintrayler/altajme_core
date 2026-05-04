# load required packages ------------------------------------------------------
library(tidyverse)
library(modifiedBChron)
library(cowplot)
theme_set(theme_bw())
# load required data ----------------------------------------------------------
top_model <- read_rds(file = './results/top_model.rds')
bottom_model <- read_rds(file = './results/bottom_model.rds')

# calculate the range between the dates 
top_position_range <- range(top_model$masterPositions)
bottom_position_range <- range(bottom_model$masterPositions)

# slice out just the data within the dated range. This deals with extreme sed rates
# in the extrapolation
top_i <- top_model$predictPositions |> 
  between(left = top_position_range[1]+1, right = top_position_range[2] - 1)

bottom_i <- bottom_model$predictPositions |> 
  between(left = bottom_position_range[1] + 1, right = bottom_position_range[2] - 1)

# calculate sed rate for each model iteration
top_sed_rate = -diff(top_model$predictPositions[top_i]) / 
  apply(X = top_model$model[top_i, ],
        MARGIN = 2,
        FUN = diff)

bottom_sed_rate = -diff(bottom_model$predictPositions[bottom_i]) / 
  apply(X = bottom_model$model[bottom_i, ],
        MARGIN = 2,
        FUN = diff)

# calculate credible intervals
top_sed_CI <- apply(X = top_sed_rate,
                    MARGIN = 1, 
                    FUN = quantile,
                    prob = c(0.025,
                             0.5, 
                             0.975)) |> 
  t() |> 
  as.data.frame() |> 
  add_column(position = top_model$predictPositions[top_i][-1]) 

bottom_sed_CI <- apply(X = bottom_sed_rate,
                       MARGIN = 1, 
                       FUN = quantile,
                       prob = c(0.025,
                                0.5, 
                                0.975)) |> 
  t() |> 
  as.data.frame() |> 
  add_column(position = bottom_model$predictPositions[bottom_i][-1]) 

# combine them
sed_CI <- rbind(top_sed_CI, 
                bottom_sed_CI)

# plot sedimentation rate
sed_rates <- sed_CI |> 
  ggplot(mapping = aes(x = position,
                       y = `50%`)) + 
  geom_path() + 
  geom_ribbon(mapping = aes(ymin = `2.5%`, 
                            ymax = `97.5%`),
              alpha = 0.25) + 
  coord_flip() + 
  ylab('Sedimentation Rate (m/Myr)') + 
  xlab('Position (m)') + 
  xlim(-340, 50) + 
  scale_y_log10() + 
  guides(x = "axis_logticks") + 
  geom_vline(xintercept = -272.66)
