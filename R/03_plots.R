# load required packages ------------------------------------------------------
library(tidyverse)
library(modifiedBChron)
library(cowplot)
library(RColorBrewer)
library(ggridges)
source('./R/02_calculate_sed_rate.R')

# load geochron data for names 
geochron_all <- read.csv(file = './data/geochron.csv')

# assign colors 
colors <- brewer.pal(n = length(geochron_all$sample),
                     name = 'Set2')

names(colors) <- geochron_all$sample

# load the isotope data for the GTS2020 curve
gts <- read.csv(file = './data/isotopes.csv') |> 
  select(GTS_age, depth_m) |> 
  mutate(depth_m = -depth_m)

# load required data ----------------------------------------------------------
top_model <- read_rds(file = './results/top_model.rds')
bottom_model <- read_rds(file = './results/bottom_model.rds')
old_model <- read_rds(file = './results/old_model.rds')

# plot the old dates model
old_admp <- old_model$HDI |>
  ggplot(mapping = aes(y = position)) +
  geom_ribbon(mapping = aes(xmin = `2.5%`,
                            xmax = `97.5%`),
              alpha = 0.5) +
  geom_line(mapping = aes(x = `50%`)) +
  xlab('Age') +
  ylab('position') +
  geom_density_ridges2(data = old_model$likelihoods,
                       mapping = aes(x = age,
                                     y = position,
                                     height = probability,
                                     group = id,
                                     fill = id),
                       stat = 'identity',
                       color = NA,
                       scale = 1) + 
  xlab('Age (Ma)') +
  ylab('Position (m)') + 
  ylim(-330, 50) + 
  xlim(437, 423) +
  scale_fill_manual(values = colors) + 
  geom_line(data = gts,
            mapping = aes(x = GTS_age,
                          y = depth_m),
            inherit.aes = FALSE,
            color = 'red',
            lineend = 'round',
            linetype = 'dashed')
    

# plot the new dates model
new_admp <- top_model$HDI |>
  ggplot(mapping = aes(y = position)) +
  geom_ribbon(mapping = aes(xmin = `2.5%`,
                            xmax = `97.5%`),
              alpha = 0.5) +
  geom_line(mapping = aes(x = `50%`)) +
  geom_density_ridges2(data = top_model$likelihoods,
                       mapping = aes(x = age,
                                     y = position,
                                     height = probability,
                                     group = id,
                                     fill = id),
                       stat = 'identity',
                       color = NA,
                       scale = 1) +
  geom_ribbon(data = bottom_model$HDI,
              aes(xmin = `2.5%`,
                  xmax = `97.5%`),
              alpha = 0.5) +  
  geom_line(data = bottom_model$HDI,
            mapping = aes(x = `50%`)) +
  xlab('Age (Ma)') +
  ylab('Position (m)') +
  geom_density_ridges2(data = bottom_model$likelihoods,
                       mapping = aes(x = age,
                                     y = position,
                                     height = probability,
                                     group = id,
                                     fill = id),
                       stat = 'identity',
                       color = NA,
                       scale = 1) + 
  scale_fill_manual(values = colors) + 
  theme_bw() + 
  theme(legend.title = element_blank()) + 
  ylim(-330, 50) +
  xlim(437, 423) +
  geom_line(data = gts,
            mapping = aes(x = GTS_age,
                          y = depth_m),
            inherit.aes = FALSE,
            color = 'red',
            lineend = 'round',
            linetype = 'dashed')


legend = get_plot_component(new_admp, 'guide-box')

pdf(file = './figures/age_models.pdf',
    width = 7.5, 
    height = 4)
plot_grid(old_admp + theme(legend.position = 'none'),
          new_admp + theme(legend.position = 'none'),
          # legend,
          sed_rates,
          nrow = 1,
          rel_widths = c(2, 2, 1.5))
dev.off()
# calculate hiatus duration 



duration = -(top_model$model[length(top_model$predictPositions), ] - 
  bottom_model$model[1, ])
duration <- duration[duration > 0]
duration_CI <- quantile(duration, prob = c(0.025, 0.5, 0.975))

pdf('./figures/duration.pdf',
    width = 1.5,
    height = 1.5)
ggplot() +
  geom_density(mapping = aes(x = duration),
               fill = 'grey',
               color = NA) + 
  geom_vline(mapping = aes(xintercept = duration_CI),
             linetype = 'dashed')
dev.off()
