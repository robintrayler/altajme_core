# load required packages ------------------------------------------------------
library(tidyverse)
library(modifiedBChron)
library(cowplot)
library(RColorBrewer)
library(ggridges)
theme_set(theme_bw())

# load geochron data for names 
geochron_all <- read.csv(file = './data/geochron.csv')

# assign colors 
colors <- brewer.pal(n = length(geochron_all$sample),
                     name = 'Set2')

names(colors) <- geochron_all$sample

# load the isotope data for the GTS2020 curve
isotopes <- read.csv(file = './data/isotopes.csv') |> 
  mutate(depth_m = -depth_m)

# load required data ----------------------------------------------------------
top_model <- read_rds(file = './results/top_model.rds')
bottom_model <- read_rds(file = './results/bottom_model.rds')
old_model <- read_rds(file = './results/old_model.rds')


hiatus_position <- -272.66 # meters depth

top_isotopes <- isotopes |> filter(depth_m > -272.66)
bottom_isotopes <- isotopes |> filter(depth_m < -272.66)

top_predict <- agePredict(model = top_model, newPositions = top_isotopes$depth_m)
bottom_predict <- agePredict(model = bottom_model, newPositions = bottom_isotopes$depth_m)

old_predict <- agePredict(model = old_model, newPositions = isotopes$depth_m)


isotopes$old_age <- old_predict$HDI$`0.5`
isotopes$new_age <- c(top_predict$HDI$`0.5`, bottom_predict$HDI$`0.5`)


top_CI <- top_predict$raw |> t() |> 
  apply(MARGIN = 1, 
        FUN = quantile, probs = c(0.5, 0.025, 0.975), , na.rm = TRUE) |> 
  t() |> 
  as.data.frame() |> 
  add_column(d13C = top_isotopes$d13Ccarb)

bottom_CI <- bottom_predict$raw |> t() |> 
  apply(MARGIN = 1, 
        FUN = quantile, probs = c(0.5, 0.025, 0.975), na.rm = TRUE) |> 
  t() |> 
  as.data.frame() |> 
  add_column(d13C = bottom_isotopes$d13Ccarb)


old_CI <- old_predict$raw |> t() |> 
  apply(MARGIN = 1, 
        FUN = quantile, probs = c(0.5, 0.025, 0.975), na.rm = TRUE) |> 
  t() |> 
  as.data.frame() |> 
  add_column(d13C = isotopes$d13Ccarb) |> 
  add_column(model = 'Cramer et al. (2012)')


new_CI <- rbind(top_CI, bottom_CI) |> 
  add_column(model = 'This Study')


GTS_CI <- isotopes |> 
  select(d13C = d13Ccarb,
         `50%` = GTS_age) |>
  mutate(`2.5%` = NA,
         `97.5%` = NA) |> 
  add_column(model = 'GTS 2020') 

CI <- rbind(old_CI, new_CI, GTS_CI) |> 
  mutate(model = factor(model, levels = c('Cramer et al. (2012)', 'GTS 2020', 'This Study')))

  


pdf(file = './figures/isotopes.pdf', 
    width = 7.5,
    height = 7.5)

CI |> 
  ggplot(mapping = aes(y = `50%`, 
                       x = d13C)) + 
  geom_point(size = 0.5) + 
  geom_errorbar(mapping = aes(ymin = `2.5%`, 
                              ymax = `97.5%`),
                alpha = 0.1) + 
  facet_wrap(.~model) + 
  xlab(expression(delta^13*C[carbonate])) + 
  ylab('Age (Ma)') + 
  scale_y_reverse() + 
  theme(panel.grid = element_blank()) + 
  theme(strip.background = element_rect(fill = 'white'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12, color = 'black'),
        axis.title = element_text(size = 12, color = 'black'),
        strip.text = element_text(size = 12, color = 'black')) + 
  scale_color_manual(values = colors)
dev.off()


