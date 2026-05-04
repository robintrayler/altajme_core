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

isotopes <- isotopes |> 
  select(d13Ccarb, 
         new_age,
         old_age,
         GTS_age) |> 
  pivot_longer(cols = c(old_age, new_age, GTS_age),
               names_to = 'model',
               values_to = 'age') |> 
  mutate(model = factor(model, levels = c('old_age', 'GTS_age', 'new_age')))


labels <- c(
  old_age = "Cramer et al. (2012)",
  new_age = "This Study",
  GTS_age = "GTS 2020"
)


pdf(file = './figures/isotopes.pdf', 
    width = 7.5,
    height = 7.5)
isotopes |> 
  ggplot(mapping = aes(x = d13Ccarb,
                       y = age)) + 
  # geom_path(show.legend = FALSE) +
  geom_point(
    alpha = 1,
             show.legend = FALSE,
             shape = 16,
    size = 1,
    color = 'grey10') +

  scale_y_reverse() + 
  facet_wrap(~model, labeller = as_labeller(labels)) + 
  xlab(expression(delta^13*C[carbonate])) + 
  ylab('Age (Ma)') + 
  theme(strip.background = element_rect(fill = 'white'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12, color = 'black'),
        axis.title = element_text(size = 12, color = 'black'),
        strip.text = element_text(size = 12, color = 'black')) + 
  scale_color_manual(values = colors)
dev.off()


