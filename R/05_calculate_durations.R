library(tidyverse)
library(modifiedBChron)

top_model <- read_rds('./results/top_model.rds')
bottom_model <- read_rds('./results/bottom_model.rds')

x <- agePredict(model = top_model, 
           newPositions = -c(61.2, 136.8))

# calculate duration 
calculate_duration <- function(x) {
  z <- quantile(x, prob = c(0.5, 0.025, 0.975))
  med = z[1]
  plus <- z[3] - z[1]
  minus <- z[1] - z[2]
  
  return(data.frame(med, plus, minus))
}

# early blip 
early_blip     <- -c(297.65, 297.95)
early_blip_age <- agePredict(model = bottom_model, newPositions = early_blip)
early_blip_duration <- (early_blip_age$raw[, 2] - early_blip_age$raw[, 1]) |> calculate_duration()

# early rise 
early_rise     <- -c(280.70, 296.60)
early_rise_age <- agePredict(model = bottom_model, newPositions = early_rise)
early_rise_duration <- (early_rise_age$raw[, 2] - early_rise_age$raw[, 1]) |> calculate_duration()

# Ireviken Blue Box
blue_box     <- -c(280.63, 285.0)
blue_box_age <- agePredict(model = bottom_model, newPositions = blue_box)
ireviken_blue_box_duration <- (blue_box_age$raw[, 2] - blue_box_age$raw[, 1]) |> calculate_duration()

# Ireviken plus 2 increas
plus_2     <- -c(273.25, 280.55)
plus_2_age <- agePredict(model = bottom_model, newPositions = plus_2)
ireviken_plus_2_duration <- (plus_2_age$raw[, 2] - plus_2_age$raw[, 1]) |> calculate_duration()

# Total Excusion
total_excusion_base_age <- agePredict(model = bottom_model, newPositions = -280.55)
total_excusion_top_age <- agePredict(model = top_model, newPositions = -222.90)

total_excusion_duration <- (total_excusion_base_age$raw[,1] - total_excusion_top_age$raw[,1]) |> calculate_duration()

# quite between ireviken and mulde
quite <- -c(146.4, 222.9)
quite_age <- agePredict(model = top_model, newPositions = quite)
quite_duration <- (quite_age$raw[, 2] - quite_age$raw[, 1]) |> calculate_duration()

# mulde blue box
mulde_box <- -c(135.5, 146.4)
mulde_box_age <- agePredict(model = top_model, newPositions = mulde_box)
mulde_box_duration <- (mulde_box_age$raw[, 2] - mulde_box_age$raw[, 1]) |> calculate_duration()

# mulde + 2 increase 
mulde_plus <- -c(129.9, 136.8)
mulde_plus_age <- agePredict(model = top_model, newPositions = mulde_plus)
mulde_plus_duration <- (mulde_plus_age$raw[, 2] - mulde_plus_age$raw[, 1]) |> calculate_duration()

# mulde drop 
mulde_drop <- -c(111.0, 112.5)
mulde_drop_age <- agePredict(model = top_model, newPositions = mulde_drop)
mulde_drop_duration <- (mulde_drop_age$raw[, 2] - mulde_drop_age$raw[, 1]) |> calculate_duration()

# mulde middle
mulde_middle <- -c(102.9, 112.5)
mulde_middle_age <- agePredict(model = top_model, newPositions = mulde_middle)
mulde_middle_duration <- (mulde_middle_age$raw[, 2] - mulde_middle_age$raw[, 1]) |> calculate_duration()

# LW boundary
LW_boundary <- -c(280.63, 281.75)
LW_boundary_age <- agePredict(model = bottom_model, newPositions = LW_boundary)
LW_boundary_duration <- (LW_boundary_age$raw[, 2] - LW_boundary_age$raw[, 1]) |> calculate_duration()

# Upper Visby
Upper_Visby <- -c(272.75, 280.6)
Upper_Visby_age <- agePredict(model = bottom_model, newPositions = Upper_Visby)
Upper_Visby_duration <- (Upper_Visby_age$raw[, 2] - Upper_Visby_age$raw[, 1]) |> calculate_duration()

# Hogklint
Hogklint        <- -c(258.45, 272.75)
Hogklint_top    <- agePredict(model = top_model, newPositions = -258.45)
Hogklint_bottom <-  agePredict(model = bottom_model, newPositions = -272.75)
Hogklint_duration <- (Hogklint_bottom$raw[, 1] - Hogklint_top$raw[, 1]) |> calculate_duration()

# Tofta
Tofta <- -c(243.00, 258.45)
Tofta_age <- agePredict(model = top_model, newPositions = Tofta)
Tofta_duration <- (Tofta_age$raw[, 2] - Tofta_age$raw[, 1]) |> calculate_duration()

# Hangvar
Hangvar <- -c(234.00, 243.00)
Hangvar_age <- agePredict(model = top_model, newPositions = Hangvar)
Hangvar_duration <- (Hangvar_age$raw[, 2] - Hangvar_age$raw[, 1]) |> calculate_duration()




rbind(early_blip_duration,
      early_rise_duration,
      ireviken_blue_box_duration,
      ireviken_plus_2_duration,
      total_excusion_duration,
      quite_duration,
      mulde_box_duration,
      mulde_plus_duration,
      mulde_drop_duration,
      mulde_middle_duration,
      LW_boundary_duration,
      Upper_Visby_duration,
      Hogklint_duration,
      Tofta_duration,
      Hangvar_duration) |> 
  as.data.frame() |> 
  add_column(name = c('early blip',
                 'early rise', 
                 'ireviken_blue_box',
                 'ireviken_plus_2',
                 'total_excusion',
                 'quite',
                 'mulde_box',
                 'mulde_plus',
                 'mulde_drop',
                 'mulde_middle',
                 'LW_boundary_duration',
                 'upper_visby_duration',
                 'hogklint_duration',
                 'tofta_duration',
                 'hangvar_duration')) |> 
  write_csv('./results/durations.csv')
