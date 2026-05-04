format_model <- function(model) {
  model$HDI <- model$HDI |>
    t() |>
    as.data.frame() |>
    add_column(position = model$predictPositions)
  
  model$likelihoods <- model$likelihoods |>
    as.data.frame() |>
    set_names(nm = model$ids) |>
    add_column(age = model$ageGrid) |>
    pivot_longer(cols = model$ids,
                 names_to = 'id',
                 values_to = 'probability')
  # add the positions
  positions <- data.frame(id = model$ids,
                          position = model$masterPositions)
  
  model$likelihoods <- model$likelihoods |>
    full_join(positions, by = 'id') |>
    drop_na() |>
    mutate(id = factor(id, levels = model$ids[order(model$masterPositions)]))
  
  model$thetas <- model$thetas |>
    as.data.frame() |>
    set_names(nm = model$ids) |>
    add_column(iteration = 1:model$MC) |>
    pivot_longer(cols = model$ids,
                 names_to = 'id',
                 values_to = 'age')
  return(model)
}
