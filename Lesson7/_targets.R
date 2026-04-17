###targets.R
library(targets)
library(tarchetypes)
tar_source()
tar_option_set(packages = c("readr", "dplyr", "ggplot2", "ggsci", "sf"))

list(
  tar_target(data, get_data(3, 50)),
  tar_target(data_plot, make_data_plot(data)),
  tar_target(grid, make_grid_data(data))
)
