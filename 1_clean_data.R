library(tidyverse)
library(lubridate)
source("R/moving-average.R")

# Import the four datasets separately
BQ1 <- read_csv("data/QuebradaCuenca1-Bisley.csv")
BQ2 <- read_csv("data/QuebradaCuenca2-Bisley.csv")
BQ3 <- read_csv("data/QuebradaCuenca3-Bisley.csv")
PRM <- read_csv("data/RioMameyesPuenteRoto.csv")


# Create moving average function
BQ1_new <- moving_average(df = BQ1)
BQ2_new <- moving_average(df = BQ2)
BQ3_new <- moving_average(df = BQ3)
PRM_new <- moving_average(df = PRM)

# Combine all dfs into a new df
combined <- bind_rows(BQ1_new, BQ2_new, BQ3_new, PRM_new)

# Place values of all sites and all chemistry values into one column:
plot_data <-
  pivot_longer(
    combined,
    cols = c(
      k_mgl,
      mg_mgl,
      ca_mgl,
      no3_mgl,
      nh4_mgl
    ),
    names_to = "Ions",
    values_to = "Concentration"
  )

write_csv(plot_data, "output/cleandata.csv")
