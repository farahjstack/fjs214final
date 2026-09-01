library(tidyverse)
library(lubridate)
source("R/moving-average.R")

# IMPORT DATA ---------------------------------------------------------
BQ1 <- read_csv("data/QuebradaCuenca1-Bisley.csv") |>
  filter(Sample_Date >= ymd("1988-01-01") & Sample_Date <= ymd("1994-12-31"))


BQ2 <- read_csv("data/QuebradaCuenca2-Bisley.csv") |>
  filter(Sample_Date >= ymd("1988-01-01") & Sample_Date <= ymd("1994-12-31"))


BQ3 <- read_csv("data/QuebradaCuenca3-Bisley.csv") |>
  filter(Sample_Date >= ymd("1988-01-01") & Sample_Date <= ymd("1994-12-31"))


PRM <- read_csv("data/RioMameyesPuenteRoto.csv") |>
  filter(Sample_Date >= ymd("1988-01-01") & Sample_Date <= ymd("1994-12-31"))


# 9-WEEK MOVING AVERAGE ---------------------------------------------------------
BQ1_new <- moving_average(df = BQ1)
BQ2_new <- moving_average(df = BQ2)
BQ3_new <- moving_average(df = BQ3)
PRM_new <- moving_average(df = PRM)

# Combine the site-level results so all watersheds can be plotted and compared using a single data frame.
combined <- bind_rows(BQ1_new, BQ2_new, BQ3_new, PRM_new)

# RESHAPING ---------------------------------------------------------
reshape_data <-
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

write_csv(reshape_data, "output/cleandata.csv")
