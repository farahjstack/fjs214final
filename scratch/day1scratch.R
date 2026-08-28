library(tidyverse)
library(lubridate)

# IMPORT DATA ---------------------------------------------------------
BQ1 <- read_csv("data/QuebradaCuenca1-Bisley.csv") |>
  filter(Sample_Date >= ymd("1988-01-01") & Sample_Date <= ymd("1994-12-31"))

BQ2 <- read_csv("data/QuebradaCuenca2-Bisley.csv")
BQ3 <- read_csv("data/QuebradaCuenca3-Bisley.csv")
PRM <- read_csv("data/RioMameyesPuenteRoto.csv")


# MOVING AVERAGE FUNCTION ---------------------------------------------------------
# Use one function for all four sites so the 9-week moving average are calculated with the same method and time period for every watershed.
moving_average <- function(df) {
  # Use a fixed 1984-1994 sequence so all sites have matching
  # 9-week windows.
  result <- tibble(
    window_start = seq(
      df$Sample_Date[1],
      df$Sample_Date[nrow(df)],
      by = "9 weeks"
    ),
    Site_Name = df$Sample_ID[1],
    k_mgl = NA,
    mg_mgl = NA,
    ca_mgl = NA,
    no3_mgl = NA,
    nh4_mgl = NA
  )

  # Move throgh each 9-week period so concentrations can be summarized across the fully study period.
  for (i in 1:nrow(result)) {
    # Define the start and end of the window so each sample
    # is assigned to only one 9-week period.
    w1 <- result$window_start[i]
    w2 <- w1 + weeks(9)

    # Identify samples collected during the current window so only those observations contribute to that period's average.
    in_window <- df$Sample_Date >= w1 &
      df$Sample_Date < w2

    # Store the chemistry measurements from the same time window
    # separtely so an average can be calculated for each ion.

    mg_window <- df$Mg[in_window]

    ca_window <- df$Ca[in_window]

    no3_window <- df$'NO3-N'[in_window]

    nh4_window <- df$'NH4-N'[in_window]

    # Calculate the mean of each ion concentration.
    result$k_mgl[i] <- mean(k_window, na.rm = TRUE)

    result$mg_mgl[i] <- mean(mg_window, na.rm = TRUE)

    result$ca_mgl[i] <- mean(ca_window, na.rm = TRUE)

    result$no3_mgl[i] <- mean(no3_window, na.rm = TRUE)

    result$nh4_mgl[i] <- mean(nh4_window, na.rm = TRUE)
  }

  # Return the result
  return(result)
}

# 9-WEEK MOVING AVERAGES ---------------------------------------------------------
# Apply the same function to every watershed to keep the calculations consistent across all four study sites.
BQ1_new <- moving_average(BQ1)

BQ2_new <- moving_average(BQ2)

BQ3_new <- moving_average(BQ3)

PRM_new <- moving_average(PRM)

# COMBINE SITES ---------------------------------------------------------

# Combine the site-level results so all watersheds can be plotted and compared
# using a single data frame.
combined_dfs <- bind_rows(BQ1_new, BQ2_new, BQ3_new, PRM_new)

# PLOTTING ---------------------------------------------------------
# Place values of all sites and all chemistry values into one column:
plot_data <- combined_dfs |>
  pivot_longer(
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

clean_data <- read_csv("output/cleandata.csv")

ggplot(
  clean_data,
  mapping = aes(
    x = window_start,
    y = Concentration,
    linetype = Site_Name
  )
) +
  geom_line() +
  theme_bw() +
  facet_grid(
    vars(Ions),
    scales = "free_y",
    switch = "y"
  ) +
  scale_x_date(
    name = "Years"
  ) +
  labs(
    title = "Concentrations in Puerto Rico Streams Before and After Hurricane Hugo"
  )
