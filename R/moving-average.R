library(tidyverse)
library(lubridate)

# Import the four datasets separately
BQ1 <- read_csv("data/QuebradaCuenca1-Bisley.csv")


BQ2 <- read_csv("data/QuebradaCuenca2-Bisley.csv")


BQ3 <- read_csv("data/QuebradaCuenca3-Bisley.csv")


PRM <- read_csv("data/RioMameyesPuenteRoto.csv")

# Create moving average function
moving_average <- function(df) {
  result <- tibble(
    window_start = seq(
      ymd(df$Sample_Date[1]),
      ymd(df$Sample_Date[nrow(df)]),
      by = "9 weeks"
    ),
    k_mgl = NA,
    mg_mgl = NA,
    ca_mgl = NA,
    no3_mgl = NA,
    nh4_mgl = NA
  )

  # Fill in the iterator and sequence
  for (i in 1:nrow(result)) {
    # Create variables for the start and end of the current window
    w1 <- result$window_start[i]
    w2 <- w1 + weeks(9)

    # Create a logical vector, called "in_window",
    # that says which samples are inside the window
    # Hint: you'll compare sample dates to the start and end of the window
    in_window <- df$Sample_Date >= w1 &
      df$Sample_Date < w2

    # Use indexing to pull out the ion concentrations that fall inside the window
    k_window <- df$K[in_window]

    mg_window <- df$Mg[in_window]

    ca_window <- df$Ca[in_window]

    no3_window <- df$'NO3-N'[in_window]

    nh4_window <- df$'NH4-N'[in_window]

    # The line above gets potassium in the window. Get the rest of the ions too

    # Calculate the mean of each ion concentration and fill in the result
    result$k_mgl[i] <- mean(k_window, na.rm = TRUE)

    result$mg_mgl[i] <- mean(mg_window, na.rm = TRUE)

    result$ca_mgl[i] <- mean(ca_window, na.rm = TRUE)

    result$no3_mgl[i] <- mean(no3_window, na.rm = TRUE)

    result$nh4_mgl[i] <- mean(nh4_window, na.rm = TRUE)
  }

  # Return the result
  return(result)
}

BQ1_new <- moving_average(BQ1)

BQ2_new <- moving_average(BQ2)

BQ3_new <- moving_average(BQ3)

PRM_new <- moving_average(PRM)


# Create Site_Name column for each dataframe
BQ1_new <- BQ1_new |> mutate(Site_Name = "BQ1")

BQ2_new <- BQ2_new |> mutate(Site_Name = "BQ2")

BQ3_new <- BQ3_new |> mutate(Site_Name = "BQ3")

PRM_new <- PRM_new |> mutate(Site_Name = "PRM")

# Combine all dfs into a new df
combined_dfs <- bind_rows(BQ1_new, BQ2_new, BQ3_new, PRM_new)

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
    names_to = "Chemical",
    values_to = "Moving_Average"
  )

ggplot(
  data = plot_data,
  mapping = aes(
    x = ,
    y =
  )
) +
geom_line() +
labs(
  title =
    x = ,
    y =
) +
scale_x_date(
  limits = c(
    ymd("1988-01-31"),
    ymd("1994-12-31")
  )
) + theme_minmimal()

