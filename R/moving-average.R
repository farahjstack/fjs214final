moving_average <- function(df) {
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
    w1 <- result$window_start[i]
    w2 <- w1 + weeks(9)

    # Identify samples collected during the current window so only those observations contribute to that period's average.
    in_window <- df$Sample_Date >= w1 &
      df$Sample_Date < w2

    # Store the chemistry measurements from the same time window separtely so an average can be calculated for each ion.
    k_window <- df$K[in_window]
    mg_window <- df$Mg[in_window]
    ca_window <- df$Ca[in_window]
    no3_window <- df$'NO3-N'[in_window]
    nh4_window <- df$'NH4-N'[in_window]

    # Calculate the mean of each ion concentration. Ignore missing observations when averaging so available
    # measurements can still contribute to the smoothed stream chemistry record.
    result$k_mgl[i] <- mean(k_window, na.rm = TRUE)
    result$mg_mgl[i] <- mean(mg_window, na.rm = TRUE)
    result$ca_mgl[i] <- mean(ca_window, na.rm = TRUE)
    result$no3_mgl[i] <- mean(no3_window, na.rm = TRUE)
    result$nh4_mgl[i] <- mean(nh4_window, na.rm = TRUE)
  }
  # Return the result
  return(result)
}
