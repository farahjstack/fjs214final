library(tidyverse)
library(lubridate)

BQ1 <- read_csv("data/QuebradaCuenca1-Bisley.csv")


BQ2 <- read_csv("data/QuebradaCuenca2-Bisley.csv")


BQ3 <- read_csv("data/QuebradaCuenca3-Bisley.csv")


PRM <- read_csv("data/RioMameyesPuenteRoto.csv")

glimpse(BQ1)
glimpse(BQ2)
glimpse(BQ3)

BQ1 <- BQ1 |>
  filter(
    Sample_Date >= ymd("1988-01-31"),
    Sample_Date <= ymd("1994-12-31")
  )

BQ1_new <- tibble(
  window_start = seq(
    ymd("1988-01-31"),
    ymd("1994-12-31") - weeks(9),
    by = "1 week"
  )
)

BQ1_new$K <- NA_real_

# Create 9-week moving average of K (using for loop)
for (i in 1:nrow(BQ1_new)) {
  w1 <- BQ1_new$window_start[i] # Start of Window
  w2 <- w1 + weeks(9) # End of 9-week Window

  # K values in 9-week window
  K_values <- BQ1$K[
    BQ1$Sample_Date >= w1 &
      BQ1$sample_Date < w2
  ]

  # Find mean K values in 9-week window:
  K_mean <- mean(K_values, na.rm = TRUE)

  # Mean in BQ1_new
  BQ1_new$K[i] <- K_mean
}


ggplot(
  data = BQ1_new,
  mapping = aes(
    x = window_start,
    y = K
  )
) +
  geom_line() +
  labs(
    title = "9-Week Moving Average of Potassium (BQ1)",
    x = "Year",
    y = "Potassium (K mg/L)"
  ) +
  scale_x_date(
    limits = c(
      ymd("1988-01-31"),
      ymd("1994-12-31")
    ),
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  theme_minimal()
