library(tidyverse)


BQ1 <- read_csv("data/QuebradaCuenca1-Bisley.csv")


BQ2 <- read_csv("data/QuebradaCuenca2-Bisley.csv")


BQ3 <- read_csv("data/QuebradaCuenca3-Bisley.csv")


PRM <- read_csv("data/RioMameyesPuenteRoto.csv")

glimpse(BQ1)
glimpse(BQ2)
glimpse(BQ3)

# Creates 9-week blocks:
window_start <- seq(
  ymd(BQ1$Sample_Date[1]),
  ymd(BQ1$Sample_Date[nrow(BQ1)]),
  by = "9 weeks"
)

#Creating a column "Window_Start"
BQ1_new <- tibble(
  window_start = seq(
  ymd(BQ1$Sample_Date[1]),
  ymd(BQ1$Sample_Date[nrow(BQ1)]),
  by = "9 weeks"
  )
)

# This is our new column for potassium moving average
BQ1_new$K <- NA_real_

for (i in 1:nrow(BQ1_new)) {
  # i is our iterator
  # 1:nrow(BQ1_new) is our sequence
  # i will take on those values, one at a time
 
  # What's the start of the window? Call it w1
  w1 <- BQ1_new$window_start[i]
  print(w1)

  # What's the end of the window? W2 is a new variable where we add 9-weeks to W1)
  w2 <- w1 + weeks(9)
  print(w2)

  # What potassium (K) values are inside that window?
  K_values <- BQ1$K[BQ1$Sample_Date >= w1 & BQ1$Sample_Date < w2]
  print(K_values)

  # What's the mean potassium (K) concentration?
  K_mean <- mean(K_values, na.rm = TRUE)

  # How do you put it in the result(K)?
  BQ1_new$K[i] <- K_mean

}

# View the new dataframe (window_start dates and mean K values!)
glimpse(BQ1_new)


# Plotting the moving average of potassium:
ggplot(
  data = BQ1_new,
  mapping = aes(
    x = window_start,
    y = K
)
) +
geom_line() +
  labs(
    x = "Date",
    y = "9-week moving average K (mg/L)"
  ) +
  theme_minimal()


