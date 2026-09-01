# The input to this function should be a data frame containing stream chemistry data
moving_average <- function(____) {
  # Initialize a tibble to contain the results
  result <- tibble(
    window_start = seq(____, ____, by = _____),
    k_mgl = NA,
    mg_mgl = NA,
    # Fill in the rest of the ions
  )

  # Fill in the iterator and sequence
  for (___ in ___) {
    # Create variables for the start and end of the current window
    w1 <- ___
    w2 <- ___

    # Create a logical vector, called "in_window", that says which samples are inside the window
    # Hint: you'll compare sample dates to the start and end of the window
    in_window <- ___

    # Use indexing to pull out the ion concentrations that fall inside the window
    k_window <- ___$___[___]
    # The line above gets potassium in the window. Get the rest of the ions too

    # Calculate the mean of each ion concentration and fill in the result
    result$k_mgl[___] <- mean(___)
  }
  
  # Return the result
}