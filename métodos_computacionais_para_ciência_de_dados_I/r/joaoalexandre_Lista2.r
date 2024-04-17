# Question 1: Factorial Calculation


# This function calculates the factorial of a number using a loop.
# It iteratively multiplies values from 1 to n.
calculate_factorial <- function(n) {
  value <- 1
  for (i in 1:n) {
    value <- value * i
  }
  return(value)
}


# Measuring and comparing execution times of calculating factorial using a loop and the built-in factorial function.
n <- 10
start_time_for <- Sys.time()
factorial_result_loop <- calculate_factorial(n)
end_time_for <- Sys.time()


start_time_factorial <- Sys.time()
factorial_result_builtin <- factorial(n)  # Using R's built-in factorial function for comparison.
end_time_factorial <- Sys.time()


time_for <- end_time_for - start_time_for
time_factorial <- end_time_factorial - start_time_factorial


# Outputting the time taken for each factorial calculation method.
print(paste("Time for factorial using loop: ", time_for))
print(paste("Time for factorial using factorial(): ", time_factorial))


# Question 2: Finding Maximum Value


# This function finds the maximum value in a vector using a for loop.
# It iterates through each element, keeping track of the highest value found.
find_max_value <- function(vector) {
  max_value <- vector[1]
  for (value in vector) {
    if (value > max_value) {
      max_value <- value
    }
  }
  return(max_value)
}


# Measuring execution time for finding the maximum value in a Fibonacci sequence using a loop and the built-in max function.
vetor_fibonacci <- c(1, 1, 3, 5, 8, 13, 21, 34, 55, 89)
start_time_for_max <- Sys.time()
max_value_loop <- find_max_value(vetor_fibonacci)
end_time_for_max <- Sys.time()


start_time_max <- Sys.time()
max_value_builtin <- max(vetor_fibonacci)  # Using R's built-in max function for comparison.
end_time_max <- Sys.time()


time_for_max <- end_time_for_max - start_time_for_max
time_max <- end_time_max - start_time_max


# Outputting the time taken and the speed difference for finding the maximum value.
print(paste("Time to find max using loop: ", time_for_max))
print(paste("Time to find max using max(): ", time_max))
speed_difference <- as.numeric(time_for_max) / as.numeric(time_max)
print(paste("The max() function is", speed_difference, "times faster than the loop."))


# Question 3: Summing a Vector


# This function calculates the sum of a vector using a while loop.
# It adds each element to the sum until it has iterated through the entire vector.
sum_with_while <- function(vector) {
  sum <- 0
  i <- 1
  while (i <= length(vector)) {
    sum <- sum + vector[i]
    i <- i + 1
  }
  return(sum)
}


# This function calculates the sum of a vector using a repeat loop.
# Similar to the while loop but with a break condition inside the loop.
sum_with_repeat <- function(vector) {
  sum <- 0
  i <- 1
  repeat {
    sum <- sum + vector[i]
    i <- i + 1
    if (i > length(vector)) {
      break
    }
  }
  return(sum)
}


vector_1_1000 <- 1:1000


# Measuring execution time for summing a vector using custom functions and the built-in sum function.
start_time_while <- Sys.time()
sum_vector_with_while <- sum_with_while(vector_1_1000)
end_time_while <- Sys.time()


start_time_repeat <- Sys.time()
sum_vector_with_repeat <- sum_with_repeat(vector_1_1000)
end_time_repeat <- Sys.time()


start_time_builtin <- Sys.time()
sum_vector_with_sum <- sum(vector_1_1000)
end_time_builtin <- Sys.time()


time_while <- end_time_while - start_time_while
time_repeat <- end_time_repeat - start_time_repeat
time_builtin <- end_time_builtin - start_time_builtin


# Outputting the time taken and the speed difference for summing the vector.
diff_time_while <- as.numeric(time_while) / as.numeric(time_builtin)
diff_time_repeat <- as.numeric(time_repeat) / as.numeric(time_builtin)
print(paste("While loop is", diff_time_while, "times slower than built-in sum()."))
print(paste("Repeat loop is", diff_time_repeat, "times slower than built-in sum()."))


# Question 4:) Measures of Central Tendency


# Load necessary package for geometric mean if not already installed
if (!require(psych)) install.packages("psych", dependencies=TRUE)
if (!require(DescTools)) install.packages("DescTools", dependencies=TRUE)


# Function to calculate measures of central tendency
calculate_measure <- function(vector, measure) {
  if (measure == "mean") {
    result <- mean(vector)
  } else if (measure == "median") {
    result <- median(vector)
  } else if (measure == "geometric_mean") {
    # Using psych::geometric.mean for geometric mean
    result <- psych::geometric.mean(vector)
  } else if (measure == "harmonic_mean") {
    # Corrected usage: Using DescTools::Hmean for harmonic mean
    result <- DescTools::Hmean(vector)
  } else if (measure == "trimmed_mean") {
    # Calculating trimmed mean with 10% trimmed from both ends
    result <- mean(vector, trim = 0.1)
  } else {
    result <- NA
    warning("Chosen measure is invalid.")
  }
  return(result)
}


# Example usage of the function
vector <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
chosen_measure <- "mean" # Options: "mean", "median", "geometric_mean", "harmonic_mean", "trimmed_mean"
result <- calculate_measure(vector, chosen_measure)
print(result)


# Question 5:) Measures of Central Tendency using switch


# Load necessary packages for geometric mean and harmonic mean if not already installed
if (!require(psych)) install.packages("psych", dependencies=TRUE)
if (!require(DescTools)) install.packages("DescTools", dependencies=TRUE)


# Function to calculate central tendency measures using switch
calculate_measure_switch <- function(vector, measure) {
  result <- switch(measure,
                   "mean" = mean(vector),
                   "median" = median(vector),
                   "geometric_mean" = psych::geometric.mean(vector),
                   "harmonic_mean" = DescTools::Hmean(vector),
                   "trimmed_mean" = mean(vector, trim = 0.1),
                   stop("Chosen measure is invalid.")
  )
  return(result)
}


# Example usage of the function
vector <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
chosen_measure <- "mean" # Options: "mean", "median", "geometric_mean", "harmonic_mean", "trimmed_mean"
result <- calculate_measure_switch(vector, chosen_measure)
print(result)


# Question 6:) Triangular Density Function


triangular_density <- function(x, a, b, c) {
  if (x < a || x > c) {
    # If x is outside of [a, c], the density is zero.
    return(0)
  } else if (a <= x && x <= b) {
    # Calculate density in the first interval [a, b].
    return((2 * (x - a)) / ((b - a) * (c - a)))
  } else if (b < x && x <= c) {
    # Calculate density in the second interval (b, c].
    return((2 * (c - x)) / ((c - b) * (c - a)))
  } else {
    # If x is not in any of the specified intervals, return 0.
    return(0)
  }
}


# Example usage:
# Define the parameters for the triangular distribution
a <- 1
b <- 3
c <- 5


# Define the point at which to calculate the density
x <- 2.5


# Calculate the density
density <- triangular_density(x, a, b, c)
print(density)


# Question 7) Reading Names from a Text Connection


# Creating a text connection that simulates a text file with 5 names
names_connection <- textConnection("John
Maria
Peter
Anna
Luis")


# Using readLines to read from the text connection and store the names in a vector
names <- readLines(names_connection)


# Closing the text connection
close(names_connection)


# Displaying the names vector
print(names)


# Question 8) Writing and Reading Data


# Install and load the necessary packages if not installed already
if (!requireNamespace("nycflights13", quietly = TRUE)) {
  install.packages("nycflights13")
}
if (!requireNamespace("gapminder", quietly = TRUE)) {
  install.packages("gapminder")
}
if (!requireNamespace("MASS", quietly = TRUE)) {
  install.packages("MASS")
}
if (!requireNamespace("writexl", quietly = TRUE)) {
  install.packages("writexl")
}


# Load libraries
library(nycflights13)
library(gapminder)
library(MASS)
library(writexl)


# Choose datasets
data1 <- as.data.frame(faithful)  # Sample dataset 1 from MASS package
data2 <- nycflights13::flights     # Sample dataset 2 from nycflights13 package
data3 <- gapminder::gapminder     # Sample dataset 3 from gapminder package


# Write data to text file (.txt)
write.table(data1, "data1.txt", sep = "\t", row.names = FALSE)
write.csv(data2, "data2.csv", row.names = FALSE)
write_xlsx(data3, "data3.xlsx")


# Import data back into R
data1 <- read.table("data1.txt", header = TRUE)
data2 <- read.csv("data2.csv", header = TRUE)
data3 <- readxl::read_xlsx("data3.xlsx")


# Displaying data
print(head(data1))
print(head(data2))
print(head(data3))


# Question 9) Custom Function for Incrementing a Variable


`+=` <- function(variable, value) {
  variable <- variable + value
  return(variable)
}


# Example usage:
x <- 5
x <- `+=`(x, 3)
print(x)  # Output will be 8


# Enabling tests to verify the correctness of custom functions against the built-in functions.
run_tests <- TRUE


if (run_tests) {
  failed_tests <- c()


  # Test for factorial calculation correctness
  if (factorial_result_loop != factorial_result_builtin) {
    failed_tests <- c(failed_tests, "Factorial calculation")
  }


  # Test for maximum value finding correctness
  if (max_value_loop != max_value_builtin) {
    failed_tests <- c(failed_tests, "Max value finding")
  }


  # Test for sum calculation correctness using while loop
  if (sum_vector_with_while != sum_vector_with_sum) {
    failed_tests <- c(failed_tests, "Sum calculation with while loop")
  }


  # Test for sum calculation correctness using repeat loop
  if (sum_vector_with_repeat != sum_vector_with_sum) {
    failed_tests <- c(failed_tests, "Sum calculation with repeat loop")
  }


  # Tests for measures of central tendency
  test_vector <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  expected_results <- list(
    mean = mean(test_vector),
    median = median(test_vector),
    geometric_mean = psych::geometric.mean(test_vector),
    harmonic_mean = DescTools::Hmean(test_vector),
    trimmed_mean = mean(test_vector, trim = 0.1)
  )
  

  for (measure in names(expected_results)) {
    calculated_result <- calculate_measure(test_vector, measure)
    if (!is.na(calculated_result) && calculated_result != expected_results[[measure]]) {
      failed_tests <- c(failed_tests, paste("Central Tendency -", measure))
    }
  }


  # Test for triangular density function correctness
  test_density <- triangular_density(2.5, 1, 3, 5)
  expected_density <- (2 * (2.5 - 1)) / ((3 - 1) * (5 - 1)) # Manually calculated expected value
  if (test_density != expected_density) {
    failed_tests <- c(failed_tests, "Triangular density function")
  }


  # Test for reading names from a text connection
  expected_names <- c("John", "Maria", "Peter", "Anna", "Luis")
  if (!all(names == expected_names)) {
    failed_tests <- c(failed_tests, "Reading names from text connection")
  }


  # Test for incrementing variable function correctness
  x <- 5
  x <- `+=`(x, 3)
  if (x != 8) {
    failed_tests <- c(failed_tests, "Incrementing variable")
  }


  # Print test results
  if (length(failed_tests) > 0) {
    cat("Tests failed for the following sections:\n")
    for (test in failed_tests) {
      cat(test, "\n")
    }
  } else {
    cat("All tests passed successfully.\n")
  }
}