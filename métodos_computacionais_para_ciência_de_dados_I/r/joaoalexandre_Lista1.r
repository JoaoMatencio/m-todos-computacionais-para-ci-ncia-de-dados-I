# List - 1
# Documenting R code with tests at the end

# 1) Create different sequences and vectors

# a) Sequence from -3 to 3 (step size implicitly 1)
vetor_seq_a <- -3:3

# b) Sequence from 2.4 to 10.4 (step size implicitly 1)
vetor_seq_b <- 2.4:10.4

# c) Sequence from 1 to 80 with step size 2
vetor_seq_c <- seq(1, 80, 2)

# d) Repeated sequence of 4:1, each number repeated 3 times
vetor_seq_d <- rep(4:1, each = 3)

# e) Repeated sequence of 1:3, entire sequence repeated 3 times
vetor_seq_e <- rep(1:3, times = 3)

# f) Character vector with Brazilian states
vetor_af.caracteres <- c('Parana', 'Sao Paulo', 'Minas Gerais')

# g) Create a factor vector indicating block membership
vetor_blocos <- factor(rep(c('Bloco_1', 'Bloco_2', 'Bloco_3'), times = c(2, 3, 3)))

# 2) Create a 10x10 matrix with the product of the row and column indices

matrix <- outer(1:10, 1:10, `*`)
for (i in 1:10) {
  for (j in 1:10) {
    matrix[i, j] = i * j
  }
}

# 3) Create a data frame with columns x, y = x^2, and z = exp(x) for x from 0 to 50
data_frame <- data.frame(x = 0:50, y = (0:50)^2, z = exp(0:50))

# 4) Numerical sequences with specific patterns

# a) Sequence where each term is half of the previous, starting with 1
a <- numeric(100)
a[1] <- 1
for (i in 2:100) {
  a[i] <- a[i-1] / 2
}

# b) Sequence where each term is the reciprocal of (i-1)*20 + 2
b <- numeric(100)
b[1] <- 1
for (i in 2:100){
  b[i] <- 1 / ((i-1)*20 + 2)
}

# c) Alternating series where each term is 1/i * (-1)^(i-1)
c <- numeric(100)
c[1] <- 1
for (i in 2:100){
  c[i] <- (1/i) * (-1)^(i-1)
}

# 5) Using base R to analyze ChickWeight dataset

# Load ChickWeight data
data_chick <- data.frame(ChickWeight)

# a) Filter ChickWeight for Diet 2
data_chick_diet_2 <- subset(data_chick, Diet == 2)

# b) Calculate mean weight for Diet 2 at Time 2
data_chick_diet_2_and_time_2 <- subset(data_chick_diet_2, Time == 2)

# c) Calculate mean weight for Diet 2 at Time 2
mean_data_chick_diet_2_and_time_2 <- mean(data_chick_diet_2_and_time_2$weight)

# 5) Using tidyverse to analyze ChickWeight dataset

# Load tidyverse
library(tidyverse)

# Load ChickWeight data
data("ChickWeight")

# a) Filter ChickWeight for Diet 2
chick_diet_2 <- ChickWeight %>% filter(Diet == 2)

# b) Calculate mean weight for Diet 2 at Time 2
mean_weight_diet_2_time_2 <- ChickWeight %>% 
  filter(Diet == 2, Time == 2) %>%
  summarize(mean_weight = mean(weight))

# Enable or disable tests
run_tests <- TRUE

if (run_tests) {
  failed_tests <- c()
  
  # Test for sequence vectors
  expected_seq_e <- c(1, 2, 3, 1, 2, 3, 1, 2, 3)
  if (!all(vetor_seq_e == expected_seq_e)) {
    failed_tests <- c(failed_tests, "vetor_seq_e")
  }
  
  # Test for the 10x10 matrix
  expected_matrix_first_row <- 1:10
  if (!all(matrix[1, ] == expected_matrix_first_row)) {
    failed_tests <- c(failed_tests, "matrix first row")
  }
  
  # Test for the data frame
  expected_df <- data.frame(x = 0:4, y = (0:4)^2, z = exp(0:4))
  if (!all.equal(data_frame[1:5, ], expected_df)) {
    failed_tests <- c(failed_tests, "data_frame first 5 rows")
  }
  
  # Tests for numerical sequences
  expected_a <- c(1, 0.5, 0.25, 0.125, 0.0625)
  if (!all(a[1:5] == expected_a)) {
    failed_tests <- c(failed_tests, "sequence a")
  }
  
  # Assuming specific expected values for sequences b and c
  expected_b <- c(1, 1/22, 1/42, 1/62, 1/82)  # Placeholder, adjust accordingly
  if (!all(b[1:5] == expected_b)) {
    failed_tests <- c(failed_tests, "sequence b")
  }
  
  # Assuming specific expected values for sequence c
  expected_c <- c(1, -0.5, 0.333, -0.25, 0.2)
  tolerance <- 1.0e-3
  if (!all(abs(c[1:5] - expected_c) < tolerance)) {
    failed_tests <- c(failed_tests, "sequence c")
  }
  
  # Assuming specific expected values for ChickWeight analyses
  # Placeholder, adjust accordingly
  expected_mean_weight <- 49.4  # Example expected value
  if (mean_data_chick_diet_2_and_time_2 != expected_mean_weight) {
    failed_tests <- c(failed_tests, "ChickWeight mean weight base R")
  }
  
  if (mean_weight_diet_2_time_2$mean_weight != expected_mean_weight) {
    failed_tests <- c(failed_tests, "ChickWeight mean weight tidyverse")
  }
  
  # Print results
  if (length(failed_tests) > 0) {
    cat("Tests failed for:\n")
    print(failed_tests)
  } else {
    cat("All tests passed successfully.\n")
  }
}
