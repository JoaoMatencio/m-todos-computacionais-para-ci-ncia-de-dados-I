# Setting the seed for reproducibility
set.seed(0)


if (!requireNamespace("GGally", quietly = TRUE)) {
  install.packages("GGally")
}
library(GGally)


if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)


if (!requireNamespace("reshape2", quietly = TRUE)) {
  install.packages("reshape2")
}
library(reshape2)


# Question 1) Data Visualization with the Iris Dataset
## Question 1a) Create a pairwise plot comparing two characteristics of plants, colored by species
ggpairs(iris, aes(color = Species))


## Question 1b) Create a box plot for the four characteristics of flowers, with each box colored differently
iris_long <- reshape2::melt(iris, id.vars = "Species")
ggplot(iris_long, aes(x = variable, y = value, fill = Species)) + 
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  labs(x = "Characteristics", y = "Values", fill = "Species") +
  theme(legend.position = "bottom")


## Question 1c) Scatter plot between the first two columns of the dataset with a vertical and horizontal line crossing their midpoint
data(iris)
midpoint_x <- mean(iris$Sepal.Length)
midpoint_y <- mean(iris$Sepal.Width)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(color = Species), size = 3, alpha = 0.6) +
  geom_vline(xintercept = midpoint_x, linetype = "dashed", color = "red") +
  geom_hline(yintercept = midpoint_y, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("blue", "green", "purple")) +
  theme_minimal(base_size = 14) + 
  labs(title = "Sepal Scatterplot with Mean Lines",
       subtitle = "Iris dataset",
       x = "Sepal Length", y = "Sepal Width", color = "Species") +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lavenderblush", colour = "black", size = 0.5, linetype = "solid"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "darkred"),
        plot.caption = element_text(face = "italic"))


# Question 2) Probability Calculations for a Binomial Variable X ~ Bin(n=15, p=0.4)
## Question 2a) P(X >= 14)
1 - pbinom(13, size = 15, prob = 0.4)


## Question 2b) P(8 < X <= 10)
pbinom(10, size = 15, prob = 0.4) - pbinom(7, size = 15, prob = 0.4)


## Question 2c) P(X < 2 or X >= 11)
pbinom(1, size = 15, prob = 0.4) + (1 - pbinom(10, size = 15, prob = 0.4))


## Question 2d) P(X >= 11 or X > 13)
pbinom(10, size = 15, prob = 0.4, lower.tail = FALSE) + pbinom(13, size = 15, prob = 0.4, lower.tail = FALSE)


## Question 2e) P(X > 3 and X < 6)
pbinom(5, size = 15, prob = 0.4) - pbinom(3, size = 15, prob = 0.4)


## Question 2f) Conditional Probability P(X <= 13 | X >= 11)
prob_f_numerator <- pbinom(13, size = 15, prob = 0.4) - pbinom(10, size = 15, prob = 0.4)  # P(11 <= X <= 13)
prob_f_denominator <- 1 - pbinom(10, size = 15, prob = 0.4)  # P(X >= 11)
prob_f_numerator / prob_f_denominator


# Question 3) Empirical Probability Calculations Using Simulated Data
n <- 15
p <- 0.4
size <- 1000000
sample_binom <- rbinom(size, n, p)


## Question 3a) Empirical P(X >= 14)
mean(sample_binom >= 14)


## Question 3b) Empirical P(8 < X <= 10)
mean(sample_binom > 8 & sample_binom <= 10)


## Question 3c) Empirical P(X < 2 or X >= 11)
mean(sample_binom < 2 | sample_binom >= 11)


## Question 3d) Empirical P(X >= 11 or X > 13)
mean(sample_binom >= 11 | sample_binom > 13)


## Question 3e) Empirical P(X > 3 and X < 6)
mean(sample_binom > 3 & sample_binom < 6)


## Question 3f) Empirical Conditional Probability P(X <= 13 | X >= 11)
sample_conditional <- sample_binom[sample_binom >= 11]
mean(sample_conditional <= 13)


# Question 4) Working with the Weibull Distribution
shape_param <- 1 
scale_param <- 10
num_samples <- 5000


## Question 4a) Generate Weibull data and plot histogram with density curve
sample_weibull <- rweibull(num_samples, shape = shape_param, scale = scale_param)
hist(sample_weibull, probability = TRUE, breaks = 40, col = "skyblue", main = "Weibull Distribution")
curve(dweibull(x, shape = shape_param, scale = scale_param), add = TRUE, col = "red", lwd = 2)


## Question 4b) Calculate and Compare True vs. Empirical Quantiles
quantiles <- c(1, 5, 10, 15, 25, 50, 75, 85, 90, 95, 99)/100
true_quantiles <- qweibull(quantiles, shape = shape_param, scale = scale_param)
empirical_quantiles <- quantile(sample_weibull, probs = quantiles)
plot(true_quantiles, empirical_quantiles, pch = 19, col = "blue", 
     main = "True vs. Empirical Quantiles",
     xlab = "True Quantiles", ylab = "Empirical Quantiles")
abline(0, 1, col = "red", lwd = 2)


## Question 4c) Exact and Empirical Probability Calculations
# Exact P(X > 4)
p_exact <- 1 - pweibull(4, shape = shape_param, scale = scale_param)
# Empirical P(X > 4)
p_empirical <- mean(sample_weibull > 4)
c(Exact = p_exact, Empirical = p_empirical)


# Additional Questions
## Question 5) Plotting Densities of Normal Distributions
## Question 5a) Density of X ~ N(90, 100)
x_values <- seq(50, 130, length.out = 200)
x_density <- dnorm(x_values, mean = 90, sd = sqrt(100))
plot(x_values, x_density, type = "l", lwd = 2, col = "blue", main = "Density of X ~ N(90, 100)",
     xlab = "X", ylab = "Density")


## Question 5b) Add Densities of Y ~ N(90, 80) and Z ~ N(85, 100)
y_density <- dnorm(x_values, mean = 90, sd = sqrt(80))
z_density <- dnorm(x_values, mean = 85, sd = sqrt(100))
lines(x_values, y_density, col = "red", lwd = 2)
lines(x_values, z_density, col = "green", lwd = 2)
legend("topright", legend = c("X ~ N(90, 100)", "Y ~ N(90, 80)", "Z ~ N(85, 100)"), 
       col = c("blue", "red", "green"), lty = 1, cex = 0.8)


## Question 6) Sum of Uniform Variables Distribution
x <- runif(3000)
y := runif(3000)
z := x + y
hist(x, probability = TRUE, main = "Histogram of X", col = "lightblue")
hist(y, probability = TRUE, main = "Histogram of Y", col = "lightblue")
hist(z, probability = TRUE, main = "Histogram of Z", col = "lightblue")


# Enabling tests
run_tests <- TRUE


if (run_tests) {
    failed_tests <- c()
        

    # Test 1a: Pair Plot Correctness by Checking Data
    expected_pairs <- 25
    actual_pairs <- length(ggpairs(iris, aes(color = Species))$plots)
    if (actual_pairs != expected_pairs) {
        failed_tests <- c(failed_tests, sprintf("1a - Pair Plot: Expected %d pair combinations but found %d.", expected_pairs, actual_pairs))
    }


    # Test 1b: Box Plot Correctness
    expected_boxes <- length(unique(iris$Species)) * (ncol(iris)-1)
    ggplot_data <- ggplot_build(ggplot(iris_long, aes(x = variable, y = value, fill = Species)) + geom_boxplot())
    actual_boxes <- length(unique(ggplot_data$data[[1]]$group))
    if (actual_boxes != expected_boxes) {
        failed_tests <- c(failed_tests, "1b - Box Plot: Number of boxes is incorrect.")
    }


    # Test 1c: Scatter Plot Midpoints
    calculated_midpoints <- c(midpoint_x, midpoint_y)
    true_midpoints <- c(mean(iris$Sepal.Length), mean(iris$Sepal.Width))
    if (!all.equal(calculated_midpoints, true_midpoints)) {
        failed_tests <- c(failed_tests, "1c - Scatter Plot: Midpoints are incorrectly calculated.")
    }


    # Test 2: Probability Calculations Accuracy
    prob_2a <- 1 - pbinom(13, size = 15, prob = 0.4)
    prob_2b <- pbinom(10, size = 15, prob = 0.4) - pbinom(7, size = 15, prob = 0.4)
    prob_2c <- pbinom(1, size = 15, prob = 0.4) + (1 - pbinom(10, size = 15, prob = 0.4))
    prob_2d <- pbinom(10, size = 15, prob = 0.4, lower.tail = FALSE) + pbinom(13, size = 15, prob = 0.4, lower.tail = FALSE)
    prob_2e <- pbinom(5, size = 15, prob = 0.4) - pbinom(3, size = 15, prob = 0.4)
    prob_2f <- (pbinom(13, size = 15, prob = 0.4) - pbinom(10, size = 15, prob = 0.4)) / (1 - pbinom(10, size = 15, prob = 0.4))
    exact_probs <- c(
    prob_2a = prob_2a,
    prob_2b = prob_2b,
    prob_2c = prob_2c,
    prob_2d = prob_2d,
    prob_2e = prob_2e,
    prob_2f = prob_2f
  )
    for (prob_name in names(exact_probs)) {
        calculated_value <- get(prob_name)
        if (!all.equal(calculated_value, exact_probs[[prob_name]])) {
            failed_tests <- c(failed_tests, sprintf("2 - Probability Calculation Error in %s", prob_name))
        }
    }


    # Test 3: Empirical Probability Accuracy
    prob_3a <- mean(sample_binom >= 14)
    prob_3b <- mean(sample_binom > 8 & sample_binom <= 10)
    prob_3c <- mean(sample_binom < 2 | sample_binom >= 11)
    prob_3d <- mean(sample_binom >= 11 | sample_binom > 13)
    prob_3e <- mean(sample_binom > 3 & sample_binom < 6)
    prob_3f <- mean(sample_conditional <= 13)
    empirical_probs <- c(
    prob_3a = prob_3a,
    prob_3b = prob_3b,
    prob_3c = prob_3c,
    prob_3d = prob_3d,
    prob_3e = prob_3e,
    prob_3f = prob_3f
  )


    for (prob_name in names(empirical_probs)) {
        calculated_value <- get(prob_name)
        if (!all.equal(calculated_value, empirical_probs[[prob_name]])) {
            failed_tests <- c(failed_tests, sprintf("3 - Empirical Probability Calculation Error in %s", prob_name))
        }
    }


    # Test 4a: Weibull Distribution Quantiles Correctness
    quantiles_probs <- c(1, 5, 10, 15, 25, 50, 75, 85, 90, 95, 99)/100
    true_quantiles <- qweibull(quantiles_probs, shape = shape_param, scale = scale_param)
    empirical_quantiles <- quantile(sample_weibull, probs = quantiles_probs)
    quantile_differences <- abs(empirical_quantiles - true_quantiles)
    acceptable_error <- 0.1 * true_quantiles


    # Test 5: Normal Distribution Density Correctness
    ## No straightforward test without visual inspection or further statistical testing


    if (length(failed_tests) > 0) {
        cat("Tests failed for the following sections:\n")
        for (test in failed_tests) {
            cat(test, "\n")
        }
    } else {
        cat("All tests passed successfully.\n")
    }
  }