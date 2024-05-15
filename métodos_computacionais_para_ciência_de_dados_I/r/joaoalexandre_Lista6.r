# Load necessary libraries
library(readr)
library(dplyr)
library(future.apply)
library(microbenchmark)

# Question 1) Data Manipulation with the Census 2010 Dataset

## Question 1a) Read the Data Files by State (available in the `dados` folder)

# Define the path to the folder containing the state data
data_path <- "métodos_computacionais_para_ciência_de_dados_I/r/joaoalexandre_Lista6/dados/estados"

# List the files in the folder
state_files <- list.files(data_path, pattern = "*.txt", full.names = TRUE)

# Create a list to store the data
state_data <- list()

# Read the data from each file and store it in the list
for (file in state_files) {
  state_name <- tools::file_path_sans_ext(basename(file))
  state_data[[state_name]] <- read_delim(file, delim = ";", col_names = TRUE, trim_ws = TRUE, show_col_types = FALSE)
}

## Question 1b) Combine the Data into a Single File

# Combine all dataframes into a single dataframe
combined_data <- bind_rows(state_data)

# Check the structure of the combined dataframe
str(combined_data)

# Save the combined dataframe to a CSV file
write_csv(combined_data, "combined_data.csv")

## Question 1c) Observe that the Dataset Contains Segmented Population Data

# Load the combined dataset
combined_data <- read_csv("combined_data.csv")

# Check the structure of the combined dataset
str(combined_data)

# Display the first few rows of the combined dataset
head(combined_data)

# Summary of the columns
summary(combined_data)

# Analyze the population segmented by different criteria

# Total population by state
population_by_state <- combined_data %>%
  group_by(NO_UF) %>%
  summarise(Total_Population = sum(POP, na.rm = TRUE))

# Display the total population by state
print(population_by_state)

# Population by gender
population_by_gender <- combined_data %>%
  group_by(SEXO) %>%
  summarise(Total_Population = sum(POP, na.rm = TRUE))

# Display the population by gender
print(population_by_gender)

# Population by ethnicity
population_by_ethnicity <- combined_data %>%
  group_by(ETNIA) %>%
  summarise(Total_Population = sum(POP, na.rm = TRUE))

# Display the population by ethnicity
print(population_by_ethnicity)

# Population by domicile situation (urban/rural)
population_by_domicile <- combined_data %>%
  group_by(SIT_DOM) %>%
  summarise(Total_Population = sum(POP, na.rm = TRUE))

# Display the population by domicile situation
print(population_by_domicile)

# Total population by domicile situation
cat("\n1. **Total Population by Domicile Situation**:\n")
cat("   - Rural: ", population_by_domicile$Total_Population[population_by_domicile$SIT_DOM == "Rural"], "\n")
cat("   - Urban: ", population_by_domicile$Total_Population[population_by_domicile$SIT_DOM == "Urbana"], "\n")

# Total population by state
cat("\n2. **Total Population by State**:\n")
print(population_by_state)

# Total population by gender
cat("\n3. **Total Population by Gender**:\n")
print(population_by_gender)
library(future.apply)

# Total population by ethnicity
cat("\n4. **Total Population by Ethnicity**:\n")
print(population_by_ethnicity)

## Question 1d) Create an Auxiliary Database Containing Populations by Municipality

# Population by municipality
population_by_municipality <- combined_data %>%
  group_by(COD_MUN, NO_MUN) %>%
  summarise(Total_Population = sum(POP, na.rm = TRUE))

# Display the population by municipality
print(population_by_municipality)

# Save the population by municipality to a CSV file
write_csv(population_by_municipality, "population_by_municipality.csv")

# Question 1e) Discover municipalities with more than 500,000 inhabitants

# Load the population by municipality data
population_by_municipality <- read_csv("population_by_municipality.csv")

# Filter municipalities with more than 500,000 inhabitants
municipalities_over_500k <- population_by_municipality %>%
  filter(Total_Population > 500000)

# Display the municipalities with more than 500,000 inhabitants
print(municipalities_over_500k)

# Question 1f) Restrict the initial dataset to municipalities found in item 5

# Load necessary libraries
library(readr)
library(dplyr)

# Load the combined data
combined_data <- read_csv("combined_data.csv")

# Load the population by municipality data
population_by_municipality <- read_csv("population_by_municipality.csv")

# Filter municipalities with more than 500,000 inhabitants
municipalities_over_500k <- population_by_municipality %>%
  filter(Total_Population > 500000)

# Restrict the combined data to municipalities with more than 500,000 inhabitants
restricted_data <- combined_data %>%
  filter(COD_MUN %in% municipalities_over_500k$COD_MUN)

# Save the restricted data to a CSV file
write_csv(restricted_data, "restricted_data.csv")

# Display the first few rows of the restricted data
head(restricted_data)

## Question 1g) Create 4 auxiliary databases

# Load the restricted data
restricted_data <- read_csv("restricted_data.csv")

# i. Population by municipality and sex
population_by_sex <- restricted_data %>%
  group_by(COD_MUN, NO_MUN, SEXO) %>%
  summarise(Total_Population = sum(POP, na.rm = TRUE))

# Save the population by sex data to a CSV file
write_csv(population_by_sex, "population_by_sex.csv")

# ii. Population by municipality and ethnicity
population_by_ethnicity <- restricted_data %>%
  group_by(COD_MUN, NO_MUN, ETNIA) %>%
  summarise(Total_Population = sum(POP, na.rm = TRUE))

# Save the population by ethnicity data to a CSV file
write_csv(population_by_ethnicity, "population_by_ethnicity.csv")
2. Populações do Censo 2010 - Parte 2
(a) Para cada município do item 5 da parte 1 crie um relatório
# iii. Population by municipality and literacy
population_by_literacy <- restricted_data %>%
  group_by(COD_MUN, NO_MUN, LE_ESCREVE) %>%
  summarise(Total_Population = sum(POP, na.rm = TRUE))

# Save the population by literacy data to a CSV file
write_csv(population_by_literacy, "population_by_literacy.csv")

# iv. Population by municipality and domicile situation
population_by_domicile <- restricted_data %>%
  group_by(COD_MUN, NO_MUN, SIT_DOM) %>%
  summarise(Total_Population = sum(POP, na.rm = TRUE))

# Save the population by domicile data to a CSV file
write_csv(population_by_domicile, "population_by_domicile.csv")

## Question 1h) Create a Summary of the Databases
# Plan for parallel execution
plan(multisession)

# Function to read a single file
read_state_data <- function(file) {
  state_name <- tools::file_path_sans_ext(basename(file))
  data <- read_delim(file, delim = ";", col_names = TRUE, trim_ws = TRUE, show_col_types = FALSE)
  return(data)
}

# Read the data files in parallel
state_data_parallel <- future_lapply(state_files, read_state_data)

# Convert the list of dataframes to a named list
names(state_data_parallel) <- tools::file_path_sans_ext(basename(state_files))

# Combine the sequential and parallel results for verification
combined_state_data <- list(
  sequential = state_data,
  parallel = state_data_parallel
)

## Question 1h) Perform item (a) in parallel

# Define the path to the folder containing the state data
data_path <- "métodos_computacionais_para_ciência_de_dados_I/r/joaoalexandre_Lista6/dados/estados"

# List the files in the folder
state_files <- list.files(data_path, pattern = "*.txt", full.names = TRUE)

# Plan for parallel execution
plan(multisession)

# Function to read a single file
read_state_data <- function(file) {
  state_name <- tools::file_path_sans_ext(basename(file))
  data <- read_delim(file, delim = ";", col_names = TRUE, trim_ws = TRUE, show_col_types = FALSE)
  return(data)
}

# Read the data files in parallel
state_data_parallel <- future_lapply(state_files, read_state_data)

# Convert the list of dataframes to a named list
names(state_data_parallel) <- tools::file_path_sans_ext(basename(state_files))

str(state_data_parallel[[1]])

## Question 1i) Was it worth parallelizing the code?

# Sequential execution function
sequential_execution <- function() {
  state_data_sequential <- list()
  for (file in state_files) {
    state_name <- tools::file_path_sans_ext(basename(file))
    state_data_sequential[[state_name]] <- read_delim(file, delim = ";", col_names = TRUE, trim_ws = TRUE, show_col_types = FALSE)
  }
  return(state_data_sequential)
}

# Measure the execution time of sequential and parallel execution
timing <- microbenchmark(
  sequential = sequential_execution(),
  parallel = future_lapply(state_files, read_state_data),
  times = 10
)

print(timing)
# Analysis:
# Unit: milliseconds
#        expr      min       lq     mean   median       uq      max neval
#  sequential 433.4031 455.9937 472.0987 467.7661 481.3233 522.0807    10
#    parallel 726.2932 742.9350 787.9050 774.2910 822.9276 924.2868    10

# Conclusion:
# Based on the benchmark results, the parallel execution took significantly longer than the sequential execution. The mean execution time for the parallel version was approximately 788 milliseconds, while the sequential version took around 472 milliseconds on average. This suggests that, in this specific case, parallelizing the code was not worth it, as it increased the overall execution time.

# Several factors could contribute to this outcome:
# 1. Overhead of Parallelization: The setup and management of parallel tasks can introduce additional overhead, which may outweigh the benefits of parallel processing for smaller tasks or datasets.
# 2. I/O Bound Nature: If the task involves significant input/output operations (such as reading files from disk), the benefits of parallel execution might be reduced due to the I/O bottleneck.
# 3. System Resources: Limited CPU cores or competition with other processes for resources can also impact the performance of parallel execution.

# In summary, for this particular dataset and task, the overhead introduced by parallelization outweighed its benefits, making the sequential execution more efficient.


