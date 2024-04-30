if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
library(dplyr)
library(tidyr)
data(Titanic)
data(sleep)


# Question 1) Data Manipulation with the Titanic Dataset
## Question 1a) Count of Male and Female Passengers
sex_counts <- margin.table(Titanic, margin = 2)
male <- sex_counts["Male"]
female <- sex_counts["Female"]


## Question 1b) Table Of survived child and adult passengers separeted
Titanic2 <- as.data.frame(Titanic)
result <- Titanic2 %>%
  group_by(Age, Survived) %>%
  summarise(Count = sum(Freq))
print(result)


## Question 1c) Summaries of each column in Titanic2
for(column in names(Titanic2)) {
  print(summary(Titanic2[[column]]))
}


# Question 2) Data Manipulation with the Titanic Dataset (Part 2)
## Question 2a) Unstacking the data frequency by the variable Class
unstacked_table <- Titanic2 %>%
  spread(key = Class, value = Freq)
print(unstacked_table)


## Question 2b) Creating a dataset of only male passengers
male_passengers <- Titanic2 %>%
  filter(Sex == "Male")
print(male_passengers)


## Question 2c) Creating a dataset of male crew members and removing the 'Survived' column
crew_male_passengers <- Titanic2 %>%
  filter(Sex == "Male", Class == "Crew") %>%
  select(-Survived)
print(crew_male_passengers)


## Question 2d) Sorting the data by Sex and Age
sorted_titanic <- Titanic2 %>%
  arrange(Sex, Age)
print(sorted_titanic)


## Question 2e) Adding a column for the proportion of frequencies called 'prop'
Titanic2 <- Titanic2 %>%
  group_by(Class, Sex, Age, Survived) %>%
  mutate(TotalFreq = sum(Freq),
         prop = ifelse(TotalFreq == 0, 0, Freq / TotalFreq)) %>%
  ungroup() %>%
  select(-TotalFreq)
print(Titanic2)


## Question 2f) Displaying the count of deceased men and women by class
deceased_counts <- Titanic2 %>%
  filter(Survived == "No") %>%
  group_by(Sex, Class) %>%
  summarise(DeceasedCount = sum(Freq), .groups = 'drop')
print(deceased_counts)


# Question 3) Data Manipulation with the sleep Dataset
## Question 3a) Performing a parametric test to check for differences in extra sleep time between the two medications
t_test_results <- sleep %>%
  t.test(extra ~ group, data = ., paired = TRUE)
print(t_test_results)


## Question 3b) Performing a non-parametric test to check for differences in extra sleep time between the two medications
wilcox_test_results <- sleep %>%
  wilcox.test(extra ~ group, data = ., paired = TRUE)
print(wilcox_test_results)


run_tests <- TRUE
if (run_tests) {
  failed_tests <- c()
  Titanic_data <- as.data.frame(Titanic)


  # Test 1a: Verify count of Male and Female Passengers
  expected_male_count <- sum(Titanic_data$Freq[Titanic_data$Sex == "Male"])
  expected_female_count <- sum(Titanic_data$Freq[Titanic_data$Sex == "Female"])  
  if (male != expected_male_count || female != expected_female_count) {
    failed_tests <- c(failed_tests, "1a - Incorrect counts for Male or Female passengers.")
  }


  # Test 1b: Check survival table for children and adults
  if (nrow(result) == 0 || is.null(result$Count)) {
    failed_tests <- c(failed_tests, "1b - Survival table for children and adults may be incorrect or empty.")
  }


  # Test 1c: Check summaries generated for each column
  summaries_complete <- TRUE
  for (column in names(Titanic_data)) {
    summary_data <- summary(Titanic_data[[column]])
    if (is.null(summary_data)) {
      summaries_complete <- FALSE
      break
    }
  }
  if (!summaries_complete) {
    failed_tests <- c(failed_tests, "1c - One or more summaries are incomplete.")
  }


  # Test 2a: Verify unstacking operation
  if (nrow(unstacked_table) == 0 || is.null(unstacked_table)) {
    failed_tests <- c(failed_tests, "2a - Unstacking of data by class failed or is empty.")
  }


  # Test 2b-2d: Check filtering and sorting operations
  if (nrow(male_passengers) == 0 || nrow(sorted_titanic) == 0) {
    failed_tests <- c(failed_tests, "2b-2d - Filtering or sorting operations may be incorrect.")
  }


  # Test 2e: Check proportions in Titanic2
  if (any(is.na(Titanic2$prop)) || any(Titanic2$prop < 0)) {
    failed_tests <- c(failed_tests, "2e - Proportion column contains NA or negative values.")
  }


  # Test 2f: Verify counts of deceased by class and sex
  if (nrow(deceased_counts) == 0) {
    failed_tests <- c(failed_tests, "2f - Count of deceased men and women by class may be incorrect.")
  }


  # Test 3a & 3b: Statistical test results
  if (is.null(t_test_results) || is.null(wilcox_test_results)) {
    failed_tests <- c(failed_tests, "3 - Statistical tests did not execute or returned null results.")
  }


  if (length(failed_tests) > 0) {
    cat("Tests failed for the following sections:\n")
    for (test in failed_tests) {
      cat(test, "\n")
    }
  } else {
    cat("All tests passed successfully.\n")
  }
}