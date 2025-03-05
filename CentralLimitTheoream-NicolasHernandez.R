# Nicolas Hernandez Week 5 Assignment B

setwd('C:/Users/Nicol/Desktop/R/Week-6/')

# I will be using the melbourne housing dataset


library(readxl)
library(Hmisc)
Housing <- read_excel('MelbourneHousing.xlsx')
View(Housing)


# Clean and Set as numeric if needed
Housing$Price <- as.numeric(Housing$Price)
Housing$Landsize <- as.numeric(Housing$Landsize)
Housing$Rooms <- as.numeric(Housing$Rooms)

CleanHousing <- data.frame(
  'Price' = Housing$Price,
  'Land.Size' = Housing$Landsize,
  'Rooms' = Housing$Rooms,
  'Date' = Housing$Date
)
CleanHousing

##We will be taking the stai=tistics of the dataset 
#using the prices


# Function to display basic statistics and a histogram for a given numeric data set
DisplayBasicStatistics <- function(data_set) {
  # Calculate basic statistics
  mean_val <- mean(data_set)
  median_val <- median(data_set)
  sd_val <- sd(data_set)
  
  # Display the statistics
  print(sprintf("Basic Statistics:"))
  print(sprintf("Mean: %.2f", mean_val))
  print(sprintf("Median: %.2f", median_val))
  print(sprintf("Standard Deviation: %.2f", sd_val))
  
  print(" ")
  
  # Display a histogram of the data set
  hist(data_set, main = "Histogram of Data Set", xlab = "Values", col = "lightblue")
}

# Function to test the Central Limit Theorem using simulation
TestCentralLimitTheorem <- function(n, mu, sigma, N) {
  # Initialize empty vectors to hold sample means and sample standard deviations
  SampleMean <- c()
  SampleStandardDeviation <- c()
  
  # Loop n times to perform the sampling experiment
  for (i in 1:n) {
    # Draw a sample of size N from a normal distribution with mean mu and standard deviation sigma
    sample_data <- rnorm(N, mean = mu, sd = sigma)
    # Compute the sample mean and sample standard deviation
    sample_mean <- mean(sample_data)
    sample_sd <- sd(sample_data)
    # Append the results to the vectors as shown in the example
    SampleMean <- append(SampleMean, sample_mean)
    SampleStandardDeviation <- append(SampleStandardDeviation, sample_sd)
  }
  
  # Compute the mean of the sample means and the standard deviation of the sample standard deviations
  mean_of_sample_means <- mean(SampleMean)
  sd_of_sample_sds <- sd(SampleStandardDeviation)
  
  # Print out the results using sprintf and print
  print(sprintf("Results of Central Limit Theorem Simulation:"))
  print(sprintf("Mean of Sample Means: %.2f (expected ~ %.2f)", mean_of_sample_means, mu))
  print(sprintf("Standard Deviation of Sample Standard Deviations: %.2f (sample size = %d)", 
                sd_of_sample_sds, N))
  
  # Display a histogram of the sample means for visualization
  hist(SampleMean, main = "Histogram of Sample Means", xlab = "Sample Means", col = "lightgreen")
}

# Example tests:

DisplayBasicStatistics(CleanHousing$Price)

# pick a large number of runs and using mean,sd from previous function
#Picking an N of 30 as teh CLT holds well for that value
TestCentralLimitTheorem(n = 1000, mu = 1075684.08, sigma = 639310.72, N = 30)







