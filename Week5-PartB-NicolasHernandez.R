# Nicolas Hernandez Week 5 Assignment B

setwd('C:/Users/Nicol/Desktop/R/Week-5/')

# use excel library
library(readxl)
library(Hmisc)
Housing <- read_excel('MelbourneHousing.xlsx')
View(Housing)

# Commands to use
# which, grepl, sink, for-loop, if-then, functions...

# First we should clean our data

# (0)
# Set as numeric
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

# 1 - How many homes sold in 2016 with missing recorded land size
check <- length(which(CleanHousing$Land.Size == 0))
check

missinglandsize2016 <- length(which(grepl('2016', as.character(CleanHousing$Date)) & CleanHousing$Land.Size == 0))
missinglandsize2016
#777

# 2 - How many homes sold in 2017 with missing recorded land size
missinglandsize2017 <- length(which(grepl('2017', as.character(CleanHousing$Date)) & CleanHousing$Land.Size == 0))
missinglandsize2017
#400

#3 Weighted mean and Sd
weights <- CleanHousing$Land.Size[grepl('2017', CleanHousing$Date)]
prices2017 <- CleanHousing$Price[grepl('2017', CleanHousing$Date)]

# Weighted Mean and SD
weighted_mean_price2017 <- sum(prices2017 * weights) / sum(weights)
weighted_sd2017 <- sqrt(sum(weights * (prices2017 - weighted_mean_price2017)^2) / sum(weights)) # sqrt for sd, used stackoverflow

#Weighted mean and weighted SD
print(sprintf('Weighted Mean Price 2017: %.2f', weighted_mean_price2017))
print(sprintf('Weighted SD 2017: %.2f', weighted_sd2017))

#unweighted mean and SD
regular_mean_price2017 <- mean(prices2017)
regular_sd2017 <- sd(prices2017)

#regular mean and regular SD
print(sprintf('Regular Mean Price 2017: %.2f', regular_mean_price2017))
print(sprintf('Regular SD 2017: %.2f', regular_sd2017))


#4- Mean and mean of home sold in 2017 by room count in table 

roomsize <- c(2,3,4)
options(max.print=100)

#Sink so output file looks nice
sink(file = 'Table.txt',
     type = 'output')

print('Rooms  |  Mean Price  |  Median Price')
print('-------------------------------------')
for (r in c(2, 3, 4)) {
  roomPrices <- CleanHousing$Price[grepl('2017', CleanHousing$Date) & CleanHousing$Rooms == r]
  roomPrices
  meanVal <- mean(roomPrices)
  medianVal <- median(roomPrices)
  print(sprintf('%d     |  %.2f       |  %.2f', r, meanVal, medianVal))
}
print('Finished')
sink()
print('check')

#5 Comparing the Standard Deviation of prices for 2,3,4 room homes



# All homes 2017 data
all_prices_2017 <- CleanHousing$Price[grepl('2017', CleanHousing$Date)]
all_weights_2017 <- CleanHousing$Land.Size[grepl('2017', CleanHousing$Date)]

# Regular Standard Deviation (without weights) for all homes in 2017
regular_sd_2017 <- sd(all_prices_2017)

# Weighted Standard Deviation for all homes in 2017
weighted_variance_2017 <- wtd.var(all_prices_2017, weights = all_weights_2017)
weighted_sd_2017 <- sqrt(weighted_variance_2017)

print(sprintf('All Homes 2017 | Regular SD: %.2f', regular_sd_2017))
print(sprintf('All Homes 2017 | Weighted SD: %.2f', weighted_sd_2017))

# SD for 2-room, 3-room, and 4-room homes in 2017

for (r in c(2, 3, 4)) {
  room_prices <- CleanHousing$Price[grepl('2017', CleanHousing$Date) & CleanHousing$Rooms == r]
  room_weights <- CleanHousing$Land.Size[grepl('2017', CleanHousing$Date) & CleanHousing$Rooms == r]
  
  # Regular Standard Deviation  for each room size
  room_regular_sd <- sd(room_prices)
  
  # Weighted Standard Deviation using wtd.var()
  room_weighted_variance <- wtd.var(room_prices, weights = room_weights)
  room_weighted_sd <- sqrt(room_weighted_variance)
  
  # Print the SD for each room size to format nicely
  print(sprintf('Rooms: %d | Regular SD: %.2f | Weighted SD: %.2f', r, room_regular_sd, room_weighted_sd))
}


#Part 6 and 7

# Add the address column to CleanHousing
CleanHousing$Address <- Housing$Address

missing_land_size_2017 <- CleanHousing[grepl('2017', CleanHousing$Date) & CleanHousing$Land.Size == 0,]

sink(file = 'MissingLandSize.txt', type = 'output')

print("Addresses with Missing Land Size (Land Size = 0):")
print(missing_land_size_2017$Address)


sink()
print('check')


# PART 7

#Data frame with Address and Price and a Tax column
tax_data <- CleanHousing[grepl('2017', CleanHousing$Date), c('Address', 'Price')]

# Create a new column for tax
tax_data$Tax <- NA  # Initializing the Tax column with NA

# Making sure this works
head(tax_data, 5)

# Step 2: Create a tax function based on selling price
compute_tax <- function(price) {
  if (price <= 1000000) {
    return(price * 0.015)  # 1.5% tax for prices <= $1,000,000
  } else if (price > 1000000 && price < 3000000) {
    return(price * 0.0115)  # 1.15% tax for prices between $1,000,000 and $3,000,000
  } else {
    return(price * 0.01)  # 1% tax for prices >= $3,000,000
  }
}

# Test the function on an individual row
print(compute_tax(tax_data$Price[1]))  # First row test ...  WORKS

# Step 3: Apply the function to the tax column using a for-loop

numrows <- length(CleanHousing$Address)


for (i in 1: numrows) {
  tax_data$Tax[i] <- compute_tax(tax_data$Price[i])
}

# Step 4: Output all addresses and their property tax to a text file
sink(file = 'PropertyTax.txt', type = 'output')

# Print the header
print("Address                  Tax Amount\n")
print("-----------------------------------------\n")

# Loop through each row and print address and tax
for (i in 1:nrow(tax_data)) {
  # Format the output as specified
  cat(sprintf("%-25s $%.2f\n", tax_data$Address[i], tax_data$Tax[i]))
}

# End the sink
sink()
print('check')
View('PropertyTax.txt')




