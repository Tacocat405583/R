#Nicolas Hernandez
#Week 7 Assignment B

setwd("C:/Users/Nicolas/Desktop/Code/R/")

library(readxl)
library(Hmisc)


#(1) plotting the price versus the landsize for all homes in the dataset
#using the plot function

Housing <- read_excel("CleanMelbourneHousing.xlsx")

View(Housing)

Housing$Price <- as.numeric(Housing$Price)
Housing$Landsize <- as.numeric(Housing$Landsize)
Housing$Rooms <- as.numeric(Housing$Rooms)

# Same data frame as before but with regionname
CleanHousing <- data.frame(
  'Price' = Housing$Price,
  'LandSize' = Housing$Landsize,
  'Rooms' = Housing$Rooms,
  'Date' = Housing$Date,
  'Regionname' = Housing$Regionname
)

plot(CleanHousing$LandSize,CleanHousing$Price,main="Price vs Land Size",
     xlab="Land Size",ylab = "Price",col = "red")

#not a very clear relationship and is skewed right. Some outliers.


#(2)
#we can use subsets to find the data in the region between 200 and 600 similarly to strings in class

RangeHousing <- subset(CleanHousing,LandSize >= 200 & LandSize <=  600 )

#plot linear model

model <- lm(Price ~ LandSize,data = RangeHousing)

#plot regression line
plot(RangeHousing$LandSize,RangeHousing$Price,main="Price vs. Land Size (200-600)",
     xlab = "Land Size", ylab = "Price", col = "green",)
abline(model,col="red")

#find R^2 value
summary(model)


#(3)
#Find Sd and the mean of the 8 regions, which region sare the most expensive


#I did it the long way

# Create vectors for each region based on the updated region names that I could find
#I can only apologize
Northern_Metropolitan <- c()
Southern_Metropolitan <- c()
South_Eastern_Metropolitan <- c()
South_Western_Metropolitan <- c()
Western_Metropolitan <- c()
Eastern_Metropolitan <- c()
Northern_Victoria <- c()
Western_Victoria <- c()
Eastern_Victoria <- c()
Southern_Victoria <- c()

# Populate vectors with home prices based on region
for (i in 1:nrow(CleanHousing)) {
  region <- CleanHousing$Regionname[i]
  price <- CleanHousing$Price[i]
  
  if (region == "Northern Metropolitan") {
    Northern_Metropolitan <- c(Northern_Metropolitan, price)
  } else if (region == "Southern Metropolitan") {
    Southern_Metropolitan <- c(Southern_Metropolitan, price)
  } else if (region == "South-Eastern Metropolitan") {
    South_Eastern_Metropolitan <- c(South_Eastern_Metropolitan, price)
  } else if (region == "South-Western Metropolitan") {
    South_Western_Metropolitan <- c(South_Western_Metropolitan, price)
  } else if (region == "Western Metropolitan") {
    Western_Metropolitan <- c(Western_Metropolitan, price)
  } else if (region == "Eastern Metropolitan") {
    Eastern_Metropolitan <- c(Eastern_Metropolitan, price)
  } else if (region == "Northern Victoria") {
    Northern_Victoria <- c(Northern_Victoria, price)
  } else if (region == "Western Victoria") {
    Western_Victoria <- c(Western_Victoria, price)
  } else if (region == "Eastern Victoria") {
    Eastern_Victoria <- c(Eastern_Victoria, price)
  } else if (region == "Southern Victoria") {
    Southern_Victoria <- c(Southern_Victoria, price)
  }
}

# Calculate mean and SD for each region
RegionStats <- data.frame(
  Regionname = c("Northern Metropolitan", "Southern Metropolitan", "South-Eastern Metropolitan", 
                 "South-Western Metropolitan", "Western Metropolitan", "Eastern Metropolitan",
                 "Northern Victoria", "Western Victoria", "Eastern Victoria", "Southern Victoria"),
  Mean = c(mean(Northern_Metropolitan), mean(Southern_Metropolitan), mean(South_Eastern_Metropolitan),
           mean(South_Western_Metropolitan), mean(Western_Metropolitan), mean(Eastern_Metropolitan),
           mean(Northern_Victoria), mean(Western_Victoria), mean(Eastern_Victoria), mean(Southern_Victoria)),
  SD = c(sd(Northern_Metropolitan), sd(Southern_Metropolitan), sd(South_Eastern_Metropolitan),
         sd(South_Western_Metropolitan), sd(Western_Metropolitan), sd(Eastern_Metropolitan),
         sd(Northern_Victoria), sd(Western_Victoria), sd(Eastern_Victoria), sd(Southern_Victoria))
)

RegionStats

# Find the most expensive value
summary(RegionStats$Mean)
#I see that ther are 2 NA values that I havent cleaned

# Remove NAs before finding the maximum value
RegionStats_clean <- RegionStats[!is.na(RegionStats$Mean), ] #na due to error in cleaning so I have to use function
#otherwise I would treat them as strings and get rid of them

# Find the maximum value of the Mean column
max_value <- max(RegionStats_clean$Mean)

# Find the region corresponding to the maximum value
# by checking each row to see if it matches the max value
#it works aslong as names are in order
most_expensive_region <- RegionStats_clean$Regionname[RegionStats_clean$Mean == max_value]
most_expensive_region



#(4)
#Create new Collumn

CleanHousing$PricePerArea <- CleanHousing$Price/CleanHousing$LandSize

#(5) Create 90% confidence interval for each region

#due to missing data, again i have to use, is.na
sum(is.na(CleanHousing$PricePerArea))
cleaned_data <- CleanHousing$PricePerArea[!is.na(CleanHousing$PricePerArea)]
summary(cleaned_data)
var(cleaned_data)

#Found a division by error after testing 
CleanHousing$PricePerArea[CleanHousing$LandSize == 0 | is.na(CleanHousing$LandSize)] <- 0

# Now calculate the 90% confidence interval
cleaned_data <- CleanHousing$PricePerArea[CleanHousing$PricePerArea != 0]  # Exclude 0 values from analysis

# Perform the t-test to calculate the confidence interval
t.test(cleaned_data,
       mu=mean(CleanHousing$PricePerArea),
       conf.level = 0.90, 
       alternative = 'two.sided')

#finally works

#(6)

#Lots of error due to a vector not beign paired correctly which threw many errors

# Initialize empty vectors to store the region names and results
Region <- c()
Result <- c()

# Calculate the overall mean of PricePerArea
overall_mean <- mean(CleanHousing$PricePerArea)

# Loop through each region in RegionStats
for (i in 1:nrow(RegionStats)) {
  # Subset the data for the current region
  region_data <- CleanHousing$PricePerArea[CleanHousing$Regionname == RegionStats$Regionname[i]]
  
  # Check if there are enough observations for the t-test (at least 2)
  if (length(region_data) >= 2) {
    # Perform the one-sample t-test to compare region mean to the overall mean
    t_result <- t.test(region_data, mu = overall_mean, conf.level = 0.90)
    
    # Append region name and p-value to the results
    Region <- append(Region, RegionStats$Regionname[i])
    Result <- append(Result, t_result$p.value)
    #WILL THROW ERROR IF NOT PUT, USED STACK OVERFLOW
  } else {
    # If there are not enough data points, append NA for that region
    # Ive tried not including the two vectors with no data but to no avail
    Region <- append(Region, RegionStats$Regionname[i])
    Result <- append(Result, NA)
  }
}


t_test_df <- data.frame(Region, Result)
# Print the results of our test
print(t_test_df)


#(7)

#Tax brakcet from last week

compute_tax1 <- function(price) {
  if (price <= 1000000) {
    return(price * 0.015)  # 1.5% tax for prices <= $1,000,000
  } else if (price > 1000000 && price < 3000000) {
    return(price * 0.0115)  # 1.15% tax for prices between $1,000,000 and $3,000,000
  } else {
    return(price * 0.01)  # 1% tax for prices >= $3,000,000
  }
}

#sapplyformta
#df$z <- sapply(df$y,f)
#View(df)

CleanHousing$PropertyTax1 <- sapply(CleanHousing$PricePerArea,compute_tax1)
View(CleanHousing)


#(8)

#Same thing but with Price per area

# Tax based on Price Per Area
compute_tax2 <- function(PPA) {
  if (PPA <= 3500) {
    return(PPA * 0.01)  # 1% tax for PPA <= 3500
  } else if (PPA > 3500 && PPA <= 15000) {
    return(PPA * 0.0115)  # 1.15% tax for 3500 < PPA <= 15000
  } else {
    return(PPA * 0.015)  # 1.5% tax for PPA > 15000
  }
}

CleanHousing$PropertyTax2 <- sapply(CleanHousing$PricePerArea, compute_tax2)
View(CleanHousing)




#(9) Difference and total revenue change

CleanHousing$Difference <- CleanHousing$PropertyTax2 - CleanHousing$PropertyTax1
TotalRevenueChange <- sum(CleanHousing$Difference)
TotalRevenueChange





