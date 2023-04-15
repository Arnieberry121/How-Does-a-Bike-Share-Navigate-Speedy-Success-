#load packages
library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)
library(readr)
library(psych)
library(hrbrthemes)
library(ggplot2)


#importing data but we have data from January to december 
#therefore we need to join all the csv file into one file


january_2022 <- read_csv("202201-divvy-tripdata.csv")

february_2022 <- read_csv("202202-divvy-tripdata.csv")

march_2022 <- read_csv("202203-divvy-tripdata.csv")

april_2022 <- read_csv("202204-divvy-tripdata.csv")

may_2022 <- read_csv("202205-divvy-tripdata.csv")

june_2022 <- read_csv("202206-divvy-tripdata.csv")

july_2022 <- read_csv("202207-divvy-tripdata.csv")

august_2022 <- read_csv("202208-divvy-tripdata.csv")

september_2022 <- read_csv("202209-divvy-tripdata.csv")

october_2022 <- read_csv("202210-divvy-tripdata.csv")

november_2022 <- read_csv("202211-divvy-tripdata.csv")

december_2022 <- read_csv("202212-divvy-tripdata.csv")

# Total number of rows is [1] 5667717
sum(nrow(january_2022) + nrow(february_2022) 
    + nrow(march_2022) + nrow(april_2022) + nrow(may_2022) 
    + nrow(june_2022) + nrow(july_2022) + nrow(august_2022)
    + nrow(september_2022) + nrow(october_2022) + nrow(november_2022) + nrow(december_2022))


#Combine Data of 12 month into for smooth workflow
##after merging all the csv file we got a total of more than 5millions rows
#which is hard to workwith on spreadsheets
trip_final <- rbind(january_2022,february_2022,march_2022,april_2022,
                    may_2022,june_2022,july_2022,august_2022,september_2022,october_2022,november_2022,december_2022)

#The file is too large it can not be saved and read by the system 
#therefore we need to clean the data to proceed



#Setting global variable size to inf
options(future.globals.maxSize = Inf)


#Final data validation
str(trip_final)
View(head(trip_final))
View(tail(trip_final))
dim(trip_final)
summary(trip_final)
names(trip_final)


#Data Cleaning stage

#Count rows with "na" values
colSums(is.na(trip_final))

#Remove missing
clean_trip_final <- trip_final[complete.cases(trip_final),]
#Remove duplicates
clean_trip_final1 <- distinct(clean_trip_final)
#Remove data with greater start_at than end_at
clean_trip_final2<- clean_trip_final1 %>% 
  filter(started_at < ended_at)
#Remove na
clean_trip_final3 <- drop_na(clean_trip_final2)
clean_trip_final4 <- remove_empty(clean_trip_final3)
clean_trip_final5 <- remove_missing(clean_trip_final4)


#Check Cleaned data
colSums(is.na(clean_trip_final5))
View(filter(clean_trip_final5, clean_trip_final5$started_at > clean_trip_final5$ended_at))



#Renaming column for better context
clean_trip_final6 <- rename(clean_trip_final5, costumer_type = member_casual, bike_type = rideable_type)


#Separate date in date, day, month, year for better analysis
clean_trip_final6$date <- as.Date(clean_trip_final6$started_at)
clean_trip_final6$week_day <- format(as.Date(clean_trip_final6$date), "%A")
clean_trip_final6$month <- format(as.Date(clean_trip_final6$date), "%b_%y")
clean_trip_final6$year <- format(clean_trip_final6$date, "%Y")


#Separate column for time
clean_trip_final6$time <- as.POSIXct(clean_trip_final6$started_at, format = "%Y-%m-%d %H:%M:%S")
clean_trip_final6$time <- format(clean_trip_final6$time, format = "%H:%M")


#Add ride length column
clean_trip_final6$ride_length <- difftime(clean_trip_final6$ended_at, clean_trip_final6$started_at, units = "mins")

#Select the data we are going to use
clean_trip_final7 <- clean_trip_final6 %>% 
  select(bike_type, costumer_type, month, year, time, started_at, week_day, ride_length)

#Remove stolen bikes
clean_trip_final8 <- clean_trip_final7[!clean_trip_final7$ride_length>1400,] 
clean_trip_final9 <- clean_trip_final8[!clean_trip_final8$ride_length<5,]


tail(clean_trip_final9)

#Save the cleaned data
write.csv(clean_trip_final9,file = "clean_trip_final9.csv",row.names = FALSE)


str(clean_trip_final9)
names(clean_trip_final9)
head(clean_trip_final9)
tail(clean_trip_final9)





#Data Analysis with the newly cleaned file "clean_trip_final9.csv"

#Total no. of customers
View(table(clean_trip_final9$costumer_type))

#total number of casual and member customer from January to December 2022
total_customer <- (table(clean_trip_final9$costumer_type))
#data visualization of casual and member customer from January to December 2022
ggplot(data = data.frame(total_customer), aes(x = Var1, y = Freq)) + geom_bar(stat = "identity") + 
  labs(title="total number of casual&member riders from Jan-Dec", x="customer type", y="Orders Frequency")



#Total rides for each customer type in minutes
View(setNames(aggregate(ride_length ~ costumer_type, clean_trip_final9, sum), c("customer_type", "total_ride_len(mins)")))

#data visualization of total rides by each customer type

total_ride <- setNames(aggregate(ride_length ~ costumer_type, clean_trip_final9, sum), c("customer_type", "total_ride_min"))

ggplot(total_ride, aes(x=customer_type, y=total_ride_min)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  labs(title="Total Ride Length by Customer Type", x="Customer Type", y="Total Ride Length (mins)")



#Differences between members and casual riders in terms of length of ride
View(clean_trip_final9 %>% 
       group_by(costumer_type) %>% 
       summarise(min_length_mins = min(ride_length), max_length_min = max(ride_length),
                 median_length_mins = median(ride_length), mean_length_min = mean(ride_length)))
#data visualization average ride length between casual and member riders from Jan to Dec
mean_ride_length <- clean_trip_final9 %>% group_by(costumer_type) %>% summarise(mean_length_min = mean(ride_length))

ggplot(data = mean_ride_length, aes(x = costumer_type, y = mean_length_min)) + geom_bar(stat = "identity")


#Average ride_length for users by day_of_week and Number of total rides by day_of_week
View(clean_trip_final9 %>% 
       group_by(week_day) %>% 
       summarise(Avg_length = mean(ride_length),
                 number_of_ride = n()))

#ggplot(clean_trip_final9, aes(x = week_day, y = ride_length, color = costumer_type)) +
#  geom_line() +
#  labs(title = "Ride Length by Weekday and Customer Type",
#       x = "Weekday",
#      y = "Ride Length (min)",
#       color = "Customer Type")


#Average ride_length by month
View(clean_trip_final9 %>% 
       group_by(month) %>% 
       summarise(Avg_length = mean(ride_length),
                 number_of_ride = n()))

#Average ride length comparison by each week day according to each customer type
View(aggregate(clean_trip_final9$ride_length ~ clean_trip_final9$costumer_type + 
                 clean_trip_final9$week_day, FUN = mean))


#Average ride length comparison by each month according to each customer type
View(aggregate(clean_trip_final9$ride_length ~ clean_trip_final9$costumer_type + 
                 clean_trip_final9$month, FUN = mean))

#Analyze rider length data by customer type and weekday
View(clean_trip_final9 %>% 
       group_by(costumer_type, week_day) %>% 
       summarise(number_of_ride = n(),
                 avgerage_duration = mean(ride_length),
                 median_duration = median(ride_length),
                 max_duration = max(ride_length),
                 min_duration = min(ride_length)))


#Analyze rider length data by customer type and month
View(clean_trip_final9 %>% 
       group_by(costumer_type, month) %>% 
       summarise(nummber_of_ride = n(),
                 average_duration = mean(ride_length),
                 median_duration = median(ride_length),
                 max_duration = max(ride_length),
                 min_duration = min(ride_length)))


