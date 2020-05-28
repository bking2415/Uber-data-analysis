"Import Librarys"
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
# Intergrate Tablaeau 
library(Rserve)
# Start R connection
Rserve(args = "--no-save")

"Creating vector of colors to be implemented in our plots"
colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")
          
"Reading Data into variables"
data_vector = c()
# Loop through files in directory
files <- list.files("uber")
print(files)
# loop through files 
for (file in files) {
  # Split on hyphem
  split <- strsplit(file, "-")
  print(split)
  # Last index of split
  last_value <- length(split[[1]])
  
  data <- split[[1]][last_value]
  print(data)
  # Split on period to get rid of .csv 
  split_data <- strsplit(data, "[.]")
  print(split_data)
  # Concatenate string for file path of data
  file_path = paste("uber/", file, sep = "")
  print(file_path)
  # Return first value in split
  var_name <- split_data[[1]][1] 
  print(var_name)
  data_vecter <- append(data_vector, var_name)
  print(data_vecter)
  # Assign name .csv file data
  assign(var_name, read.csv(file_path))
}

data_2014 <- rbind(apr14, may14, jun14, jul14, aug14, sep14)
# Format Date.Time column of data
data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")
data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)
# Create a Day column to Data Frame
data_2014$day <- factor(day(data_2014$Date.Time))
# Create a Month column to Data Frame
data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE))
# Create a Year column to Data Frame
data_2014$year <- factor(year(data_2014$Date.Time))
# Create a Day of Week column to Data Frame (Mon, Tues, Wednes., etc)
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label = TRUE))
# Format Time column of data
data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))

# Write new .csv file to R folder
write.csv(data_2014,"data_2014.csv", row.names = FALSE)

# Plotting the trips by hours in a day
hour_data <- data_2014 %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n()) 
datatable(hour_data)

ggplot(hour_data, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

month_hour <- data_2014 %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())

ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma)

"Plotting data by trips during every day of the month"
day_group <- data_2014 %>%
  group_by(day) %>%
  dplyr::summarize(Total = n()) 
datatable(day_group)

ggplot(day_group, aes(day, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue") +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

"Number of Trips taking place during months in a year"
month_weekday <- data_2014 %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())

ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity", position = "dodge") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

"Finding out the number of Trips by bases"
# Trips by Bases
ggplot(data_2014, aes(Base)) + 
  geom_bar(fill = "darkred") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")

# Trips by Bases and Month
ggplot(data_2014, aes(Base, fill = month)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month") +
  scale_fill_manual(values = colors)

# Trips by Bases and Days of the Week
ggplot(data_2014, aes(Base, fill = dayofweek)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and DayofWeek") +
  scale_fill_manual(values = colors)

"Creating a Heatmap visualization of day, hour and month"
day_and_hour <- data_2014 %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())
datatable(day_and_hour)

# First, we will plot Heatmap by Hour and Day.
ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Hour and Day")

# Second, we will plot Heatmap by Month and Day.
ggplot(day_month_group, aes(day, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day")

# Third, a Heatmap by Month and Day of the Week.
ggplot(month_weekday, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week")

#Fourth, a Heatmap that delineates Month and Bases.

month_base <-  data_2014 %>%
  group_by(Base, month) %>%
  dplyr::summarize(Total = n()) 

day0fweek_bases <-  data_2014 %>%
  group_by(Base, dayofweek) %>%
  dplyr::summarize(Total = n()) 

ggplot(month_base, aes(Base, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Bases")

# Finally, we will plot the heatmap, by bases and day of the week.
ggplot(day0fweek_bases, aes(Base, dayofweek, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Bases and Day of Week")

"Creating a map visualization of rides in New York"
# Long and Lat of New York
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

ggplot(data_2014, aes(x=Lon, y=Lat)) +
  geom_point(size=1, color = "blue") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")

ggplot(data_2014, aes(x=Lon, y=Lat, color = Base)) +
  geom_point(size=1) +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE")


