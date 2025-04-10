# Load required libraries
library(ggplot2)
library(dplyr)
library(ggcorrplot)

data <- read.csv("~/Downloads/Chicago_rides_october24.csv")

# Convert time columns to datetime format
data$Trip.Start.Timestamp <- as.POSIXct(data$Trip.Start.Timestamp, format="%Y-%m-%d %H:%M:%S")
data$Trip.End.Timestamp <- as.POSIXct(data$Trip.End.Timestamp, format="%Y-%m-%d %H:%M:%S")

# Create a trip duration variable in minutes
data$Trip.Duration.Minutes <- data$Trip.Seconds / 60

# Histogram of fare amounts with log scale
p1 <- ggplot(data, aes(x = Fare)) +
  geom_histogram(binwidth = 0.2, fill = "blue", color = "black", alpha = 0.7, boundary = 1) +
  scale_x_log10(breaks = c(1, 5, 10, 20, 50, 100, 300)) + 
  labs(title = "Fare Amount Distribution", x = "Fare ($)", y = "Frequency") +
  theme_minimal()

# Histogram of tip amounts with log scale
p2 <- ggplot(data, aes(x = Tip)) +
  geom_histogram(binwidth = 0.2, fill = "green", color = "black", alpha = 0.7, boundary = 1) +
  scale_x_log10(breaks = c(0.5, 1, 2, 5, 10, 20, 50)) +
  labs(title = "Tip Amount Distribution", x = "Tip ($)", y = "Frequency") +
  theme_minimal()

# Histogram of trip duration
p3 <- ggplot(data, aes(x = Trip.Duration.Minutes)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black", alpha = 0.7) + 
  coord_cartesian(xlim = c(0, 60)) +
  labs(title = "Trip Duration Distribution", x = "Duration (minutes)", y = "Frequency") +
  theme_minimal()

# Histogram of trip distance
p8 <- ggplot(data, aes(x = Trip.Miles)) +
  geom_histogram(binwidth = 2.3, fill = "purple", color = "black", alpha = 0.7) +
  coord_cartesian(xlim = c(0, 30)) +
  labs(title = "Trip Distance Distribution", x = "Distance (miles)", y = "Frequency") +
  theme_minimal()

# Correlation matrix
num_vars <- data %>% select(Fare, Tip, Trip.Duration.Minutes, Trip.Miles) %>%
  rename(Distance = Trip.Miles, Duration = Trip.Duration.Minutes)
cor_matrix <- cor(num_vars, use = "complete.obs")
diag(cor_matrix) <- 1 

p4 <- ggcorrplot(cor_matrix, type = "full", lab = TRUE, lab_size = 5, 
                 colors = c("#1E90FF", "white", "#DC143C"),
                 outline.col = "white", title = "Correlation Matrix of Key Variables", ggtheme = theme_minimal())

# Time periods and weekdays
data <- data %>%
  mutate(Hour = as.numeric(format(Trip.Start.Timestamp, "%H")),
         Time.Period = factor(case_when(
           Hour >= 0 & Hour < 6  ~ "Night",
           Hour >= 6 & Hour < 12 ~ "Morning",
           Hour >= 12 & Hour < 18 ~ "Afternoon",
           Hour >= 18 & Hour < 24 ~ "Evening"
         ), levels = c("Morning", "Afternoon", "Evening", "Night")),  
         Weekday = factor(format(Trip.Start.Timestamp, "%A"),
                          levels = c("Sunday", "Saturday", "Friday", "Thursday", 
                                     "Wednesday", "Tuesday", "Monday")))

# Filter and aggregate data
data_clean <- data %>% filter(!is.na(Time.Period), !is.na(Weekday), !is.na(Tip))
agg_data <- data_clean %>% 
  group_by(Weekday, Time.Period) %>% 
  summarise(Avg_Tip = sum(Tip) / n(),  
            Trip_Count = n(), .groups = "drop")

# Heatmap of tips by time period and day
p5 <- ggplot(agg_data, aes(x = Time.Period, y = Weekday, fill = Avg_Tip)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Average Tip Amount by Time Period and Day",
       x = "Time of Day", y = "Day of the Week", fill = "Avg Tip") +
  theme_minimal()


# Community area tipping analysis
tip_by_distance <- data %>%
  mutate(Distance_Bin = cut(Trip.Miles, breaks = c(0, 1, 3, 5, 10, 20, Inf),
                            labels = c("0-1 mi", "1-3 mi", "3-5 mi", "5-10 mi", "10-20 mi", "20+ mi"),
                            include.lowest = TRUE)) %>%
  group_by(Distance_Bin) %>%
  summarise(Tip_Rate = mean(Tip.Given, na.rm = TRUE))

p6 <- ggplot(tip_by_distance, aes(x = Distance_Bin, y = Tip_Rate)) +
  geom_col(fill = "steelblue") +
  labs(title = "Probability of Tipping by Trip Distance", x = "Trip Distance (miles)", y = "Percentage of Trips with Tip") +
  theme_minimal()

# Tip percentage by fare
data <- data %>% mutate(Fare_Bin = cut(Fare, breaks = c(0, 5, 10, 20, 30, 50, 100, Inf),
                                       labels = c("0-5$", "5-10$", "10-20$", "20-30$", "30-50$", "50-100$", "100+$"),
                                       include.lowest = TRUE))
tip_by_fare <- data %>% filter(Fare > 0) %>% group_by(Fare_Bin) %>%
  summarise(Avg_Tip_Rate = mean(Tip / Fare, na.rm = TRUE))

p7 <- ggplot(tip_by_fare, aes(x = Fare_Bin, y = Avg_Tip_Rate)) +
  geom_col(fill = "darkred") +
  labs(title = "Tip Percentage by Fare Amount", x = "Fare Range ($)", y = "Average Tip as % of Fare") +
  theme_minimal()

# Print all plots
print(p1)
print(p2)
print(p3)
print(p8)
print(p4)
print(p5)
print(p6)
print(p7)



# Create a mapping between Community Area numbers and their names
community_names <- c(
  "1" = "Rogers Park", "2" = "West Ridge", "3" = "Uptown", "4" = "Lincoln Square", 
  "5" = "North Center", "6" = "Lake View", "7" = "Lincoln Park", "8" = "Near North Side",
  "9" = "Edison Park", "10" = "Norwood Park", "11" = "Jefferson Park", "12" = "Forest Glen",
  "13" = "North Park", "14" = "Albany Park", "15" = "Portage Park", "16" = "Irving Park",
  "17" = "Dunning", "18" = "Montclare", "19" = "Belmont Cragin", "20" = "Hermosa",
  "21" = "Avondale", "22" = "Logan Square", "23" = "Humboldt Park", "24" = "West Town",
  "25" = "Austin", "26" = "West Garfield Park", "27" = "East Garfield Park", "28" = "Near West Side",
  "29" = "North Lawndale", "30" = "South Lawndale", "31" = "Lower West Side", "32" = "Loop",
  "33" = "Near South Side", "34" = "Armour Square", "35" = "Douglas", "36" = "Oakland",
  "37" = "Fuller Park", "38" = "Grand Boulevard", "39" = "Kenwood", "40" = "Washington Park",
  "41" = "Hyde Park", "42" = "Woodlawn", "43" = "South Shore", "44" = "Chatham",
  "45" = "Avalon Park", "46" = "South Chicago", "47" = "Burnside", "48" = "Calumet Heights",
  "49" = "Roseland", "50" = "Pullman", "51" = "South Deering", "52" = "East Side",
  "53" = "West Pullman", "54" = "Riverdale", "55" = "Hegewisch", "56" = "Garfield Ridge",
  "57" = "Archer Heights", "58" = "Brighton Park", "59" = "McKinley Park", "60" = "Bridgeport",
  "61" = "New City", "62" = "West Elsdon", "63" = "Gage Park", "64" = "Clearing",
  "65" = "West Lawn", "66" = "Chicago Lawn", "67" = "West Englewood", "68" = "Englewood",
  "69" = "Greater Grand Crossing", "70" = "Ashburn", "71" = "Auburn Gresham", "72" = "Beverly",
  "73" = "Washington Heights", "74" = "Mount Greenwood", "75" = "Morgan Park", "76" = "O'Hare",
  "77" = "Edgewater"
)

# Replace numeric codes with names in the dataset if the columns exist
if ("Pickup.Community.Area" %in% colnames(data) & "Dropoff.Community.Area" %in% colnames(data)) {
  data <- data %>%
    mutate(
      Pickup.Community.Area = community_names[as.character(Pickup.Community.Area)],
      Dropoff.Community.Area = community_names[as.character(Dropoff.Community.Area)]
    )
}

# Calculate the percentage of trips with a tip for each Pickup Community Area
pickup_tips <- data %>%
  filter(!is.na(Pickup.Community.Area)) %>%
  group_by(Pickup.Community.Area) %>%
  summarise(Tip_Rate = mean(Tip.Given, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Tip_Rate)) %>%
  slice_head(n = 20)  # Select only the top 20 areas

# Calculate the percentage of trips with a tip for each Dropoff Community Area
dropoff_tips <- data %>%
  filter(!is.na(Dropoff.Community.Area)) %>%
  group_by(Dropoff.Community.Area) %>%
  summarise(Tip_Rate = mean(Tip.Given, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Tip_Rate)) %>%
  slice_head(n = 20)  # Select only the top 20 areas

# Plot for Pickup Community Area
p9 <- ggplot(pickup_tips, aes(x = reorder(Pickup.Community.Area, Tip_Rate), y = Tip_Rate)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Community Areas for Tipping (Pickup)", 
       x = "Pickup Community Area", 
       y = "Percentage of Trips with Tip") +
  theme_minimal()

# Plot for Dropoff Community Area
p10 <- ggplot(dropoff_tips, aes(x = reorder(Dropoff.Community.Area, Tip_Rate), y = Tip_Rate)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(title = "Top 20 Community Areas for Tipping (Dropoff)", 
       x = "Dropoff Community Area", 
       y = "Percentage of Trips with Tip") +
  theme_minimal()

# Print the new plots
print(p9)
print(p10)

# Calculate the percentage of trips with a tip between shared and non-shared rides
shared_tips_rate <- data %>%
  filter(!is.na(Trip.Pooled) & !is.na(Tip.Given)) %>%
  group_by(Trip.Pooled) %>%
  summarise(Tip_Rate = mean(Tip.Given, na.rm = TRUE) * 100, .groups = "drop")

# Rename values for clarity
shared_tips_rate$Trip.Pooled <- ifelse(shared_tips_rate$Trip.Pooled, "Shared", "Non-Shared")

# Print numerical results
print(shared_tips_rate)

# Create a barplot to compare tip rates between shared and non-shared rides
p11 <- ggplot(shared_tips_rate, aes(x = Trip.Pooled, y = Tip_Rate, fill = Trip.Pooled)) +
  geom_col() +
  labs(title = "Tip Rate by Ride Type", x = "Ride Type (Shared vs Non-Shared)", y = "Percentage of Trips with Tip (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("Shared" = "steelblue", "Non-Shared" = "darkred"))

# Print the plot
print(p11)


