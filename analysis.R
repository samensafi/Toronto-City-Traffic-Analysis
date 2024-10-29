# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(sf)
library(tidyr)

locations <- read_csv("/Users/samensafi/Desktop/University/R Project/TorontoTrafficAnalysis/locations.csv")
traffic_data <- read_csv("/Users/samensafi/Desktop/University/R Project/TorontoTrafficAnalysis/raw-data-2020-2029.csv")

glimpse(locations)
glimpse(traffic_data)
summary(traffic_data)

# reducing to necessary columns
traffic_data <- traffic_data %>%
  select(count_date, location_id, lng, lat, 
         sb_cars_r, sb_cars_t, sb_cars_l,
         nb_cars_r, nb_cars_t, nb_cars_l, 
         wb_cars_r, wb_cars_t, wb_cars_l, 
         eb_cars_r, eb_cars_t, eb_cars_l, 
         sb_truck_r, sb_truck_t, sb_truck_l,
         nb_truck_r, nb_truck_t, nb_truck_l,
         wb_truck_r, wb_truck_t, wb_truck_l, 
         eb_truck_r, eb_truck_t, eb_truck_l, 
         nx_peds, sx_peds, ex_peds, wx_peds)

head(traffic_data)

# checking for missing values
missing_values <- sapply(traffic_data, function(x) sum(is.na(x)))
print("Missing values per column:")
print(missing_values)

# selecting necessary columns only
locations_reduced <- locations %>%
  select(`Location ID`, `Location Name`, Intersection, District)

# merging the two data
traffic_data <- traffic_data %>%
  left_join(locations_reduced, by = c("location_id" = "Location ID"))


head(traffic_data)

colnames(traffic_data)


# aggregating total traffic volume by mode and direction

car_volume <- traffic_data %>%
  summarise(
    sb_cars = sum(sb_cars_r + sb_cars_t + sb_cars_l, na.rm = TRUE),
    nb_cars = sum(nb_cars_r + nb_cars_t + nb_cars_l, na.rm = TRUE),
    wb_cars = sum(wb_cars_r + wb_cars_t + wb_cars_l, na.rm = TRUE),
    eb_cars = sum(eb_cars_r + eb_cars_t + eb_cars_l, na.rm = TRUE)
  )

truck_volume <- traffic_data %>%
  summarise(
    sb_trucks = sum(sb_truck_r + sb_truck_t + sb_truck_l, na.rm = TRUE),
    nb_trucks = sum(nb_truck_r + nb_truck_t + nb_truck_l, na.rm = TRUE),
    wb_trucks = sum(wb_truck_r + wb_truck_t + wb_truck_l, na.rm = TRUE),
    eb_trucks = sum(eb_truck_r + eb_truck_t + eb_truck_l, na.rm = TRUE)
  )

pedestrian_volume <- traffic_data %>%
  summarise(
    nx_peds = sum(nx_peds, na.rm = TRUE),
    sx_peds = sum(sx_peds, na.rm = TRUE),
    ex_peds = sum(ex_peds, na.rm = TRUE),
    wx_peds = sum(wx_peds, na.rm = TRUE)
  )

print("total car volume by direction:")
print(car_volume)

print("total truck volume by direction:")
print(truck_volume)

# visualizing

car_volume_long <- car_volume %>%
  pivot_longer(cols = everything(), names_to = "Direction", values_to = "Volume")

ggplot(car_volume_long, aes(x = Direction, y = Volume)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "total car volume by direction", x = "Direction", y = "Volume") +
  theme_minimal()


truck_volume_long <- truck_volume %>%
  pivot_longer(cols = everything(), names_to = "Direction", values_to = "Volume")

ggplot(truck_volume_long, aes(x = Direction, y = Volume)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "total truck volume by direction", x = "Direction", y = "Volume") +
  theme_minimal()


pedestrian_volume_long <- pedestrian_volume %>%
  pivot_longer(cols = everything(), names_to = "Direction", values_to = "Volume")

ggplot(pedestrian_volume_long, aes(x = Direction, y = Volume)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "total pedestrian volume by direction", x = "Direction", y = "Volume") +
  theme_minimal()


print("total pedestrian volume by direction:")
print(pedestrian_volume)


car_volume_long <- car_volume %>%
  pivot_longer(cols = everything(), names_to = "Direction", values_to = "Volume")

ggplot(car_volume_long, aes(x = Direction, y = Volume)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "total car volume by direction", x = "Direction", y = "Volume") +
  theme_minimal()


truck_volume_long <- truck_volume %>%
  pivot_longer(cols = everything(), names_to = "Direction", values_to = "Volume")

ggplot(truck_volume_long, aes(x = Direction, y = Volume)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "total truck volume by direction", x = "Direction", y = "Volume") +
  theme_minimal()


pedestrian_volume_long <- pedestrian_volume %>%
  pivot_longer(cols = everything(), names_to = "Direction", values_to = "Volume")

ggplot(pedestrian_volume_long, aes(x = Direction, y = Volume)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "total pedestrian volume by direction", x = "Direction", y = "Volume") +
  theme_minimal()