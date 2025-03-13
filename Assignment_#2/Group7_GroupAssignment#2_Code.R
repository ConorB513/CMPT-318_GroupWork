install.packages("zoo")
install.packages("lubridate")
install.packages("dplyr")
install.packages("naniar")
install.packages("hms")

library(zoo)
library(lubridate)
library(dplyr)
library(naniar)
library(ggplot2)
library(hms)

# import dataset
dataset <- read.csv("./Group_Assignment_Dataset.txt", header = TRUE)

# use z-score to identify anomalies
allGap <- dataset[,c("Global_active_power")];
gapZScores <- (allGap - mean(allGap)) / sd(allGap);
dataset$Global_active_power[abs(gapZScores) > 3] <- NA

allGrp <- dataset[,c("Global_reactive_power")];
grpZScores <- (allGrp - mean(allGrp)) / sd(allGrp);
grpZScores_df <- data.frame(Global_reactive_power_Z = grpZScores)
dataset$Global_reactive_power[abs(grpZScores) > 3] <- NA

allVolt <- dataset[,c("Voltage")];
voltZScores <- (allVolt - mean(allVolt)) / sd(allVolt);
dataset$Voltage[abs(voltZScores) > 3] <- NA

allGI <- dataset[,c("Global_intensity")];
giZScores <- (allGI - mean(allGI)) / sd(allGI);
dataset$Global_intensity[abs(giZScores) > 3] <- NA

allSM1 <- dataset[,c("Sub_metering_1")];
sm1ZScores <- (allSM1 - mean(allSM1)) / sd(allSM1);
dataset$Sub_metering_1[abs(sm1ZScores) > 3] <- NA

allSM2 <- dataset[,c("Sub_metering_2")];
sm2ZScores <- (allSM2 - mean(allSM2)) / sd(allSM2);
dataset$Sub_metering_2[abs(sm2ZScores) > 3] <- NA

allSM3 <- dataset[,c("Sub_metering_3")];
sm3ZScores <- (allSM3 - mean(allSM3)) / sd(allSM3);
dataset$Sub_metering_3[abs(sm3ZScores) > 3] <- NA
colSums(is.na(dataset))

# apply linear interpolation to values that are NA
dataset[,c("Global_reactive_power")] <- na.approx(dataset[,c("Global_reactive_power")]);
dataset[,c("Global_active_power")] <- na.approx(dataset[,c("Global_active_power")]);
dataset[,c("Voltage")] <- na.approx(dataset[,c("Voltage")]);
dataset[,c("Global_intensity")] <- na.approx(dataset[,c("Global_intensity")]);
dataset[,c("Sub_metering_1")] <- na.approx(dataset[,c("Sub_metering_1")]);
dataset[,c("Sub_metering_2")] <- na.approx(dataset[,c("Sub_metering_2")]);
dataset[,c("Sub_metering_3")] <- na.approx(dataset[,c("Sub_metering_3")]);

colSums(is.na(dataset))

# slice the dataset into as many complete weeks (Monday-Sunday)
dataset$Date <- as.Date(dataset$Date, format="%d/%m/%Y")

dataset$Week <- week(dataset$Date)

week_counter <- dataset %>%
  group_by(Week) %>%
  summarise(days_in_week = n_distinct(Date), .groups = "drop")

# filter only complete weeks
complete_weeks <- week_counter %>%
  filter(days_in_week == 7) %>%
  select(Week)

# merge with original data to retain only complete weeks
dataset_complete_weeks <- dataset %>%
  inner_join(complete_weeks, by = "Week")

# calculate moving average for each complete week
# fill extends by using closest available rolling mean value to fill in missing data
dataset_complete_weeks <- dataset_complete_weeks %>%
  group_by(Week) %>%
  mutate(Global_intensity_smoothened = rollmean(Global_intensity, k = 7, fill = "extend", align = "center")) %>%
  ungroup()

# compute the average smoothened week
# group data across different days of the week by Time column
average_smoothened_week <- dataset_complete_weeks %>%
  group_by(Time) %>%
  summarise(GI_average_smoothened = mean(Global_intensity_smoothened, na.rm = TRUE)) %>%
  ungroup()

# merge the dataset with the average smoothened week
dataset_with_avg <- dataset_complete_weeks %>%
  left_join(average_smoothened_week, by = "Time")

# compute deviation for each observation
dataset_with_avg <- dataset_with_avg %>%
  mutate(Deviation = Global_intensity_smoothened - GI_average_smoothened)  

# compute the standard deviation of deviations for each week - store all anomaly scores in table
week_anomaly_scores <- dataset_with_avg %>%
  group_by(Week) %>%
  summarise(Anomaly_Score = sd(Deviation, na.rm = TRUE)) %>%
  ungroup()

# identify the most and least anomalous weeks
most_anomalous_week <- week_anomaly_scores %>% arrange(desc(Anomaly_Score)) %>% slice(1)
least_anomalous_week <- week_anomaly_scores %>% arrange(Anomaly_Score) %>% slice(1)

most_anomalous_week_id <- most_anomalous_week$Week
least_anomalous_week_id <- least_anomalous_week$Week

# extract data for most and least anomalous weeks
most_anomalous_data <- dataset_with_avg %>% filter(Week == most_anomalous_week_id)
least_anomalous_data <- dataset_with_avg %>% filter(Week == least_anomalous_week_id)

# properly format the Time column in all data frames
average_smoothened_week$Time <- as_hms(average_smoothened_week$Time)
most_anomalous_data$Time <- as_hms(most_anomalous_data$Time)
least_anomalous_data$Time <- as_hms(least_anomalous_data$Time)

# get the average global intensity values for each time point in most and least anomalous week
least_anomalous_avg <- least_anomalous_data %>%
  group_by(Time) %>%
  summarise(Global_intensity_smoothened = mean(Global_intensity_smoothened, na.rm = TRUE)) %>%
  ungroup()

most_anomalous_avg <- most_anomalous_data %>%
  group_by(Time) %>%
  summarise(Global_intensity_smoothened = mean(Global_intensity_smoothened, na.rm = TRUE)) %>%
  ungroup()

# plot the average smoothened versions of the most and the least anomalous weeks against the average smoothened week
ggplot() +
  geom_line(data = least_anomalous_avg, aes(x = Time, y = Global_intensity_smoothened, color = "Least Anomalous Week", group = 1), linewidth = 1) +
  geom_line(data = most_anomalous_avg, aes(x = Time, y = Global_intensity_smoothened, color = "Most Anomalous Week", group = 1), linewidth = 1) +
  geom_line(data = average_smoothened_week, aes(x = Time, y = GI_average_smoothened, color = "Average Smoothened Week", group = 1), linewidth = 1) +
  labs(title = "Anomalous Weeks vs. Average Smoothened Global Intensity",
       x = "Time of Day",
       y = "Global Intensity (Smoothened)",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Least Anomalous Week" = "blue", "Most Anomalous Week" = "red", "Average Smoothened Week" = "green"))
