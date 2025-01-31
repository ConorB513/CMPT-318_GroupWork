#adding packages
install.packages("zoo")
library(zoo)

install.packages("dplyr")
library(dplyr)

install.packages("ggplot2")
library(ggplot2)

install.packages("ggcorrplot")
library(ggcorrplot)

#importing dataset
dataset <- read.csv("./Group_Assignment_Dataset.txt")
#convert to date format
dataset$Date <- as.Date(dataset$Date, format="%d/%m/%Y")

#Filter data for one week
one_week_data1 <- dataset %>% filter(Date >= as.Date("2007-02-12") & Date <= as.Date("2007-02-18"))

# ----- QUESTION 1 -----

#Use approx to do linear interpolation
one_week_data1[,c("Global_reactive_power")] <- na.approx(one_week_data1[,c("Global_reactive_power")]);
one_week_data1[,c("Global_active_power")] <- na.approx(one_week_data1[,c("Global_active_power")]);
one_week_data1[,c("Voltage")] <- na.approx(one_week_data1[,c("Voltage")]);
one_week_data1[,c("Global_intensity")] <- na.approx(one_week_data1[,c("Global_intensity")]);
one_week_data1[,c("Sub_metering_1")] <- na.approx(one_week_data1[,c("Sub_metering_1")]);
one_week_data1[,c("Sub_metering_2")] <- na.approx(one_week_data1[,c("Sub_metering_2")]);
one_week_data1[,c("Sub_metering_3")] <- na.approx(one_week_data1[,c("Sub_metering_3")]);

#calculate and print anomaly percentages
allGrp <- one_week_data1[,c("Global_reactive_power")];
grpZScores <- (allGrp - mean(allGrp)) / sd(allGrp);
numOutliersGrp <- length(grpZScores[abs(grpZScores) > 3]);
print(paste0("number of outliers for Global Reactive Power = ", numOutliersGrp));
print(numOutliersGrp/length(grpZScores) * 100);

allGap <- one_week_data1[,c("Global_active_power")];
gapZScores <- (allGap - mean(allGap)) / sd(allGap);
numOutliersGap <- length(gapZScores[abs(gapZScores) > 3]);
print(paste0("number of outliers for Global Active Power = ", numOutliersGap));
print(numOutliersGap/length(gapZScores)) * 100;


allVolt <- one_week_data1[,c("Voltage")];
voltZScores <- (allVolt - mean(allVolt)) / sd(allVolt);
numOutliersVolt <- length(grpZScores[abs(grpZScores) > 3]);
print(paste0("number of outliers for Voltage = ", numOutliersVolt));
print(numOutliersVolt/length(voltZScores)) * 100;

allGI <- one_week_data1[,c("Global_intensity")];
giZScores <- (allGI - mean(allGI)) / sd(allGI);
numOutliersGI <- length(giZScores[abs(giZScores) > 3]);
print(paste0("number of outliers for Global Intensity = ", numOutliersGI));
print(numOutliersGI/length(giZScores)) * 100;

allSM1 <- one_week_data1[,c("Sub_metering_1")];
sm1ZScores <- (allSM1 - mean(allSM1)) / sd(allSM1);
numOutliersSM1 <- length(sm1ZScores[abs(sm1ZScores) > 3]);
print(paste0("number of outliers for Sub Metering 1 = ", numOutliersSM1));
print(numOutliersSM1/length(sm1ZScores)) * 100;

allSM2 <- one_week_data1[,c("Sub_metering_2")];
sm2ZScores <- (allSM2 - mean(allSM2)) / sd(allSM2);
numOutliersSM2 <- length(sm2ZScores[abs(sm2ZScores) > 3]);
print(paste0("number of outliers for Sub Metering 2 = ", numOutliersSM2));
print(numOutliersSM2/length(sm2ZScores)) * 100;

allSM3 <- one_week_data1[,c("Sub_metering_3")];
sm3ZScores <- (allSM3 - mean(allSM3)) / sd(allSM3);
numOutliersSM3 <- length(sm3ZScores[abs(sm3ZScores) > 3]);
print(paste0("number of outliers for Sub Metering 3 = ", numOutliersSM3));
print(numOutliersSM3/length(sm3ZScores)) * 100;

# ----- QUESTION 2 -----

# Select only numeric columns
numeric_columns <- one_week_data1[, c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")]

# calculate pearsons correlation values
cor_matrix <- cor(numeric_columns, use = "complete.obs", method = "pearson")

print(cor_matrix)

ggcorrplot(cor_matrix, lab = TRUE, title = "Household Electricity Consumption Correlation Matrix")

# ----- QUESTION 3 -----

#weekday daytime dataframe:
weekday_daytime_df <- data.frame(
  Time      = one_week_data1$Time[one_week_data1$Date == as.Date("2007-02-12") & one_week_data1$Time >= "07:00:00" & one_week_data1$Time <= "17:00:00"], 
  Monday    = one_week_data1 %>% filter(Date == as.Date("2007-02-12") & Time >= "07:00:00" & Time <= "17:00:00") %>% pull(Global_intensity),
  Tuesday   = one_week_data1 %>% filter(Date == as.Date("2007-02-13") & Time >= "07:00:00" & Time <= "17:00:00") %>% pull(Global_intensity),
  Wednesday = one_week_data1 %>% filter(Date == as.Date("2007-02-14") & Time >= "07:00:00" & Time <= "17:00:00") %>% pull(Global_intensity),
  Thursday  = one_week_data1 %>% filter(Date == as.Date("2007-02-15") & Time >= "07:00:00" & Time <= "17:00:00") %>% pull(Global_intensity),
  Friday    = one_week_data1 %>% filter(Date == as.Date("2007-02-16") & Time >= "07:00:00" & Time <= "17:00:00") %>% pull(Global_intensity)
)

weekday_daytime_avg_df <- data.frame(
  Time = weekday_daytime_df$Time,  # Keep the same time column
  Weekday_Daytime_Avg = rowMeans(weekday_daytime_df[, 2:6], na.rm = TRUE)  # Compute row-wise mean for Monday - Friday
)

#weekend daytime dataframe:
weekend_daytime_df <- data.frame(
  Time      = one_week_data1$Time[one_week_data1$Date == as.Date("2007-02-12") & one_week_data1$Time >= "07:00:00" & one_week_data1$Time <= "17:00:00"],
  Saturday  = one_week_data1 %>% filter(Date == as.Date("2007-02-17") & Time >= "07:00:00" & Time <= "17:00:00") %>% pull(Global_intensity),
  Sunday    = one_week_data1 %>% filter(Date == as.Date("2007-02-18") & Time >= "07:00:00" & Time <= "17:00:00") %>% pull(Global_intensity)
)

weekend_daytime_avg_df <- data.frame(
  Time = weekend_daytime_df$Time, 
  Weekend_Daytime_Avg = rowMeans(weekend_daytime_df[, 2:3], na.rm = TRUE)
)

#weekday night dataframe:
weekday_night_df <- data.frame(
  Time      = one_week_data1$Time[one_week_data1$Date == as.Date("2007-02-12") & one_week_data1$Time >= "17:01:00" & one_week_data1$Time <= "23:59:00"], 
  Monday    = one_week_data1 %>% filter(Date == as.Date("2007-02-12") & Time >= "17:01:00" & Time <= "23:59:00") %>% pull(Global_intensity),
  Tuesday   = one_week_data1 %>% filter(Date == as.Date("2007-02-13") & Time >= "17:01:00" & Time <= "23:59:00") %>% pull(Global_intensity),
  Wednesday = one_week_data1 %>% filter(Date == as.Date("2007-02-14") & Time >= "17:01:00" & Time <= "23:59:00") %>% pull(Global_intensity),
  Thursday  = one_week_data1 %>% filter(Date == as.Date("2007-02-15") & Time >= "17:01:00" & Time <= "23:59:00") %>% pull(Global_intensity),
  Friday    = one_week_data1 %>% filter(Date == as.Date("2007-02-16") & Time >= "17:01:00" & Time <= "23:59:00") %>% pull(Global_intensity)
)

weekday_night_avg_df <- data.frame(
  Time = weekday_night_df$Time, 
  Weekday_night_Avg = rowMeans(weekday_night_df[, 2:6], na.rm = TRUE)
)

#weekend night datafram:
weekend_night_df <- data.frame(
  Time      = one_week_data1$Time[one_week_data1$Date == as.Date("2007-02-12") & one_week_data1$Time >= "17:01:00" & one_week_data1$Time <= "23:59:00"],
  Saturday  = one_week_data1 %>% filter(Date == as.Date("2007-02-17") & Time >= "17:01:00" & Time <= "23:59:00") %>% pull(Global_intensity),
  Sunday    = one_week_data1 %>% filter(Date == as.Date("2007-02-18") & Time >= "17:01:00" & Time <= "23:59:00") %>% pull(Global_intensity)
)

weekend_night_avg_df <- data.frame(
  Time = weekday_night_df$Time, 
  Weekend_night_Avg = rowMeans(weekend_night_df[, 2:3], na.rm = TRUE)
)


#daytime plot:
daytime_plot_data <- data.frame(
  Time = weekday_daytime_avg_df$Time,
  Weekday_Daytime_Avg = weekday_daytime_avg_df$Weekday_Daytime_Avg,
  Weekend_Daytime_Avg = weekend_daytime_avg_df$Weekend_Daytime_Avg
)

#ensure time format
daytime_plot_data$Time <- as.POSIXct(daytime_plot_data$Time, format="%H:%M:%S")

# Reshape data to long format for ggplot
daytime_plot_data_long <- tidyr::pivot_longer(daytime_plot_data, cols = c("Weekday_Daytime_Avg", "Weekend_Daytime_Avg"), names_to = "Type", values_to = "Avg")

#plot daytime using ggplot
ggplot(daytime_plot_data_long, aes(x = Time, y = Avg, color = Type)) +
  #dot plot
  geom_point(alpha = 0.6, size = 2) + 
  # Linear regression lines
  geom_smooth(method = "lm", se = FALSE, aes(linetype = "Linear"), linewidth = 1) +
  # Polynomial regression lines (degree 3 for some curvature)
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, aes(linetype = "Polynomial"), size = 1) +
  #set colours
  scale_color_manual(values = c("Weekday_Daytime_Avg" = "blue", "Weekend_Daytime_Avg" = "red")) +
  # Custom line types for legend
  scale_linetype_manual(values = c("Linear" = "dashed", "Polynomial" = "solid")) +
  # create legend
  labs(title = "Weekday vs. Weekend Daytime Average Global Intensity", x = "Time", y = "Average Global Intensity",  linetype = "Regression Type")

#night plot:
night_plot_data <- data.frame(
  Time = weekday_night_avg_df$Time,
  Weekday_Night_Avg = weekday_night_avg_df$Weekday_night_Avg,
  Weekend_Night_Avg = weekend_night_avg_df$Weekend_night_Avg
)

# ensure time format
night_plot_data$Time <- as.POSIXct(night_plot_data$Time, format="%H:%M:%S")

# Reshape data for ggplot
night_plot_data_long <- tidyr::pivot_longer(night_plot_data, cols = c("Weekday_Night_Avg", "Weekend_Night_Avg"), names_to = "Type", values_to = "Avg")

# Create dot plot
ggplot(night_plot_data_long, aes(x = Time, y = Avg, color = Type)) +
  #dot plot
  geom_point(alpha = 0.6, size = 2) + 
  # Linear regression lines
  geom_smooth(method = "lm", se = FALSE, aes(linetype = "Linear"), size = 1) +
  # Polynomial regression lines (degree 3 for some curvature)
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, aes(linetype = "Polynomial"), linewidth = 1) +
  #set colours
  scale_color_manual(values = c("Weekday_Night_Avg" = "blue", "Weekend_Night_Avg" = "red")) +
  #create legend
  labs(title = "Weekday vs. Weekend Night Average Global Intensity", x = "Time", y = "Average Global Intensity", linetype = "Regression Type")

