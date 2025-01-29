install.packages("zoo")
library(zoo)

install.packages("dplyr")
library(dplyr)
dataset <- read.csv("./Group_Assignment_Dataset/Group_Assignment_Dataset.txt")
#convert to date format
dataset$Date <- as.Date(dataset$Date, format="%d/%m/%Y")

#Filter data for one week
one_week_data1 <- dataset %>% filter(Date >= as.Date("2007-02-12") & Date <= as.Date("2007-02-18"))

one_week_data1[,c("Global_reactive_power")] <- na.approx(one_week_data1[,c("Global_reactive_power")]);
one_week_data1[,c("Global_active_power")] <- na.approx(one_week_data1[,c("Global_active_power")]);
one_week_data1[,c("Voltage")] <- na.approx(one_week_data1[,c("Voltage")]);
one_week_data1[,c("Global_intensity")] <- na.approx(one_week_data1[,c("Global_intensity")]);
one_week_data1[,c("Sub_metering_1")] <- na.approx(one_week_data1[,c("Sub_metering_1")]);
one_week_data1[,c("Sub_metering_2")] <- na.approx(one_week_data1[,c("Sub_metering_2")]);
one_week_data1[,c("Sub_metering_3")] <- na.approx(one_week_data1[,c("Sub_metering_3")]);

colSums(is.na(one_week_data1))


allGrp <- one_week_data1[,c("Global_reactive_power")];
grpZScores <- (allGrp - mean(allGrp)) / sd(allGrp);
numOutliersGrp <- length(grpZScores[abs(grpZScores) > 3]);
print(paste0("number of outliers for Global Reactive Power = ", numOutliers));
print(numOutliersGrp/length(grpZScores) * 100);

allGap <- one_week_data1[,c("Global_active_power")];
gapZScores <- (allGap - mean(allGap)) / sd(allGap);
numOutliersGrp <- length(gapZScores[abs(gapZScores) > 3]);
print(numOutliersGap/length(gapZScores)) * 100;


allVolt <- one_week_data1[,c("Voltage")];
voltZScores <- (allVolt - mean(allVolt)) / sd(allVolt);
numOutliersVolt <- length(grpZScores[abs(grpZScores) > 3]);
print(numOutliersVolt/length(voltZScores)) * 100;

allGI <- one_week_data1[,c("Global_intensity")];
giZScores <- (allGI - mean(allGI)) / sd(allGI);
numOutliersGI <- length(giZScores[abs(giZScores) > 3]);
print(numOutliersGI/length(giZScores)) * 100;

allSM1 <- one_week_data1[,c("Sub_metering_1")];
sm1ZScores <- (allSM1 - mean(allSM1)) / sd(allSM1);
numOutliersSM1 <- length(sm1ZScores[abs(sm1ZScores) > 3]);
print(numOutliersSM1/length(sm1ZScores)) * 100;

allSM2 <- one_week_data1[,c("Sub_metering_2")];
sm2ZScores <- (allSM2 - mean(allSM2)) / sd(allSM2);
numOutliersSM2 <- length(sm2ZScores[abs(sm2ZScores) > 3]);
print(numOutliersSM2/length(sm2ZScores)) * 100;

allSM3 <- one_week_data1[,c("Sub_metering_3")];
sm3ZScores <- (allSM3 - mean(allSM3)) / sd(allSM3);
numOutliersSM3 <- length(sm3ZScores[abs(sm3ZScores) > 3]);
print(numOutliersSM3/length(sm3ZScores)) * 100;








