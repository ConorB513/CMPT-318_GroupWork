install.packages('stats');
library(stats);

install.packages("zoo");
library(zoo);

dataset <- read.csv("./TermProjectData.txt", header = TRUE);

#1 - scale features - standardization with na values linearly interpolated
gap <- na.approx(dataset$Global_active_power);
dataset$Global_active_power <- (gap-mean(gap))/sd(gap);

grp <- na.approx(dataset$Global_reactive_power);
dataset$Global_reactive_power <- (grp - mean(grp))/sd(grp);

volt <- na.approx(dataset$Voltage);
dataset$Voltage <- (volt - mean(volt))/sd(volt);

gi <- na.approx(dataset$Global_intensity);
dataset$Global_intensity <- (gi - mean(gi))/sd(gi);

sm1 <- na.approx(dataset$Sub_metering_1);
dataset$Sub_metering_1 <- (sm1 - mean(sm1))/sd(sm1);

sm2 <- na.approx(dataset$Sub_metering_2);
dataset$Sub_metering_2 <- (sm2 - mean(sm2))/sd(sm2);

sm3 <- na.approx(dataset$Sub_metering_3);
dataset$Sub_metering_3 <- (sm3 - mean(sm3))/sd(sm3);



