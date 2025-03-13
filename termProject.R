install.packages('stats');
library(stats);

install.packages("zoo");
library(zoo);

dataset <- read.csv("./TermProjectData.txt");

#1 - scale features - standardization with na values linearly interpolated

# - rule = 2 extends the first non na value to the beginning/end if it is na
gap <- na.spline(dataset$Global_active_power);
dataset$Global_active_power <- (gap-mean(gap))/sd(gap);

grp <- na.spline(dataset$Global_reactive_power);
dataset$Global_reactive_power <- (grp - mean(grp))/sd(grp);

volt <- na.spline(dataset$Voltage);
dataset$Voltage <- (volt - mean(volt))/sd(volt);

gi <- na.spline(dataset$Global_intensity);
dataset$Global_intensity <- (gi - mean(gi))/sd(gi);

sm1 <- na.spline(dataset$Sub_metering_1);
dataset$Sub_metering_1 <- (sm1 - mean(sm1))/sd(sm1);

sm2 <- na.spline(dataset$Sub_metering_2);
dataset$Sub_metering_2 <- (sm2 - mean(sm2))/sd(sm2);

sm3 <- na.spline(dataset$Sub_metering_3);
dataset$Sub_metering_3 <- (sm3 - mean(sm3))/sd(sm3);


#2 - principal component analysis
#extract only the numerical columns
numerical <- dataset[c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")];

#conduct PCA
pcs <- prcomp(numerical);
summary(pcs);

#plot PCA

#xx% of the variance is accounted for with the first x PCs, use those going


