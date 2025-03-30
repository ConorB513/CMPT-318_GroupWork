#CMPT 318 Group 7 Term Project Code
#Members:
#
#Justinne Baltazar 301559449
#Rashed Hadi 301469578
#Sean Coloma 301440149
#Conor Benson 301585511


# --- Part 0: Install Packages and Import Project Data---

install.packages('stats');
library(stats);

install.packages("zoo");
library(zoo);

install.packages("depmixS4");
library(depmixS4);

install.packages("ggfortify");
library(ggfortify);

dataset <- read.csv("./TermProjectData.txt");

# --- Part 1: Feature Scaling ----

#standardization with na values spline interpolated

# - spline interpolation works at NA endpoints
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


# --- Part 2: Feature Engineering ----

#extract only the numerical columns
numerical <- dataset[c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")];

#conduct PCA
pcs <- prcomp(numerical);
pcaFeatures <- pcs$x
summary(pcs);

#plot PCA information
#plotting PCS projected onto the PC1, PC2 axes
autoplot(pcs);

#plotting Cummulative Variance of PCs
plot(cumsum(pcs$sdev^2/sum(pcs$sdev^2)), type = "b", ylim=0:1, xlab = "# of PCs used", ylab = "% of Variance Captured");


# --- Part 3: HMM Training and Testing ---

# Convert Date column to Date format
dataset$Date <- as.Date(dataset$Date, format="%d/%m/%Y");

# Combine date and time with first three PCA features (PC1 to PC3)
pcaSubset <- data.frame(Date = dataset$Date, Time = dataset$Time, 
                        PC1 = pcaFeatures[,1], 
                        PC2 = pcaFeatures[,2]);


# Add a column for the day of the week
pcaSubset$Day <- weekdays(pcaSubset$Date)

# Define date ranges
training_start <- as.Date("16/12/2006", format="%d/%m/%Y");
training_end <- as.Date("31/12/2008", format="%d/%m/%Y");
testing_start <- as.Date("01/01/2009", format="%d/%m/%Y");
testing_end <- as.Date("31/12/2009", format="%d/%m/%Y");

# Separate 3 years for training and 1 year for testing
trainingData <- subset(pcaSubset, Date >= training_start & Date <= training_end)
testData <- subset(pcaSubset, Date >= testing_start & Date <= testing_end)


# Filter only Fridays between 17:00:00 and 21:00:00
trainingData <- subset(trainingData, Day == "Friday" & Time >= "17:00:00" & Time <= "21:00:00")
testData <- subset(testData, Day == "Friday" & Time >= "17:00:00" & Time <= "21:00:00")


# Round the data to the nearest half-integer and convert to factors
trainingData_discrete <- trainingData
trainingData_discrete$PC1 <- as.factor(round(trainingData_discrete$PC1 * 2) / 2)
trainingData_discrete$PC2 <- as.factor(round(trainingData_discrete$PC2 * 2) / 2)


# ----- Training -----

#set seed to determine random init parameter selection
set.seed(123)

# Define the number of sequences (ntimes in rep())
ntimes_val <- as.numeric(table(trainingData_discrete$Date))

# Train HMM models for states 4 to 20 using training data
training_results <- list()

for (states in 18:18) {
  cat("\n--- Training HMM with", states, "states ---\n")
  
  model <- depmix(response = list(PC1 ~ 1, PC2 ~ 1), 
                  data = trainingData_discrete, 
                  family = list(multinomial(), multinomial()),
                  nstates = states, 
                  ntimes = ntimes_val)
  
  # Fit the model, increase EM iterations, tighten convergence tolerance
  fitModel <- fit(model, emcontrol = em.control(maxit = 1000, tol = 1e-8))
  
  summary(fitModel)
  
  training_results[[as.character(states)]] <- list(
    logLik = logLik(fitModel),
    BIC = BIC(fitModel),
    summary = summary(fitModel)
  )
  
  cat("States:", states, " | logLik:", logLik(fitModel), " | BIC:", BIC(fitModel), "\n")
}

# ----- Testing ----- Test HMM model for 15, 18 and 20 states using training data

# Round the data to the nearest half-integer and convert to factors
testingData_discrete <- testData
testingData_discrete$PC1 <- as.factor(round(testingData_discrete$PC1 * 2) / 2)
testingData_discrete$PC2 <- as.factor(round(testingData_discrete$PC2 * 2) / 2)

ntimes_val_testing <- as.numeric(table(testingData_discrete$Date))


cat("\n--- Testing HMM with 15 states ---\n")


model <- depmix(response = list(PC1 ~ 1, PC2 ~ 1), 
                data = testingData_discrete, 
                family = list(multinomial(), multinomial()),
                nstates = 15, 
                ntimes = ntimes_val_testing)


fitModel <- fit(model, emcontrol = em.control(maxit = 1000, tol = 1e-8))


cat("States:", 15, " | logLik:", logLik(fitModel), " | BIC:", BIC(fitModel), "\n")


cat("\n--- Testing HMM with 18 states ---\n")


model <- depmix(response = list(PC1 ~ 1, PC2 ~ 1), 
                data = testingData_discrete, 
                family = list(multinomial(), multinomial()),
                nstates = 18, 
                ntimes = ntimes_val_testing)


fitModel <- fit(model, emcontrol = em.control(maxit = 1000, tol = 1e-8))

cat("States:", 18, " | logLik:", logLik(fitModel), " | BIC:", BIC(fitModel), "\n")

cat("\n--- Testing HMM with 20 states ---\n")

model <- depmix(response = list(PC1 ~ 1, PC2 ~ 1), 
                data = testingData_discrete, 
                family = list(multinomial(), multinomial()),
                nstates = 20, 
                ntimes = ntimes_val_testing)


fitModel <- fit(model, emcontrol = em.control(maxit = 1000, tol = 1e-8))

cat("States:", 20, " | logLik:", logLik(fitModel), " | BIC:", BIC(fitModel), "\n")

print()

# --- Part 4:  Anomaly Detection --- 

periodRanges <- list(
  c(1, 964),         # Period 1:  4 Fridays
  c(965, 2169),      # Period 2:  5 Fridays
  c(2170, 3374),     # Period 3:  5 Fridays
  c(3375, 4579),     # Period 4:  5 Fridays
  c(4580, 5784),     # Period 5:  5 Fridays
  c(5785, 6989),     # Period 6:  5 Fridays
  c(6990, 8194),     # Period 7:  5 Fridays
  c(8195, 9399),     # Period 8:  5 Fridays
  c(9400, 10604),    # Period 9:  5 Fridays
  c(10605, 11568)    # Period 10: 4 Fridays
)

# Initialize vectors to store results
subsetLogLikelihoodValues <- c()
normalizedLogLikelihoodValues <- c()

# Store the 10 ranges 
dateRanges <- character(10)

# Store the size of each range
subsetSizes <- numeric(10)

for (i in 1:10) {
  cat("Iteration:", i, "\n")
  
  startIndex <- ranges[[i]][1]
  endIndex <- ranges[[i]][2]
  
  subset_data <- testingData_discrete[startIndex:endIndex, ]
  
  ntimes_val_subset <- as.numeric(table(subset_data$Date))
  
  # Get the start and end date of the period
  startDate <- as.character(min(subset_data$Date))  
  endDate <- as.character(max(subset_data$Date))  
  
  dateRanges[i] <- paste(startDate, "to", endDate)
  
  # Calculate likelihood with optimal HMM
  model <- depmix(response = list(PC1 ~ 1, PC2 ~ 1), 
                  data = subset_data, 
                  family = list(multinomial(), multinomial()),
                  nstates = 15, 
                  ntimes = ntimes_val_subset)
  
  fitModel <- fit(model)
  
  subsetLogLikelihoodValues[i] <- logLik(fitModel)
  subsetSizes[i] <- nrow(subset_data)
  
  # Normalize by dividing log-likelihood by subset size
  normalizedLogLikelihoodValues[i] <- subsetLogLikelihoodValues[i] / nrow(subset_data)
  
  cat("\n--- Period", i, "(", startDate, "to", endDate, ") ---\n",
      "Subset:", i, ", logLik:", subsetLogLikelihoodValues[i], 
      ", Subset Size:", nrow(subset_data), 
      ", Normalized logLik:", normalizedLogLikelihoodValues[i], "\n")
}

# Print results
print(dateRanges)
print(subsetLogLikelihoodValues)
print(normalizedLogLikelihoodValues)



# Original Log-Likelihood Plot
plot(subsetLogLikelihoodValues, 
     type = "o", col = "blue", pch = 16, 
     xlab = "Period", ylab = "Log-Likelihood",
     main = "Log-Likelihood Values Across 2009 Periods")



install.packages("ggplot2")
library(ggplot2)

data <- data.frame(
  Period = 1:10,
  normalizedLikelihoods = normalizedLogLikelihoodValues
)

#Plot normalized normalized likelihoods 
ggplot(data, aes(x = Period, y = normalizedLikelihoods)) +
  geom_line(color = "black", linetype = "dotted") +                
  geom_point(color = "black", size = 3) +   
  geom_text(aes(label = round(normalizedLikelihoods, 2)), hjust = -0.25, vjust = 0, size = 3) + 
  geom_hline(yintercept = -1.855, color = "blue", linetype = "dashed", size = 1.2) +  
  labs(title = "Normalized Log-Likelihood Values Across 2009 Periods", x = "Period", y = "Normalized Log-Likelihood") +  
  scale_x_continuous(breaks = 1:10) + 
  scale_y_continuous(breaks = seq(-2.5, -0.75, by = 0.25)) +
  theme(
    plot.margin = margin(10, 10, 10, 10), 
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    plot.background = element_rect(color = "black", fill = "white", size = 2),  
    panel.background = element_blank()  ) + 
  annotate("text", x = 5, y = -2, label = "Training Log-Likelihood (-1.855)", 
           color = "blue", size = 4, fontface = "italic", hjust = 0, vjust = 1)


#Plotting Deviation Data

#Calculate absolute deviations 
periodDeviations <- abs(-1.855 - normalizedLogLikelihoodValues)


# Create a data frame with periods and absolute deviation
deviationData <- data.frame(Period = 1:10, 
                            periodDeviations = periodDeviations)

# Find the period with the largest deviation
maxDeviationPeriod <- deviationData$Period[which.max(deviationData$periodDeviations)]

#Calculate the mean deviation of the periods
meanDeviation <- mean(deviationData$periodDeviations)

#Create a plot of the deviation data
ggplot(deviationData, aes(x = Period, y =  periodDeviations, fill = (Period == maxDeviationPeriod))) +
  geom_bar(stat = "identity") +        
  labs(title = "Period Deviation from Training Likelihood", x = "Period", y = "Absolute Deviation") +
  scale_x_continuous(breaks = 1:10) + 
  scale_fill_manual(values = c("lightgrey", "red"), guide = "none") +  # Color all bars gray except max deviation period
  theme(
    plot.margin = margin(10, 10, 10, 10), 
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    plot.background = element_rect(color = "black", fill = "white", size = 2),  
    panel.background = element_blank()
  )+ 
  annotate("point", x = 8.7, y = max(deviationData$periodDeviations) - 0.075, color = "red", size = 3) + 
  annotate("text", x = 8.8, y = max(deviationData$periodDeviations) - 0.075, label = "Max Deviation (0.75)", 
           color = "black", size = 4, fontface = "italic", hjust = 0, vjust = 0.5)+
  
  geom_hline(yintercept = meanDeviation, color = "green", linetype = "dashed", size = 1) +
  annotate("point", x = 8.7, y = max(deviationData$periodDeviations) - 0.15, color = "green", size = 3) + 
  annotate("text", x = 8.8, y = max(deviationData$periodDeviations) - 0.15, 
           label = "Mean Deviation (0.369)", 
           color = "black", size = 4, fontface = "italic", hjust = 0, vjust = 0.5)