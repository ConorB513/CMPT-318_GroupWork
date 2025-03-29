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

dataset <- read.csv("./TermProjectData.txt");

# --- Part 1: Feature Scaling ----

#standardization with na values linearly interpolated

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


# --- Part 2: Feature Engineering ----

#extract only the numerical columns
numerical <- dataset[c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")];

#conduct PCA
pcs <- prcomp(numerical);
pcaFeatures <- pcs$x
head(pcaFeatures);

#plot PCA


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



# --- Part 4:  Anomaly Detection --- 

totalRows <- nrow(testingData_discrete)

#Get size of subset by dividing and rounding up
subsetSize <- ceiling(totalRows / 10)
subsetLogLikelihoodValues <- c()
normalizedLogLikelihoodValues <- c()

#Store the 10 ranges 
dateRanges <- character(10)

#Store the size of each range
subsetSizes <- numeric(10)

for (i in 1:10) {
  cat("Iteration:", i, "\n")
  
  #Subtract by 1 as R uses 1-based indexing 
  startIndex <- ((i - 1) * subsetSize) + 1
  
  endIndex <- min(i * subsetSize, totalRows)  
  
  subset_data <- testingData_discrete[startIndex:endIndex, ]
  
  ntimes_val_subset <- as.numeric(table(subset_data$Date))
  
  #Get the start and end data of the period 
  startDate <- as.character(min(subset_data$Date))  
  endDate <- as.character(max(subset_data$Date))  
  
  dateRanges[i] <- paste(startDate, "to", endDate)
  
  #Calculate likelihood with optimal HMM
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


#Used ggplot2 for a more robust plot 
install.packages("ggplot2")
library(ggplot2)

data <- data.frame(
  Period = 1:length(normalizedLogLikelihoodValues),
  Normalized_LL = normalizedLogLikelihoodValues
)

#Plot normalized normalized likelihoods 
ggplot(data, aes(x = Period, y = Normalized_LL)) +
  geom_line(color = "black") +                
  geom_point(color = "black", size = 3) +   
  geom_text(aes(label = round(Normalized_LL, 3)), hjust = -0.25, vjust = 0, size = 3) + 
  geom_hline(yintercept = -1.85, color = "blue", linetype = "dashed", size = 1.2) +  
  labs(title = "Normalized Log-Likelihood Values Across 2009 Periods", x = "Period", y = "Normalized Log-Likelihood") +  
  scale_x_continuous(breaks = 1:10) + 
  theme(
    plot.margin = margin(10, 10, 10, 10), 
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    plot.background = element_rect(color = "black", fill = "white", size = 2),  
    panel.background = element_blank()  ) + 
  annotate("text", x = 5, y = -2, label = "Training Log-Likelihood (-1.85)", 
           color = "blue", size = 4, fontface = "italic", hjust = 0, vjust = 1)


#Calculate absolute deviations 

deviations <- abs(-1.85 - normalizedLogLikelihoodValues)


# Create a data frame with Period number and Deviation
data <- data.frame(Period = 1:length(normalizedLogLikelihoodValues), 
                   Deviation = deviations)

# Find the period with the largest deviation
max_deviation_period <- data$Period[which.max(data$Deviation)]

# Plot deviations
library(ggplot2)
ggplot(data, aes(x = Period, y = Deviation)) +
  geom_bar(stat = "identity", fill = "lightgrey") +         
  geom_point(aes(x = Period, y = Deviation), color = "black", size = 3) + 
  geom_text(aes(label = round(Deviation, 3)), hjust = -0.3, vjust = -0.5, size = 3) + 
  geom_vline(xintercept = max_deviation_period, color = "black", linetype = "dashed", size = 1) +
  labs(title = "Deviation from Training Likelihood",x = "Period", y = "Deviation ") +
  scale_x_continuous(breaks = 1:10) + 
  theme(
    plot.margin = margin(10, 10, 10, 10), 
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    plot.background = element_rect(color = "black", fill = "white", size = 2),  
    panel.background = element_blank()
  ) 


