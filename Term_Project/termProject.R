#CMPT 318 Group 7 Term Project Code

# --- Install Packages ---
install.packages('stats');
library(stats);

install.packages("zoo");
library(zoo);

install.packages("depmixS4");
library(depmixS4);

# --- Import Project Data ---
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
pcaFeatures <- pcs$x
head(pcaFeatures);

#plot PCA

#xx% of the variance is accounted for with the first x PCs, use those going

# --- Part 3: Training HMM Models ---

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

# Step 3: Train HMM models for states 4 to 20 using training data
training_results <- list()  # Store model results

for (states in 4:20) {
  cat("\n--- Training HMM with", states, "states ---\n")
  
  # Define the HMM model with 3 response variables
  model <- depmix(response = list(PC1 ~ 1, PC2 ~ 1), 
                  data = trainingData_discrete, 
                  family = list(multinomial(), multinomial()),
                  nstates = states, 
                  ntimes = ntimes_val)
  
  # Fit the model, increase EM iterations, tighten convergence tolerance
  fitModel <- fit(model, emcontrol = em.control(maxit = 1000, tol = 1e-8))
  
  # Store results
  training_results[[as.character(states)]] <- list(
    logLik = logLik(fitModel),
    BIC = BIC(fitModel),
    summary = summary(fitModel)
  )
  
  # Print model evaluation metrics
  cat("States:", states, " | logLik:", logLik(fitModel), " | BIC:", BIC(fitModel), "\n")
}

# ----- Testing -----

# Round the data to the nearest half-integer and convert to factors
testingData_discrete <- testData
testingData_discrete$PC1 <- as.factor(round(testingData_discrete$PC1 * 2) / 2)
testingData_discrete$PC2 <- as.factor(round(testingData_discrete$PC2 * 2) / 2)

ntimes_val_testing <- as.numeric(table(testingData_discrete$Date))

# Step 4: Test HMM model for 18 states using training data
cat("\n--- Testing HMM with", states, "states ---\n")

# Define the HMM model with 3 response variables
model <- depmix(response = list(PC1 ~ 1, PC2 ~ 1), 
                data = testingData_discrete, 
                family = list(multinomial(), multinomial()),
                nstates = 20, 
                ntimes = ntimes_val_testing)

# Fit the model, increase EM iterations, tighten convergence tolerance
fitModel <- fit(model, emcontrol = em.control(maxit = 1000, tol = 1e-8))

# Print model evaluation metrics
cat("States:", states, " | logLik:", logLik(fitModel), " | BIC:", BIC(fitModel), "\n")


# --- Anomaly Detection --- 

totalRows <- nrow(testingData_discrete)

subsetSize <- ceiling(totalRows / 10)

subsetLogLikelihoodValues <- c()

for (i in 1:10) {
   cat("Iteration:", i, "\n")
  

  startIndex <- ((i - 1) * subsetSize) + 1
  

  endIndex <- min(i * subsetSize, totalRows)  
  
  
  subset_data <- testingData_discrete[startIndex:endIndex, ]
  
 
  ntimes_val_subset <- as.numeric(table(subset_data$Date))
  
 
  model <- depmix(response = list(PC1 ~ 1, PC2 ~ 1), 
                  data = subset_data, 
                  family = list(multinomial(), multinomial()),
                  nstates =20, 
                  ntimes = ntimes_val_subset)
  

  fitModel <- fit(model)
  
  subsetLogLikelihoodValues[i] <- logLik(fitModel)
  
  cat("Subset:", i, ", logLik:", subsetLogLikelihoodValues[i], "\n")
}

# Define Y-axis ticks (adjust spacing)
# Plot the sorted values
plot(subsetLogLikelihoodValues, type = "o", col = "blue", pch = 16, xaxt = "n", 
     xlab = "Index", ylab = "Value", 
     main = "Sorted Values (Highest to Lowest)")
axis(1, at = 1:length(subsetLogLikelihoodValues), labels = 1:length(subsetLogLikelihoodValues))

print(subsetLogLikelihoodValues)

print("Done!")

