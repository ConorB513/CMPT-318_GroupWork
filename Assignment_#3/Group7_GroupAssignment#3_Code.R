# CMPT 318 Group 7 Assignment 3

# Install needed packages
install.packages("depmixS4")
install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(depmixS4)
library(ggplot2)

# import Dataset
dataset <- read.csv("./Group_Assignment_Dataset.txt", header = TRUE)

#convert to date format
dataset$Date <- as.Date(dataset$Date, format="%d/%m/%Y")

#Extract Tuesdays from 2-8 am data
tuesdaysData <- dataset  %>% filter(
  weekdays(as.Date(Date)) == "Tuesday",
  Time >= "02:00:00" & Time <= "04:59:00")

# Extract global active power from selected time window
tuesdayGlobalActivePower <- tuesdaysData$Global_active_power
tuesdayGlobalActivePower <- data.frame(Global_active_power = tuesdayGlobalActivePower)

numOfStates <- c(4, 6, 8, 10, 11, 12, 13, 14, 16)
logValues <- c()
bicValues <- c()

# ----- Question 1 -----

# Train model using depmix for different number of states to find optimal n

# --- 4 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower, 
                nstates = 4, 
                ntimes = rep(180,52))

fitModel <- fit(model)
logValues <- append(logValues, logLik(fitModel))

BIC(fitModel)
print(summary(fitModel))

bicVal <- BIC(fitModel)
bicValues <- append(bicValues, bicVal)


# --- 6 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower, 
                nstates = 6, 
                ntimes = rep(180,52))

fitModel <- fit(model)
logValues <- append(logValues, logLik(fitModel))

BIC(fitModel)
print(summary(fitModel))

bicVal <- BIC(fitModel)
bicValues <- append(bicValues, bicVal)



# --- 8 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower, 
                nstates = 8, 
                ntimes = rep(180,52))

fitModel <- fit(model)
logValues <- append(logValues, logLik(fitModel))

BIC(fitModel)
print(summary(fitModel))

bicVal <- BIC(fitModel)
bicValues <- append(bicValues, bicVal)



# --- 10 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower, 
                nstates = 10, 
                ntimes = rep(180,52))

fitModel <- fit(model)
logValues <- append(logValues, logLik(fitModel))

BIC(fitModel)
print(summary(fitModel))

bicVal <- BIC(fitModel)
bicValues <- append(bicValues, bicVal)



# --- 11 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower, 
                nstates = 11, 
                ntimes = rep(180,52))

fitModel <- fit(model)
logValues <- append(logValues, logLik(fitModel))

BIC(fitModel)
print(summary(fitModel))

bicVal <- BIC(fitModel)
bicValues <- append(bicValues, bicVal)


# --- 12 states -----

model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower, 
                nstates = 12, 
                ntimes = rep(180,52))

fitModel <- fit(model)
logValues <- append(logValues, logLik(fitModel))

BIC(fitModel)
print(summary(fitModel))

bicVal <- BIC(fitModel)
bicValues <- append(bicValues, bicVal)



# --- 13 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower, 
                nstates = 13, 
                ntimes = rep(180,52))

fitModel <- fit(model)
logValues <- append(logValues, logLik(fitModel))

BIC(fitModel)
print(summary(fitModel))

bicVal <- BIC(fitModel)
bicValues <- append(bicValues, bicVal)



# --- 14 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower, 
                nstates = 14, 
                ntimes = rep(180,52))

fitModel <- fit(model)
logValues <- append(logValues, logLik(fitModel))

BIC(fitModel)
print(summary(fitModel))

bicVal <- BIC(fitModel)
bicValues <- append(bicValues, bicVal)



# --- 16 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower, 
                nstates = 16, 
                ntimes = rep(180,52))

fitModel <- fit(model)
logValues <- append(logValues, logLik(fitModel))

BIC(fitModel)
print(summary(fitModel))

bicVal <- BIC(fitModel)
bicValues <- append(bicValues, bicVal)


# Plot log-likelihood and BIC values
ggplot(data.frame(numOfStates, logValues), aes(x = numOfStates, y = logValues)) + 
  geom_line() + geom_point() +
  labs(title = "Log-likelihood vs. Number of States", x = "Number of States", y = "Log-likelihood") + 
  theme_minimal()

ggplot(data.frame(numOfStates, bicValues), aes(x = numOfStates, y = bicValues)) + 
  geom_line() + geom_point() +
  labs(title = "BIC vs. Number of States", x = "Number of States", y = "BIC") + 
  theme_minimal()

# ----- Question 2 -----

# round data to nearest half integer
tuesdayGlobalActivePower_discrete <- tuesdayGlobalActivePower
tuesdayGlobalActivePower_discrete$Global_active_power <- round(tuesdayGlobalActivePower_discrete$Global_active_power * 2) / 2
tuesdayGlobalActivePower_discrete$Global_active_power <- as.factor(tuesdayGlobalActivePower_discrete$Global_active_power)


# retain states with new rounded data

# --- 4 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower_discrete, 
                family = multinomial(),
                nstates = 4, 
                ntimes = rep(180,52))

fitModel <- fit(model)
cat("logLik: ", logLik(fitModel), "\n")
BIC(fitModel)
print(summary(fitModel))

# --- 6 states -----

model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower_discrete,
                family = multinomial(),
                nstates = 6, 
                ntimes = rep(180,52))

fitModel <- fit(model)
cat("logLik: ", logLik(fitModel), "\n")
BIC(fitModel)
print(summary(fitModel))

# --- 8 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower_discrete,
                family = multinomial(),
                nstates = 8, 
                ntimes = rep(180,52))

fitModel <- fit(model)
cat("logLik: ", logLik(fitModel), "\n")
BIC(fitModel)
print(summary(fitModel))

# --- 10 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower_discrete,
                family = multinomial(),
                nstates = 10, 
                ntimes = rep(180,52))

fitModel <- fit(model)
cat("logLik: ", logLik(fitModel), "\n")
BIC(fitModel)
print(summary(fitModel))

# --- 11 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower_discrete,
                family = multinomial(),
                nstates = 11, 
                ntimes = rep(180,52))

fitModel <- fit(model)
cat("logLik: ", logLik(fitModel), "\n")
BIC(fitModel)
print(summary(fitModel))

# --- 12 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower_discrete,
                family = multinomial(),
                nstates = 12, 
                ntimes = rep(180,52))

fitModel <- fit(model)
cat("logLik: ", logLik(fitModel), "\n")
BIC(fitModel)
print(summary(fitModel))

# --- 13 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower_discrete,
                family = multinomial(),
                nstates = 13, 
                ntimes = rep(180,52))

fitModel <- fit(model)
cat("logLik: ", logLik(fitModel), "\n")
BIC(fitModel)
print(summary(fitModel))

# --- 14 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower_discrete, 
                family = multinomial(),
                nstates = 14, 
                ntimes = rep(180,52))

fitModel <- fit(model)
cat("logLik: ", logLik(fitModel), "\n")
BIC(fitModel)
print(summary(fitModel))

# --- 16 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower_discrete, 
                family = multinomial(),
                nstates = 16, 
                ntimes = rep(180,52))

fitModel <- fit(model)
cat("logLik: ", logLik(fitModel), "\n")
BIC(fitModel)
print(summary(fitModel))










