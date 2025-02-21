# CMPT 318 Group 7 Assignment 3

# Install needed packages
install.packages("depmixS4")
install.packages("dplyr")
library(dplyr)
library(depmixS4)

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

# Train model using depmix for different number of states to find optimal n

# --- 4 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower, 
                nstates = 4, 
                ntimes = rep(180,52))

fitModel <- fit(model)
BIC(fitModel)
print(summary(fitModel))


# --- 6 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower, 
                nstates = 6, 
                ntimes = rep(180,52))

fitModel <- fit(model)
BIC(fitModel)
print(summary(fitModel))


# --- 8 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower, 
                nstates = 8, 
                ntimes = rep(180,52))

fitModel <- fit(model)
BIC(fitModel)
print(summary(fitModel))


# --- 10 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower, 
                nstates = 10, 
                ntimes = rep(180,52))

fitModel <- fit(model)
BIC(fitModel)
print(summary(fitModel))


# --- 11 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower, 
                nstates = 11, 
                ntimes = rep(180,52))

fitModel <- fit(model)
BIC(fitModel)
print(summary(fitModel))

# --- 12 states ----- OPTIMAL N
# LogLik: 10247.78 
# BIC: -18968.49
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower, 
                nstates = 12, 
                ntimes = rep(180,52))

fitModel <- fit(model)
BIC(fitModel)
print(summary(fitModel))

# --- 13 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower, 
                nstates = 13, 
                ntimes = rep(180,52))

fitModel <- fit(model)
BIC(fitModel)
print(summary(fitModel))


# --- 14 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower, 
                nstates = 14, 
                ntimes = rep(180,52))

fitModel <- fit(model)
BIC(fitModel)
print(summary(fitModel))


# --- 15 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower, 
                nstates = 15, 
                ntimes = rep(180,52))

fitModel <- fit(model)
BIC(fitModel)
print(summary(fitModel))


# --- 16 states -----
model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower, 
                nstates = 16, 
                ntimes = rep(180,52))

fitModel <- fit(model)
BIC(fitModel)
print(summary(fitModel))













