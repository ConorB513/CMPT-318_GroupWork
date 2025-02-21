

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

tuesdayGlobalActivePower <- tuesdaysData$Global_active_power

#Extract the global active power on Tuesdays from 2-5 am 
tuesdayGlobalActivePower <- data.frame(Global_active_power = tuesdayGlobalActivePower)



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



# --- 12 states -----

model <- depmix(response = Global_active_power ~ 1
                , data = tuesdayGlobalActivePower, 
                nstates = 12, 
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













