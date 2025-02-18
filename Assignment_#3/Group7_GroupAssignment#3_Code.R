

install.packages("depmixS4")
library(depmixS4)



# import dataset
dataset <- read.csv("./Group_Assignment_Dataset.txt", header = TRUE)


#convert to date format
dataset$Date <- as.Date(dataset$Date, format="%d/%m/%Y")

#Extract tuesdays from 2-8 am data
tuesdaysData <- subset(data, format(DateTime, "%A") == "Tuesday" & 
                      format(DateTime, "%H") >= "02" & format(DateTime, "%H") < "08")


#Extract the global active power on tuesdays from 2-8 am 
tuesdayGlobalActivePower <- tuesdaysData$Global_active_power


#Start in steps of 2 states
state_range <- seq(4, 16, by = 2)

# Store the log likelihood and bic values in vectors
log_likelihoods <- numeric(7)
bic_values <- numeric(7)



# --- 4 states -----
model <- depmix(response = Global_Active_Power ~ 1
                , data = tuesdayGlobalActivePower, 
                nstates = 4, 
                ntimes = 52)

fitModel <- fit(model)
summary(fitModel)


# --- 6 states -----





# --- 8 states -----







# --- 10 states -----






# --- 12 states -----





# --- 14 states -----









# --- 16 states -----


















