library(tidyr)
library(dplyr)
library(lubridate)

entered_time <- 77               # ENTER MINUTES AFTER 6 p.m.
entered_student <- "Trystan"        # ENTER STUDENT NAME

times <- read.csv("C:/Users/shane/OneDrive/Desktop/StudentArrivalTime/Math 171 and 172 Sign In - Form Responses.csv")

split_data <- do.call(rbind, lapply(strsplit(as.character(times$Timestamp), " "), function(x) x[1:2])) #splits the Timestamp column
times$Date <- split_data[, 1] #adds back the date column
times$Time <- split_data[, 2] #adds back the time column

times <- times %>% select(Time, Name)

stutimes <- times[tolower(times$Name) %in% tolower(entered_student) | 
                    startsWith(tolower(times$Name), tolower(entered_student)), ] # Makes a seperate df for each student's times
stutimes <- stutimes$Time

split_time <- strsplit(stutimes, ":")
# Convert to numeric and calculate: (hours * 3600) + (minutes * 60) + seconds
numeric_seconds <- sapply(split_time, function(x) {
  x <- as.numeric(x)
  x[1] * 3600 + x[2] * 60 + x[3]
})

minutes_after_6 <- (numeric_seconds - (16*60*60) )/60

t <- (entered_time - mean(minutes_after_6)) / sd(minutes_after_6)

pt(t, length(minutes_after_6)-1) # Probability that student will sign in before entered time, given that they will sign in today
1-pt(t, length(minutes_after_6)-1) # Probability that student will sign in after entered time, given that they will sign in today
hist(minutes_after_6)
length(minutes_after_6)
