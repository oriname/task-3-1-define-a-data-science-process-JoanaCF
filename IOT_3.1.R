#### TASK 3.1 #####
# Define a Data Science Process
# Joana
# 09/11 - 14/11
# IOT Analytics

#### General goal ####
# Develop analytics for a new set of electrical sub-metering devices, which measure power consumption and energy usage. 
# To create "smart homes" that provide home owners with analytics on their power usage.

#### Specific goal #### 
# Provide economic incentive recommendations to homeowners.
# The company and its customers need to understand: if benefit of installing sub-metering devices justifies its cost.

#### Deliverable: ####
# Written ppt report that will be delivered/ presented to their management team (business people);
# Tell them how will we conduct the analysis and what they are likely to gain

#### 0. Clean environment ####
rm(list=ls())

#### 1. Download required packages ####

install.packages("dplyr")
install.packages("tidyr")
library(dplyr)
library(tidyr)
library(chron)
#### 2. Upload dataset ####
household_power_consumption <- read.csv("~/Desktop/UBIQUM/2. Tasks/Course 3/Task 1/Data/household_power_consumption.txt", 
                                        sep=";", dec = ".", na.strings = c("NA"," ","?"), stringsAsFactors = FALSE)
View(household_power_consumption)
str(household_power_consumption)
summary(household_power_consumption)

## 2.1 Change the name of the dataset
PowerConsumption <- household_power_consumption

## 2.2 Create a duplicate dataset 
PowerConsumption_double <- household_power_consumption

#### 3. Merge date and time ####
PowerConsumption <-cbind(PowerConsumption,paste(PowerConsumption$Date,PowerConsumption$Time), stringsAsFactors=FALSE)
colnames(PowerConsumption)[10] <-"DateTime"
View(PowerConsumption)
colnames(PowerConsumption)
head(PowerConsumption)
ncol(PowerConsumption)
PowerConsumption <- PowerConsumption[,c(ncol(PowerConsumption), 1:(ncol(PowerConsumption)-1))] # change columns order - datetime goes to column 1 and disapears from colum 10. 

#### 4. Change data types - date and time ####
## 4.1 DateTime
PowerConsumption$DateTime <- strptime(PowerConsumption$DateTime, "%d/%m/%Y %H:%M:%S", tz = "Europe/Paris")
str(PowerConsumption)

## 4.2 Date
PowerConsumption$Date <- as.Date(PowerConsumption$Date, "%d/%m/%Y")
str(PowerConsumption)

## 4.3 Time
PowerConsumption$Time <- hms(PowerConsumption$Time)

#### 5. Generate new variables ####
summary(PowerConsumption)

#5.1 Global_apparent_power
PowerConsumption$Global_apparent_power=sqrt((PowerConsumption$Global_active_power^2)+(PowerConsumption$Global_reactive_power^2))
str(PowerConsumption)
PowerConsumption[1,]

#5.2 Total_Sub_metering
PowerConsumption$Global_Sub_metering <- PowerConsumption$Sub_metering_1 + PowerConsumption$Sub_metering_2 +PowerConsumption$Sub_metering_3
str(PowerConsumption)

#5.3 Hour
substr(PowerConsumption[1,]$Time, 1,2)
PowerConsumption$Hour <- substr(PowerConsumption$Time, 1,2)
str(PowerConsumption)

PowerConsumption$Hour<-as.numeric(PowerConsumption$Hour)
is.numeric(PowerConsumption$Hour)

#5.4 No_Sub_metering_Energy
PowerConsumption$No_Sub_metering_Energy<- PowerConsumption$Global_active_power*1000/60 - (PowerConsumption$Sub_metering_1 + PowerConsumption$Sub_metering_2 +PowerConsumption$Sub_metering_3)
str(PowerConsumption)

#5.5 Period of the day
summary(PowerConsumption$Hour)
PowerConsumption$Period_of_day <- ifelse(PowerConsumption$Hour <= 8,"Night", 
                                         ifelse(PowerConsumption$Hour <=  13, "Morning", 
                                                ifelse(PowerConsumption$Hour <= 20, "Afternoon", "Evening")))

str(PowerConsumption)
summary(PowerConsumption$Period_of_day)
sum(PowerConsumption$Period_of_day=="Morning")
### C: 432300
sum(PowerConsumption$Period_of_day=="Afternoon")
### C: 605436
sum(PowerConsumption$Period_of_day=="Evening")
### C: 259383
sum(PowerConsumption$Period_of_day=="Night")
### C: 778140
PowerConsumption$Period_of_day <- as.factor(PowerConsumption$Period_of_day)


#5.6 Season of the year 
PowerConsumption$Year <- substr(PowerConsumption$DateTime,1,4) 
str(PowerConsumption)
summary(PowerConsumption$Year)
PowerConsumption$Year <- as.numeric(PowerConsumption$Year)

#5.6 Month of the year
substr(PowerConsumption[1,]$DateTime,6,7) 
PowerConsumption$Month <- substr(PowerConsumption$DateTime,6,7)
str(PowerConsumption)
PowerConsumption$Month <- as.numeric(PowerConsumption$Month)

#5.7 Season of the year
PowerConsumption$Season_of_year <- ifelse(PowerConsumption$Month <=5 & PowerConsumption$Month >=3,"Spring", 
                                  ifelse(PowerConsumption$Month <=8 & PowerConsumption$Month >= 6, "Summer", 
                                  ifelse(PowerConsumption$Month <=11 & PowerConsumption$Month >=9 , "Fall", "Winter")))

str(PowerConsumption)
summary(PowerConsumption$Season_of_year)
PowerConsumption$Season_of_year <- as.factor(PowerConsumption$Season_of_year)

sum(PowerConsumption$Season_of_year=="Summer")
### C: 529920
sum(PowerConsumption$Season_of_year=="Fall")
### C: 518223
sum(PowerConsumption$Season_of_year=="Spring")
### C: 529920
sum(PowerConsumption$Season_of_year=="Winter")
### C: 497196

#5.7 Day
substr(PowerConsumption[1,]$DateTime,9,10) 
PowerConsumption$Day <- substr(PowerConsumption$DateTime,9,10)
str(PowerConsumption)
PowerConsumption$Day <- as.numeric(PowerConsumption$Day)

sum(PowerConsumption$Date =="2006-12-16")
### C: 396
sum(PowerConsumption$Date =="2006-12-17")
sum(PowerConsumption$Date =="2006-12-18")
### C: 1440 - and most of them 
sum(PowerConsumption$Date =="2006-04-28")
### C: no observations for 28, 29, 30 of april, 2006

#5.7 Period of month
PowerConsumption$Period_of_month <- ifelse(PowerConsumption$Day <=10 ,"Beggining",
                                           ifelse(PowerConsumption$Day >=20, "Ending" , "Middle"))
str(PowerConsumption)
PowerConsumption$Period_of_month <- as.factor(PowerConsumption$Period_of_month)
str(PowerConsumption)

sum(PowerConsumption$Period_of_month=="Beggining")
### C: 676800 
sum(PowerConsumption$Period_of_month=="Middle")
### C: 613836
sum(PowerConsumption$Period_of_month=="Ending")
### C: 784623

# 5.8 Day of the week
PowerConsumption$Day_of_week <- wday(PowerConsumption$Date, label = FALSE, week_start = 1)

PowerConsumption$Type_of_day <- ifelse(PowerConsumption$Day_of_week <=5, "Working_day", "Weekend")
str(PowerConsumption)

PowerConsumption$Day_of_week <- as.factor(PowerConsumption$Day_of_week)
PowerConsumption$Type_of_day <- as.factor(PowerConsumption$Type_of_day)

#### 6. Treat missing values ####
# The dataset contains some missing values in the measurements (nearly 1,25% of the rows). All calendar timestamps are present in the dataset but for some timestamps, the measurement values are missing: a missing value is represented by the absence of value between two consecutive semi-colon attribute separators. For instance, the dataset shows missing values on April 28, 2007.
is.na(PowerConsumption)
sum(is.na(PowerConsumption))
### P: 25979 per variable / 260030
which(is.na(PowerConsumption))
### P: Starting in 141637


#### 7. Deal with timezone ####
# France
#PowerConsumption$DateTime = as.POSIXct(PowerConsumption$DateTime,tz="Europe/Paris")
#summary(PowerConsumption)
#str(PowerConsumption)

#PowerConsumption$DateTime = as.POSIXlt(PowerConsumption$DateTime,tz="Europe/Paris")
#summary(PowerConsumption)
#str(PowerConsumption)
#PowerConsumption[1,]

#Sys.timezone(location = TRUE)

#install.packages("lubridate")
library(lubridate)
#force_tz(PowerConsumption, "Europe/Paris")
### E: Nothing happened

#PowerConsumption$TimeZone <- ymd_hms(PowerConsumption$DateTime, tz = "Europe/Paris")
#str(PowerConsumption)


#### 8. Exploration ####

#plot(PowerConsumption$Global_active_power)
#View(PowerConsumption)

