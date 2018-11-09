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

#### 4. Change data types - date and time
PowerConsumption$DateTime <- strptime(PowerConsumption$DateTime, "%d/%m/%Y %H:%M:%S")
str(PowerConsumption)
PowerConsumption$Date <- as.Date(PowerConsumption$Date, "%d/%m/%Y")
str(PowerConsumption)

substr(PowerConsumption[1,]$Time,1,2)

#### 5. Generate new variables ####

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

#5.5 Period of the day

#5.6 Season of the year


#### 6. Treat missing values ####
# The dataset contains some missing values in the measurements (nearly 1,25% of the rows). All calendar timestamps are present in the dataset but for some timestamps, the measurement values are missing: a missing value is represented by the absence of value between two consecutive semi-colon attribute separators. For instance, the dataset shows missing values on April 28, 2007.

#### 7. Deal with timezone ####
# France

