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
