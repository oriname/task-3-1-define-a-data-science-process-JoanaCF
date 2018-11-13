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
library(lubridate)
PowerConsumption$DateTime <- strptime(PowerConsumption$DateTime, "%d/%m/%Y %H:%M:%S")
str(PowerConsumption)

## 4.2 Date
PowerConsumption$Date <- as.Date(PowerConsumption$Date, "%d/%m/%Y")
str(PowerConsumption)

## 4.3 Time
PowerConsumption$Time <- hms(PowerConsumption$Time)

#### 5. Generate new variables ####
summary(PowerConsumption)

#5.1 Global_apparent_power
#PowerConsumption$Global_apparent_power=sqrt((PowerConsumption$Global_active_power^2)+(PowerConsumption$Global_reactive_power^2))
#str(PowerConsumption)
#PowerConsumption[1,]

# 5.1 Global_power
PowerConsumption$Global_power=(PowerConsumption$Global_active_power+ PowerConsumption$Global_reactive_power)
str(PowerConsumption)

#5.1.1 Global power in watt hour 
PowerConsumption$Global_power_wh = PowerConsumption$Global_power*1000/60

#5.1.2 Global active power in watt hour 
PowerConsumption$Global_active_power_wh = PowerConsumption$Global_active_power*1000/60

#5.1.3 Global reactive power in watt hour
PowerConsumption$Global_reactive_power_wh = PowerConsumption$Global_reactive_power*1000/60

#5.2 Total_Sub_metering
PowerConsumption$Global_Sub_metering <- PowerConsumption$Sub_metering_1 + PowerConsumption$Sub_metering_2 +PowerConsumption$Sub_metering_3
str(PowerConsumption)

#5.3 Hour
substr(PowerConsumption[1,]$Time, 1,2)
PowerConsumption$Hour <- substr(PowerConsumption$Time, 1,2)
str(PowerConsumption$Hour)

PowerConsumption$Hour<-as.numeric(PowerConsumption$Hour)
is.numeric(PowerConsumption$Hour)
str(PowerConsumption)

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
str(PowerConsumption$DateTime)

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

library(dplyr)                   
library(lubridate) 

select(PowerConsumption, PowerConsumption$Sub_metering_1 == "NA") 
#### E: Column `Time` classes Period and Interval from lubridate are currently not supported.
str(PowerConsumption)
summary(PowerConsumption)
glimpse(PowerConsumption)

PowerConsumption %>% filter(is.na(Sub_metering_1))
#### E: Column `Time` classes Period and Interval from lubridate are currently not supported.

str(PowerConsumption_double)
summary(PowerConsumption_double)
PowerConsumption_double %>% filter(is.na(Sub_metering_1))
### C: 4 NA in Dec 16, day 21 and 30 / 2006 - 2 min // 2 min in jan/2007, day 14 and 28 // 2 min in fev/ 2007, day 22 // 1 min in march/2007, day 25 // day 28/ 04 from 00:21...

PowerConsumption_double %>% filter(is.na(Sub_metering_2))
PowerConsumption_double %>% filter(is.na(Sub_metering_3))
PowerConsumption_double %>% filter(is.na(Global_active_power))
PowerConsumption_double %>% filter(is.na(Voltage))
### C: same NAs

#### 7. Deal with timezone ####
tz(PowerConsumption_double)
tz(PowerConsumption)
### C: UTC 

#France
#PowerConsumption$DateTime = as.POSIXct(PowerConsumption$DateTime,tz="Europe/Paris")
#PowerConsumption$TimeZone <- ymd_hms(PowerConsumption$DateTime, tz = "Europe/Paris")

#### 8. Exploration ####
str(PowerConsumption)
head(PowerConsumption)

library(scatterplot3d)
library(ggplot2)
scatterplot3d(PowerConsumption_byday$Global_active_power_wh, PowerConsumption_byday$Global_reactive_power_wh)
qplot(PowerConsumption_byday$Global_active_power_wh, PowerConsumption_byday$Global_reactive_power_wh, col="orange")
qplot(PowerConsumption_byday$Global_Sub_meter, PowerConsumption_byday$Global_active_power_wh, col="orange")
str(PowerConsumption_byday)
str(PowerConsumption)

##8.1 Create a subsample of the dataset - consumption_by_day
PowerConsumption_byday <- PowerConsumption %>%
  group_by(Date) %>%
  summarise(Global_power_wh = sum(Global_power_wh), Global_reactive_power_wh=sum(Global_reactive_power), Global_active_power_wh=sum(Global_active_power), Global_Sub_meter=sum(Global_Sub_metering), Sub_metering_1=sum(Sub_metering_1), Sub_metering_2=sum(Sub_metering_2), Sub_metering_3=sum(Sub_metering_3))
summary(PowerConsumption_byday)
#? summarise

#8.1.1 Plot subsample
qplot(PowerConsumption_byday)
qplot(PowerConsumption_byday$Date, PowerConsumption_byday$Global_Sub_meter)
qplot(PowerConsumption_byday$Date, PowerConsumption_byday$Global_power_wh)
qplot(PowerConsumption_byday$Date, PowerConsumption_byday$Global_active_power_wh)
qplot(PowerConsumption_byday$Date, PowerConsumption_byday$Global_reactive_power_wh)

qplot(PowerConsumption_byday$Date, PowerConsumption_byday$Sub_metering_1)
qplot(PowerConsumption_byday$Date, PowerConsumption_byday$Sub_metering_2)
qplot(PowerConsumption_byday$Date, PowerConsumption_byday$Sub_metering_3)

##8.1 Create a subsample of the dataset - consumption_by_type_of_day
PowerConsumption %>%
  group_by(Month) %>%
  summarise(Global_power_wh = sum(Global_power_wh))
summary(PowerConsumption_Month)

str(PowerConsumption)


# PowerConsumption_double_2007 <- PowerConsumption_double %>% filter(Date <= as.Date("31/12/2007"))
## Continuar...


#plot(PowerConsumption$Global_active_power)
#View(PowerConsumption)

# install.packages("makeR")
# library(makeR)
# calendarHeat(PowerConsumption$Global_power, ncolors = 9, color = "r2b", date.form = "%Y-%m-%d")
