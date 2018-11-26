############################################ TASK 3.1 ############################################
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

#install.packages("dplyr")
#install.packages("tidyr")
library(dplyr)
library(tidyr)
library(chron)

#### 2. Upload dataset ####
household_power_consumption <- read.csv("~/Desktop/UBIQUM/2. Tasks/Course 3/Task 1/Data/household_power_consumption.txt", 
                                        sep=";", dec = ".", na.strings = c("NA"," ","?"), stringsAsFactors = FALSE)
#View(household_power_consumption)
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
PowerConsumption$DateTime <- dmy_hms(PowerConsumption$DateTime)
str(PowerConsumption)

## 4.2 Date
PowerConsumption$Date <- dmy(PowerConsumption$Date)

## 4.3 Time
#PowerConsumption$Time <- hms(PowerConsumption$Time)
# Don't do anything with time

#### 5. Generate new variables ####


#5.1 Global_apparent_power
#PowerConsumption$Global_apparent_power=sqrt((PowerConsumption$Global_active_power^2)+(PowerConsumption$Global_reactive_power^2))
#str(PowerConsumption)
#PowerConsumption[1,]

# 5.1 Global_power
PowerConsumption$Global_power=(PowerConsumption$Global_active_power+ PowerConsumption$Global_reactive_power)

#5.1.1 Global power in watt hour 
PowerConsumption$Global_power_wh = PowerConsumption$Global_power*1000/60

#5.1.2 Global active power in watt hour 
PowerConsumption$Global_active_power_wh = PowerConsumption$Global_active_power*1000/60

#5.1.3 Global reactive power in watt hour
PowerConsumption$Global_reactive_power_wh = PowerConsumption$Global_reactive_power*1000/60

#5.2 Total_Sub_metering
PowerConsumption$Global_Sub_metering <- PowerConsumption$Sub_metering_1 + PowerConsumption$Sub_metering_2 +PowerConsumption$Sub_metering_3


#5.3 Hour
#substr(PowerConsumption[1,]$Time, 1,2)
#summary(PowerConsumption$Time)
#PowerConsumption$Hour <- substr(PowerConsumption$Time, 1,2)
#str(PowerConsumption$Hour)
#summary(PowerConsumption$Hour)

#PowerConsumption$Hour<-as.numeric(PowerConsumption$Hour)
#is.numeric(PowerConsumption$Hour)
#str(PowerConsumption)

summary(PowerConsumption$DateTime)
PowerConsumption$Hour <- substr(PowerConsumption$DateTime, 12,13)
PowerConsumption$Hour<-as.numeric(PowerConsumption$Hour)
summary(PowerConsumption$Hour)

#5.4 No_Sub_metering_Energy
PowerConsumption$No_Sub_metering_Energy<- PowerConsumption$Global_active_power*1000/60 - (PowerConsumption$Sub_metering_1 + PowerConsumption$Sub_metering_2 +PowerConsumption$Sub_metering_3)



#5.5 Period of the day
summary(PowerConsumption$Hour)
PowerConsumption$Period_of_day <- ifelse(PowerConsumption$Hour <= 8,"4-Night", 
                                         ifelse(PowerConsumption$Hour <=  13, "1-Morning", 
                                                ifelse(PowerConsumption$Hour <= 20, "2-Afternoon", "3-Evening")))

#PowerConsumption$Period_of_day2 <-  ifelse(PowerConsumption$Hour <= 8,"Night", "rest of day")
#summary(PowerConsumption$Period_of_day2)

PowerConsumption$Period_of_day <- as.factor(PowerConsumption$Period_of_day)


summary(PowerConsumption$Period_of_day)
sum(PowerConsumption$Period_of_day=="1-Morning")
### C: 432300
sum(PowerConsumption$Period_of_day=="2-Afternoon")
### C: 605436
sum(PowerConsumption$Period_of_day=="3-Evening")
### C: 259383
sum(PowerConsumption$Period_of_day=="4-Night")
### C: 778140

#5.6 Season of the year 

PowerConsumption$Year <- substr(PowerConsumption$DateTime,1,4) 
PowerConsumption$Year <- as.numeric(PowerConsumption$Year)

#5.6 Month of the year
substr(PowerConsumption[1,]$DateTime,6,7) 
PowerConsumption$Month <- substr(PowerConsumption$DateTime,6,7)
str(PowerConsumption)
PowerConsumption$Month <- as.numeric(PowerConsumption$Month)

#5.7 Season of the year
PowerConsumption$Season_of_year <- ifelse(PowerConsumption$Month <=5 & PowerConsumption$Month >=3,"1-Spring", 
                                  ifelse(PowerConsumption$Month <=8 & PowerConsumption$Month >= 6, "2-Summer", 
                                  ifelse(PowerConsumption$Month <=11 & PowerConsumption$Month >=9 , "3-Fall", "4-Winter")))

str(PowerConsumption)
summary(PowerConsumption$Season_of_year)
PowerConsumption$Season_of_year <- as.factor(PowerConsumption$Season_of_year)

sum(PowerConsumption$Season_of_year=="2-Summer")
### C: 529920
sum(PowerConsumption$Season_of_year=="3-Fall")
### C: 518223
sum(PowerConsumption$Season_of_year=="1-Spring")
### C: 529920
sum(PowerConsumption$Season_of_year=="4-Winter")
### C: 497196

#5.7 Day
substr(PowerConsumption[1,]$DateTime,9,10) 
PowerConsumption$Day <- substr(PowerConsumption$DateTime,9,10)
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

PowerConsumption$Period_of_month <- as.factor(PowerConsumption$Period_of_month)


sum(PowerConsumption$Period_of_month=="Beggining")
### C: 676800 
sum(PowerConsumption$Period_of_month=="Middle")
### C: 613836
sum(PowerConsumption$Period_of_month=="Ending")
### C: 784623

# 5.8 Day of the week
PowerConsumption$Day_of_week <- wday(PowerConsumption$Date, label = TRUE, week_start = 1)


PowerConsumption$Type_of_day <- ifelse(PowerConsumption$Day_of_week == "Sun" & PowerConsumption$Day_of_week=="Sat", "Working_day", "Weekend")


PowerConsumption$Day_of_week <- as.factor(PowerConsumption$Day_of_week)
PowerConsumption$Type_of_day <- as.factor(PowerConsumption$Type_of_day)



#### 6. Treat missing values ####
# The dataset contains some missing values in the measurements (nearly 1,25% of the rows). All calendar timestamps are present in the dataset but for some timestamps, the measurement values are missing: a missing value is represented by the absence of value between two consecutive semi-colon attribute separators. For instance, the dataset shows missing values on April 28, 2007.
is.na(PowerConsumption)
#sum(is.na(PowerConsumption))
### P: 25979 per variable / 1923067
#which(is.na(PowerConsumption))
### P: Starting in 141637


## 6.1 Dataset without missing values // clean
PowerConsumption$DateTime <- as.POSIXct(PowerConsumption$DateTime)
PowerConsumption_c <- PowerConsumption[complete.cases(PowerConsumption),]


library(dplyr)                   
library(lubridate) 

#select(PowerConsumption, PowerConsumption$Sub_metering_1 == "NA") 
#### E: Column `Time` classes Period and Interval from lubridate are currently not supported.
#str(PowerConsumption)
#summary(PowerConsumption)
#glimpse(PowerConsumption)

#PowerConsumption %>% filter(is.na(Sub_metering_1))
#### E: Column `Time` classes Period and Interval from lubridate are currently not supported.

#str(PowerConsumption_double)
#summary(PowerConsumption_double)
#PowerConsumption_double %>% filter(is.na(Sub_metering_1))
### C: 4 NA in Dec 16, day 21 and 30 / 2006 - 2 min // 2 min in jan/2007, day 14 and 28 // 2 min in fev/ 2007, day 22 // 1 min in march/2007, day 25 // day 28/ 04 from 00:21...

#PowerConsumption_double %>% filter(is.na(Sub_metering_2))
#PowerConsumption_double %>% filter(is.na(Sub_metering_3))
#PowerConsumption_double %>% filter(is.na(Global_active_power))
#PowerConsumption_double %>% filter(is.na(Voltage))
### C: same NAs

#### 7. Deal with timezone ####
tz(PowerConsumption_double)
tz(PowerConsumption)
### C: UTC 

#France
#PowerConsumption$DateTime = as.POSIXct(PowerConsumption$DateTime,tz="Europe/Paris")
#PowerConsumption$TimeZone <- ymd_hms(PowerConsumption$DateTime, tz = "Europe/Paris")

#### 8. Exploration by day - Dataset by day ####
str(PowerConsumption)
head(PowerConsumption)

library(scatterplot3d)
library(ggplot2)
PowerConsumption_byday <- PowerConsumption_c %>%
  group_by(Date) %>%
  summarise(Global_power_wh = sum(Global_power_wh), Global_reactive_power_wh=sum(Global_reactive_power), Global_active_power_wh=sum(Global_active_power), Global_Sub_meter=sum(Global_Sub_metering), Sub_metering_1=sum(Sub_metering_1), Sub_metering_2=sum(Sub_metering_2), Sub_metering_3=sum(Sub_metering_3))
summary(PowerConsumption_byday)

#scatterplot3d(PowerConsumption_byday$Global_active_power_wh, PowerConsumption_byday$Global_reactive_power_wh)
#qplot(PowerConsumption_byday$Global_active_power_wh, PowerConsumption_byday$Global_reactive_power_wh, col="orange")
#qplot(PowerConsumption_byday$Date, PowerConsumption_byday$Global_active_power_wh, col="orange")
#qplot(PowerConsumption_byday$Date, PowerConsumption_byday$Global_power_wh, color="orange")


##8.1 Create a subsample of the dataset - consumption_by_day
PowerConsumption$DateTime <- as.POSIXct(PowerConsumption$DateTime)
PowerConsumption_byday <- PowerConsumption %>%
  group_by(Date) %>%
  summarise(Global_power_wh = sum(Global_power_wh), Global_reactive_power_wh=sum(Global_reactive_power), Global_active_power_wh=sum(Global_active_power), Global_Sub_meter=sum(Global_Sub_metering), Sub_metering_1=sum(Sub_metering_1), Sub_metering_2=sum(Sub_metering_2), Sub_metering_3=sum(Sub_metering_3))
summary(PowerConsumption_byday)
#? summarise

#8.1.1 Plot subsample 
#qplot(PowerConsumption_byday)
#qplot(PowerConsumption_byday$Date, PowerConsumption_byday$Global_Sub_meter)
#qplot(PowerConsumption_byday$Date, PowerConsumption_byday$Global_power_wh)
#qplot(PowerConsumption_byday$Date, PowerConsumption_byday$Global_active_power_wh)
#qplot(PowerConsumption_byday$Date, PowerConsumption_byday$Global_reactive_power_wh)

#qplot(PowerConsumption_byday$Date, PowerConsumption_byday$Sub_metering_1)
#qplot(PowerConsumption_byday$Date, PowerConsumption_byday$Sub_metering_2)
#qplot(PowerConsumption_byday$Date, PowerConsumption_byday$Sub_metering_3)


#### 9. Generic Exploration #### 

#hist(PowerConsumption$Global_active_power)
#hist(PowerConsumption$Global_reactive_power)
#hist(PowerConsumption$Global_power)

#hist(PowerConsumption$Voltage)
#hist(PowerConsumption$Global_intensity)
#hist(PowerConsumption$Sub_metering_1)
#summary(PowerConsumption$Sub_metering_1)
####C: Max very different from mean: 88 - 3 times
#which(PowerConsumption$Sub_metering_1==40)
### C: more than 200 around 40
#hist(PowerConsumption$Sub_metering_2)
#summary(PowerConsumption$Sub_metering_2)
####C: Max very different from: 80
#hist(PowerConsumption$Sub_metering_3)
#summary(PowerConsumption$Sub_metering_3)
####C: Max is lower: 31 and mean is higher: 6.45

#hist(PowerConsumption_byday$Sub_metering_2)
#hist(PowerConsumption_byday$Sub_metering_1)
#hist(PowerConsumption_byday$Sub_metering_3)

#### 9.A Create a subsample of the dataset - consumption_by_type_of_day ####
#summary(PowerConsumption_c)

#PowerConsumption_Type_of_day <- PowerConsumption_c %>%
 # group_by(Type_of_day) %>%
 # summarise(Global_power_wh = mean(Global_power_wh), Global_reactive_power_wh=mean(Global_reactive_power), Global_active_power_wh=mean(Global_active_power), Global_Sub_meter=mean(Global_Sub_metering), Sub_metering_1=mean(Sub_metering_1), Sub_metering_2=mean(Sub_metering_2), Sub_metering_3=mean(Sub_metering_3))
#summary(PowerConsumption_Type_of_day)
#qplot(PowerConsumption_Type_of_day$Global_power_wh, PowerConsumption_Type_of_day$Global_active_power_wh)

#### 9.B Create a subsample of the dataset - By hour ####
PowerConsumption_Hour <- PowerConsumption_c %>%
  group_by(Hour, Date) %>%
  summarise(Global_power_wh = sum(Global_power_wh), Global_reactive_power_wh=sum(Global_reactive_power), Global_active_power_wh=sum(Global_active_power), Global_Sub_meter=sum(Global_Sub_metering), Sub_metering_1=sum(Sub_metering_1), Sub_metering_2=sum(Sub_metering_2), Sub_metering_3=sum(Sub_metering_3))
summary(PowerConsumption_Hour)

#qplot(PowerConsumption_Hour$Hour, PowerConsumption_Hour$Global_power_wh)
#qplot(PowerConsumption_Hour$Hour, PowerConsumption_Hour$Sub_metering_1, col="orange")
#qplot(PowerConsumption_Hour$Hour, PowerConsumption_Hour$Sub_metering_2, col="orange")
#qplot(PowerConsumption_Hour$Hour, PowerConsumption_Hour$Sub_metering_3, col="orange")

#ggplot(PowerConsumption_Hour, aes(x=Hour, y=Sub_metering_3))+geom_bar(stat = "identity")+scale_fill_brewer()
#ggplot(PowerConsumption_Hour, aes(x=Hour, y=Sub_metering_2))+geom_bar(stat = "identity")+scale_fill_brewer()
#ggplot(PowerConsumption_Hour, aes(x=Hour, y=Sub_metering_1))+geom_bar(stat = "identity")+scale_fill_brewer(palette="Set2")
#ggplot(PowerConsumption_Hour, aes(x=Hour, y=Global_active_power_wh, colours="orange"))+geom_bar(stat = "identity")
#ggplot(PowerConsumption_Hour, aes(x=Hour, y=Sub_metering_2, colours="orange"))+geom_bar(stat = "identity")
#ggplot(PowerConsumption_Hour, aes(x=Hour, y=Sub_metering_3, colours="orange"))+geom_bar(stat = "identity")
#ggplot(PowerConsumption_Hour, aes(x=Hour, y=Sub_metering_1, colours="orange"))+geom_bar(stat = "identity")


#### 9.1 Create a subsample of the dataset - Month ####
PowerConsumption_Month <- PowerConsumption_c %>%
  group_by(Month,Year) %>%
  summarise(Global_power_wh = sum(Global_power_wh), Global_reactive_power_wh=sum(Global_reactive_power), Global_active_power_wh=sum(Global_active_power), Global_Sub_meter=sum(Global_Sub_metering), Sub_metering_1=sum(Sub_metering_1), Sub_metering_2=sum(Sub_metering_2), Sub_metering_3=sum(Sub_metering_3))
summary(PowerConsumption_Month)
#qplot(PowerConsumption_Month$Month, PowerConsumption_Month$Global_power_wh, color="orange")
#qplot(PowerConsumption_Month$Month, PowerConsumption_Month$Global_active_power_wh)
#qplot(PowerConsumption_Month$Month, PowerConsumption_Month$Global_reactive_power_wh, col="orange")
#qplot(PowerConsumption_Month$Month, PowerConsumption_Month$Global_Sub_meter, col="purple")
#qplot(PowerConsumption_Month$Month, PowerConsumption_Month$Sub_metering_1, col="purple")
#qplot(PowerConsumption_Month$Month, PowerConsumption_Month$Sub_metering_2, col="purple")
#qplot(PowerConsumption_Month$Month, PowerConsumption_Month$Sub_metering_3, col="purple")

#ggplot(PowerConsumption_Month, aes(x=Month, y=Sub_metering_3))+geom_bar(stat = "identity")+facet_wrap(~ Year)
PowerConsumption_Month$Month <- as.factor(PowerConsumption_Month$Month)

#ggplot(PowerConsumption_Month, aes(x=Month, y=Global_active_power_wh, fill=Month))+geom_bar(stat = "identity")+scale_fill_brewer(palette="Paired")+facet_wrap(~ Year)
#ggplot(PowerConsumption_Month, aes(x=Month, y=Sub_metering_3, fill=Month))+geom_bar(stat = "identity")+scale_fill_brewer(palette="Set3")+facet_wrap(~ Year)
#ggplot(PowerConsumption_Month, aes(x=Month, y=Sub_metering_1, fill=Month))+geom_bar(stat = "identity")+scale_fill_brewer(palette="Set3")+facet_wrap(~ Year)
#ggplot(PowerConsumption_Month, aes(x=Month, y=Global_power_wh, fill=Month))+geom_bar(stat = "identity")+scale_fill_brewer(palette="Spectral")+facet_wrap(~ Year)


#### 9.2 Create a subsample of the dataset - Year ####
PowerConsumption_Year <- PowerConsumption_c %>%
  group_by(Year) %>%
  summarise(Global_power_wh = sum(Global_power_wh), Global_reactive_power_wh=sum(Global_reactive_power), Global_active_power_wh=sum(Global_active_power), Global_Sub_meter=sum(Global_Sub_metering), Sub_metering_1=sum(Sub_metering_1), Sub_metering_2=sum(Sub_metering_2), Sub_metering_3=sum(Sub_metering_3))
summary(PowerConsumption_Year)

#qplot(PowerConsumption_Year$Year, PowerConsumption_Year$Global_power_wh, color="orange")
#qplot(PowerConsumption_Year$Year, PowerConsumption_Year$Global_active_power_wh, col="orange")
### C: decreasing trajectory
#qplot(PowerConsumption_Year$Year, PowerConsumption_Year$Global_reactive_power_wh, col="orange")
### C: on 2009 there is a pick

#qplot(PowerConsumption_Year$Year, PowerConsumption_Year$Global_Sub_meter, col="yellow")
### C:pick on 2007 and 2009
#qplot(PowerConsumption_Year$Year, PowerConsumption_Year$Sub_metering_1, col="yellow")
### C:pick on 2007 and 2009
#qplot(PowerConsumption_Year$Year, PowerConsumption_Year$Sub_metering_2, col="yellow")
### C:descending
#qplot(PowerConsumption_Year$Year, PowerConsumption_Year$Sub_metering_3, col="yellow")
### C:not descending, but pick in 2009
#str(PowerConsumption)

PowerConsumption_Year$Year <- as.factor(PowerConsumption_Year$Year)
#ggplot(PowerConsumption_Year, aes(x=Year, y=Global_power_wh, fill=Year))+geom_bar(stat = "identity")
#ggplot(PowerConsumption_Year, aes(x=Year, y=Global_active_power_wh, fill=Year))+geom_bar(stat = "identity")+ scale_fill_brewer(palette="Set2") 
#ggplot(PowerConsumption_Year, aes(x=Year, y=Sub_metering_1, fill=Year))+geom_bar(stat = "identity")
#ggplot(PowerConsumption_Year, aes(x=Year, y=Sub_metering_2, fill=Year))+geom_bar(stat = "identity")
#ggplot(PowerConsumption_Year, aes(x=Year, y=Sub_metering_3, fill=Year))+geom_bar(stat = "identity")

#### 9.3 Create a subsample of the dataset - Period of day ####
PowerConsumption_Period_of_day<- PowerConsumption_c %>%
  group_by(Period_of_day) %>%
  summarise(Global_power_wh = mean(Global_power_wh), Global_reactive_power_wh=mean(Global_reactive_power), Global_active_power_wh=mean(Global_active_power), Global_Sub_meter=mean(Global_Sub_metering), Sub_metering_1=mean(Sub_metering_1), Sub_metering_2=mean(Sub_metering_2), Sub_metering_3=mean(Sub_metering_3))
summary(PowerConsumption_Period_of_day)

#qplot(PowerConsumption_Period_of_day$Period_of_day, PowerConsumption_Period_of_day$Global_power_wh, col="yellow")
#ggplot(PowerConsumption_Period_of_day, aes(x=Period_of_day, y=Global_power_wh, fill=Period_of_day))+geom_bar(stat = "identity")

#### 9.4 Create a subsample of the dataset - Season of year ####
PowerConsumption_Season_year<- PowerConsumption_c %>%
  group_by(Season_of_year) %>%
  summarise(Global_power_wh = sum(Global_power_wh), Global_reactive_power_wh=sum(Global_reactive_power), Global_active_power_wh=sum(Global_active_power), Global_Sub_meter=sum(Global_Sub_metering), Sub_metering_1=sum(Sub_metering_1), Sub_metering_2=sum(Sub_metering_2), Sub_metering_3=sum(Sub_metering_3))
summary(PowerConsumption_Season_year)

#qplot(PowerConsumption_Season_year$Season_of_year, PowerConsumption_Season_year$Global_power_wh, col="yellow")
#qplot(PowerConsumption_Season_year$Season_of_year, PowerConsumption_Season_year$Global_active_power_wh, col="yellow")
#qplot(PowerConsumption_Season_year$Season_of_year, PowerConsumption_Season_year$Global_reactive_power_wh, col="yellow")

#qplot(PowerConsumption_Season_year$Season_of_year, PowerConsumption_Season_year$Global_Sub_meter, col="yellow")
#qplot(PowerConsumption_Season_year$Season_of_year, PowerConsumption_Season_year$Sub_metering_1, col="yellow")
#qplot(PowerConsumption_Season_year$Season_of_year, PowerConsumption_Season_year$Sub_metering_2, col="yellow")
#qplot(PowerConsumption_Season_year$Season_of_year, PowerConsumption_Season_year$Sub_metering_3, col="yellow")
### C: everything very low in the summer, except for reactive energy

#ggplot(PowerConsumption_Season_year, aes(x=Season_of_year, y=Global_active_power_wh, fill=Season_of_year))+geom_bar(stat = "identity")+ scale_fill_brewer(palette="Set2") 
#ggplot(PowerConsumption_Season_year, aes(x=Season_of_year, y=Sub_metering_1, fill=Season_of_year))+geom_bar(stat = "identity")
#ggplot(PowerConsumption_Season_year, aes(x=Season_of_year, y=Sub_metering_2, fill=Season_of_year))+geom_bar(stat = "identity")
#ggplot(PowerConsumption_Season_year, aes(x=Season_of_year, y=Sub_metering_3, fill=Season_of_year))+geom_bar(stat = "identity")


#### 9.5 Create a subsample of the dataset - Day of the week ####
PowerConsumption_Day_of_week<- PowerConsumption_c %>%
  group_by(Day_of_week) %>%
  summarise(Global_power_wh = sum(Global_power_wh), Global_reactive_power_wh=sum(Global_reactive_power), Global_active_power_wh=sum(Global_active_power), Global_Sub_meter=sum(Global_Sub_metering), Sub_metering_1=sum(Sub_metering_1), Sub_metering_2=sum(Sub_metering_2), Sub_metering_3=sum(Sub_metering_3))
summary(PowerConsumption_Day_of_week)

#qplot(PowerConsumption_Day_of_week$Day_of_week, PowerConsumption_Day_of_week$Global_power_wh, col="yellow")
#qplot(PowerConsumption_Day_of_week$Day_of_week, PowerConsumption_Day_of_week$Global_active_power_wh, col="yellow")
#qplot(PowerConsumption_Day_of_week$Day_of_week, PowerConsumption_Day_of_week$Global_reactive_power_wh, col="yellow")

#qplot(PowerConsumption_Day_of_week$Day_of_week, PowerConsumption_Day_of_week$Global_Sub_meter, col="yellow")
#qplot(PowerConsumption_Day_of_week$Day_of_week, PowerConsumption_Day_of_week$Sub_metering_1, col="yellow")
#qplot(PowerConsumption_Day_of_week$Day_of_week, PowerConsumption_Day_of_week$Sub_metering_2, col="yellow")
### C: something is going on
#qplot(PowerConsumption_Day_of_week$Day_of_week, PowerConsumption_Day_of_week$Sub_metering_3, col="yellow")
### C: something is going on

#ggplot(PowerConsumption_Day_of_week, aes(x=Day_of_week, y=Global_power_wh, fill=Day_of_week))+geom_bar(stat = "identity")+ scale_fill_brewer(palette="Set2") 
#ggplot(PowerConsumption_Day_of_week, aes(x=Day_of_week, y=Sub_metering_1, fill=Day_of_week))+geom_bar(stat = "identity")+ scale_fill_brewer(palette="Set3") 
#ggplot(PowerConsumption_Day_of_week, aes(x=Day_of_week, y=Sub_metering_2, fill=Day_of_week))+geom_bar(stat = "identity")+ scale_fill_brewer(palette="Set2") 
#ggplot(PowerConsumption_Day_of_week, aes(x=Day_of_week, y=Sub_metering_3, fill=Day_of_week))+geom_bar(stat = "identity")+ scale_fill_brewer(palette="Set2") 
#ggplot(PowerConsumption_Day_of_week, aes(x=Day_of_week, y=Global_active_power_wh, fill=Day_of_week))+geom_bar(stat = "identity")+ scale_fill_brewer(palette="Set3") 



############################################ TASK 3.2 - FORECASTING ############################################
#### 1. Try filtering data by date - test #### 

#PowerConsumption_test <- PowerConsumption_double
#PowerConsumption_test$Date <- dmy(PowerConsumption_test$Date)
#str(PowerConsumption_test)
#PowerConsumption_test %>% filter(Date > "2009-12-16")

#### 2. Try filtering data by date - Deleted Time variable #### 
#PowerConsumption_c %>% filter(Date > dmy("2009-12-16"))
### E: All formats failed to parse. No formats found. 

#PowerConsumption_c$Time <- NULL
#str(PowerConsumption_c)
#PowerConsumption_c %>% filter(Date > ymd("2009-12-16"))

#head(PowerConsumption_c)

#### 3. Missing values
#### 3.1 Missing Values Descriptive ######## 

PowerConsumption %>% filter(is.na(Global_active_power))
PowerConsumption_NA <- PowerConsumption %>% filter(is.na(Global_active_power))
#View(PowerConsumption_NA)
#summary(PowerConsumption_NA)

#plot(PowerConsumption_NA$Date)
#plot(PowerConsumption_NA$DateTime)
### H: interpret the plot

PowerConsumption_NA$Time <- NULL

count(PowerConsumption_NA%>% filter(Year==2007))
count(PowerConsumption_NA%>% filter(Year==2006))
count(PowerConsumption_NA%>% filter(Year==2008))
count(PowerConsumption_NA%>% filter(Year==2009))
count(PowerConsumption_NA%>% filter(Year==2010))

PowerConsumption_NA_Day <- PowerConsumption_NA %>%
  group_by(Day, Year)

#plot(PowerConsumption_NA_Day$Hour)
### C: anyhour
#plot(PowerConsumption_NA_Day$Year)
### C: insignificant 2008 and 2006, similar in 2009 and 2007 and big in 2010 
#plot(PowerConsumption_NA_Day$Day)
### C: more frequent from 13/ 14 onwards ? 
#plot(PowerConsumption_NA_Day$Season_of_year)
### C: much more in summer
#plot(PowerConsumption_NA_Day$Day_of_week)
### C: much more in weekends
#plot(PowerConsumption_NA_Day$Period_of_day)
### C: much less in evening, some more in night 



#### 2010 ####

## jan
PowerConsumption_NA_10 <- PowerConsumption_NA%>% filter(Year==2010)
PowerConsumption_NA_10_jan <- PowerConsumption_NA%>% filter(Year==2010 & Month==1)
#plot(PowerConsumption_NA_10$Date) 
summary(PowerConsumption_NA_10_jan)
summary(PowerConsumption_NA_10_jan %>% filter(Day==12))
### C: starts at 14:53:00 until 23:59:00
summary(PowerConsumption_NA_10_jan %>% filter(Day==13))
### C: all day
summary(PowerConsumption_NA_10_jan %>% filter(Day==14))
### C: until 19:01:00
head(PowerConsumption_NA_10)

## mar
PowerConsumption_NA_10_mar <- PowerConsumption_NA%>% filter(Year==2010 & Month==3)
#plot(PowerConsumption_NA_10_mar$Day) 

summary(PowerConsumption_NA_10_mar %>% filter(Day==21))

## aug
PowerConsumption_NA%>% filter(Year==2010 & Month>3 & Month < 9)
summary(PowerConsumption_NA%>% filter(Year==2010 & Month==8 & Day == 21))
### C: another black out in august - from 17 / 08 until 22 / 08

## sept 
summary(PowerConsumption_NA%>% filter(Year==2010 & Month==9 & Day == 25))
summary(PowerConsumption_NA%>% filter(Year==2010 & Month==9 & Day == 28))
### C: another black out from 25 - 28 sept

#### 2006 ####
summary(PowerConsumption_NA%>% filter(Year==2006))
head(PowerConsumption_NA)
### C: 2 minutes per day (2 days)

#### 2008 ####
PowerConsumption_NA_08 <- PowerConsumption_NA%>% filter(Year==2008)
#plot(PowerConsumption_NA_08$Date)
head(PowerConsumption_NA_08)
PowerConsumption_NA_08%>% filter(Month <= 9)
summary(PowerConsumption_NA_08%>% filter(Month == 10))
### C: black out 50 min  
is.na(PowerConsumption_NA_08$DateTime)
PowerConsumption_NA_08%>% filter(Month == 11)
summary(PowerConsumption_NA_08%>% filter(Month == 12 & Day == 10))
### back out 1h10 
summary(PowerConsumption_NA_08%>% filter(Month == 12 & Day == 20))
### 1 min

#### 2009 ####
PowerConsumption_NA_09 <- PowerConsumption_NA%>% filter(Year==2009)
#plot(PowerConsumption_NA_09$Date)

## fev
summary(PowerConsumption_NA_09%>% filter(Month == 2))
PowerConsumption_NA_09_fev <- PowerConsumption_NA%>% filter(Year==2009 & Month == 2)
#plot(PowerConsumption_NA_09_fev$Day)
summary(PowerConsumption_NA%>% filter(Year==2009 & Month == 2))

summary(PowerConsumption_NA%>% filter(Year==2009 & Month == 2 & Day == 17))

## jun
head(PowerConsumption_NA_09%>% filter(Month > 2 & Month <= 6))
summary(PowerConsumption_NA_09%>% filter(Month > 2 & Month == 6))
PowerConsumption_NA_09%>% filter(Month > 2 & Month == 7)
PowerConsumption_NA_09%>% filter(Month > 2 & Month == 6 & Day == 15)
summary(PowerConsumption_NA_09%>% filter(Month > 2 & Month == 6 & Day == 15))
### C: black out - 3 days 

## ago
summary(PowerConsumption_NA_09%>% filter(Month > 2 & Month == 8))
### C: black out < 1 day 

head(PowerConsumption_NA_09%>% filter(Month > 2 & Month >=9))

#### 2007 ####

PowerConsumption_NA_07<- PowerConsumption_NA%>% filter(Year==2007)
#plot(PowerConsumption_NA_07$Date)

## apr
PowerConsumption_NA_07_apr <- PowerConsumption_NA_07 %>% filter(Month == 4)
#plot(PowerConsumption_NA_07_apr$Day)
summary(PowerConsumption_NA_07_apr)
summary(PowerConsumption_NA_07_apr %>% filter(Day == 30))
### C: 3 days of black out 

## jun
PowerConsumption_NA_07_jun <- PowerConsumption_NA_07 %>% filter(Month == 6)
#plot(PowerConsumption_NA_07_jun$Date)

PowerConsumption_NA_07_jun%>% filter(Day == 9)
PowerConsumption_NA_07_jun%>% filter(Day > 9)

## set - dez
PowerConsumption_NA_07 %>% filter(Month > 7)

## jul
PowerConsumption_NA_07_jul <- PowerConsumption_NA_07 %>% filter(Month == 7)
#plot(PowerConsumption_NA_07_jul$Day)
summary(PowerConsumption_NA_07_jul %>% filter(Day > 15))

#### 3.2 Identify consecutive values #### 
PowerConsumption_byday$consecutive_day <- c(NA, diff(ymd(PowerConsumption_byday$Date))==1)
#View(PowerConsumption_byday)
PowerConsumption_byday %>% filter(consecutive_day == "FALSE")

#### 3.3 Replace #### 
#na.locf()

## install.packages("dendextend")
library(dendextend)
PowerConsumption$Global_active_power <- na.locf(PowerConsumption$Global_active_power, recursive = TRUE)
is.na(PowerConsumption$Global_active_power)

## install.packages("zoo")
library (zoo)

# Global active power
na.locf(PowerConsumption$Global_active_power, recursive = TRUE)
PowerConsumption$Global_active_power <- na.locf(PowerConsumption$Global_active_power, recursive = TRUE)
is.na(PowerConsumption$Global_active_power)
sum(is.na(PowerConsumption$Global_active_power))

# Global reactive power
PowerConsumption$Global_reactive_power <- na.locf(PowerConsumption$Global_reactive_power, recursive = TRUE)
sum(is.na(PowerConsumption$Global_reactive_power))
summary(PowerConsumption)

# Voltage 
na.locf(PowerConsumption$Voltage, recursive = TRUE)
PowerConsumption$Voltage <- na.locf(PowerConsumption$Voltage, recursive = TRUE)
sum(is.na(PowerConsumption$Voltage))
summary(PowerConsumption$Voltage)
summary(PowerConsumption_c$Voltage)
sum(is.na(PowerConsumption_double$Voltage))
summary(PowerConsumption_double$Voltage)

# Global intensity 
PowerConsumption$Global_intensity <- na.locf(PowerConsumption$Global_intensity, recursive = TRUE)
sum(is.na(PowerConsumption$Global_intensity))

# Sub_metering_1
PowerConsumption$Sub_metering_1 <- na.locf(PowerConsumption$Sub_metering_1, recursive = TRUE)
sum(is.na(PowerConsumption$Sub_metering_1))

# Sub_metering_2
PowerConsumption$Sub_metering_2 <- na.locf(PowerConsumption$Sub_metering_2, recursive = TRUE)
sum(is.na(PowerConsumption$Sub_metering_2))

# Sub_metering_3
PowerConsumption$Sub_metering_3 <- na.locf(PowerConsumption$Sub_metering_3, recursive = TRUE)
sum(is.na(PowerConsumption$Sub_metering_3))

# Global_power
PowerConsumption$Global_power <- PowerConsumption$Global_active_power + PowerConsumption$Global_reactive_power
summary(PowerConsumption$Global_power)

# Global_active_power_wh
PowerConsumption$Global_active_power_wh <- PowerConsumption$Global_active_power*1000/60
summary(PowerConsumption$Global_active_power_wh)

# Global_reactive_power_wh
PowerConsumption$Global_reactive_power_wh <- PowerConsumption$Global_reactive_power*1000/60
summary(PowerConsumption$Global_reactive_power_wh)

# Global_power_wh
PowerConsumption$Global_power_wh = PowerConsumption$Global_reactive_power_wh + PowerConsumption$Global_active_power_wh
summary(PowerConsumption$Global_power_wh)

# Global_sub_meter
PowerConsumption$Global_Sub_metering <- PowerConsumption$Sub_metering_1 + PowerConsumption$Sub_metering_2 + PowerConsumption$Sub_metering_3
summary(PowerConsumption$Global_Sub_metering)

PowerConsumption$No_Sub_metering_Energy<- PowerConsumption$Global_active_power*1000/60 - (PowerConsumption$Sub_metering_1 + PowerConsumption$Sub_metering_2 +PowerConsumption$Sub_metering_3)
summary(PowerConsumption$No_Sub_metering_Energy)
summary(PowerConsumption_c$No_Sub_metering_Energy)
### E: I have negative values for this. 

#### 4. Check dataset #### 

## 4.1 Data Types
str(PowerConsumption)

## 4.2 Data sample
str(PowerConsumption)
PowerConsumption_sub <- PowerConsumption %>% filter(Year >= "2007")
summary(PowerConsumption_sub)

### just created
PowerConsumption_sub_2 <- PowerConsumption %>% filter(!(Year == "2010" & Month =="11"))
PowerConsumption_sub_3 <- PowerConsumption_sub_2 %>% filter(Year != "2006")
summary(PowerConsumption_sub_3)

PowerConsumption_2006 <- PowerConsumption %>% filter(Year == "2006")
summary(PowerConsumption_2006)


#write.table(PowerConsumption_sub, "~/Desktop/UBIQUM/2. Tasks/Course 3/Task 1/Data/PowerConsumption_sub2.txt", col.names=F, row.names =F)
#write.csv(PowerConsumption_sub, "~/Desktop/UBIQUM/2. Tasks/Course 3/Task 1/Data/PowerConsumption_sub2.csv", col.names=T, row.names =F)

#### 5. Time series - Training ####  

## 5.1 Grouped by day 
PowerConsumption$Time <- NULL
str(PowerConsumption)

library(dplyr)
PowerConsumption_Day_ <- PowerConsumption_sub %>%
  group_by(Date) %>% 
  summarise(sum_global_power_wh=sum(Global_power_wh), 
                sum_global_active_wh = sum(Global_active_power_wh), 
                sum_global_reactive_wh = sum(Global_reactive_power_wh),
                sum_sub_meter_1 = sum(Sub_metering_1), 
                sum_sub_meter_2 = sum(Sub_metering_2),
                sum_sub_meter_3 = sum(Sub_metering_3),
                sum_no_sub_meter = sum(No_Sub_metering_Energy),
                sum_global_sub_meter = sum(Global_Sub_metering),
                sum_global_intensity = sum(Global_intensity),
                sum_voltage = sum(Voltage))
summary(PowerConsumption_Day_)
           
## 5.2 install packages

#install.packages("stats")
library(stats)

ts(data = PowerConsumption_Day_,frequency = 365, start = 2007)
ts_test <- ts(data = PowerConsumption_Day_,frequency = 365, start = 2007)
plot(ts_test)
print(ts_test)

   
## 5.3.1 group by - test

PowerConsumption_Day_test <- PowerConsumption_sub %>%
  group_by(Date) %>% 
  summarise(sum_global_power_wh=sum(Global_power_wh))
summary(PowerConsumption_Day_test)
View(PowerConsumption_Day_test)
PowerConsumption_day_test_1c <- PowerConsumption_Day_test[c(2)]
View(PowerConsumption_day_test_1c)

## time series - test

ts(data=PowerConsumption_day_test_1c, frequency = 365, start = 2007)
ts_test_2 <- ts(data=PowerConsumption_day_test_1c, frequency = 365, start = 2007)
print(ts_test_2)
plot(ts_test_2, main="Evolution of Global Power Consumption by day (2007 - 2010)")
plot.ts(ts_test_2, main="Evolution of Global Power Consumption by day (2007 - 2010)")

## 5.3.2 group by test 2

PowerConsumption_Day_test2 <- PowerConsumption_sub %>%
  group_by(Date) %>% 
  summarise(sum_global_power_wh=sum(Global_power_wh),
          sum_global_active_wh = sum(Global_active_power_wh), 
          sum_global_reactive_wh = sum(Global_reactive_power_wh),
          sum_sub_meter_1 = sum(Sub_metering_1), 
          sum_sub_meter_2 = sum(Sub_metering_2),
          sum_sub_meter_3 = sum(Sub_metering_3),
          sum_no_sub_meter = sum(No_Sub_metering_Energy),
          sum_global_sub_meter = sum(Global_Sub_metering),
          sum_global_intensity = sum(Global_intensity),
          sum_voltage = sum(Voltage))
summary(PowerConsumption_Day_test2)
PowerConsumption_Day_test2<- PowerConsumption_Day_test2[c(2:10)]
summary(PowerConsumption_Day_test2)

ts(data=PowerConsumption_Day_test2, frequency= 365, start = 2007)
ts_test2 <- ts(data=PowerConsumption_Day_test2, frequency= 365, start = 2007)
print(ts_test2)
plot(ts_test2)
plot.ts(ts_test2, plot.type = "s", col=1:9) 
plot.ts(ts_test2, plot.type = "m", col=1:9, mar=c(gap=0.3, 5.1, gap=0.3, 2.1)) 
plot(ts_test2[, 2])
plot(ts_test2[, 3])
plot(ts_test2[, 5])
summary(ts_test2)

plot.ts(ts_test2[,1:5], plot.type = "s", col=1:5) 
plot.ts(ts_test2[,"sum_sub_meter_1"], plot.type = "s") 

#
library("TTR")
plot.ts(SMA(ts_test2[,"sum_sub_meter_1"],n=8))
ts_test2[,"sum_sub_meter_1"]
test_decompose <-decompose (ts_test2[,"sum_sub_meter_1"])
plot(test_decompose)

ts_test2_adjusted <- ts_test2 - test_decompose$seasonal
plot.ts(ts_test2_adjusted, plot.type = "s",col=1:5) 
plot.ts(ts_test2_adjusted, plot.type = "s",col=1:5) 


## Forecasts using Exponential Smoothing

# Sub_meter_1 - forecast 1

HoltWinters(ts_test2[,"sum_sub_meter_1"], beta=FALSE, gamma=FALSE)
ts_test2_sub_meter_1_forecast <- HoltWinters(ts_test2[,"sum_sub_meter_1"], beta=FALSE, gamma=FALSE)
ts_test2_sub_meter_1_forecast$fitted
plot(ts_test2_sub_meter_1_forecast)
ts_test2_sub_meter_1_forecast$SSE
### C: SSE: 3531952628

# Sub_meter_1 - forecast 2

ts_test2_sub_meter_1_forecast1051 <- HoltWinters(ts_test2[,"sum_sub_meter_1"], beta=FALSE, gamma=FALSE, l.start=1051)
ts_test2_sub_meter_1_forecast1051$fitted
plot(ts_test2_sub_meter_1_forecast1051)
ts_test2_sub_meter_1_forecast1051$SSE
### C: SSE: 3511725680

#install.packages("forecast")
library(forecast)
forecast.HoltWinters(ts_test2[,"sum_sub_meter_1"], h=365)
forecast:::forecast.HoltWinters(ts_test2_sub_meter_1_forecast1051, h=365)
ts_test2_sub_meter_1_forecast1051_h365 <- forecast:::forecast.HoltWinters(ts_test2_sub_meter_1_forecast1051, h=365)  
forecast:::plot.forecast(ts_test2_sub_meter_1_forecast1051_h365)












###################### ATTEMPT 1  - GLOBAL POWER // ALL DATASET #################
#### A. Describe dataset ####
head(PowerConsumption_sub)
str(PowerConsumption_sub)
View(PowerConsumption_sub)
summary(PowerConsumption_sub)
### C: 2,053,283 observations
### C: 25 variables
### C: Time as character; Factor: Type_of_day, Day_of_week, Period_of_month, Season_of_year, Period_of_day, 
### C: DateTime as POSIXct, Date as Date
### C: No NAs - treated as before - na.locf(PowerConsumption$Global_active_power, recursive = TRUE)

str(PowerConsumption_sub_3)
### C: 2016000 obs. of  25 variables

#### B.1 Group by month - recently changed this to PowerConsumption_sub_3 ####  
library(dplyr)
Consumption_Month_<- PowerConsumption_sub_3 %>%
  group_by(Year, Month) %>% 
  summarise(Global_power_wh=sum(Global_power_wh), 
            Global_active_wh = sum(Global_active_power_wh), 
            Global_reactive_wh = sum(Global_reactive_power_wh),
            Sub_meter_1_wh = sum(Sub_metering_1), 
            Sub_meter_2_wh = sum(Sub_metering_2),
            Sub_meter_3_wh = sum(Sub_metering_3),
            No_sub_meter_wh = sum(No_Sub_metering_Energy),
            Global_sub_meter_wh = sum(Global_Sub_metering),
            Global_intensity_amp = sum(Global_intensity),
            Voltage_volt = sum(Voltage))
summary(Consumption_Month_)
View(Consumption_Month_)
### ! Make sure they are ordered by date 

#### B.2 Deal with low values - Global power wh ####

Consumption_Month_ %>% filter(Global_power_wh < 400000)
### C: 1 point - 8/2008: 300828

plot(Consumption_Month_$Global_power_wh)
### C: Outlier 
plot(Consumption_Month_$Global_active_wh)
plot(Consumption_Month_$Global_reactive_wh)
### C: No outliers here 

plot(Consumption_Month_$Sub_meter_1_wh)
### C: Outlier 
plot(Consumption_Month_$Sub_meter_2_wh)
### C: Outlier 
plot(Consumption_Month_$Sub_meter_3_wh)
### C: Outlier 
plot(Consumption_Month_$No_sub_meter_wh)
### C: Outlier

plot(PowerConsumption_byday$Global_power_wh)
head(PowerConsumption_byday$Date)

PowerConsumption_byday_0808 <- PowerConsumption_byday %>% filter(Date>=(as.Date("2008-08-01"))&
                                                                   Date<=(as.Date("2008-08-31")))
plot(PowerConsumption_byday_0808$Sub_metering_3)
plot(PowerConsumption_byday_0808$Sub_metering_1)
### C: sub meter 1 was down from most of the month, except for 2 days

plot(Log_Global_energy_decomp)
plot(Log_Global_energy - Log_Global_energy_decomp$random)
### C: weird - I loose a lot of observations

##replace value of 08/2008
select(Consumption_Month_ %>% filter(Month==8), Global_power_wh)
### C: Aug/07:652288; Aug/08: 300828; Aug/09:627046; Aug/10:557203

Global_power_wh_Aug <- select(Consumption_Month_ %>% filter(Month==8), Global_power_wh, Year)
Global_power_wh_Aug
min(Global_power_wh_Aug$Global_power_wh)
mean(Global_power_wh_Aug$Global_power_wh)

min(select(Global_power_wh_Aug %>% filter(Year != 2008), Global_power_wh, Year)[,1])
### C: 557203.4
min_global_power_wh_aug <- min(select(Global_power_wh_Aug %>% filter(Year != 2008), Global_power_wh, Year)[,1])

Consumption_Month_$Global_power_wh <- ifelse(Consumption_Month_$Month == 8 & Consumption_Month_$Year == 2008,min_global_power_wh_aug, Consumption_Month_$Global_power_wh) 
head(Consumption_Month_$Global_power_wh)
select(Consumption_Month_ %>% filter(Month==8), Global_power_wh)

#### C. Convert in time series #### 
library(stats)
ts(data = Consumption_Month_,frequency = 12, start = 2007)
### C: Correct order 

Consumption_Month_ts <- ts(data = Consumption_Month_,frequency = 12, start = 2007)
str(Consumption_Month_ts)
### C: 12 "attributes", first 2 are year and month 
plot(Consumption_Month_ts[,3:12])

#### D. Plotting / exploring variables ####
#### D.1 Plot Global energy, global active enrgy and global reactive energy#### 
plot(Consumption_Month_ts[,3:5], 
     main="Evolution of global energy, active energy and reactive energy by month, in wh, (2007 - 2010)")
### C: Global and active: +/- additive, constant trend, no constant mean / moving average, seasonality, seems to have randomness
### C: reactive: +/- additive, no trend & no constant mean / moving average - no stationarity, seems to have seasonality, seems to have randomness

plot.ts(Consumption_Month_ts[,3:5], 
        main="Evolution of global energy, active energy and reactive energy by month, in wh, (2007 - 2010)")
### C: same output as before

plot.ts(Consumption_Month_ts[,3:5], plot.type = "s", col=1:3, main="Evolution of global energy, active energy and reactive energy by month, in wh, (2007 - 2010)") 
### C: misses the legend and y-axis
### C: Carefull: this doesn't make sense anymore - I have corrected for the low value of global power and not fpr the others

#### D.2 Plot sub-metering 1, sub-metering 2 and submetering 3 #### 
plot.ts(Consumption_Month_ts[,6:8], main="Evolution of sub-meter 1, sub-meter 2 and sub-meter 3, in wh, (2007 - 2010)") 
plot.ts(Consumption_Month_ts[,6:8], plot.type= "s", col=1:3, main="Evolution of sub-meter 1, sub-meter 2 and sub-meter 3, in wh, (2007 - 2010)")
### C: misses the legend and y-axis
### C: sub-meter-1 : multiplicative, no constant trend, no seasonality, moving average, and some randomness
### C: sub-meter-2 and 3 : aditive, constant trend, no seasonality, constant average, and some randomness

#### D.3 Plot no sub-meter and global sub-meter #### 
plot.ts(Consumption_Month_ts[,9:10], main="Evolution of Energy captured by submeters and Energy not caputured by sub-meters, in wh, (2007 - 2010)") 
plot.ts(Consumption_Month_ts[,9:10],plot.type= "s", col=1:2, main="Evolution of Energy captured by submeters and Energy not caputured by sub-meters, in wh, (2007 - 2010)") 
### C: misses the legend and y-axis
### C: sub-meter and no sub-meter: multiplicative, no constant mean, seasonality and some randomness


#### E. Focus on Global energy // CAREFUL - I HAVE RUN IT AGAIN WITH THE NEW VARIABLE (ACTIVE + REACTIVE) AND HAVENT CHECKED THE RESULTS #### 

#### E.1 Correcting for some multiplicative elements ####
Log_Global_energy <-log(Consumption_Month_ts[,3])
plot(Log_Global_energy)
### C: Global energy: assstivie, moving average, constant trend, seasonality and some randomness
ggseasonplot(Log_Global_energy, season.labels = TRUE, year.labels = TRUE, year.labels.left = TRUE, continuous = FALSE)

#### E.2 Decomposing ####
? decompose
decompose(Log_Global_energy)
### C: confirmed: addtitive
Log_Global_energy_decomp <- decompose(Log_Global_energy)
plot(Log_Global_energy_decomp)
### C: Definitely some seasonality, definitely spme randomness (although in a shorter period), some unclear trend

#### E.3 Trial: adjust for seasonality ####
plot(Log_Global_energy - Log_Global_energy_decomp$seasonal)
### C: should I do something with summer of 2008?

#### E.4 Trial: adjust for randomness ####
plot(Log_Global_energy - Log_Global_energy_decomp$random)

#### E.5 HoltWinters - modelling - attempt 1 ####
library(forecast)
HoltWinters(Log_Global_energy)
### C: Holt-Winters exponential smoothing with trend and additive seasonal component / Smoothing parameters: alpha: 0; beta : 0; gamma: 0.1249263
### C: seems to corroborate that the most important element is seasonality 

Log_Global_energy_forecastHW <- HoltWinters(Log_Global_energy)
plot(Log_Global_energy_forecastHW)
Log_Global_energy_forecastHW$SSE
### C: SSE: 0.8646212 / after correcting the low point of global_power: 0.3274959

#### E.6 HoltWinters - forecast - attempt 1 ####
forecast:::forecast.HoltWinters(Log_Global_energy_forecastHW, h=13)
Log_Global_energy_forecastHW_h13 <- forecast:::forecast.HoltWinters(Log_Global_energy_forecastHW, h=13)
Log_Global_energy_forecastHW_h13
forecast:::plot.forecast(Log_Global_energy_forecastHW_h13)

forecast:::forecast.HoltWinters(Log_Global_energy_forecastHW, h=7)
Log_Global_energy_forecastHW_h7 <- forecast:::forecast.HoltWinters(Log_Global_energy_forecastHW, h=7)
Log_Global_energy_forecastHW_h7
forecast:::plot.forecast(Log_Global_energy_forecastHW_h7)

forecast:::forecast.HoltWinters(Log_Global_energy_forecastHW, h=25)
Log_Global_energy_forecastHW_h25 <- forecast:::forecast.HoltWinters(Log_Global_energy_forecastHW, h=25)
Log_Global_energy_forecastHW_h25
forecast:::plot.forecast(Log_Global_energy_forecastHW_h25)
### C: unlog

#### E.7 HoltWinters - forecast - unlogged - attempt 1 ####
unLog_Global_energy_forecastHW_h13 <- forecast:::forecast.HoltWinters(Log_Global_energy_forecastHW, h=13)
unLog_Global_energy_forecastHW_h13$mean <- exp(unLog_Global_energy_forecastHW_h13$mean)
unLog_Global_energy_forecastHW_h13$upper <- exp(unLog_Global_energy_forecastHW_h13$upper)
unLog_Global_energy_forecastHW_h13$lower <- exp(unLog_Global_energy_forecastHW_h13$lower)
unLog_Global_energy_forecastHW_h13$x <- exp(unLog_Global_energy_forecastHW_h13$x)

plot(unLog_Global_energy_forecastHW_h13, main = "Forecast (HW) of Global energy (nov10 - dez11)")
library(forecast)
forecast:::plot.forecast(unLog_Global_energy_forecastHW_h13, main = "Forecast (HW) of Global energy (nov10 - dez11)")
max(Consumption_Month_$Global_active_wh)
### C: 1,210,092 wh

#### E.8 HoltWinters - ploting errors - attempt 1 ####
Log_Global_energy_forecastHW$fitted[,1]-Log_Global_energy_forecastHW$fitted[,2]
hist(Log_Global_energy_forecastHW$fitted[,1]-Log_Global_energy_forecastHW$fitted[,2])

autoplot(unLog_Global_energy_forecastHW_h13) +
  ggtitle("Forecasts HW - Global energy (nov10 - dec11") +
  xlab("Year") + ylab("Global energy in wh")

autoplot(unLog_Global_energy_forecastHW_h13, facets=TRUE) +
  xlab("Year") + ylab("Log_Predicted_Global_Energy") +
  ggtitle("HW:Predicted Global Energy in wh (nov10-dez11)")

#### E.9 HoltWinters - assess correlation of errors with lags - attempt 1 ####
#Acf(Log_Global_energy_forecastHW_h13)
### E: what is this?

Acf(Log_Global_energy_forecastHW_h13$residuals, lag.max=20)
### C: P=0
Pacf(Log_Global_energy_forecastHW_h13$residuals, lag.max=20)
### C: P=0

plot.ts(Log_Global_energy_forecastHW_h13$residuals)
### C: exception: summer 2008 

#### F.1 ARIMA - chek for stationarity - attempt 1 ####
Log_Global_energy_diff <- diff(Log_Global_energy, differences=1)
Log_Global_energy_diff
plot.ts(Log_Global_energy_diff)
### C: not stationary
acf(Log_Global_energy_diff)
pacf(Log_Global_energy_diff)
### C: acf - p:2
### C: pacf - p:2

#Log_Global_energy_diff2 <- diff(Log_Global_energy, differences=2)
#plot.ts(Log_Global_energy_diff2)
### C: still not stationary

#Log_Global_energy_diff3 <- diff(Log_Global_energy, differences=3)
#plot.ts(Log_Global_energy_diff3)

#Log_Global_energy_diff4 <- diff(Log_Global_energy, differences=4)
#plot.ts(Log_Global_energy_diff4)

#Log_Global_energy_diff5 <- diff(Log_Global_energy, differences=5)
#plot.ts(Log_Global_energy_diff5)

#Log_Global_energy_diff6 <- diff(Log_Global_energy, differences=6)
#plot.ts(Log_Global_energy_diff6)

#Log_Global_energy_diff7 <- diff(Log_Global_energy, differences=7)
#plot.ts(Log_Global_energy_diff7)

# (...)

#Log_Global_energy_diff25 <- diff(Log_Global_energy, differences=25)
#plot.ts(Log_Global_energy_diff25)

#### F.2 ARIMA - parameters - attempt 1 ####
acf(Log_Global_energy_diff10, lag.max=20)
### C: p=7/8

pacf(Log_Global_energy_diff10, lag.max=20)
### C: p=3

auto.arima(Log_Global_energy)
### C: (1,1,0)


#### G.1 Linear Model Time Series - modelling / forecasting - attempt 1 ####
#### G.1.1 Time Series with trend + season - attempt 1####
Log_Global_energy_fit_tslm <- tslm(Log_Global_energy ~ trend + season)
Log_Global_energy_forecast_tslm <- forecast(Log_Global_energy_fit_tslm)
plot(Log_Global_energy_forecast_tslm)
Log_Global_energy_forecast_tslm

autoplot(Log_Global_energy_forecast_tslm, facets=TRUE) +
  xlab("Year") + ylab("Log_Predicted_Global_Energy") +
  ggtitle("LM: Predicted Global Energy in wh (nov10-dez11)")

Log_Global_energy_forecast_tslm$SSE
### E: NULL

Acf(Log_Global_energy_forecast_tslm$residuals, lag.max=20)
### C: P=1
Pacf(Log_Global_energy_forecast_tslm$residuals, lag.max=20)
### C: P=1

#### G.1.2 Time Series with season - attempt 1####
Log_Global_energy_fit_tslm2 <- tslm(Log_Global_energy ~ season)
Log_Global_energy_forecast_tslm2 <- forecast(Log_Global_energy_fit_tslm2)
plot(Log_Global_energy_forecast_tslm2)
Log_Global_energy_forecast_tslm2
autoplot(Log_Global_energy_forecast_tslm2, facets=TRUE) +
  xlab("Year") + ylab("Log_Predicted_Global_Energy") +
  ggtitle("LM2: Predicted Global Energy in wh (nov10-dez11)")


###################### ATTEMPT 2 - GLOBAL POWER - TRAINING AND TESTING SETS ################# #####
#### H. Split dataset - test and training ####
library(stats)

36/47 # size of training
training_month<-subset(Consumption_Month_,Year>=2007 & Year <=2009)
training_month_ts <- ts(data = training_month, frequency = 12, start = 2007)

plot(Consumption_Month_$Global_active_wh)
plot(training_month$Global_active_wh)
training_month_ts

12/47 # size of test
test_month<-subset(Consumption_Month_,Year>=2010)
test_month_ts <- ts(data=test_month, frequency = 12, start = 2010)

#### I. Holt Winters - attempt 2 ####

# Holt Winters - training - global power
training_month_ts_global_power <- training_month_ts[,3]
plot(training_month_ts_global_power)

traing_month_ts_global_power_HW_forecast <-HoltWinters(training_month_ts_global_power)
traing_month_ts_global_power_HW_forecast$SSE
plot(traing_month_ts_global_power_HW_forecast)
# E:152108397057

## predicting - 2010
traing_month_ts_global_power_HW_forecast_12 <- forecast:::forecast.HoltWinters(traing_month_ts_global_power_HW_forecast, h=12)
plot(traing_month_ts_global_power_HW_forecast_12)
traing_month_ts_global_power_HW_forecast_12

## plotting errors 
test_month_ts[,3]
traing_month_ts_global_power_HW_forecast_12
df_traing_month_ts_global_power_HW_forecast_12<-data.frame(traing_month_ts_global_power_HW_forecast_12)
errors_traintest_HW_12 <- df_traing_month_ts_global_power_HW_forecast_12[1:11,2]-test_month_ts[,3]
plot(errors_traintest_HW_12)

MRE_traintest_HW_12<-errors_traintest_HW_12/test_month_ts[,3]
MRE_traintest_HW_12
plot(MRE_traintest_HW_12)

#### J. ARIMA - attemp 2 (train and test) / INCOMPLETE ####
plot.ts(diff(training_month_ts_global_power, differences=1))
plot.ts(diff(training_month_ts_global_power, differences=2))
plot.ts(diff(training_month_ts_global_power, differences=3))
diff_training_month_ts_global_power <-diff(training_month_ts_global_power, differences=1)
plot.ts(diff(training_month_ts_global_power, differences=4))
### C: d = 1 / ARIMA(p,1,q)

acf(diff_training_month_ts_global_power, lag.max=20)
### C: q=
pacf(diff_training_month_ts_global_power,lag.max=20)
### C: p=3
auto.arima(training_month_ts_global_power)
plot.ts(training_month_ts_global_power)

### W: dont know what this is

library(forecast)
traing_month_ts_global_power_arima_010<-arima(training_month_ts_global_power, order=c(0,1,0))
forecast:::forecast.Arima(traing_month_ts_global_power_arima_010, h=12)
traing_month_ts_global_power_arima_010_forecast <-forecast:::forecast.Arima(traing_month_ts_global_power_arima_010, h=12)
plot(traing_month_ts_global_power_arima_010_forecast)
### E: always the same forecast points

data.frame(traing_month_ts_global_power_arima_010_forecast)
df.traing_month_ts_global_power_arima_010_forecast<-data.frame(traing_month_ts_global_power_arima_010_forecast)
df.traing_month_ts_global_power_arima_010_forecast[1:11,1]
### E: always the same forecast points

errors_traintest_arima_010 <- df.traing_month_ts_global_power_arima_010_forecast[1:11,1]-test_month_ts[,3]
errors_traintest_arima_010
MRE_traintest_arima010 <- errors_traintest_arima_010/test_month_ts[,3]
plot(MRE_traintest_arima010)
### E: 


#### K. TSLM - attemp 2 (train and test) ####
# predict 2010
?tslm
training_Global_energy_fit_tslm <- tslm(training_month_ts_global_power ~ trend + season)
plot(training_Global_energy_fit_tslm)
### E: doesnt work
training_Global_energy_forecast_tslm <- forecast(training_Global_energy_fit_tslm, h=12)
training_Global_energy_forecast_tslm
#plot(training_Global_energy_forecast_tslm$fitted)

# compute errors
df.training_month_ts_global_power_tslm_forecast <- data.frame(training_Global_energy_forecast_tslm)
df.training_month_ts_global_power_tslm_forecast[1:11,1]
df.training_month_ts_global_power_tslm_forecast[1:11,1]-test_month_ts[,3]
errors_traintest_tslm <- df.training_month_ts_global_power_tslm_forecast[1:11,1]-test_month_ts[,3]
plot(errors_traintest_tslm)
MRE_traintest_ts <- errors_traintest_tslm/test_month_ts[,3]

#### L. Compare models performance ####
plot(MRE_traintest_HW_12)
plot(MRE_traintest_ts)
plot(MRE_traintest_arima010)

mean(MRE_traintest_HW_12)
mean(MRE_traintest_ts)
mean(MRE_traintest_arima010)

# Forecasts 12 months
plot(traing_month_ts_global_power_HW_forecast_12)
plot(training_Global_energy_forecast_tslm)
plot(traing_month_ts_global_power_arima_010_forecast)

plot(traing_month_ts_global_power_HW_forecast)
### E: I could not plot the a similar graph for the other models

library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

test_MRE<-grid.arrange(MRE_traintest_HW_12, MRE_traintest_ts)
autoplot(MRE_traintest_HW_12)
autoplot(MRE_traintest_ts)
### C: quite similar performances
autoplot(MRE_traintest_arima010)
### C: ignore MRE of arima






###################### ATTEMPT 3 - GLOBAL ACTIVE ENERGY  // ALL DATASET ################# ####
#### M. Check distribution #### 
summary(Consumption_Month_)
View(Consumption_Month_)
summary(Consumption_Month_$Global_active_wh)
### C: min: 205,740 wh / max: 1,210,092 wh 
plot(Consumption_Month_$Global_active_wh)
Consumption_Month_$Global_active_wh
### C: i=20
#### N. Deal with outlier - global active and global reactive ####

# global active
Consumption_Month_ %>% filter(Month==8)
data.frame(select(Consumption_Month_,Global_active_wh, Month, Year)) %>% filter(Month==8) 
### E: min: 2008 - 205740.1 // 2010 - 477502.2 

Consumption_Month_$Global_active_wh <- ifelse(Consumption_Month_$Month == 8 & Consumption_Month_$Year ==2008, 477502.2, Consumption_Month_$Global_active_wh) 
head(Consumption_Month_$Global_active_wh)
min(Consumption_Month_$Global_active_wh)
plot(Consumption_Month_$Global_active_wh)

head(Consumption_Month_$Global_active_wh)
head(Consumption_Month_$Global_power_wh)
head(Consumption_Month_$Global_reactive_wh)

data.frame(select(Consumption_Month_,Global_active_wh, Month, Year)) %>% filter(Month==8) 
data.frame(select(Consumption_Month_,Global_reactive_wh, Month, Year)) %>% filter(Month==8) 
data.frame(select(Consumption_Month_,Global_power_wh, Month, Year)) %>% filter(Month==8) 
### E : still wrong for august ??? - ignore global power

# global reactive 
plot(Consumption_Month_$Global_reactive_wh)
### C: no outlier on august 2008

#### O. New time series #### 
library(stats)
ts(data = Consumption_Month_,frequency = 12, start = 2007)
Consumption_Month_ts_2 <- ts(data = Consumption_Month_,frequency = 12, start = 2007)

str(Consumption_Month_ts_2)
plot(Consumption_Month_ts_2[,3:12])
plot(Consumption_Month_ts_2[,4:5])
### C: distribution of active and reactive power

# active
ActivePower_wh_ts_month<-Consumption_Month_ts_2[,4]
ActivePower_wh_ts_month
plot(ActivePower_wh_ts_month)
plot(log(ActivePower_wh_ts_month))
### C: no need to smoothing / correct for multiplicative effect
ggseasonplot(ActivePower_wh_ts_month, season.labels = TRUE, year.labels = TRUE, year.labels.left = TRUE, continuous = FALSE)
### C: confirmed no outlier 

# reactive
plot(Consumption_Month_ts_2[,4:5])
Consumption_Month_ts_2[,5]
ReactivePower_wh_ts_month<-Consumption_Month_ts_2[,5]
plot(ReactivePower_wh_ts_month)
plot(log(ReactivePower_wh_ts_month))
ggseasonplot(ReactivePower_wh_ts_month, season.labels = TRUE, year.labels = TRUE, year.labels.left = TRUE, continuous = FALSE)
### C: no need to smoothing / correct for multiplicative effect

#### P. Decompose ts - active #### 
ActivePower_wh_ts_month_decomp <- decompose(ActivePower_wh_ts_month)
plot(ActivePower_wh_ts_month_decomp)
### C: random is short, trend is short - clear seasonality, descending trend

#### Q.1 HW modelling - active ####
library(forecast)
HoltWinters(ActivePower_wh_ts_month)
### C: alpha: 0.09578282, beta : 0.04570365, gamma: 0.5545159 - seasonality component gives more weight to last observations
plot(HoltWinters(ActivePower_wh_ts_month))
### C: more errors in the winter 2008, overestimates the consumption.
HW_ActivePower_wh_month <- HoltWinters(ActivePower_wh_ts_month)
plot(HW_ActivePower_wh_month)
HW_ActivePower_wh_month$fitted

#### Q.2 HW forecasting - active ####
forecast:::forecast.HoltWinters(HW_ActivePower_wh_month, h=14)
Forecast_HW_ActivePower_wh_month <- forecast:::forecast.HoltWinters(HW_ActivePower_wh_month, h=14)

plot(Forecast_HW_ActivePower_wh_month$residuals)
### C: confirm what I have seen about models' missperformance
hist(Forecast_HW_ActivePower_wh_month$residuals)
### C: a little bit skewed - due to the overestimatin of december 2009

autoplot(Forecast_HW_ActivePower_wh_month)
### C: model predicts badly the first month / dec 2010
Forecast_HW_ActivePower_wh_month$fitted

#### R.1 TSLM - modelling - active #### 
tslm(ActivePower_wh_ts_month ~ trend + season)
### E: I cant get the graphic // Hit <Return> to see next plot: 

#### R.2 TSLM - forecasting - active #### 
LM_ActivePower_wh_month <- tslm(ActivePower_wh_ts_month ~ trend + season)
forecast(LM_ActivePower_wh_month, h=14)
Forecast_LM_ActivePower_wh_month <- forecast(LM_ActivePower_wh_month, h=14)
autoplot(Forecast_LM_ActivePower_wh_month)

# residuals
Forecast_LM_ActivePower_wh_month$residuals
### C: I dont have NAS in the first 12 observations/predictions
hist(Forecast_LM_ActivePower_wh_month$residuals)
### C: normal distribution of residuals

#### S.1 ARIMA - modelling - active #### 
plot.ts(diff(ActivePower_wh_ts_month, differences=1))
plot.ts(diff(ActivePower_wh_ts_month, differences=2))
### C: p=2
dif_ActivePower_wh_ts_month <- diff(ActivePower_wh_ts_month, differences=2)

acf(dif_ActivePower_wh_ts_month, lag.max=20)
q=2/3
pacf(dif_ActivePower_wh_ts_month, lag.max=20)
p=0

#### S.2 ARIMA - forecasting - active #### 
auto.arima(dif_ActivePower_wh_ts_month)
### C: ARIMA(3,0,0)(1,1,0)[12]
auto.arima(ActivePower_wh_month)
### C: ARIMA(0,0,0)(1,1,0)[12] 
arima(ActivePower_wh_ts_month, order=c(0,0,0), seasonal =list(order=c(1,1,0)))
ARIMA_ActivePower_wh_month <- arima(ActivePower_wh_ts_month, order=c(3,0,0), seasonal =list(order=c(1,0,0)))
ARIMA_auto_ActivePower_wh_month <- ARIMA_ActivePower_wh_month 

plot(ARIMA_auto_ActivePower_wh_month)
#### E: I cant plot it // cant understand
Forecast_ARIMA_ActivePower_wh_month <- forecast:::forecast.Arima(ARIMA_auto_ActivePower_wh_month, h=14)
autoplot(Forecast_ARIMA_ActivePower_wh_month)

Forecast_ARIMA_ActivePower_wh_month$residuals
hist(Forecast_ARIMA_ActivePower_wh_month$residuals)
### C: Normally distributed



###################### ATTEMPT 4 - GLOBAL ACTIVE ENERGY // TRAINING AND TESTING SETS #####################
#### T. testing and training datasets - active energy #### 
training_month_ts
test_month_ts

training_month_ts[,4]
test_month_ts[,4]

ActivePower_wh_month_training <- training_month_ts[,4]
ActivePower_wh_month_testing <- test_month_ts[,4]

#### U.1 HW - modeling // active energy - test and train ####
HoltWinters(ActivePower_wh_month_training)
### C:  alpha: 0, beta : 0, gamma: 0.313282
plot(HoltWinters(ActivePower_wh_month_training))
### C: as expected, it doesnt show the initial and last observations

train_HW_ActivePower_wh_month <- HoltWinters(ActivePower_wh_month_training)
train_HW_ActivePower_wh_month$fitted

#### U.2 HW - forecasting // active energy - test and train ####
## predict 2010
forecast:::forecast.HoltWinters(train_HW_ActivePower_wh_month, h=10)
Forecast_train_HW_ActivePower_wh_month <- forecast:::forecast.HoltWinters(train_HW_ActivePower_wh_month, h=10)
autoplot(Forecast_train_HW_ActivePower_wh_month)
Forecast_train_HW_ActivePower_wh_month$fitted
hist(Forecast_train_HW_ActivePower_wh_month$residuals)
### C: not normally distributted

# absolute arror
train_forecast_HW_2010 <- data.frame(Forecast_train_HW_ActivePower_wh_month)[,1]

abs_errors_forecast_HW_active_2010 <- train_forecast_HW_2010 - ActivePower_wh_month_testing
autoplot(abs_errors_forecast_HW_active_2010)
mean(abs(abs_errors_forecast_HW_active_2010))
### C: MAE - 61,686.98

# relative error
relative_errors_forecast_HW_active_2010 <- abs_errors_forecast_HW_active_2010/ActivePower_wh_month_testing
plot(relative_errors_forecast_HW_active_2010)
mean(abs(relative_errors_forecast_HW_active_2010))
### C: MRE - 0.08284149

#### V.1 ts Linear Model - modeling // active energy - test and train ####
tslm(ActivePower_wh_month_training ~ trend + season)

train_LM_ActivePower_wh_month <- tslm(ActivePower_wh_month_training ~ trend + season)
train_LM_ActivePower_wh_month$fitted

#### V.2 ts Linear Model - forecasting // active energy - test and train ####
forecast(train_LM_ActivePower_wh_month, h=10)
Forecast_train_LM_ActivePower_wh_month <- forecast(train_LM_ActivePower_wh_month, h=10)
autoplot(Forecast_train_LM_ActivePower_wh_month)
hist(Forecast_train_LM_ActivePower_wh_month$residuals)
### not so normaly distributed 

# absolute error
ActivePower_wh_month_testing
train_forecast_LM_2010 <- data.frame(Forecast_train_LM_ActivePower_wh_month)[,1]

abs_errors_forecast_LM_active_2010 <- train_forecast_LM_2010-ActivePower_wh_month_testing
abs_errors_forecast_LM_active_2010
autoplot(abs_errors_forecast_LM_active_2010)
mean(abs(abs_errors_forecast_LM_active_2010))
### C: MAE - 57345.84

# relative error
relative_errors_forecast_LM_active_2010 <- abs_errors_forecast_LM_active_2010 / ActivePower_wh_month_testing
relative_errors_forecast_LM_active_2010
autoplot(relative_errors_forecast_LM_active_2010)
mean(abs(relative_errors_forecast_LM_active_2010))
### C: MRE - 0.0765351

#### W.1 ARIMA - modeling // active energy - test and train ####
ActivePower_wh_month_training
autoplot(ActivePower_wh_month_training)
auto.arima(ActivePower_wh_month_training)
### C: The chosen test encountered an error, so no seasonal differencing is selected. Check the time series data.
train_ARIMA_ActivePower_wh_month<-arima(ActivePower_wh_month_training, order = c(0,0,0), seasonal=list(order=c(1,1,0)))

#### W.2 ARIMA - forecasting // active energy - test and train ####
forecast:::forecast.Arima(train_ARIMA_ActivePower_wh_month, h=10)
Forecast_train_ARIMA_ActivePower_wh_month <- forecast:::forecast.Arima(train_ARIMA_ActivePower_wh_month, h=10)
autoplot(Forecast_train_ARIMA_ActivePower_wh_month)

Forecast_train_ARIMA_ActivePower_wh_month$residuals
hist(Forecast_train_ARIMA_ActivePower_wh_month$residuals)
### C: not so normally distributted

## absolute error
ActivePower_wh_month_testing
train_forecast_ARIMA_2010 <- data.frame(Forecast_train_ARIMA_ActivePower_wh_month)[,1]

abs_errors_forecast_ARIMA_active_2010 <- train_forecast_ARIMA_2010 - ActivePower_wh_month_testing
abs_errors_forecast_ARIMA_active_2010
autoplot(abs_errors_forecast_ARIMA_active_2010)
mean(abs(abs_errors_forecast_ARIMA_active_2010))
### C: MAE - 52706.92

## relative error
relative_errors_forecast_ARIMA_active_2010 <- abs_errors_forecast_ARIMA_active_2010/ActivePower_wh_month_testing
autoplot(relative_errors_forecast_ARIMA_active_2010)
mean(abs(relative_errors_forecast_ARIMA_active_2010))
### C: MRE - 0.07015942

#### X. Comparing models // active energy - test and train ####

## HW
autoplot(Forecast_train_HW_ActivePower_wh_month)
### C: MAE - 61,686.98 § MRE - 0.083
plot(relative_errors_forecast_HW_active_2010)
### C: worse predictions: june 2010
autoplot (Forecast_train_HW_ActivePower_wh_month) + autolayer(ActivePower_wh_month_testing, series="Dataset")

## LM
autoplot(Forecast_train_LM_ActivePower_wh_month)
### C: MAE - 57,345.84 $ MRE -  0.077
hist(Forecast_train_LM_ActivePower_wh_month$residuals)
### C: normally distributted
plot(relative_errors_forecast_LM_active_2010)
### C: worse predictions: fev 2010
autoplot (Forecast_train_LM_ActivePower_wh_month, PI= FALSE) + autolayer(ActivePower_wh_month_testing, series="Dataset")
autoplot (Forecast_train_LM_ActivePower_wh_month, PI= TRUE) + autolayer(ActivePower_wh_month_testing, series="Dataset")

## ARIMA
autoplot(Forecast_train_LM_ActivePower_wh_month)
### C: MAE - 52,706.92 § MRE - 0.070
hist(Forecast_train_ARIMA_ActivePower_wh_month$residuals)
### C: not so normally distributted
plot(relative_errors_forecast_ARIMA_active_2010)
### C: worse predictions: june 2010 / very different from the other relative errors' distribution
autoplot (Forecast_train_ARIMA_ActivePower_wh_month, PI =FALSE) + autolayer(ActivePower_wh_month_testing, series="Dataset")
autoplot (Forecast_train_ARIMA_ActivePower_wh_month, PI =TRUE) + autolayer(ActivePower_wh_month_testing, series="Dataset")

## ALL
autoplot (ActivePower_wh_month, series="Real data") + autolayer(Forecast_train_LM_ActivePower_wh_month, series="LM", PI= FALSE) + autolayer(Forecast_train_ARIMA_ActivePower_wh_month, PI =FALSE, series = "ARIMA") + autolayer(Forecast_train_HW_ActivePower_wh_month, PI =FALSE, series = "HW")

accuracy(Forecast_train_HW_ActivePower_wh_month,ActivePower_wh_month_testing, test = NULL)
accuracy(Forecast_train_LM_ActivePower_wh_month,ActivePower_wh_month_testing, test = NULL)
accuracy(Forecast_train_ARIMA_ActivePower_wh_month,ActivePower_wh_month_testing, test = NULL)
### C: mean error (ME), root mean squared error (RMSE), mean absolute error (MAE), mean percentage error (MPE), mean absolute percentage error (MAPE), mean absolute scaled error (MASE) and the first-order autocorrelation coefficient (ACF1)
### C: BEST MODEL IS ARIMA

#### Y. Combining models ####
#install.packages("opera")
#library(opera)

#install.packages("forecastHybrid")
#library(forecastHybrid)
#Hybrid_models <-hybridModel(ActivePower_wh_month, weights="equal")

#forecast(Hybrid_models)
#mixture

#### Z. Applying the best model to the whole dataset // active energy ####
autoplot(ActivePower_wh_month)
ActivePower_wh_ts_month
ARIMA_ActivePower_wh_month<-arima(ActivePower_wh_ts_month, order = c(0,0,0), seasonal=list(order=c(1,1,0)))
forecast:::forecast.Arima(ARIMA_ActivePower_wh_month, h=14)
Final_forecast_ARIMA_ActivePower_wh_month <-forecast:::forecast.Arima(ARIMA_ActivePower_wh_month, h=14)
autoplot(Final_forecast_ARIMA_ActivePower_wh_month)


###################### ATTEMPT 5 - GLOBAL REACTIVE ENERGY // TRAINING AND TESTING SETS #####################
#### AA. Setting testing and training datasets ####
training_month_ts 
test_month_ts

ReactivePower_wh_month_training <- training_month_ts[,5]
ReactivePower_wh_month_testing <- test_month_ts[,5]

#### AB.1 HW - modeling // reactive energy - testing and training ####
?HoltWinters
HoltWinters(ReactivePower_wh_month_training, seasonal = c("multiplicative"))
### C:  alpha: 0, beta : 0, gamma:0.8355229
plot(HoltWinters(ReactivePower_wh_month_training))
### C: as expected, it doesnt show the initial and last observations // predicts badly from march/april 2009

train_HW_ReactivePower_wh_month <- HoltWinters(ReactivePower_wh_month_training)
train_HW_ReactivePower_wh_month$fitted

#### AB.2 HW - forecasting // reactive energy - testing and training ####

## predict 2010
forecast:::forecast.HoltWinters(train_HW_ReactivePower_wh_month, h=10)
Forecast_train_HW_ReactivePower_wh_month <- forecast:::forecast.HoltWinters(train_HW_ReactivePower_wh_month, h=10)
autoplot(Forecast_train_HW_ReactivePower_wh_month)
Forecast_train_HW_ReactivePower_wh_month$fitted
hist(Forecast_train_HW_ReactivePower_wh_month$residuals)
### C: normally distributted

# absolute arror
Forecast_train_HW_ReactivePower_wh_month
train_forecast_HW_2010_reactive <- data.frame(Forecast_train_HW_ReactivePower_wh_month)[,1]
abs_errors_forecast_HW_reactive_2010 <- train_forecast_HW_2010_reactive  - ReactivePower_wh_month_testing
abs_errors_forecast_HW_reactive_2010
plot(abs_errors_forecast_HW_reactive_2010)
### C: there are is onw pick ~ june 
mean(abs(abs_errors_forecast_HW_reactive_2010))
### C: MAE - 14,071.47

# relative arror
relative_errors_forecast_HW_reactive_2010 <- abs_errors_forecast_HW_reactive_2010 / ReactivePower_wh_month_testing
relative_errors_forecast_HW_reactive_2010
plot(relative_errors_forecast_HW_reactive_2010)
mean(abs(relative_errors_forecast_HW_reactive_2010))
### C: MRE -  0.1631529


#### AC.1 ts Linear Model - modelling // reactive energy - testing and training ####
tslm(ReactivePower_wh_month_training ~ trend + season)
train_LM_ReactivePower_wh_month <- tslm(ReactivePower_wh_month_training ~ trend + season)
train_LM_ReactivePower_wh_month$fitted
#### AC.2 ts Linear Model - forecasting // reactive energy - testing and training ####
## predict 2010
forecast(train_LM_ReactivePower_wh_month, h=10)
Forecast_train_LM_ReactivePower_wh_month <- forecast(train_LM_ReactivePower_wh_month, h=10)
autoplot(Forecast_train_LM_ReactivePower_wh_month)
hist(Forecast_train_LM_ReactivePower_wh_month$residuals)
### C: not normally distributes // skewed to the right. someting around reasiduals = 10000

# absolute error
ReactivePower_wh_month_testing
train_forecast_LM_2010_reactive <- data.frame(Forecast_train_LM_ReactivePower_wh_month)[,1]

abs_errors_forecast_LM_reactive_2010 <- train_forecast_LM_2010_reactive - ReactivePower_wh_month_testing
abs_errors_forecast_LM_reactive_2010
plot(abs_errors_forecast_LM_reactive_2010)
### C: same pick as before - june 2010
mean(abs(abs_errors_forecast_LM_reactive_2010))
### C: MAE - 12268.17

# relative error
relative_errors_forecast_LM_reactive_2010 <- abs_errors_forecast_LM_reactive_2010 / ReactivePower_wh_month_testing
relative_errors_forecast_LM_reactive_2010
plot(relative_errors_forecast_LM_reactive_2010)
### C: same pick as before - june 2010
mean(abs(relative_errors_forecast_LM_reactive_2010))
### C: MRE - 0.1416168



#### AD.1 ARIMA - modelling // reactive energy - testing and training  #### 
ReactivePower_wh_month_training
auto.arima(ReactivePower_wh_month_training)
train_ARIMA_ReactivePower_wh_month<-arima(ReactivePower_wh_month_training, order = c(1,0,0), seasonal=list(order=c(0,1,3)))
#### AD.2 ARIMA - forecasting // reactive energy - testing and training  #### 
forecast:::forecast.Arima(train_ARIMA_ReactivePower_wh_month, h=10)
Forecast_train_ARIMA_ReactivePower_wh_month <- forecast:::forecast.Arima(train_ARIMA_ReactivePower_wh_month, h=10)
autoplot(Forecast_train_ARIMA_ReactivePower_wh_month)
Forecast_train_ARIMA_ReactivePower_wh_month$residuals
hist(Forecast_train_ARIMA_ReactivePower_wh_month$residuals)
### C: not so normally distributed - weird pick

## absolute error
ReactivePower_wh_month_testing
train_forecast_ARIMA_2010_reactive <- data.frame(Forecast_train_ARIMA_ReactivePower_wh_month)[,1]

abs_errors_forecast_ARIMA_reactive_2010 <- train_forecast_ARIMA_2010_reactive - ReactivePower_wh_month_testing
plot(abs_errors_forecast_ARIMA_reactive_2010)
### C_: same pick - june 2010
mean(abs(abs_errors_forecast_ARIMA_reactive_2010))
### C: MAE - 12117.61

## relative error
relative_errors_forecast_ARIMA_reactive_2010 <- abs_errors_forecast_ARIMA_reactive_2010 / ReactivePower_wh_month_testing
plot(relative_errors_forecast_ARIMA_reactive_2010)
### C_: same pick - june 2010
mean(abs(relative_errors_forecast_ARIMA_reactive_2010))
### C: MRE - 0.1337376

autoplot(ReactivePower_wh_ts_month)
### C: consumption doesnt grow that much on that month, compared to homologous period 
#### AE. Comparing models // reactive energy - testing and training #### 

#HW
autoplot(Forecast_train_HW_ReactivePower_wh_month)
### C: MAE - 14,071.47 / MRE -  0.1631529
autoplot (Forecast_train_HW_ReactivePower_wh_month, PI =FALSE) + autolayer(ReactivePower_wh_month_testing, series="Dataset")

#LM 
autoplot(Forecast_train_LM_ReactivePower_wh_month)
### C: MAE - 12268.17 / MRE - 0.1416168
autoplot (Forecast_train_LM_ReactivePower_wh_month, PI =FALSE) + autolayer(ReactivePower_wh_month_testing, series="Dataset")

#ARIMA
autoplot(Forecast_train_ARIMA_ReactivePower_wh_month)
### C: MAE - 12117.61 / MRE - 0.1337376
autoplot (Forecast_train_ARIMA_ReactivePower_wh_month, PI =FALSE) + autolayer(ReactivePower_wh_month_testing, series="Dataset")

# ALL 
autoplot(ReactivePower_wh_ts_month, series="Real data", main = "Forecast of Reactive energy by different models", xlab = "Time", ylab="Consumption of Reactive enrgy in wh") + autolayer(Forecast_train_LM_ReactivePower_wh_month, series="LM", PI= FALSE) + autolayer(Forecast_train_ARIMA_ReactivePower_wh_month, PI =FALSE, series = "ARIMA") + autolayer(Forecast_train_HW_ReactivePower_wh_month, PI =FALSE, series = "HW")

accuracy(Forecast_train_HW_ReactivePower_wh_month,ReactivePower_wh_month_testing, test = NULL)
accuracy(Forecast_train_LM_ReactivePower_wh_month,ReactivePower_wh_month_testing, test = NULL)
accuracy(Forecast_train_ARIMA_ReactivePower_wh_month,ReactivePower_wh_month_testing, test = NULL)
### C: BEST MODEL IS ARIMA
#### AF. Applying the best model to the whole dataste // reactive energy ####
autoplot(ReactivePower_wh_ts_month)
ReactivePower_wh_ts_month
ARIMA_ReactivePower_wh_month<-arima(ReactivePower_wh_ts_month, order = c(1,0,0), seasonal=list(order=c(0,1,3)))
forecast:::forecast.Arima(ARIMA_ReactivePower_wh_month, h=14)
Final_forecast_ARIMA_ReactivePower_wh_month <-forecast:::forecast.Arima(ARIMA_ReactivePower_wh_month, h=14)
autoplot_reactive<-autoplot(Final_forecast_ARIMA_ReactivePower_wh_month)
autoplot_reactive

###################### SHINY #####################
#### BA. setting up - ui and server #### 
library(shinydashboard)
library(shiny)
library(shinythemes)

ui <- dashboardPage(
  dashboardHeader(
    title = "Energy Consumption - Dashboard Example", titleWidth = 450, 
                  dropdownMenu(type = "messages",
                        messageItem(
                        from = "Customer support",
                        message = "Customer support is steady this month"),
                        messageItem(
                        from = "New User",
                        message = "How do I register?",
                        icon = icon("question"))),
                  dropdownMenu(type = "notifications",
                                     notificationItem(
                                       text = "Reaching the energy limit of the month",
                                       icon("users")
                                       ))),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Forecast", tabName = "forecast", icon = icon("dashboard")),
    menuItem("Breakdown", tabName = "breakdown", icon = icon("th")),
    menuItem("Analysis", tabName = "analysis", icon = icon("th"), badgeLabel = "new", badgeColor = "yellow"),
    dateRangeInput("dates", label = "Date range"),
    checkboxGroupInput("checkGroup", 
                       label = "Type of energy", 
                       choices = list("Active" = 1, 
                                      "Reactive" = 2),
                       selected = 1))),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "forecast",
              h2("Forecast of energy consumption"),
              fluidRow(
                column(width = 12,
                box(plotOutput("autoplot_reactive"))))),
    # second tab content
      tabItem(tabName = "breakdown", 
              h2("Breakdown of energy consumption"),
              fluidRow(
                  valueBox(20, "Total energy consumed (wh)", color="purple"),
                  valueBox(16, "Active energy consumed (wh)", color="yellow"),
                  valueBox(4, "Reactive energy consumed (wh)", color="red"),
                  box(plotOutput("piechart_energy")),
                  box(plotOutput("barchart_energy_comparison"),
                  numericInput("num", label ="Number of previous months to compare with",value =  3,
                                   min=1, max=12, step=NA)))),
      # third tab content 
      tabItem(tabName = "analysis", 
              h2("Analysis of energy consumption by period"),
              tabBox(
                title = "Evolution of energy consumption (2006 - 2010)",
                # The id lets us use input$tabset1 on the server to find the current tab - dont know what this means
                id = "tabset1", height = "300px", width = "400px",
                tabPanel("Year", "Consumption by year "),
                tabPanel("Month", "Consumption by month"),
                tabPanel("Season", "Consumption by season of the year"),
                tabPanel("Day of the week", "Consumption by day of the week"),
                tabPanel("Day of the week", "Consumption by hour of the day")),
              tabBox(
                title = "Breakdown of energy consumption (2006 - 2010)",
                # The id lets us use input$tabset1 on the server to find the current tab - dont know what this means
                id = "tabset2", height = "300px", width = "400px",
                tabPanel("Year", "Consumption by year "),
                tabPanel("Month", "Consumption by month"),
                tabPanel("Season", "Consumption by season of the year"),
                tabPanel("Day of the week", "Consumption by day of the week"),
                tabPanel("Day of the week", "Consumption by hour of the day"))
              )))
)

server <- function(input, output) {
    output$autoplot_reactive <- renderPlot({autoplot_reactive})
    output$piechart_energy <- renderPlot({autoplot_reactive})
    output$barchart_energy_comparison <- renderPlot({autoplot_reactive})
    output$barchart_energy_period <- renderPlot({autoplot_reactive})
    output$piechart_energy_period <- renderPlot({autoplot_reactive})
  ## to be developed
  # output$messageMenu <- renderMenu({})
  # output$notificationMenu <- renderMenu({})
  # output$tabset1 <- renderTable({XXX})
  # output$tabset2 <- renderTable({XXX})
}

shinyApp(ui,server)




## resources
# https://rstudio.github.io/shinydashboard/get_started.html
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/

