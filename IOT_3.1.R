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

#### 8. Exploration by day ####
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


#### Task 3.2 #### 

#### 9.6 Create a subsample of the dataset - from this date to this date

#PowerConsumption_c %>% filter(Date >= dmy("16/12/2007")) 
#ggplot(PowerConsumption_Month, aes(x=Month))+geom_line(aes(y=Global_power_wh))+geom_line(aes(y=Global_active_power_wh))

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
#### 3.1 Descriptive ######## 

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
### P : wrong package 

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

# Global_power_wh
PowerConsumption$Global_power_wh = PowerConsumption$Global_power*1000/60
summary(PowerConsumption$Global_power_wh)

# Global_active_power_wh
PowerConsumption$Global_active_power_wh <- PowerConsumption$Global_active_power*1000/60
summary(PowerConsumption$Global_active_power_wh)

# Global_reactive_power_wh
PowerConsumption$Global_reactive_power_wh <- PowerConsumption$Global_reactive_power*1000/60
summary(PowerConsumption$Global_reactive_power_wh)

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



##### TASK 3.2 - FORECASTING  #####

#### A . Describe dataset ####
head(PowerConsumption_sub)
str(PowerConsumption_sub)
View(PowerConsumption_sub)
summary(PowerConsumption_sub)
### C: 2,053,283 observations
### C: 25 variables
### C: Time as character; Factor: Type_of_day, Day_of_week, Period_of_month, Season_of_year, Period_of_day, 
### C: DateTime as POSIXct, Date as Date
### C: No NAs - treated as before - na.locf(PowerConsumption$Global_active_power, recursive = TRUE)

#### B. Group by month ####
library(dplyr)
Consumption_Month_<- PowerConsumption_sub %>%
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




#### RESOURCE ####
#https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html
