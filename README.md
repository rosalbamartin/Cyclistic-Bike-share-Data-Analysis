Cyclistic Bike-share Data Analysis

#### INTRODUCTION

Hi everyone!  I am glad to share with you my case study on the Cyclistic-bike-share. As part of the Google Data Analytics Professional Certificate, the future junior data analysts are requested to complete a Capstone Project.The students need to answer key business questions and specific tasks following the steps of the data analysis process to demonstrate the knowledge and the skills learnt in this program. The following information summarizes all the steps that I followed for the preparation,  analysis and visualization of this data along with my conclusions and recommendations.  I used excel and RStudio for this. 

#### SCENARIO

Cyclistic is a fictitious bike-share program based in Chicago. It offers a fleet of 5,824 bicycles that are geotracked and locked into a network of 692 stations.  Cyclistic sets itself apart by offering different types of bikes making bike-share more inclusive to people with disabilities and riders who can’t use a standard two-wheeled bike. 

 Customers who purchase single-ride or full-day passes are referred to as casual riders. Customers who purchase annual memberships are Cyclistic members. Cyclistic’s finance analysts have concluded that annual members are much more profitable than casual riders.

 As a junior data analyst  I am part of the  Marketing  Analytics team who are responsible for collecting, analyzing, and reporting data that helps guide Cyclistic marketing strategy.

#### STAKEHOLDERS

* Cyclistics
* The Director of Marketing
* Marketing Analytics Team
* Cyclistic Executive Team

#### BUSINESS TASK

To gain  insights through historical data to understand  how casual riders and annual members use Cyclistic bikes differently. Identify trends that will help design a new marketing strategy to maximize the number of annual members. 

#### DATA SOURCES USED

The data is located in an amazon web service link  (https://divvy-tripdata.s3.amazonaws.com/index.html) . Here you can access the historical trip datasets that  are separated in several csv files.This data has been made available by Motivate International Inc. via license. 


#### PREPARE

I downloaded 12 months of Cyclistic trip data starting from January 2021 to December 2021 and created a folder on my desktop to house the files. I saved  the spreadsheets in excel and proceed to add columns and analyze each month with the following calculations:

* Ride_length. Calculate the length of each ride by subtracting the column “started_at” from the column “ended_at”.
* Create a column called “day_of_week,” and calculate the day of the week that each ride started.
* Total number of rides.
* Mean of ride_length.
* Max ride_length.
* Mode of day_of_week.

Created a pivot table to quickly calculate and visualize the following data:

* Average ride_length for members and casual riders. 
* Average ride_length for users by day_of_week. 
* Member_casual; Values = Average of ride_length.
* Number of rides for users by day_of_week by adding Count of trip_id to Values.

The files were too big to merge into excel so I chose to use RStudio to perform the manipulation of data,  final analysis and the visualizations.


#### RSTUDIO

**Install and load required packages:**

```{r eval=FALSE}
library(tidyverse) #for data import and wrangling
library(lubridate) #for date functions 
library(anytime)   #easier date and time conversion
library(ggplot2)   #helps visualize data
```
```{r eval=FALSE}
getwd() #displays your working directory
setwd("../insert/Documents") #sets your working directory to simplify call to data
```


#### COLLECT DATA

**Upload Divvy datasets  (csv files)**

```{r eval=FALSE}
 X202101 <- read_csv("../insert/202101.csv")
 X202102 <- read_csv("../insert/202102.csv")
 X202103 <- read_csv("../insert/202103.csv")
 X202104 <- read_csv("../insert/202104.csv")
 X202105 <- read_csv("../insert/202105.csv")
 X202106 <- read_csv("../insert/202106.csv")
 X202107 <- read_csv("../insert/202107.csv")
 X202108 <- read_csv("../insert/202108.csv")
 X202109 <- read_csv("../insert/202109.csv")
 X202110 <- read_csv("../insert/202110.csv")
 X202111 <- read_csv("../insert/202111.csv")
 X202112 <- read_csv("../insert/202112.csv")

```


#### WRANGLE DATA AND COMBINE INTO A SINGLE FILE
 
**Compare column names for each of the files**
```{r eval=FALSE}
Colnames (X202101) 
Colnames (X202102) 
Colnames (X202103) 
Colnames (X202104) 
Colnames (X202105) 
Colnames (X202106) 
Colnames (X202107) 
Colnames (X202108) 
Colnames (X202109) 
Colnames (X202110) 
Colnames (X202111) 
Colnames (X202112) 
```

**Combine individual  data frames into one year view** 

```{r eval=FALSE}
all2021<-rbind(X202101, X202102, X202103, X202104, X202105, X202106, X202107, X202108, X202109, X202110, X202111, X202112)

```


#### CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS

**Inspect the new table that has been created**

```{r eval=FALSE}
colnames(all2021)  #List of column names
nrow(all2021)  #How many rows are in the data frame?
dim(all2021)   #Dimensions of the data frame?
head(all2021)  #See the first 6 rows of the data frame.  Also tail(all2021)
str(all2021)   #See list of columns and data types (numeric, character, etc)
summary(all2021)  #Statistical summary of data. Mainly for numerics
```

**Calculate ride length  and convert it to minutes**

```{r eval=FALSE}

all2021$ride_length <- difftime(all2021$ended_at, all2021$started_at, units = "mins")
```

**Add columns that list the date, month, day, year,  time and hour of each ride**  

```{r eval=FALSE}
all2021$date <- as.Date(all2021$started_at) The default format is yyyy-mm-dd
all2021$day_of_week <-wday(all2021$started_at)  #calculate day of week
all2021$day_of_week <- format(as.Date(all2021$date), "%A") #create new column for day of week
all2021$month <- format(as.Date(all2021$date), "%m") #create new column for month
all2021$day <- format(as.Date(all2021$date), "%d") #create new column for day
all2021$year <- format(as.Date(all2021$date), "%Y") #create new column for day
all2021$time <- format(as.Date(all2021$date), "%H:%M:%S") #format time as HH:MM:SS
all2021$time <- as_hms((all2021$started_at)) #create new column for time
all2021$hour <- hour(all2021$time) #create new column for hour

```

**Inspect the structure of the columns**

```{r eval=FALSE}
str(all2021)
```

**We will create a new version of the dataframe (all2021b) since data is being removed**

```{r eval=FALSE}
all2021b <-all2021
```

#### CLEAN THE DATA

**remove unnecessary data from the environment**
```{r eval=FALSE}
remove (X202101, X202102, X202103, X202104, X202105, X202106, X202107, X202108, X202109, X202110, X202111, X202112)
```

```{r eval=FALSE}
all2021 <- all2021[!(all2021b$ride_length <=0),] #remove all values less than 0 or negative 
all2021<-distinct(all2021b)  #remove duplicated rows
all2021 <- na.omit(all2021b) #remove all NA values 
all2021<-all2021b %>%        #remove unnecessary columns
  select(-c(ride_id, start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng))

View(all2021) #View the table with the changes 
```

#### CONDUCT DESCRIPTIVE ANALYSIS

**Descriptive analysis on ride_length (all figures in minutes)**

```{r eval=FALSE}
mean(all2021b$ride_length) #straight average (total ride length / rides)
median(all2021b$ride_length) #midpoint number in the ascending array of ride lengths
max(all2021b$ride_length) #longest ride
min(all2021b$ride_length) #shortest ride
summary(all2021b$ride_length) #condense the four lines above to one line
```

**Compare members and casual users**

```{r eval=FALSE}
aggregate(all2021$ride_length ~ all2021$member_casual, FUN = mean)
aggregate(all2021$ride_length ~ all2021$member_casual, FUN = median)
aggregate(all2021$ride_length ~ all2021$member_casual, FUN = max)
aggregate(all2021$ride_length ~ all2021$member_casual, FUN = min)
```

**See the average ride time by each day for members vs casual users**
```{r eval=FALSE}
aggregate(all2021$ride_length ~ all2021$member_casual + all2021$day_of_week, FUN = mean)
```

**Notice that the days of the week are out of order. Let's fix that**
```{r eval=FALSE}
all2021$day_of_week <- ordered(all2021$day_of_week, levels=c( "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

```

**Now, let's run the average ride time by each day for members vs casual users**

```{r eval=FALSE}
aggregate(all2021$ride_length ~ all2021$member_casual + all2021$day_of_week, FUN = mean)
```

**Analyze ridership data by type and weekday**
```{r eval=FALSE}
all2021 %>% 
   group_by(member_casual, day-of_week) %>%  #groups by user type and weekday
  summarise(number_of_rides = n()	#calculates the number of rides and average duration 
  ,average_duration = mean(ride_length)) %>% 	# calculates the average duration
  arrange(member_casual, day_of_week)			# sorts
```


#### VISUALIZATIONS


**RIDES BY RIDER TYPE**
```{r}
all2021 %>%                            
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n()) %>%
  arrange(member_casual, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge") +
  labs(x='Day of Week', y='Total Number of Rides', title='Rides per Day of Week', fill = 'User type') +
  scale_y_continuous(breaks = c(200000, 300000, 4000000), labels = c("200K", "300K", "400K"))

```
Casual riders have a longer average ride length of 20+ minutes during the week,  having longer rides during the weekends. Members show a steadier usage of the service throughout the week.



**RIDES BY MONTH**

```{r}
all2021 %>%  
  group_by(member_casual, month) %>%  
  summarise(total_rides = n(),`average_duration_(mins)` = mean(ride_length)) %>%
  arrange(member_casual) %>%
  ggplot(aes(x=month, y=total_rides, fill = member_casual)) + geom_col(position = "dodge") +
  labs(x= "Month", y= "Total Number of Rides", title = "Rides by Month", fill = "Type of User") +
  scale_y_continuous(breaks = c(100000, 200000, 300000, 400000), labels = c("100K", "200K", "300K", "400K")) + theme(axis.text.x = element_text(angle = 45))

```
Summer is the favorite season for both types of users, however the trend is more relevant for casual riders. Members use the service more during the summer and also during early autumn months. Then their activity decreases gradually during the cold months.  Casual users have very little activity during the winter months. 


**POPULAR BIKE TYPE**
 
```{r}
all2021 %>%                            
   group_by(member_casual, rideable_type) %>%
   summarise(number_of_rides = n()) %>%
   arrange(member_casual, rideable_type) %>%
   ggplot(aes(x = rideable_type, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge") +
   labs(x='Type of bike', y='Number of Rentals', title='Popular Bike Ride', fill = 'User of type') +
   scale_y_continuous(breaks = c(500000, 1000000, 1500000, 2000000), labels = c("500K", "1MM", "1.5MM", "2MM"))

````

The preferred type of bike for both users is the Classic bike followed by the electric bike. Docked bikes are the least preferred for both users, especially for members.  


#### SUMMARY OF DATA AND CONCLUSIONS

Casual riders prefer to ride more during the weekends, especially on saturdays.
Casual riders use bikes for leisure and recreation purposes. 
Casual riders use the service for longer time during the week than long-term members.
Summer is the favorite season for both types of users.
The most popular bike for the riders was the classic bike. 
The afternoon is the busiest time for members and casual users.
The peak times for members are before and after working hours.
Annual members use bikes for commuting between work.


#### RECOMMENDATIONS

* Create a Cyclistic app  where users can register and track their activity. This app will generate a profile of each user, collect personal information and obtain data about their habits and preferences.
* Create a reward program within the app in which casual riders can automatically become annual members with a special discount after a certain amount of miles.
* Use the app to Insert ads with  other services and products within the community of the routes most used by the rider to get additional profits from the sponsors. 


Thank you!

