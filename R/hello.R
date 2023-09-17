# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

Q2_2019 <-  read_csv ("/Users/adityasahu/Desktop/Google Data Analytics/csv file/Divvy_Trips_2019_Q2.csv")

Q3_2019 <- read_csv ("/Users/adityasahu/Desktop/Google Data Analytics/csv file/Divvy_Trips_2019_Q3.csv")
Q4_2019 <- read_csv ("/Users/adityasahu/Desktop/Google Data Analytics/csv file/Divvy_Trips_2019_Q4.csv")
Q1_2020 <- read_csv ("/Users/adityasahu/Desktop/Google Data Analytics/csv file/Divvy_Trips_2020_Q1.csv")

colnames(Q1_2020)
colnames(Q2_2019)
colnames(Q3_2019)
colnames(Q4_2019)
(Q4_19_rename <- rename (Q4_2019
                         ,ride_id = trip_id
                         ,rideable_type = bikeid
                         ,started_at = start_time
                         ,ended_at = end_time
                         ,start_station_name = from_station_name
                         ,start_station_id = from_station_id
                         ,end_station_name = to_station_name
                         ,end_station_id = to_station_id
                         ,member_casual = usertype))
(Q3_19_rename <- rename (Q3_2019
                         ,ride_id = trip_id
                         ,rideable_type = bikeid
                         ,started_at = start_time
                         ,ended_at = end_time
                         ,start_station_name = from_station_name
                         ,start_station_id = from_station_id
                         ,end_station_name = to_station_name
                         ,end_station_id = to_station_id
                         ,member_casual = usertype))
(Q2_19_rename <- rename (Q2_2019,ride_id = "01 - Rental Details Rental ID",rideable_type = "01 - Rental Details Bike ID",started_at = "01 - Rental Details Local Start Time",ended_at = "01 - Rental Details Local End Time",start_station_name = "03 - Rental Start Station Name",start_station_id = "03 - Rental Start Station ID",end_station_name = "02 - Rental End Station Name",end_station_id = "02 - Rental End Station ID",member_casual = "User Type"))
str (Q1_2020)
str (Q2_19_rename)
str (Q3_19_rename)
str (Q4_19_rename)
q4_2019_mutate <- mutate (Q4_19_rename, ride_id= as.character(ride_id) ,rideable_type= as.character(rideable_type))
q3_2019_mutate <- mutate (Q3_19_rename, ride_id= as.character(ride_id) ,rideable_type = as.character(rideable_type))
q2_2019_mutate <- mutate (Q2_19_rename, ride_id= as.character(ride_id) ,rideable_type= as.character(rideable_type))

all_trips <- bind_rows(Q1_2020,q4_2019_mutate, q3_2019_mutate, q2_2019_mutate)

alltrips <- all_trips %>% select(-c(tripduration, gender, birthyear, "01 - Rental Details Duration In Seconds Uncapped", "Member Gender", "05 - Member Details Member Birthday Year"))
colnames(alltrips)
nrow(alltrips)
dim(alltrips)
str(alltrips)
head(alltrips)
table(alltrips$member_casual)

alltrips <- alltrips %>% mutate(member_casual= recode(member_casual, "Subscriber" = "member", "Customer" = "casual"))
table (alltrips$member_casual)

alltrips_ridelength <- alltrips %>% mutate(ride_length = difftime(ended_at, started_at))
str(alltrips_ridelength)

alltrips_ridelength$date <- as.Date(alltrips_ridelength$started_at)
alltrips_ridelength <- alltrips_ridelength[!(alltrips_ridelength$ride_length<=0),]
mean(alltrips_ridelength$ride_length)

median (alltrips_ridelength$ride_length)

min(alltrips_ridelength$ride_length)
max (alltrips_ridelength$ride_length)

comparemean <- alltrips_ridelength %>%
  group_by(member_casual) %>%
  summarise(count = n(), meanlength = mean(ride_length))

comparemedian <- alltrips_ridelength %>%
  group_by(member_casual) %>%
  summarise ("count"=n(), medianlength= median(ride_length))

comparemax <- alltrips_ridelength %>%
  group_by(member_casual) %>%
  summarise ("count"=n(), maxlength= max(ride_length))

compareday <- alltrips_ridelength %>%
  group_by(date) %>%
  summarise (daylength= mean(ride_length))

type_weekday <- alltrips_ridelength %>%
  mutate(weekday=wday(started_at,label= TRUE)) %>%
  group_by (member_casual,weekday) %>%
  summarise(number_of_rides=n(), avg_duration= mean(ride_length))

alltrips<-alltrips_ridelength %>%
  mutate (weekday=wday(started_at,label = TRUE))
compareday<-compareday %>%
  mutate (weekday=wday(date, label=TRUE))

mean_max<-inner_join (comparemean, comparemax, comparemedian, by="member_casual")
mean_max_median<-inner_join(mean_max, comparemedian, by="member_casual")

mean_max_median<-mean_max_median %>%
  select (-c (count.y))

number_of_rides<-alltrips_ridelength %>%
  mutate(weekday=wday (date, label= TRUE)) %>%
  group_by (member_casual, weekday) %>%
  summarise (number_of_rides = n(), average_duration = mean(ride_length))

#Ride duration by weekday, sorted by rider type
ggplot (alltrips, aes(ride_length, weekday, color=member_casual))+geom_jitter ()+
  labs (x="Ride Length", y="Day of the Week",title = "Ride Duration by Day of the Week")
#Insights
#Casual riders have the longest ride length and with the longest length on Thursday
#Member have the longest length on Tuesdays

ggplot (type_weekday, aes(x=member_casual, y=number_of_rides, fill=weekday))+geom_bar(stat="identity",position=position_dodge())+
  labs(x="Member Type", y="Number of Rides", title = "Number of Rides by Rider Type")
#Insights
# Member riders have the highest number of rides in total
# Most active day for members is Tuesday while most active day for casual riders is Sunday.

ggplot (type_weekday, aes (x=member_casual,y=avg_duration, fill=member_casual))+
  geom_bar (stat="identity") +labs(x= "Member Type" , y="Average Duration", title="Average Ride Duration")
#Insights
#Casual member have the highest ride duration

ggplot (type_weekday, aes (y=number_of_rides , group=weekday, color=weekday))+
  geom_boxplot()+labs (y="Number fo Rides", title="Average Active day for Riders")
#Insights
#The average active day for riding is Tuesday, which is logical since members who have the highest number of rides are most active on Tuesdavs










