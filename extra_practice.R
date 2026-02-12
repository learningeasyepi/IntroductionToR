####=======EXTRA PRACTICE================

###load the data set crash_data and Precipitation
###Crash data has the number of car crashes in multiple Maryland counties by month
###For 2018. Precipitation is the average monthly rainfall per county over
###the same period

Precipitation <- read_csv("Lesson2/Precipitation.csv")
crash_data <- readRDS("~/Documents/R_tutorials/Lesson2/crash_data.RDS")

###Merge the data together by county and month variables
data <- crash_data %>%
  inner_join(Precipitation, by = c("month", "County_name" = "County")) 


#Make a new variable for car crashes per 100000 people
data <- data %>%
  mutate(accident_rate = (accident_count / Population)*100000)

##What month+county combo has the highest accident rate?
data %>%
  arrange(desc(accident_rate)) %>%
  head(1)

##What is the average accident rate by month for all counties?
tapply(data$accident_rate, data$month, summary)

#by county?
tapply(data$accident_rate, data$County_name, summary)

##Group the data by county to get the yearly average accident rate
agg_data <- data %>%
  group_by(County_name) %>%
  summarise(pop = mean(Population),
            accident_count = sum(accident_count)) %>%
  mutate(rate = (accident_count / pop)*100000)


###Make a line plot of accident rate by month for all 13 counties
data %>%
  ggplot()+
  geom_line(aes(x = month, y = accident_rate, group= County_name, color = County_name))+
  theme_bw()+
  labs(x = "Month", y = "Car accident rate per 100000")


###Get rid of the county that is making the plot weird
data2 <- data %>%
  filter(County_name != "MONTGOMERY")

data2 %>%
  ggplot()+
  geom_line(aes(x = month, y = accident_rate, group= County_name, color = County_name))+
  theme_bw()+
  labs(x = "Month", y = "Car accident rate per 100000")

##Try changing the colors of the lines


###Plot the accident rate againt precipitation as point
data2 %>%
  ggplot()+
  geom_point(aes(x = prec_in, y = accident_rate, group= County_name, color = County_name))+
  theme_bw()

##Practice making the labels accurate



