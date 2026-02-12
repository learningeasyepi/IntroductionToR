####Data joins, basic manipulation, and plotting in ggplot

###========Data Joins============
a <- crossing(year = 2010:2020,
                city_name = c("Amsterdam", "Rotterdam")) %>%
              mutate(average_aqi = round(rnorm(22, 40, 5)))

b <- crossing(year = 2005:2015,
              city = c("Amsterdam", "Rotterdam")) %>%
  mutate(asthma_rate = rnorm(22, 8, 2))


###We have a dataset with the average air qaility index in 2 cities over 2010-2020
###and another with the average asthma rate per 100 people in 2 cities over 2005:2015

##We want to join these data together so we can describe the relationship between 
##asthma and aqi

###We can join the data using functions from dplyr (in tidyverse)
##depending on the type of join we use, we keep different amounts of data.
##To join data, we need variables that match in both datasets

###left join: keep all of the data on the left side (the dataset we name first)
##and only keep values of the second dataset that having matching values of our join by variables
left_join_data1 <- left_join(a, b, by = c("city", "year"))
left_join_data1 <- a %>%
  left_join(b)

##note which years are kept
summary(left_join_data1)

###what if we switch the order?
left_join_data2 <- left_join(b,a, by = c("city", "year"))
left_join_data2 <- b %>%
  left_join(a)

##note which years are kept
summary(left_join_data2)

###Right join: keep all of the data on the right side (the second dataset we name)
##and only keep values of the first dataset that have matching values of our join by variables
right_join_data1 <- right_join(a,b, by = c("city_name" = "city", "year"))
right_join_data1 <- a %>%
  right_join(b, by = c("city_name" = "city", "year"))

##note which years are kept
summary(right_join_data1)

###what if we switch the order?
right_join_data2 <- right_join(b,a, by = c("city" = "city_name", "year"))
right_join_data2 <- b %>%
  right_join(a, by = c("city" = "city_name", "year"))

##note which years are kept
summary(right_join_data2)

##Full join: keep all values of each dataset
full_join_data <- a %>%
  full_join(b, by = c("city_name" = "city", "year"))

summary(full_join_data)

###Inner join: keep only values where the joining variables are present in both datasets
inner_join_data <- a %>%
  inner_join(b, by = c("city_name" = "city", "year"))

rm(list=ls())

#####========Practice loading and merging data===================

###Load the mortality data
###data is from CDC wonder and is the number of deaths associated with drug and alcohol
##use in 5 counties from 2018 -2023
mortality <- read_csv("/Users/kellybroen/Documents/R_tutorials/Lesson2/mortality.csv") 

##Load county level social variables 
svi <- readRDS("~/Documents/R_tutorials/Lesson2/svi.rds")

###Join the data so you can look at how mortality and svi are associated 
###Tip: mortality$`County Code` is the same as svi$STCNTY 
data <- mortality %>%
  left_join(svi, by = c("County Code" = "STCNTY"))

summary(data)

###=====Data manipulation with tidyverse==========
###=====Mutate====================================
###Let's make a new variable for the rate of deaths per 100,000
data$mortality_rate <- (data$Deaths / data$Population) * 100000

data <- data %>%
  mutate(mortality_rate = (Deaths / Population) * 100000)

###tapply is a base R function
###We can look at the mortality rate by type of death, county, and year
tapply(data$mortality_rate, data$County, summary)
tapply(data$mortality_rate, data$Year, summary)
tapply(data$mortality_rate, data$`Drug/Alcohol Induced`, summary)

##Multiple stratifications
tapply(data$mortality_rate, list(data$Year, data$`Drug/Alcohol Induced`), mean)
tapply(data$mortality_rate, list(data$County, data$`Drug/Alcohol Induced`), mean)
tapply(data$mortality_rate, list(data$County, data$Year), mean)

tapply(data$mortality_rate, list(data$County,data$Year, data$`Drug/Alcohol Induced`), mean)

##difficult to read when you stratify by too much


###=====Filter function=========
####Let's filter to only drug and alcohol related deaths
unique(data$`Drug/Alcohol Induced`)

daa_data <- data %>%
  filter(`Drug/Alcohol Induced` %in% c("Drug-induced causes",
                                       "Alcohol-induced causes"))
#OR
daa_data <- data %>%
  filter(`Drug/Alcohol Induced` != "All other non-drug and non-alcohol causes")


##we filter with boolean logic statements and keep TRUE values
##different statements: 
#Equals: ==
#Not equal: !=
#Greater than, less than, greater than or equal to, less than or equal to: >, <, >=,<=
#Within a set of values: %in%
#Not within: kind of tricky, the ! goes at the start and then same as within

##In base R:
daa_data <- data[data$`Drug/Alcohol Induced` != "All other non-drug and non-alcohol causes",]

###===========group_by, summarize=========
##let's aggregate the daa data by county and year
##and get the mean mortality rate and svi
agg_data <- daa_data %>%
  group_by(County, Year) %>%
  summarise(mortality_rate = mean(mortality_rate),
            svi = mean(svi))

###Pipes based coding is nice because we could do all of this in one block
##it's clear, and easy to debug
agg_data <- mortality %>%
  left_join(svi, by = c("County Code" = "STCNTY")) %>%
  mutate(mortality_rate = (Deaths / Population) * 100000) %>%
  filter(`Drug/Alcohol Induced` != "All other non-drug and non-alcohol causes") %>%
  group_by(County, Year) %>%
  summarise(mortality_rate = mean(mortality_rate),
            svi = mean(svi))

###=========Plotting in ggplot========
#GG = grammar of graphics

##Histogram
#let's make a histogram of drug and alcohol mortality rate 
ggplot()+ #this command opens the plot, and from here we use + (not pipes %>%)
  geom_histogram(data = agg_data, aes(x = mortality_rate), bins = 15)
##the geom_* determines what type of plot we want to use
#then we say what data we want to use
#aes (short for aesthetics), is where we put the values that change within the dataset
#bins argument is specific for historgrams and says how many bins we should make for our data
#try changing it and see what happens

agg_data %>%
ggplot() +
  geom_point(aes(x = Year, y = mortality_rate, color = County))+
  theme_bw()

##let's make a line plot of the drug/alcohol mortality rate over time per county
ggplot() + ##this line opens the plot
  geom_line(data = agg_data, aes(x = Year, y = mortality_rate, color = County))+
  ##geom_line for a line plot
  ##we're using the aggregated data
  #Since we have multiple counties, we need to specify that or the lines get weird
  theme_classic() + #you can make plots pretty quickly with different themes
  labs(x = "year",
       y = "drug/alcohol associated mortality\nrate per 100,000", ##\n is a linebreak
       color = "County in FL",
       title = "Drug and alcohol mortality rates")#labs are labels

##group data by county and plot percentile  no insurance vs percentile in poverty
data %>%
  group_by(County) %>%
  summarise(prop_no_insurance = first(prop_no_insurance),
            prop_poverty = mean(prop_poverty)) %>%
  ggplot()+
  geom_point(aes(x = prop_no_insurance, y = prop_poverty, color = County)) +
  labs(x = "Percentile no insurance", y = "Percentile poverty") +
  theme_dark()
  




