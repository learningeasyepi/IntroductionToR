###DATES <3
###And points
library(tidyverse)
library(lubridate)
library(mgcv)
library(splines)

##Another data type (like numeric or character) is date
##Dates have special properties to allow for functions to calculate time
##But, like in excel, dates can be absolute nightmares



##Important thing for my europeans to remember: R was made in the US
##And we do dates stupid
##Generally: Month - Day - Year
char_date <- c("10-1-2023", "8-2-2021", "9-24-2019")
class(char_date)

date <- as_date(char_date, format = "%m-%d-%Y")
class(date)

###Dates can have different formats
char_date <- c("8/12/2023", "1/10/2021", "1/4/2019")
class(char_date)

date <- as_date(char_date, format = "%d/%m/%Y")
class(date)

#####
date - mdy("1-1-2010")
difftime(date, mdy("1-1-2010"), unit = "weeks")
difftime(date, mdy("1-1-2010"), unit = "mins")
difftime(date, mdy("1-1-2010"), unit = "secs")


###You can enter date/time in numbers
dt <- as_datetime(1511870400) ##Time began on January 1, 1970 at midnight; how many days since then
dt

##We can have dates at day, month, year, but we can also have the exact time
##All times are in UTC (GMT, the time zone of England)
d <- as_date(17498)
d

##negative numbers subtract from January 1, 1970
d <- as_date(-1600)
d

##we can round dates or floor them 
d <- as_date(seq(17498,17998,by = 10) )
summary(d)

##Floor rounds to the first day of the month, which is useful if you want to aggregate to 
##the month the observation is in
floor_date(d, unit = "month")

##Rounding makes it to the nearest first of the month
round_date(d, unit = "month")

##Then can convert back to numbers
as.numeric(d)

###We can plot with dates

#Randomly generate something associated with date
y <- 20 + cos(month(d))*2 + rnorm(51, 0, 1)

ggplot() +
  geom_point(aes(x = d, y = y))

###So we can floor to month and average over months
data.frame(d,y) %>%
  mutate(m = floor_date(d, unit = "month")) %>%
  group_by(m) %>%
  summarise(y = mean(y)) %>%
  ggplot()+
  geom_point(aes(x = m, y= y))

###can also extract parts of the date
day(d)
month(d)
year(d)


####----------Modeling seasonal data-------------

###Generating some random seasonal data (this just makes the data, fine if you don't understand)
data <- data.frame(date = seq(ymd("2010-01-01"),
                            ymd("2013-12-01"),
                            by = "month")) %>%
  mutate(y = year(date) - 2010,
         month = month(date),
         aqi = (50 + cos(m +2) * 10) + rnorm(48, 0, 3) + y * 3)

m1 <- gam(aqi ~ y + s(m, bs = "cc"),
    data = data,
    method = "REML",
    family = "gaussian")

summary(m1)
plot(m1)

###We get the
coef(m1)


data <- crossing(date = seq(ymd("2010-01-01"),
                              ymd("2013-12-01"),
                              by = "month"),
                 id = factor(1:100)) %>%
  arrange(id, date)%>%
  mutate(u = sort(rep(rnorm(100,0, 10), 48)),
         y = year(date) - 2010,
         month = month(date),
         weight = 85 + sin(m/2)*2 + 2*y + u + rnorm(4800, 0, 10))  

data %>%
  ggplot()+
  geom_point(aes(x = month, y= weight, color = id))

m2 <- gam(weight ~ y + s(month, bs = "cc", k = 12) + s(id, bs = "re"),
          data = data, 
          method = "REML")

summary(m2)

##Edf = effective degrees of freedom
##edf = 1 means linear but as edf increases, relationship gets wigglier
##Deviance explained = proportion of the variation explained by the covariates (higher is better, like R2)
##Significance is the F/chi.sq statistics where we get out p value from

gam.check(m2)
plot(m2)
plot(m2, pages=1, shade=TRUE)


new_data <- crossing(month = 1:12, 
                     y = 0:3)


new_data$p <- predict(m2, new_data, type = "response", exclude = c("s(id)"), newdata.guaranteed = T)
new_data %>%
  mutate(date = ymd(paste(y+2010, month, 1, "-"))) %>%
  ggplot()+
  geom_line(aes(x = date, y = p))

             