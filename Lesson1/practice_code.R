##=====R Practice=======
##======February 6, 2026

###R data types and structures
#numeric: (10.5, 55, 787) - Numeric means it is continuous (decimal points0)
numeric_vector <- c(1.1, 2.3, 5.6232)
class(numeric_vector)

#integer: (1L, 55L, 100L, where the letter "L" declares this as an integer) - Integers are whole numbers (no decimal points)
integer_vector <- c(1L, 4L, 5302L)
class(integer_vector)

##R is good at switching between integers and numeric but sometimes it's important to distinguish

#complex: (9 + 3i, where "i" is the imaginary part)
complex_vector <- c(3i, 7, 19)
class(complex_vector) ##even if there's only 1 complex, the vector is complex 

#character (a.k.a. string): ("k", "R is exciting", "FALSE", "11.5")
character_vector <- c("yellow", "green", "blue")
class(character_vector)

#subset of character - factor: ("high", "medium", "low"); adds ordering to make them ordinal vs nominal
factor_vector <- factor(c("High", "Medium", "Low"))
class(factor_vector)
summary(factor_vector)
##When you don't specify the levels, it uses alphabetical order

factor_vector_levels <- factor(c("High", "Medium", "Low"), levels = c("High", "Medium", "Low"))
summary(factor_vector_levels)
class(factor_vector_levels)

#logical (a.k.a. boolean) - (TRUE or FALSE)
boolean_vector <- c(F, T, FALSE) #You can abbreviate TRUE and FALSE as T and F
class(boolean_vector)

###We can link all of our vectors in dataframes
dataframe <- data.frame(numeric_vector, integer_vector, complex_vector,
                        factor_vector, factor_vector_levels, boolean_vector)



summary(dataframe)
class(dataframe)

##We call variables from dataframes with $
dataframe$numeric_vector

#We can also link vectors in a matrix
mat <- as.matrix(cbind(factor_vector, numeric_vector))
#We can index matrices and data frames
mat[,1]
mat[1,]
##Try changing the numbers and see what shows up
mat
mat[2][1]

##can also do that with dataframes
dataframe
dataframe[,1]
dataframe[1,]
## when the comma comes first, you go down the column
## when the comma is after the number, you go across the row

#Other important data structures: lists, arrays, and more package specific types
#spatial data specifically has a bunch more (rasters, sf, ppp, etc.)

###Loading data
###You should have downloaded the 3 datasets shared with you
###You can load some data manually
###But for ease rerunning code, sharing, and reproducibility, it's good to include the code

##You'll need 2 packages: readr and readxl

##Let's load them manually 

##Now with code
csv <- read_csv("Data/tb_data.csv")
xls <- read_xlsx("Data/tb_data.xlsx")
rds <- readRDS("Data/tb_data.RDS") #RDS is an R specific file type and does not require a package
##Sometimes csvs and excel files add a weird index variable. 


##The packages haven and foreign have nice functions for loading other data types
#from stata (.dta), SPSS (.sav), and SAS (.sas7bdat)

##Spatial data has specific packages like sf, sp, raster, and terra (+ more)

#=====Basic data manipulation=========
##We use <- instead of = 
x <- 5
y <- 12

w <- c(1,2,3,4)
z <- c(3,4,5,6)

##addition/subtraction
x + y 
w - z

##division/multiplication
x * y
z / w

##square roots / exponentiation
x ^ 2
y ^ (1/2)

w ^ 2
z ^ (1/3)

##Log and e
##logarithms are base e
log(x)
exp(w)

##Use parenthesis to make sure order of operations matches what you want
##PEMDAS!
(x + (y ^ (1/2))) / y

###======Enter the tidyverse==========
require(tidyverse)

data <- readRDS("Data/tb_data.RDS")

##Base R
head(data) ##Function opens, put in the data

##tidyverse
data %>% ##tell R the data
  head() ##apply the function

##Not always necessary, sometimes base R is faster/easier
colnames(data)
summary(data)

data %>%
  colnames()
data %>%
  summary()

##but it can make a huge difference when you're applying a lot of data manipulations at once

#Let's rename our variables to things that make more sense
##https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-11-11/readme.md
##the code book is here to understand what the variables mean. 
##I want to keep the variables for: country, WHO region, year, case fatality rate, 
##Estimated number of incident cases (all forms), Estimated number of deaths from TB (all forms)
##and Estimated total population number
##And I want to rename them

##making a new dataset so we don't overwrite the old one and have
#to reload it if we mess up
data2 <- data %>% ##I want to apply my function to this dataset
  select(country,
         year = year,
         who_region = g_whoregion,
         estimate_incidence_all = e_inc_num,
         estimate_deaths_all = e_mort_num,
         population = e_pop_num) ##and here are the functions
##select is a tidyverse command to keep only certain variables
##you can also rename variables in select, but you can't do any other functions (like add or divide)

colnames(data2)
summary(data2)


##What years are in our dataset?
unique(data2$year)
range(data2$year)

##I want to calculate the number of deaths and incidence per 100,000 people 
data2 <- data2 %>%
  mutate(deaths_per_pop = estimate_deaths_all / population,
         inc_per_pop = estimate_incidence_all / population)

summary(data2)

###It looks like we have some NAs in some variables
mean(data2$estimate_deaths_all)
mean(data2$estimate_deaths_all, na.rm =T) ##R defaults to showing NA if you apply a function to a vector where any values are missing

is.na(data2$estimate_deaths_all)
sum(is.na(data2$estimate_deaths_all))
##We can do math on booleans, TRUE = 1 and FALSE = 0
mean(is.na(data2$estimate_deaths_all))

##What country(ies) have missing values?
data2 %>%
  filter(is.na(estimate_deaths_all)) %>% ##Filter requires a boolean value 
  ##Gets rid of FALSE values
  ##Here we say, keep only the rows where this variable is NA
  select(country) %>% ##Keep only the country variable
  unique() ##Get the unique value for that variable

###Ok, let's just get rid of that country then

data3 <- data2 %>%
  filter(country != "Democratic People's Republic of Korea")

data3 <- data2 %>%
  drop_na(estimate_deaths_all)

data3 <- data2 %>%
  filter(!is.na(estimate_deaths_all)) 
## a ! in front of a command means opposite, or negative

mean(is.na(data2$estimate_deaths_all))
mean(!is.na(data2$estimate_deaths_all))


###Grouping data
##Our data is per year, per country, per WHO region
##What if we want to compare changes by year by WHO region?

agg_data <- data3 %>%
  group_by(year, who_region) %>% ##the group by command tells R what groups we want to summarize by
  summarise(deaths_per_pop = mean(deaths_per_pop),
            inc_per_pop = mean(inc_per_pop)) ##We tell R how we want to summarize the data

agg_data2 <- data3 %>%
  group_by(year, who_region) %>% ##the group by command tells R what groups we want to summarize by
  summarise(estimate_incidence_all = sum(estimate_incidence_all),
            estimate_deaths_all = sum(estimate_deaths_all),
            population = sum(population)) %>% ##We tell R how we want to summarize the data
  mutate(inc_per_pop = (estimate_incidence_all / population) *100000,
         deaths_per_pop = (estimate_deaths_all / population) *100000)


summary(agg_data)
summary(agg_data2)

###Notice that if we take the mean of the rate, we get a different
##value than if we sum everything and divide again

##VERY IMPORTANT TO KNOW WHAT YOUR CODE IS DOING


##Let's look at just the Africa region
agg2023 <- agg_data2 %>%
  filter(who_region == "Africa")
##we filter with logic statements
# == means equal
# != not equal
# >, < greater than, less than
# => , =< greater than or equal to, less than or equal to


##Let's look at the incidence per 100,000 over time

##Base R
plot(agg2023$year, agg2023$inc_per_pop)
##What about raw case numbers?
plot(agg2023$year, agg2023$estimate_incidence_all)


##I hate base R for plotting. GGplot is part of the tidyverse and is so much nicer 
agg2023 %>% #tell the function what data to use
  ggplot() + #now start the plot. From here in, we use + instead of piping (%>%)
  geom_point(aes(x = year, y = inc_per_pop, color = who_region), size =  2) + #we want points so we use geom_point
#aes = aesthetics, tell it what variables should be "dynamic"
#outside aes, variables are static
  theme_bw() +#Different themes make the plot background different. BW makes it black and white, which I think is nicer to look at
  labs(x = "Year",
       y = "TB incidence per 100,000",
       color = "WHO region",
       title = "TB incidence per 100,000 over\ntime in the WHO Africa region")


##we can plot multiple regions at once with different colors
agg_data %>% #tell the function what data to use
  ggplot() + #now start the plot. From here in, we use + instead of piping (%>%)
  geom_line(aes(x = year, y = inc_per_pop, color = who_region)) + #we want lines so we use geom_line
  #aes = aesthetics, tell it what variables should be "dynamic"
  #outside aes, variables are static
  theme_bw() +#Different themes make the plot background different. BW makes it black and white, which I think is nicer to look at
  labs(x = "Year",
       y = "TB incidence per 100,000",
       color = "WHO region",
       title = "TB incidence per 100,000 over\ntime in the WHO Africa region")


##or each region can have its own box
agg_data %>% #tell the function what data to use
  ggplot() + #now start the plot. From here in, we use + instead of piping (%>%)
  geom_line(aes(x = year, y = inc_per_pop)) + #we want lines so we use geom_line
  #aes = aesthetics, tell it what variables should be "dynamic"
  #outside aes, variables are static
  theme_bw() +#Different themes make the plot background different. BW makes it black and white, which I think is nicer to look at
  labs(x = "Year",
       y = "TB incidence per 100,000",
       title = "TB incidence per 100,000 over\ntime in the WHO Africa region")+
  facet_wrap(~who_region)

###Challenge problem: Use this new dataset
challenge_data <- data %>%
  filter(country %in% c("South Africa",
                        "Bangladesh",
                        "Zambia",
                        "Peru", 
                        "Oman")) %>%
  select(country, year,
         tb_deaths_HIVpos = e_mort_tbhiv_num,
         population = e_pop_num)



##1. Find the average TB deaths with HIV per 100,000 people in South Africa in 2015 - 2019
##2. And plot this mortality rate in South Africa from 2000-2023
##3. Compare with the TB+HIV mortality in Zambia over the same time






















#Challenge solution:
challenge_data %>%
  mutate(mort_rate = (tb_deaths_HIVpos / population)*100000) %>%
  filter(country == "South Africa" & 
           year > 2014 & year < 2020) %>%
  group_by(country) %>%
  summarise(mort_rate = mean(mort_rate))


##For an extra challenge, compare with the TB+HIV mortality in Zambia over the same time

challenge_data %>%
  mutate(mort_rate = (tb_deaths_HIVpos / population)*100000) %>%
  filter((country %in% c("South Africa",
                         "Zambia")) & 
           year > 2014 & year < 2020) %>%
  group_by(country) %>%
  summarise(mort_rate = mean(mort_rate))

challenge_data %>%
  mutate(mort_rate = (tb_deaths_HIVpos / population)*100000) %>%
  filter((country %in% c("South Africa",
                         "Zambia")) &
           year %in% c(2015:2019))%>%
  ggplot() +
  geom_line(aes(x = year, y = mort_rate, color = country )) +
  theme_bw() +
  labs(x = "Year",
       y = "TB+HIV deaths per 100,000",
       color = "country",
       title = "TB+HIV mortality rate in South Africa over time")



