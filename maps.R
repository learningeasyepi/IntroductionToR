####MAPS!!!!!
library(sf)
library(tidyverse)
library(usdata)
library(tigris)

###Aerial data
shp <- tigris::counties("FL")

fl <- county %>%
  filter(state == "Florida") %>% 
  mutate(NAME = str_remove(name, " County")) %>%
  left_join(shp, by = "NAME") %>%
  st_as_sf()

fl$unemployment_rate
fl %>%
  ggplot() +
  geom_sf(aes(fill = unemployment_rate), lwd = NA)+
  theme_void() +
  scale_fill_viridis_c() +
  labs(fill = "Unemployment rate (2010)")

##Aerial data: we have a predefined unit (county, catchment area, etc)
##and we observe a variable there, usually aggregated
##We can't measure unemployment rate at the individual level, it is defined over groups

##Geostatistical data
data(us_temp)

temp <- us_temp %>%
  filter(year == 2022)

us_states <- states(cb = TRUE) |>
  filter(! STATEFP %in% c("78", "72", "60","66","69", "02", "15"))

temp %>%
  mutate(tmax_c = (tmax - 32) * (5/9)) %>%
  ggplot()+
  geom_sf(data = us_states, fill = NA)+
  geom_point(aes(x = longitude, y = latitude, color = tmax_c)) +
  theme_void()

###These points are weather stations. We have points where we ALWAYS take measurements

###Point patterns
require(spatstat)
data(gorillas)

gorillas <- st_as_sf(gorillas)

gorillas %>%
  ggplot()+
  geom_sf(aes(color = group)) +us_states <- states(cb = TRUE) |>
  shift_geometry()
  theme_void()

##This data is where gorilla nests were spotted, within a catchment area,
#and which gorilla group they were associated with.
##we can also do this with households and illness, where you only report IF a person has the disease
##this is what passive spatial surveillance looks like 

  
###When you add a time component, you get spatiotemporal data!
  class(gorillas$date)
  gorillas %>%
    drop_na(group) %>%
    mutate(year = year(date))%>%
    ggplot()+
    geom_sf(aes(color = group)) +
    theme_void()+
    facet_wrap(~year)
  
##The gorillas dataset is a really famous example dataset. You can model the change 
  #in groups over time 
  
###data usually comes pretty messy and there are a few useful packages
#install.packages(geodata)  
library(geodata)

dutch_states <- gadm(country = "NLD", level = 1) %>%
  st_as_sf()

pop_data <- data.frame(NAME = dutch_states$NAME_1,
                       population = c(502000,
                                      445000,
                                      659000,
                                      2133708,
                                      596000,
                                      0,
                                      1120000,
                                      2626000,
                                      2952000,
                                      1160000,
                                      1387000,
                                      391000,
                                      0,0))

dutch_pop <- dutch_states %>%
  left_join(pop_data,by = c("NAME_1" = "NAME"))

dutch_pop %>%
  ggplot() +
  geom_sf(aes(fill = population), lwd= NA)+
  theme_void()+
  scale_fill_viridis_c()

####Converting data frames into spatial data types
##randomly generate 50 points in north holland and pretend they are hospitals
hospitals <- dutch_states %>%
  st_sample(50, type = "random") 
hospitals <- st_coordinates(hospitals)%>%
  as.data.frame() %>%
  mutate(X = round(X,2),
         Y = round(Y,2))

h_sf <- hospitals %>%
  st_as_sf(coords = c("X", "Y"), crs =4326)

ggplot() +
  geom_sf(data = dutch_states, fill = NA)+
  geom_sf(data = h_sf) +
  theme_void()

###a little geostatistics
##how close is the nearest hospital?
##get the first nearest neighbor and return the distance for each point
library(nngeo)
#install.packages("nngeo")
nn <- st_nn(x = h_sf, y = h_sf, k = 2, returnDist = TRUE)
unlist(nn[[2]])
       