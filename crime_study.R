#loading libraries
library(tidyverse)
library(lubridate)

#reading in the incidents and calls datasets
calls <- read_csv("https://raw.githubusercontent.com/veeralakrishna/Datacamp-Project-Solutions-R/master/Data%20Science%20for%20Social%20Good_%20Crime%20Study/datasets/downsample_police-department-calls-for-service.csv")
incidents <- read_csv("https://raw.githubusercontent.com/kchoward48/DataCamp_Projects_in_R/main/downsample_police-department-incidents.csv")

#glimpse the datasets
glimpse(calls)
glimpse(incidents)

#Aggregating the reported incidents by date
daily_incidents <- incidents %>% count(Date, sort = TRUE) %>% 
  rename(n_incidents = n)

#Aggregating calls to police by date
daily_calls <- calls %>% count(Date, sort = TRUE) %>% 
  rename(n_calls = n)

#joining the datasets to examine civilian-reported and police-reported incidents
shared_dates <- inner_join(daily_calls, daily_incidents, by = "Date")

