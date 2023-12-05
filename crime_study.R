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

#Converting to a long df and plotting
plot_shared_dates <- shared_dates %>% gather(key = report, value = count, -Date)

ggplot(plot_shared_dates, aes(x = Date, y = count)) + geom_point()+
  geom_smooth(method = "lm", formula = y~x)

#examining correlation between calls and incidents
##correlation in daily incidents
daily_core <- cor(shared_dates$n_calls, shared_dates$n_incidents)

##correlation in monthly incidents
correlation_df <- shared_dates %>% mutate(Month = month(as_date(Date))) %>% 
  group_by(Month) %>% summarize(n_incidents = sum(n_incidents), n_calls = sum(n_calls))

monthly_cor <- cor(correlation_df$n_calls, correlation_df$n_incidents)

#Subsetting the calls and incidents datasets by the shared dates 
calls_shared_dates <- semi_join(calls, shared_dates, by = "Date")
identical(sort(unique(shared_dates$Date)), sort(unique(calls_shared_dates$Date)))

incidents_shared_dates <- semi_join(incidents, shared_dates, by = "Date")

#Visualizing the top call and incident crime types
plot_calls_freq <- calls_shared_dates %>% count(Descript) %>% top_n(15, n) %>%
  ggplot(aes(x = reorder(Descript, n), y = n)) + geom_bar(stat = "identity") +
  xlab("Crime Description") + ylab("Count") + ggtitle("Calls Reported Crimes")+
  coord_flip()
plot_calls_freq

plot_incidents_freq <- incidents_shared_dates %>% count(Descript) %>% top_n(15, n) %>%
  ggplot(aes(x = reorder(Descript, n), y = n)) + geom_bar(stat = "identity") +
  xlab("Crime description") + ylab("Count") + ggtitle("Incident Reported Crimes") +
  coord_flip() + theme_minimal()
plot_incidents_freq











