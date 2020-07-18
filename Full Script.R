#install and run library tidyverse
install.packages("tidyverse")
library(tidyverse)

#load data file and save to data_sheet1 and data_sheet2
data_sheet1 <- read_excel("Desktop/R Project/ProjectMove/ProjectMove.xlsx", sheet = "2019")
data_sheet2 <- read_excel("Desktop/R Project/ProjectMove/ProjectMove.xlsx", sheet = "2020")
View(data_sheet1)
View(data_sheet2)

#select variable and bind rows from 2 data sheets, save into main_data
main_data <- rbind(data_sheet1[, c("job_type", "job_date")], data_sheet2[, c("job_type", "job_date")])

View(main_data)
head(main_data)
glimpse(main_data)
table(main_data$job_type)

#create read-able data table (sanitize data)
#clean service category data
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

main_data_cleaned <- main_data %>% 
  mutate(job_date = date(job_date), year = year(job_date), month = month(job_date, label = TRUE)) %>%
  mutate(job_type = str_to_lower(job_type)) %>%
  mutate(job_type = str_replace_all(job_type, c("disposal" = "move-out export", 
                                                "local/ storage" = "move-out export",
                                                "move-in local" = "local move",
                                                "move-out local" = "local move", 
                                                "move-in import" = "inbound",
                                                "move-out export" = "outbound")))

View(main_data_cleaned)
head(main_data_cleaned)
glimpse(main_data_cleaned)

#sanity check
unique(main_data_cleaned$year)
unique(main_data_cleaned$month)
table(main_data_cleaned$year)
table(main_data_cleaned$month)

#create granular data table (taming and tidying)
main_data_ready <- main_data_cleaned %>%
  unite(month, year, col = "service_month", sep = "-") %>%
  mutate(service_month = factor(service_month, levels = c("Sep-2019",
                                                          "Oct-2019", 
                                                          "Nov-2019", 
                                                          "Dec-2019", 
                                                          "Jan-2020", 
                                                          "Feb-2020", 
                                                          "Mar-2020", 
                                                          "Apr-2020",
                                                          "May-2020", 
                                                          "Jun-2020",
                                                          "Jul-2020",
                                                          "Aug-2020", 
                                                          "Sep-2020"), order = T)) %>%
  mutate(local_move_job = ifelse(job_type == "local move", 1, 0), 
         inbound_job = ifelse(job_type == "inbound", 1, 0), 
         outbound_job = ifelse(job_type == "outbound", 1, 0))

View(main_data_ready)

#create summary data
plot_data <- main_data_ready %>% 
            filter(job_date >= "2019-12-01" & job_date <= "2020-06-30") %>%
            group_by(service_month) %>%
            summarise(inbound_job = sum(inbound_job), 
            outbound_job = sum(outbound_job), 
            total = sum(inbound_job + outbound_job))

plot_data

#create visualization
library(ggplot2)

plot_data %>% 
  gather("inbound_job", "outbound_job", key = "jobs", value = "total") %>%
  ggplot(aes(service_month, total, fill = jobs)) +
  geom_col()

