#setwd("C:/Users/21179808/github_projects/R/Data_Analytics/covid-19_south_sfrica")

library(tidyverse)
library(ggplot2)
library(cowplot)
library(dplyr)
library(lubridate)
library(plyr)
library(rvest)

raw_data <- read.csv("time_series_covid19_confirmed_global.csv", sep = ",", header = T)

southafrica_df <- filter(raw_data, Country.Region == "South Africa")
southafrica_df <- southafrica_df[, -c(1:4)]

southafrica_df <- southafrica_df %>%
  gather(key = "Date",
         value = "Confirmed_cases")

southafrica_df$Date <- substr(southafrica_df$Date, 2, 9)
southafrica_df$Date <- gsub("\\.", "-", southafrica_df$Date)
southafrica_df$Date <- as.Date(southafrica_df$Date, format="%m-%d-%y")
southafrica_df$Date2 <- as.character(southafrica_df$Date)
southafrica_df$Year <- substr(southafrica_df$Date2, start = 1, stop = 4)
southafrica_df$Month <- substr(southafrica_df$Date2, start = 6, stop = 7)
southafrica_df$Day <- substr(southafrica_df$Date2, start = 9, stop = 10)

southafrica_df <- southafrica_df %>% 
  mutate(Month_name = case_when(startsWith(Month, "01") ~ "January", 
                                startsWith(Month, "02") ~ "February",
                                startsWith(Month, "03") ~ "March",
                                startsWith(Month, "04") ~ "April",
                                startsWith(Month, "05") ~ "May",
                                startsWith(Month, "06") ~ "June",
                                startsWith(Month, "07") ~ "July",
                                startsWith(Month, "08") ~ "August",
                                startsWith(Month, "09") ~ "Sepember",
                                startsWith(Month, "10") ~ "October",
                                startsWith(Month, "11") ~ "November",
                                startsWith(Month, "12") ~ "December"))


southafrica_df<- southafrica_df%>% 
  mutate(Alert_level = case_when(Date2 < "2020-03-26" ~ "No Lockdown",
                           Date2 >= "2020-03-26" & Date2 <= "2020-04-30" ~ "Level 5", 
                           Date2 >= "2020-06-01" & Date2 <= "2020-08-17" ~ "Level 3", 
                           Date2 >= "2020-08-18" ~ "Level 2", 
                           Date2 >= "2020-09-21" & Date2 <= "2020-12-28" ~ "Level 1",
                           Date2 >= "2020-12-29" & Date2 <= "2021-02-28" ~ "Adjusted Level 3",
                           Date2 >= "2021-03-01" & Date2 <= "2021-05-30" ~ "Adjusted Level 1",
                           Date2 >= "2021-05-31" & Date2 <= "2021-06-15" ~ "Adjusted Level 2",
                           Date2 >= "2021-06-16" & Date2 <= "2021-06-27" ~ "Adjusted Level 3",
                           Date2 >= "2021-06-28" & Date2 <= "2021-07-25" ~ "Adjusted Level 4",
                           Date2 >= "2021-07-26" & Date2 <= "2020-09-12" ~ "Adjusted Level 3",
                           Date2 >= "2021-09-13" & Date2 <= "2021-09-30" ~ "Adjusted Level 2",
                           Date2 >= "2021-10-01" ~ "Adjusted Level 1",
                           TRUE ~ "Unspecified lockdown"))

attach(southafrica_df)

ggplot(southafrica_df, aes(Date, Confirmed_cases)) +
  geom_line()

ggplot(southafrica_df, aes(Date, Confirmed_cases)) +
  geom_bar(stat="identity", position = "dodge")

link = "https://www.gov.za/covid-19/about/about-alert-system"
page = read_html(link)

lockdowns_alerts = page %>% html_nodes("p:nth-child(3) , p:nth-child(4) , p:nth-child(5) , p:nth-child(6) , p:nth-child(7) , p:nth-child(8) , p:nth-child(9) , p:nth-child(10) , p:nth-child(11) , p:nth-child(12) , p:nth-child(13) , p:nth-child(14) , p:nth-child(16)") %>% html_text

Alert_level <- c("alert level 5", "alert level 3", "alert level 2", "alert level 1", "alert level 3", " alert level 1", "alert level 2", "alert level 4", "alert level 3", "alert level 4", " alert level 3", "alert level 2", "alert level 1")

Start_date <- c("26 March 2020", "01 June 2020", "18 August 2020", "21 September 2020", "29 December 2020", "01 March 2021", "31 May 2021", "16 June 2021", "27 June 2021", "28 June 2021", "26 July 2021", "13 September 2021", "01 October 2021")

