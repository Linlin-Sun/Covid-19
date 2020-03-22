## ---- message = FALSE, results = 'hide'---------------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(matrixStats)
library(kableExtra)
options(digits=2)


## ---- message = FALSE, results = 'hide'---------------------------------------------------------------------------------------
path <- getwd()
covid_Confirmed_ts <- "data/covid_19_2020_03_21_Confirmed_TS.csv"
covid_Deaths_ts <- "data/covid_19_2020_03_21_Deaths_TS.csv"
covid_Recovered_ts <- "data/covid_19_2020_03_21_Recovered_TS.csv"
c_ts <- read.csv(paste(path, covid_Confirmed_ts, sep = "/"), header = TRUE)
c_ts_col_count <- length(colnames(c_ts))
d_ts <- read.csv(paste(path, covid_Deaths_ts, sep = "/"), header = TRUE)
d_ts_col_count <- length(colnames(d_ts))
r_ts <- read.csv(paste(path, covid_Recovered_ts, sep = "/"), header = TRUE)
r_ts_col_count <- length(colnames(r_ts))


## ---- echo = FALSE------------------------------------------------------------------------------------------------------------
knitr::kable(c_ts[1:5, c(1, 2, (c_ts_col_count-5):c_ts_col_count)], 
  caption = "Sample entries of csse covid-19 time series 03/21/2020")


## -----------------------------------------------------------------------------------------------------------------------------
US_c_ts_0309 <- c_ts[1:52] %>% filter(Country.Region == "US") %>% 
    gather(date, value, 5:52) %>%
    group_by(Country.Region, date) %>% 
    summarize(total = sum(value)) %>% 
    ungroup() %>% 
    mutate(Date_temp = str_replace(date, "X", "")) %>% 
    mutate(Date = as.POSIXct(strptime(Date_temp, "%m.%d.%y")))

delta <- c_ts_col_count - 53

US_c_ts_after_0309 <- c_ts[, c(1,2,3,4, 53:(53+delta))] %>% filter(Country.Region == "US") %>% 
    filter(!str_detect(Province.State, ",") | str_detect(Province.State, "Princess")) %>% 
    gather(date, value, 5:(5+delta)) %>%
    group_by(Country.Region, date) %>% 
    summarize(total = sum(value)) %>% 
    ungroup() %>% 
    mutate(Date_temp = str_replace(date, "X", "")) %>% 
    mutate(Date = as.POSIXct(strptime(Date_temp, "%m.%d.%y")))

US_c_ts_after_0309

US_c_ts <- rbind(US_c_ts_0309, US_c_ts_after_0309) %>% select(Country.Region, Date, total)
today <- as.Date(max(US_c_ts$Date))
US_c_ts


## -----------------------------------------------------------------------------------------------------------------------------
US_c_ts %>% 
    ggplot(aes(Date, total)) +
    geom_point() +
    theme(axis.text.x = element_text(angle = 90)) +
    ylab("Total Confirmed") +
    xlab("Date") +
    labs(title = "US Coronavirus Confirmed Cases from 1/22/2020 to 03/21/2020",
         subtitle = "Data source: https://github.com/CSSEGISandData/COVID-19 provided by Johns Hopkins University")



## -----------------------------------------------------------------------------------------------------------------------------

Italy_c_ts <- c_ts %>% filter(Country.Region == "Italy")
Italy_c_ts <- Italy_c_ts %>% gather(date, value, 5:c_ts_col_count) %>% 
  mutate(Date_temp = str_replace(date, "X", "")) %>% 
  mutate(Date = as.POSIXct(strptime(Date_temp, "%m.%d.%y"))) %>% 
  select(Country.Region, Date, total = value)
# Italy_c_ts
Italy_c_ts %>% filter(Date > "2020-02-15") %>% 
  ggplot(aes(Date, total)) +
    geom_point() +
    theme(axis.text.x = element_text(angle = 90)) +
    ylab("Total Confirmed") +
    xlab("Date") +
    scale_y_log10() +
    labs(title = "Italy Coronavirus Confirmed Cases from 1/22/2020 to 03/21/2020",
      subtitle = "Data source: https://github.com/CSSEGISandData/COVID-19 provided by Johns Hopkins U")




## -----------------------------------------------------------------------------------------------------------------------------
md_padded <- function(mydate){
  paste(str_pad(month(mydate), width = 2, side = "left", pad = "0"), "-", 
        str_pad(day(mydate), width = 2, side = "left", pad = "0"), sep = ""
        )
}

max_date_shown_Italy <- today - 5
max_date_shown_Italy
US_day_0 <- as.Date("2020-03-04")
Italy_day_0 <- as.Date("2020-02-22")
days_diff <- as.integer(US_day_0 - Italy_day_0)
Italy_c_ts <- Italy_c_ts %>% mutate(Day = as.Date(Date) - Italy_day_0) %>% filter(Date <= max_date_shown_Italy)
US_c_ts <- US_c_ts %>% mutate(Day = as.Date(Date) - US_day_0)

stop_1 <- as.integer(as.Date("2020-03-13") - US_day_0)
stop_1
stop_2 <- as.integer(today - US_day_0)
stop_2

stop_3 <- as.integer(max_date_shown_Italy - Italy_day_0)
stop_3

day_0_label <- paste("0\nUS", md_padded(US_day_0), "\nItaly", md_padded(Italy_day_0))
day_0_label
day_school_closed_label <- paste(as.character(stop_1), "\nUS 03-13\nGA school\nclosed", sep = "")
day_school_closed_label
day_today_label <- paste(as.character(stop_2), "\nUS", md_padded(today), "\nItaly", md_padded(today - 11))
day_today_label
day_last_label <- paste(as.character(stop_3), "\n\nItaly", md_padded(max_date_shown_Italy))
day_last_label

y_limit <- Italy_c_ts[Italy_c_ts$Date == (Italy_day_0 + stop_3), "total"]
y_limit
# pdf_filename <- paste("US-Italy-", as.character(today), ".pdf", sep = "")
# pdf(pdf_filename, width = 8, height = 6)

rbind(Italy_c_ts, US_c_ts) %>% filter(Day >= 0) %>% 
  ggplot(aes(Day, total, fill = Country.Region)) + 
  geom_bar(stat = "identity", width = 0.5, position = "dodge") +
  theme(legend.position = "top", legend.title = element_blank()) +
  scale_fill_manual(values = c("blue", "red")) +
  scale_x_continuous(breaks = seq(0, stop_3),
      labels = c(day_0_label, 1:(stop_1 - 1), day_school_closed_label, 
                 (stop_1 + 1):(stop_2 - 1), day_today_label, 
                 (stop_2 + 1):(stop_3 - 1), day_last_label)) +
  scale_y_continuous(breaks = seq(0, y_limit, 2000)) + 
  labs(title = "Covid-19 Confirmed cases Italy vs US (Year 2020)", 
       subtitle = "Trying to see if the growth patterns are similar between Italy and US. \nDay 0 were picked when both countries have similar confirmed cases.",
       caption = "Data Source: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series",
       y = "Total Confirmed")
# dev.off()



## -----------------------------------------------------------------------------------------------------------------------------
c_by_country <- c_ts %>% 
  filter(!Country.Region == "US" | (Country.Region == "US" & !str_detect(Province.State, ","))) %>% 
  select(Country.Region, num = c(c_ts_col_count)) %>%
  group_by(Country.Region) %>% summarize(Confirmed = sum(num)) 

d_by_country <- d_ts %>% 
  filter(!Country.Region == "US" | (Country.Region == "US" & !str_detect(Province.State, ","))) %>% 
  select(Country.Region, num = c(d_ts_col_count)) %>% 
  group_by(Country.Region) %>% summarize(Deaths = sum(num)) 

r_by_country <- r_ts %>% 
  filter(!Country.Region == "US" | (Country.Region == "US" & !str_detect(Province.State, ","))) %>% 
  select(Country.Region, num = c(r_ts_col_count)) %>% 
  group_by(Country.Region) %>% summarize(Recovered = sum(num)) 

all_by_country <- 
  inner_join(inner_join(c_by_country, d_by_country, by = "Country.Region"), 
             r_by_country, by = "Country.Region") %>% 
  mutate(Country.Region = as.character(Country.Region))


## ---- message = FALSE---------------------------------------------------------------------------------------------------------
source("WorldPopulation.R")
wp <- getWorldPopulation()


## -----------------------------------------------------------------------------------------------------------------------------
wppd <- wp %>% 
  mutate(Density = str_replace_all(Density, ",", "")) %>% 
  mutate(Density = as.numeric(Density)) %>% 
  mutate(Population = str_replace_all(Population, ",", "")) %>% 
  mutate(Population = as.numeric(Population)) %>% 
  select(Country.Region, Density, Population, Median_Age)

all <- left_join(all_by_country, wppd, by = "Country.Region")

# sapply(all, function(col) {sum(is.na(col))})
# all[is.na(all$Density),]
# five countries do not get a matching density
# There is a row for Cape Verde and Cabo Verde, which are the same country
all$Density[all$Country.Region == "Cape Verde"] <-  
  wppd$Density[wppd$Country.Region == "Cabo Verde"]

all$Population[all$Country.Region == "Cape Verde"] <- 
  wppd$Population[wppd$Country.Region == "Cabo Verde"]

# There are two Congo entries in covid-19 data, one congo entry from the world population data
all$Density[str_detect(all$Country.Region, "Congo")] <- 
  wppd$Density[wppd$Country.Region == "Congo"]

all$Population[str_detect(all$Country.Region, "Congo")] <-
  wppd$Population[wppd$Country.Region == "Congo"]/2

# There is no entries for Kosovo in world population data. Cruise ship is not a real country. 
all$Density[str_detect(all$Country.Region, "Cruise Ship")] <- 10
all$Population[str_detect(all$Country.Region, "Cruise Ship")] <-2000
all$Density[str_detect(all$Country.Region, "Kosovo")] <-1810366/10890
all$Population[str_detect(all$Country.Region, "Kosovo")] <- 1810366

all <- all %>% 
  mutate("D/C %" = (Deaths/Confirmed)*100) %>% 
  mutate("C/Population" = (Confirmed/Population)*10000, 
         "D/Population" = (Deaths/Population)*10000) %>%
  mutate("D/C % by Density" = `D/C %`/Density) %>% 
  arrange(desc(Confirmed)) %>% slice(1:20)



## -----------------------------------------------------------------------------------------------------------------------------

knitr::kable(all,
  caption = "World Covid-19 Summary 03/21/2020. C/Population, D/Population, R/Population are per 10,000 people",
  format="latex", booktabs=TRUE) %>%
  kable_styling(latex_options="scale_down")



