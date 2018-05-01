library(tidyverse)
library(lubridate)
library(plotly)

data_raw <- readLines("data.txt")

date_seq <- tibble(arrived = ymd("2012-06-14") + 1:2142)

data <- tibble(text = data_raw[-1]) %>% 
    mutate(lines = ceiling(1:n()/3)) %>% 
    group_by(lines) %>% 
    summarize(text = str_c(text, collapse = "\t")) %>% 
    separate(text, 
             into = c("arrived", "loc_campground", "loc_city", "type", "price", "extra"), 
             sep = "\t") %>% 
    select(-lines, -extra) %>% 
    mutate(arrived = mdy(arrived),
           # type = as.factor(type),
           price = str_extract(price, "[0-9]+") %>% as.numeric()) %>% 
    right_join(date_seq) %>% 
    fill(-arrived)

data %>% 
    mutate(cum_price = cumsum(price)) %>% 
    ggplot() +
    geom_point(aes(x = arrived, y = cum_price, color = as.factor(year(arrived))))

plot_month <- data %>% 
    mutate(month = month(arrived),
           year = year(arrived)) %>% 
    group_by(month, year) %>% 
    summarize(month_price = sum(price)) %>% 
    mutate(date = ymd(paste(year, month, 1, sep = "-"))) %>% 
    ggplot(aes(x = date, y = month_price, text = month)) +
    geom_line() +
    geom_point()

ggplotly(plot_month)

data %>% 
    mutate(type = reorder(type, price)) %>%
    ggplot(aes(x = type, y = price)) +
    geom_boxplot() +
    geom_jitter() +
    coord_flip()

data %>% 
    separate(loc_city, c("city", "state"), sep = ", ", extra = "merge") %>% 
    group_by(state) %>% 
    mutate(max_price = max(price)) %>% 
    ungroup() %>% 
    mutate(state = reorder(state, max_price),
           type = fct_collapse(type,
                               Federal = c("Army Corps of Engineers",
                                           "BLM Boondocking",
                                           "BLM Campground",
                                           "National Forest",
                                           "National Forest Boondocking",
                                           "National Park",
                                           "National Park Boondocking",
                                           "Tennessee Valley Authority"),
                               State = c("Montana Fish & Wildlife",
                                         "State Forest Campground",
                                         "State Park",
                                         "State Park Boondocking"),
                               OtherPark = c("City Park",
                                             "County Park"),
                               Miscellaneous = c("Parking Lot",
                                                 "Private Residence",
                                                 "Private RV Park",
                                                 "House Rental"))) %>% 
    ggplot(aes(x = state, y = price, color = type)) +
    geom_jitter() +
    # geom_boxplot() +
    coord_flip()

