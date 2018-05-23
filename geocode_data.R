data_refact <- read.csv("data/data_refact.csv")

library(ggmap)
loc <- data_refact %>% 
    select(campground, city, state) %>% 
    distinct() %>% 
    mutate(full_loc = paste(campground, city, state, sep = ", "))


#good
loc_geocode <- loc %>% 
    mutate_geocode(full_loc)

#get missing
loc_geocode2 <- loc_geocode %>% 
    filter(is.na(lon)) %>% 
    select(-lon, -lat) %>% 
    mutate_geocode(full_loc)

#get more missing
loc_geocode3 <- loc_geocode2 %>% 
    filter(is.na(lon)) %>% 
    select(-lon, -lat) %>% 
    mutate_geocode(full_loc)

#get more missing
loc_geocode4 <- loc_geocode3 %>% 
    filter(is.na(lon)) %>% 
    select(-lon, -lat) %>% 
    mutate_geocode(full_loc)

#get last missing
loc_geocode5 <- loc_geocode4 %>% 
    filter(is.na(lon)) %>% 
    mutate(lat = 43.15817,
           lon = -122.133642) 
    
#combine
all_loc <- bind_rows(loc_geocode, loc_geocode2, 
          loc_geocode3, loc_geocode4, 
          loc_geocode5) %>% 
    filter(!is.na(lon)) %>% 
    select(full_loc, lon, lat)

data_combined <- data_refact %>% 
    mutate(full_loc = paste(campground, city, state, sep = ", ")) %>% 
    left_join(all_loc)

write.csv(data_combined, "data/data_geocoded.csv",
          row.names = FALSE)
