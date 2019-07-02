library(mrsq)
library(nusantr)
library(dplyr)
library(tidyr)
library(purrr)
library(sf)

# goal: build a function to measure distance between 2 stations with additional possible routing


# prepare data ------------------------------------------------------------
krl_route_split <- readRDS("data/krl_route_split.rds") %>% 
  unnest() %>% 
  st_as_sf(crs = 4326)
krl_station <- st_as_sf(krl, coords = c("longitude", "latitude"), crs = 4326) %>% 
  unnest() %>% 
krl_route <- krl_route %>% 
  filter(is_main)


# distance calculator per route -------------------------------------------
calculate_dist <- function(st_start = "Cisauk",
                           st_end = "Kebayoran",
                           route = "Tanah Abang - Rangkasbitung",
                           dat = krl_route_split) {
  dat <- dat %>% 
    filter(route_name == route)
  
  is_first <- filter(dat, station_start == st_start)$order_id < filter(dat, station_start == st_end)$order_id
  is_first2 <- filter(dat, station_end == st_start)$order_id < filter(dat, station_end == st_end)$order_id
  is_first <- ifelse(length(is_first) == 0, FALSE, is_first)
  is_first2 <- ifelse(length(is_first2) == 0, FALSE, is_first2)
  
  if (is_first | is_first2) {
    st_first = st_start; st_last = st_end
  } else {
    st_first = st_end; st_last = st_start
  }
  
  dat %>% 
    filter(order_id >= filter(dat, station_start == st_first)$order_id,
           order_id <= filter(dat, station_end == st_last)$order_id) %>% 
    as.data.frame() %>% 
    summarise(d = sum(d)) %>% 
    .[[1]]
}

# testing
calculate_dist()
calculate_dist("Cisauk", "Kebayoran")
calculate_dist("Sudimara", "Kebayoran")
calculate_dist("Jurangmangu", "Kebayoran")
calculate_dist("Rawa Buntu", "Kebayoran")
calculate_dist("Cisauk", "Rawa Buntu")
calculate_dist("Cisauk", "Palmerah")
calculate_dist("Rawa Buntu", "Cisauk")
calculate_dist("Cisauk", "Cisauk")
calculate_dist("Parung Panjang", "Kebayoran")
calculate_dist("Parung Panjang", "Tanah Abang")
calculate_dist("Kebayoran", "Tanah Abang")
calculate_dist("Cilebut", "Duren Kalibata", "Jakarta Kota - Bogor")
calculate_dist("Cilebut", "Depok Baru", "Jakarta Kota - Bogor")
calculate_dist("Depok", "Duren Kalibata", "Jakarta Kota - Bogor")
calculate_dist("Depok Baru", "Duren Kalibata", "Jakarta Kota - Bogor")

# distance calculator all -------------------------------------------------
  

