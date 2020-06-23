library(mrsq)
library(nusantr)
library(dplyr)
library(tidyr)
library(purrr)
library(sf)

# goal: build a function to measure distance between 2 stations with additional possible routing


# prepare data ------------------------------------------------------------
krl_route_split <- readRDS("data/krl_route_split.rds") 
krl_station <- st_as_sf(krl, coords = c("longitude", "latitude"), crs = 4326) %>% 
  unnest(schedule_id)
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
calculate_dist("Cisauk", "Tanah Abang")
calculate_dist("Rawa Buntu", "Cisauk")
calculate_dist("Cisauk", "Jurangmangu")
calculate_dist("Parung Panjang", "Kebayoran")
calculate_dist("Parung Panjang", "Tanah Abang")
calculate_dist("Kebayoran", "Tanah Abang")
calculate_dist("Cilebut", "Duren Kalibata", "Jakarta Kota - Bogor")
calculate_dist("Cilebut", "Depok Baru", "Jakarta Kota - Bogor")
calculate_dist("Depok", "Duren Kalibata", "Jakarta Kota - Bogor")
calculate_dist("Depok Baru", "Duren Kalibata", "Jakarta Kota - Bogor")

# distance calculator all -------------------------------------------------
st_start = "Cisauk"
st_end = "Sudirman"
# st_end = "Cikarang"
dat = krl_route_split

calculate_dist_all <- function(st_start = "Cisauk",
                               st_end = "Sudirman",
                               dat = krl_route_split) {
  # how is the algorithm?
  # one transit problem
  # two or more transit problem
  
  # route and station list
  krl_route_list <- krl_route_split %>% 
    select(route_name, station_name = station_start, d) %>% 
    rbind(krl_route_split %>% 
            arrange(desc(order_id)) %>% 
            group_by(route_name) %>% 
            filter(row_number() == 1) %>% 
            ungroup() %>% 
            mutate(d = NA_real_) %>% 
            select(route_name, station_name = station_end, d)) %>% 
    group_by(route_name) %>% 
    mutate(idx = row_number()) %>% 
    ungroup() %>% 
    arrange(route_name, idx) %>% 
    as.data.frame() %>% 
    select(-geometry) %>% 
    as_tibble()
  
  # list of transit station
  transit_station <- krl_route_list %>% 
    group_by(station_name) %>% 
    mutate(route_cnt = n()) %>% 
    filter(route_cnt > 1) %>% 
    ungroup() %>% 
    arrange(station_name)
  
  transit_station <- transit_station %>% 
    rename(route_name_a = route_name) %>% 
    inner_join(transit_station %>% 
                 select(-d, -route_cnt, -idx) %>% 
                 rename(route_name_b = route_name),
               by = "station_name") %>% 
    filter(route_name_a != route_name_b) %>% 
    select(route_name_a, route_name_b, station_name, route_cnt) %>% 
    arrange(route_name_a, route_name_b) %>% 
    group_by(route_name_a, route_name_b) %>% 
    mutate(same_route_cnt = n()) %>% 
    filter(same_route_cnt == 1) %>% 
    select(-same_route_cnt) %>% 
    ungroup() %>% 
    rename(station_transit = station_name)
  
  subset(krl_route_list, station_name == st_start)$route_name
  subset(krl_route_list, station_name == st_end)$route_name
  
  
  
  # get match station
  krl_route_list %>% 
    filter(station_name == st_start) %>% 
    select(route_name_a = route_name,
           station_start = station_name) %>% 
    mutate(r = 1) %>% 
    inner_join(krl_route_list %>% 
                 filter(station_name == st_end) %>% 
                 select(route_name_b = route_name,
                        station_end = station_name) %>% 
                 mutate(r = 1)) %>% 
    select(-r) %>% 
    inner_join(transit_station %>% 
                 select(-route_cnt)) %>% 
    mutate(d1 = pmap_dbl(list(station_start, station_transit, route_name_a), calculate_dist),
           d2 = pmap_dbl(list(station_transit, station_end, route_name_b), calculate_dist))
  
  # find transit station
  krl_route_list %>% 
    filter(station_name == st_start) %>% 
    select(-station_name, -d, -idx) %>% 
    left_join(krl_route_list, by = "route_name")
  krl_route_list %>% 
    filter(station_name == st_end) %>% 
    select(-station_name, -d, -idx) %>% 
    left_join(krl_route_list, by = "route_name")
  
  
}

