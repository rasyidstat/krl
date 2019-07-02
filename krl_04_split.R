library(mrsq)
library(nusantr)
library(dplyr)
library(tidyr)
library(purrr)
library(sf)
library(mapview)
library(lwgeom)
library(ggrepel)


# station prep ------------------------------------------------------------
# get start and end station name
krl_start_end <- krl_route %>% 
  count(route_name) %>% 
  as.data.frame() %>% 
  select(-geometry, -n) %>% 
  as_tibble() %>%
  mutate(route_name = gsub(" via.*", "", route_name)) %>% 
  separate(route_name, c("start", "end"), sep = " - ") %>% 
  gather(type, station_name) %>% 
  count(type, station_name) %>% 
  spread(type, n) %>% 
  mutate_if(is.numeric, funs(replace_na(., 0))) %>% 
  mutate(total = start + end,
         type = case_when(start > end ~ "start",
                          end > start ~ "end",
                          TRUE ~ "start/end")) %>% 
  arrange(desc(total))

# get krl station name and convert to sf
krl_final <- left_join(krl, krl_start_end)
krl_final <- st_as_sf(krl_final, coords = c("longitude", "latitude"), crs = 4326)


# route prep --------------------------------------------------------------
krl_route_final <- krl_route %>% 
  filter(is_main)


# viz route ---------------------------------------------------------------
ggplot() +
  geom_sf(data = krl_route_final, color = "grey50") +
  geom_sf(data = krl_final, aes(color = type), shape = 15) +
  geom_label_repel(data = krl_final %>%
                     filter(station_id != "idjkt_Kampungbandan2",
                            total > 0),
                   aes(geometry = geometry,
                       label = station_name,
                       color = type),
                   stat = "sf_coordinates",
                   min.segment.length = 0,
                   family = "Nunito") +
  coord_sf(datum = NA) +
  theme_nunito(legend = 3) +
  labs(x = NULL, y = NULL) +
  scale_color_discrete("Station Type")
ggsave("figs/krl_route.png", width = 10, height = 10)


# split route -------------------------------------------------------------
split_route <- function(schedule_id_input, route_id_input, df_route, df_station, threshold = 0.25, buffer_size = 0.0005) {
  #1 get route (linestring)
  route <- df_route %>% 
    filter(schedule_id == schedule_id_input,
           route_id == route_id_input)
  
  #1 get station (point)
  station <- df_station %>%
    unnest() %>% 
    filter(schedule_id == schedule_id_input)
  
  #2 convert point to polygon with small buffer size
  buff <- st_combine(st_buffer(station, buffer_size))
  
  #3 split linestring to many linestring based on polygon splitting
  all <- st_collection_extract(st_split(route$geometry, buff), "LINESTRING")
  # mapview(all) + station
  
  #4 create sf data frame of linestring
  all <- st_as_sf(
    data.frame(
      order_id = 1:length(all),
      geometry = all
    )
  )
  
  #5 measure distance of each linestring, remove distance lower than threshold value (0.25km)
  all <- all %>% 
    mutate(d = as.numeric(st_length(geometry)) * 10^-3) %>% 
    filter(d > threshold)
  
  #6 get point of line start/end for each linestring
  point <- st_cast(all, "POINT") %>% 
    group_by(order_id) %>% 
    mutate(r = row_number()) %>% 
    arrange(desc(r)) %>% 
    mutate(r2 = row_number()) %>% 
    ungroup() %>% 
    arrange(order_id) %>% 
    filter(r == 1 | r2 == 1) %>% 
    select(-d)
  
  #7 spatial join point of line start/end with station point based on the nearest one to get the start/end station name
  all_name <- st_join(point, station, join = st_is_within_distance, dist = 200) %>% 
    arrange(order_id, r) %>% 
    as.data.frame() %>% 
    select(-geometry) %>% 
    group_by(order_id) %>% 
    summarise(line_name = paste0(station_name, collapse = " - ")) %>% 
    ungroup() %>% 
    separate(line_name, c("station_start", "station_end"), sep = " - ", remove = FALSE)
  
  #8 join back to get the geometry of linestring
  all_name %>% 
    left_join(all) 
}

# test
# schedule_id_input = "idjkt_airport_train"
# route_id_input = "a-b"
# df_route = krl_route_final
# df_station = krl_final
# buffer_size = 0.0005
# threshold = 0.2

# check
# mapview(point)

# execute all
krl_route_final_master <- krl_route_final %>% 
  as.data.frame() %>% 
  select(-geometry, -validity, -direction, -is_main, -is_main_reverse)
krl_route_final_master <- krl_route_final_master %>% 
  as_tibble() %>% 
  mutate(route = map2(schedule_id, route_id, function(x, y) split_route(x, y,
                                                                        df_route = krl_route_final,
                                                                        df_station = krl_final) ))
saveRDS(krl_route_final_master, "data/krl_route_split.rds")
krl_route_final_master <- krl_route_final_master %>% 
  select(-line_name) %>%
  unnest() %>% 
  st_as_sf()
st_crs(krl_route_final_master) <- 4326


# viz route split ---------------------------------------------------------
ggplot() +
  geom_sf(data = krl_route_final_master, aes(color = line_name)) +
  geom_sf(data = krl_final, shape = 15) +
  geom_label_repel(data = krl_final %>%
                     filter(station_id != "idjkt_Kampungbandan2",
                            total > 0),
                   aes(geometry = geometry,
                       label = station_name),
                   color = "grey50",
                   stat = "sf_coordinates",
                   min.segment.length = 0,
                   family = "Nunito") +
  coord_sf(datum = NA) +
  guides(color = FALSE) +
  theme_nunito(legend = 3) +
  labs(x = NULL, y = NULL)
ggsave("figs/krl_route_split.png", width = 10, height = 10)



