library(tidyverse)
library(googleway)
library(sf)
library(stringr)

# read data
df_krl <- readRDS("data/krl_detail.rds")

# transform data
df_krl <- df_krl %>%
  as_tibble() %>%
  mutate(halte_detail = map(df_krl$route_info, "stops"),
         route = map(df_krl$route_info, "tracks")) %>%
  select(-route_info)

df_route <- df_krl %>%
  select(schedule_id = scheduleId,
         transport_id = transportId,
         validity,
         short_name = name, long_name = longName, 
         color, route) %>%
  unnest(cols = c(route)) %>%
  rename(route_name = name,
         is_hidden = isHidden)

# function
convert_shape <- function(x, y) {
  x %>%
    st_as_sf(coords = c("lon", "lat")) %>%
    group_by(gr = y) %>%
    summarise(do_union = FALSE) %>%
    st_cast("LINESTRING") %>%
    ungroup() %>%
    select(geometry)
}

# decode polyline
df_route <- df_route %>%
  mutate(shape_decode = map(shape, decode_pl)) %>%
  unnest(shape_decode)

# form geometry
df_route <- df_route %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  group_by_at(vars(-geometry)) %>%
  summarise(do_union = FALSE, .groups = "drop") %>%
  st_cast("LINESTRING")

# viz test
ggplot() +
  geom_sf(data = df_route %>%
            filter(is_hidden == FALSE,
                   direction == 1),
          aes(color = route_name)) +
  guides(color = "none")

# finalization
krl_route <- df_route %>%
  select(transport_id,
         schedule_id,
         corridor_id = short_name,
         corridor_name = long_name,
         corridor_color = color,
         route_id = id,
         route_name,
         direction,
         validity,
         is_hidden)

st_crs(krl_route) <- 4326

# set main route
krl_route <- krl_route %>%
  group_by(corridor_id, direction) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(is_main = ifelse(corridor_name == route_name, TRUE, FALSE),
         is_main_reverse = ifelse(corridor_name ==
                                    paste0(str_extract(route_name, "(?<=- ).*$"),
                                           " - ",
                                           str_extract(route_name, "^.*(?= -)")),
                                  TRUE, FALSE)) %>%
  mutate(is_main = ifelse(n == 1 & direction == 1, TRUE, is_main),
         is_main_reverse = ifelse(n == 1 & direction == 2, TRUE, is_main_reverse))

# listed corridor_id
ua <- filter(krl_route, is_main == TRUE | is_main_reverse == TRUE)$corridor_id %>%
  unique()
ua_neg <- setdiff(unique(krl_route$corridor_id), ua)
n_distinct(krl_route$corridor_id) - n_distinct(ua) # 1 corridor id do not have main
krl_route %>%
  filter(corridor_id %in% ua_neg, is_hidden == FALSE) %>%
  .$corridor_id %>%
  n_distinct() # still 1 use is_hidden argument

krl_route <- krl_route %>%
  mutate(is_main = ifelse(corridor_id %in% ua_neg &
                            is_hidden == FALSE &
                            direction == 1, TRUE, is_main),
         is_main_reverse = ifelse(corridor_id %in% ua_neg &
                                    is_hidden == FALSE &
                                    direction == 2, TRUE, is_main_reverse)) %>%
  select(-n, -is_hidden)

# save data
saveRDS(krl_route, "data/krl_route.rds")

