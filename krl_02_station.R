library(tidyverse)

# read data
df_krl <- readRDS("data/krl_detail.rds")

df_krl$route_info[[1]]

map(df_krl$route_info, "id")
map(df_krl$route_info, "name")
map(df_krl$route_info, "stops")
map(df_krl$route_info, "tracks")

# create halte data
df_krl <- df_krl %>%
  as_tibble() %>%
  mutate(halte_detail = map(df_krl$route_info, "stops"))

df_halte <- df_krl %>%
  select(schedule_id = scheduleId,
         transport_id = transportId,
         validity,
         short_name = name, long_name = longName, color,
         halte_detail) %>%
  unnest(cols = c(halte_detail)) %>%
  rename(halte_id = id,
         halte_name = name,
         area_name = areaName,
         direction_name = directionName)

df_halte_final <- df_halte %>%
  group_by(halte_id, halte_name, lat, lng) %>%
  summarise(route_cnt = n_distinct(schedule_id),
            schedule_id = list(unique(schedule_id)),
            .groups = "drop") %>%
  rename(latitude = lat, longitude = lng)

# finalization
krl <- df_halte_final %>%
  select(everything(), schedule_id, corridor_cnt = route_cnt)

# save data
saveRDS(krl, "data/krl.rds")
