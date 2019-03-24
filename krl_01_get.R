library(jsonlite)
library(tidyverse)

# get route info
main_url <- "https://www.trafi.com/api/schedules/jakarta/"
route <- paste0(main_url, "all?transportType=")

df_krl <- fromJSON(paste0(route, "train"))[[1]] %>% unnest()

# get details for each route
route_det <- function(schedule_id, transport) {
  paste0(main_url, "schedule?scheduleId=", schedule_id, "&transportType=", transport)
}

# try for krl
df_krl <- df_krl %>%
  mutate(route_url = map2_chr(gsub(" ", "\\+", scheduleId), "train", route_det),
         route_info = map(route_url, fromJSON),
         load_date = Sys.Date())

# save data (latest 10 Jan 2019)
saveRDS(df_krl, "data/krl_detail.rds")




