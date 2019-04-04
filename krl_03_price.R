library(jsonlite)
library(tidyverse)
library(hms)

## as per 24 Mar
## source: http://info.krl.co.id


# pricing
price <- "http://info.krl.co.id/RV9LDV4KTFsFERYNSVQNFkIHVB1ZFExCWBxFQhQKBg9AFUtTWh0OQ05EBgEcXwZDRFZBDFJUSFoAUARUWgMGCTo3Nzc2ZWRmYg=="

# route
route <- "http://info.krl.co.id/XXRcD1ZVSxwGFVwPbh5bT0kVbg0EAwceWyYRUVNdSltRWEcIAwpMVkNSUxAMQB1AVVUcAlFGRAxBElcfWgEADwUNBlEHOjE1Y2FjZDQz"

# station
station <- "http://info.krl.co.id/BVYFGU1OWV1LWwJVA1ZBQgZMaAlMUFcRQRsEDQFGXhwHEl4fQF1VF1dWFxBHWRdeX1lNXAAEAFYNUAJROmMwY2M5YjYy"

# schedule
schedule <- "http://info.krl.co.id/FkVYAhREAUJcWxgHEg9RRlJHTgJNAQFHAAtaUApRUgYGTFcVQB1KGl8VFhhJDB1AVVUcAlFGXAJQEVMNHQwCDwYBCVcHCDo0MDMyOTI3OQ=="


## get data
df_price <- fromJSON(price)$data
df_route <- fromJSON(route)$data
df_station <- fromJSON(station)$data
df_schedule <- fromJSON(schedule)$data


## transform data
df_price <- df_price %>% 
  mutate(price = as.numeric(tariff)) %>% 
  select(-tariff) %>% 
  as_tibble()
df_schedule <- df_schedule %>% 
  mutate(arr_time = as.hms(arr_time),
         dep_time = as.hms(dep_time),
         notstop = as.factor(notstop),
         status = as.factor(status)) %>% 
  as_tibble()


## write data
saveRDS(df_price, "data/infokrl_price.rds")
saveRDS(df_route, "data/infokrl_route.rds")
saveRDS(df_station, "data/infokrl_station.rds")
saveRDS(df_schedule, "data/infokrl_schedule.rds")



# begin here --------------------------------------------------------------
df_price <- readRDS("data/infokrl_price.rds")
krl <- readRDS("data/krl.rds") %>% 
  count(halte_name, sort = TRUE) %>% 
  mutate(halte_uc = toupper(halte_name),
         halte_uc = gsub("\\s+", "", halte_uc),
         halte_uc = ifelse(halte_uc == "TANJUNGPRIOK", "TANJUNGPRIUK", halte_uc)) %>% 
  select(-n)

df_price_final <- df_price %>% 
  rename(sts_from_code = sts_from_name,
         sts_to_code = sts_to_name) %>% 
  left_join(krl %>% 
              rename(sts_from_code = halte_uc,
                     sts_from_name = halte_name)) %>% 
  left_join(krl %>% 
              rename(sts_to_code = halte_uc,
                     sts_to_name = halte_name)) %>% 
  select(contains("sts_from"), contains("sts_to"), price)

saveRDS(df_price_final, "data/krl_price.rds")









