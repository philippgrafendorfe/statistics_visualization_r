library(dplyr)

address <- url("http://www.trutschnig.net/RTR2015.RData")
load(address)
head(RTR2015)

RTR=tbl_df(RTR2015)
RTR

glimpse(RTR)

RTR %>% select(rtr_speed_dl:rtr_ping)

RTR %>% select(-c(id, iso_adm2))

device <- 5
select(RTR, device)

select(RTR, identity(device))

select(RTR, !! device)

RTR %>% select(contains('ng'))

RTR %>% select(one_of('device','op_name', 'rtr_ping'))

RTR %>% select(starts_with('dev'), ends_with('e'))

### lab section

RTR %>% mutate(sum = latitude + longitude)

RTR %>% mutate(ping_s = rtr_ping*1E-3)

RTR %>% transmute(quot = latitude/longitude)

### complex integration

## 1.
RTR %>% group_by(device) %>% summarise(freq_of_use = n()) %>% arrange(-freq_of_use) %>% mutate(rank = rank(desc(freq_of_use)))

## 2.
RTR %>% 
  filter(rtr_speed_dl > mean(rtr_speed_dl) & rtr_speed_ul > mean(rtr_speed_ul)) %>% 
  mutate(sum_speed = rtr_speed_dl + rtr_speed_ul) %>% 
  select(sum_speed) %>% 
  arrange(-sum_speed) %>% 
  mutate(rank_speed = rank(desc(sum_speed)))

## 3.
RTR %>% 
  group_by(nw_cat, device_platform, device) %>% 
  summarise(n = n()) %>% 
  summarise(n_ = n_distinct(n))

RTR %>% 
  group_by(device_platform, nw_cat) %>% 
  summarise(n = n_distinct(device))

RTR %>% filter(device_platform == "Android", nw_cat == "2G") %>% summarise(n = n_distinct(device))

RTR %>% select(device_platform, nw_cat, device) %>% n_distinct(device)

length(unique(RTR$device))

### Joins

file1 <- RTR[1:20,] %>% select(id, device, rtr_speed_dl, rtr_speed_ul)
file2 <- RTR[10:30,] %>% select(id, mtime, latitude, longitude)

left_join(file1, file2)
left_join(file1, file2, by = 'id')

right_join(file1, file2)
















