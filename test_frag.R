library(biobankr)
library(dplyr)
library(lubridate)
library(tidyr)
x = readRDS("mean_daily_activity_long.rds")
dd = date_day_min(x)
hr_to_min = function(x) {
  time_to_min(ymd_hms(
    paste0("2017-01-01 ", x)
    ))
}
keep_times = hr_to_min(
  c("08:00:00", "16:00:00")
  )
dd$keep = 
	dd$minute >= keep_times[1] &
	dd$minute <= keep_times[2]
keep_times = seq(keep_times[1], 
  keep_times[2])
keep_times = as.character(keep_times)	
dd = dd %>% 
	select(acceleration, keep,
		minute, day)
counts = dd %>% 
	select(-keep) %>% 
	spread(key = minute, 
		value = acceleration)
weartime = counts
times = 0:1439
times = as.character(times)
times = intersect(
  colnames(weartime), times)
times = setdiff(times, keep_times)

make_false = function(x) {
  FALSE
}
make_true = function(x) {
  TRUE
}
weartime = weartime %>% 
  mutate_at(times,
    .funs = make_false)
weartime = weartime %>% 
  mutate_at(keep_times,
    .funs = make_true)

counts = counts %>% 
	select(-day)
weartime = weartime %>% 
	select(-day)	
## need this shit!
# need to look at the frag code
counts[is.na(counts)] = 0
res = frag(
  counts = counts,
  weartime = weartime,
  thresh.lower = 30)



