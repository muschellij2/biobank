# convert_1440
rm(list = ls())
library(biobankr)
library(dplyr)
library(lubridate)
library(broom)
# set to
# /dcl01/chatterj/data/ukbiobank/phenotype
pheno_dir = Sys.getenv("biobank")

#############################
# Set up directories
#############################
root_dir = file.path(
  pheno_dir,
  "accelerometer")
data_dir = file.path(
  root_dir,
  "intensity")
out_dir = file.path(root_dir,
                    "collapsed")
# think about putting in biobankr
id_file = file.path(root_dir, 
                    "ids.txt")
biobank_ids = readLines(id_file)
stub = "_90004_0_0.csv.gz"



func = "mean"
prefix = paste0(func, "_")

all_measure_files = file.path(
  out_dir, biobank_ids,
  paste0(prefix,
         "daily_", 
         "activity_measures.rds"))

iid = as.numeric(
  Sys.getenv("SGE_TASK_ID")
)
if (is.na(iid)) {
  # iid = 72505
  # iid = 50
  iid = 39442
}
# iid = which(biobank_ids == 1000888)
# id_out_dir = file.path(out_dir,
#     biobank_ids)

# for (iid in seq_along(biobank_ids)) {
print(paste0("iid is ", iid))

biobank_id = biobank_ids[iid]
# original CSV.gz
infile = file.path(
  data_dir, 
  paste0(biobank_id, stub)
)



# creating output directory
id_out_dir = file.path(out_dir,
                       biobank_id)
if (!dir.exists(id_out_dir)) {
  dir.create(id_out_dir)
}
# adding ID to df
add_id = function(df, biobank_id) {
  df %>% 
    mutate(biobank_id = biobank_id)  %>% 
    select(biobank_id, everything())
}


# Output files
daily_file = file.path(
  id_out_dir,
  paste0(prefix, 
         "daily_activity_long.rds"))


no_imp_daily_file = file.path(
  id_out_dir,
  paste0(prefix,
         "no_imputed_daily_", 
         "activity_long.rds"))

measure_file = file.path(
  id_out_dir,
  paste0(prefix,
         "daily_", 
         "activity_measures.rds"))

out_files = c(daily_file, 
              no_imp_daily_file,
              measure_file
              )

filter_df = function(df) {
  df = df %>% 
    filter(n > 0)

  df = df %>% 
    filter(is.finite(acceleration))
  df = add_id(df, biobank_id)
  return(df)
} 


make_night_day = function(df) {
  hr_min = as.POSIXlt(df$date)
  df$hour = hour(hr_min)

  # 3 am to 5:00 am
  limits = c(3, 4)
  df = df %>% 
    mutate(
      night_time = hour >= limits[1] &  
      hour <= limits[2]
      ) %>% 
  select(-hour)
  df
}


kde_mode = function(x, 
  max_val = 250,
  bw = "nrd" ) {
  x = na.omit(x)
  x = x[x < max_val]
  d = try({
    density(x, bw = bw)
  })
  if (inherits(d, "try-error")) {
    d = try({
      density(x, bw = "nrd0")
    })
  }
  if (inherits(d, "try-error")) {
    return(NA)
  }
  if (length(x) == 0) {
    return(NA)
  }
  vals = (seq(0, max(x), by =0.01))

  approx_y = approx(x = d$x, y = d$y, xout = vals)
  # d$x[ which.max(d$y)]
  approx_y$x[ which.max(approx_y$y)]
}

get_meas = function(df) {
  df = make_night_day(df)
  df = ungroup(df)  

  df = df %>% 
    mutate(day = floor_date(date, unit = "day"))
  meas = df %>% 
    mutate(
      from_day = difftime(
        time1 = day, min(day), 
        units = "days"),
      from_day = as.numeric(from_day)) %>% 
    group_by(from_day) %>% 
    summarize(
      median = median(acceleration, na.rm = TRUE),
      mean = mean(acceleration, na.rm = TRUE),
      q95 = quantile(acceleration, 
        probs = 0.95, na.rm = TRUE),
      max = max(acceleration, na.rm = TRUE),
      n = n(),
      n_non_na = sum(!is.na(acceleration))
    )    

  df = df %>% 
    filter(!is.na(acceleration)) 


  x = df$acceleration
  mode = kde_mode(x)
  day_mode = df %>% 
    group_by(night_time) %>% 
    summarize(mode = kde_mode(acceleration),
      n = n())
  non_na = sum(!is.na(x))
  probs = seq(0, 1, by = 0.05)

  q = df %>% 
    ungroup %>% 
    do(tidy(quantile(.$acceleration, 
      probs = probs, na.rm = TRUE) ))

  night_q = df %>% 
    group_by(night_time) %>% 
    do(tidy(quantile(.$acceleration, 
      probs = probs, na.rm = TRUE) ))

  res = list(
    mode = mode, 
    daynight_mode = day_mode,
    non_na = non_na,
    quant = q,
    night_quant = night_q,
    day_measures = meas
    )
}


# bad id 1000888
# if (!all(file.exists(out_files))) {
  # organize the data
  xdf = bb_read(infile)
  too_many = attributes(xdf)$too_many_days
  srate = attributes(xdf)$sampling_rate

  xdf = xdf %>% mutate(
    bad = is.na(as.numeric(acceleration)) & 
      !is.na(acceleration))
  stopifnot(all(!xdf$bad))
  xdf$acceleration = 
    as.numeric(xdf$acceleration)  
  attr(xdf, "too_many_days") = too_many
  xdf = test_start_stop(xdf)
  five_meas = get_meas(xdf)


  df = bb_summarize_minute(xdf, 
    summarize_func = func,
    keep_imputed = TRUE,
    na.rm = TRUE    
    )

  df = filter_df(df)
  min_meas = get_meas(df)
  saveRDS(df, file = daily_file)
  print(nrow(df))
  rm(list = "df");gc()

  df = bb_summarize_minute(xdf, 
    summarize_func = func,
    keep_imputed = FALSE,
    na.rm = TRUE
    )  
  df = filter_df(df)
  

  saveRDS(df, file = no_imp_daily_file)
  print(nrow(df))
  rm(list = "df");gc()


  L = list(
    five_sec = five_meas,
    minute = min_meas
    )
  saveRDS(L, file = measure_file)

# }