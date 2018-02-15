# convert_1440
rm(list=ls())
library(biobankr)
library(dplyr)
library(readr)
library(matrixStats)
library(ggplot2)
library(tidyr)
library(scales)
library(broom)
library(lubridate)
# set to
# /dcl01/chatterj/data/ukbiobank/phenotype
pheno_dir = Sys.getenv("biobank")

#############################
# Set up directories
#############################
root_dir = file.path(
  pheno_dir,
  "accelerometer")
tab_dir = file.path(root_dir,
  "tabular")
data_dir = file.path(
  root_dir,
  "intensity")
coll_dir = file.path(root_dir,
    "collapsed")
out_dir = file.path(root_dir,
    "analysis")
res_dir = file.path(root_dir,
    "results")

id_file = file.path(root_dir, 
                    "ids.txt")
biobank_ids = readLines(id_file)

ctypes = cols(
  .default = col_character(),
  biobank_id = col_integer(),
  yob = col_integer(),
  weight_imaging = col_integer(),
  height_imaging = col_integer(),
  visit = col_integer(),
  age_assessment = col_integer(),
  age_death = col_double(),
  age_recruitment = col_integer(),
  bmi = col_double(),
  bmr = col_double(),
  body_fat_pct = col_double(),
  date_attend = col_date(format = ""),
  date_death = col_date(format = ""),
  manual_weight = col_double(),
  standing_height = col_double(),
  weight = col_double()
  )
demog_file = file.path(
  tab_dir, 
  paste0("demographics_reformat", 
    ".csv.gz"))

demog = read_csv(demog_file,
  col_types = ctypes)
demog_ids = unique(demog$biobank_id)
demog$biobank_id = as.integer(
    demog$biobank_id)
demog = demog[ 
  demog$biobank_id %in% biobank_ids,]
# keep only data where there was a visit
demog = demog %>% 
  filter(!is.na(date_attend))
if (!is.Date(demog$dob)) {
  demog$dob = ymd(demog$dob)
}


# Output files
first_file = file.path(
  out_dir,
  "first_day.rds")
first_day = readRDS(first_file)

first_day = first_day[ 
  first_day$biobank_id %in% biobank_ids,]
first_day = first_day %>% 
  mutate(accel_date = ymd(accel_date))

demog = left_join(demog, first_day)
demog$age_accel = 
floor(
  as.numeric(demog$accel_date - demog$dob) / 
  365.25
  )
demog = demog %>% 
  select(-accel_date)

prefix = "mean_"
#################################
# Not imputed data
#################################
insides = c("", "no_imputed_")
thresholds = c(FALSE, TRUE)
eg = expand.grid(inside = insides, 
  threshold = thresholds,
  stringsAsFactors = FALSE)


iscen = as.numeric(
  Sys.getenv("SGE_TASK_ID")
)
if (is.na(iscen)) {
  iscen = 2
}

inside = eg$inside[iscen]
threshold = eg$threshold[iscen]
tapp = ifelse(
  threshold,
  "threshold_", "")

# not imputed data
# inside = "no_imputed_"
# Output files
pop_file = file.path(
  out_dir,
  paste0(prefix, 
         "pop_", inside,
         tapp,
         "daily_activity_long.rds"))

data = readRDS(pop_file)
data = data[ 
  data$biobank_id %in% biobank_ids,]
# data = readRDS(pop_no_imp_activ_file)
data_ids = unique(data$biobank_id)


if (threshold) {
  data = data %>% 
    filter(above_threshold)
} 
data = data %>% 
  select(-above_threshold)

#############################
# Looking at day of visit 
#############################  
day_df = data %>% 
  select(biobank_id, day) %>% 
  distinct() %>% 
  arrange(biobank_id, day) %>% 
  group_by(biobank_id) %>% 
  slice(1)

day_df$date = ymd(day_df$day)
hist_date = function(x) {
  range_date = range(x)
  breaks = seq(range_date[1], 
    range_date[2] + 
    as.period(3, "month"), 
    by = "3 month")
  h = hist(x, breaks=breaks)
}
h = hist_date(day_df$date)

day_df = left_join(day_df,
  demog,
  by = "biobank_id")

day_df$diff = day_df$date - 
  day_df$date_attend

day_df$abs_diff = abs(day_df$diff)

day_df = day_df %>% 
  select(biobank_id, day, date,
    visit, diff,
    everything()) %>% 
  arrange(biobank_id, abs_diff)

day_df = day_df %>% 
  group_by(biobank_id) %>% 
  slice(1)
sum(day_df$abs_diff < 365) 
sum(day_df$abs_diff < 365*2) 
sum(day_df$abs_diff < 365*3)
day_df = day_df %>% 
  select(biobank_id, date, date_attend,
    visit, diff)

# do differential analysis of BMI
# for increasing subsets
# also look at BMI change over time
# > sum(day_df$abs_diff < 365)
# [1] 4364
# > sum(day_df$abs_diff < 365*2)
# [1] 8359
# > sum(day_df$abs_diff < 365*3)
# [1] 12662

# plot(date ~ date_attend, data = day_df)
h = hist(as.numeric(day_df$diff))

sub_df = data %>% 
  select(biobank_id, acceleration)

probs = seq(0, 1, by = 0.25)
qdf = sub_df %>% 
  group_by(biobank_id) %>% 
  do( 
    tidy(
      t(quantile(.$acceleration, 
        probs = probs)
      )
    )
  )