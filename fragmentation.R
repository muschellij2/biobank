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
library(pheatmap)
library(fragmentation)
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
    "_first_visit.csv.gz"))

demog = read_csv(demog_file, 
  col_types = ctypes)
demog = demog[ 
  demog$biobank_id %in% biobank_ids, ]
demog_ids = unique(demog$biobank_id)
demog$biobank_id = as.integer(
    demog$biobank_id)
if (!is.Date(demog$dob)) {
  demog$dob = ymd(demog$dob)
}


# Merging day of accelerometry
first_file = file.path(
  out_dir,
  "first_day.rds")
first_day = readRDS(first_file)

first_day = first_day[ 
  first_day$biobank_id %in% biobank_ids,]
first_day = first_day %>% 
  mutate(accel_date = ymd(accel_date))

# Age at accelerometry
demog = left_join(demog, first_day)
demog$age_accel = 
floor(
  as.numeric(demog$accel_date - 
    demog$dob) / 
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
  iscen = 4
}

inside = eg$inside[iscen]
threshold = eg$threshold[iscen]
tapp = ifelse(
  threshold,
  "threshold_", "")

# not imputed data
# inside = "no_imputed_"
# Output files
pop_activ_file = file.path(
  out_dir,
  paste0(prefix, 
         "pop_", inside,
         tapp,
         "activity_long.rds"))

data = readRDS(pop_activ_file)
data = data[ 
  data$biobank_id %in% biobank_ids,]

# data = readRDS(pop_no_imp_activ_file)
data_ids = unique(data$biobank_id)


if (!all(demog_ids %in% data_ids)) {
  message(iscen)
  message(eg[iscen,])
  print(demog_ids[ 
    !demog_ids %in% data_ids])
  warning("Not all IDs present")
}
stopifnot(all(data_ids %in% demog_ids))

wide = spread(data, 
  key = minute,
  value = acceleration)

hr_to_min = function(x) {
  time_to_min(ymd_hms(
    paste0("2017-01-01 ", x)
    ))
}
keep_times = hr_to_min(
  c("08:00:00", "16:00:00")
  )
keep_times = seq(keep_times[1], 
  keep_times[2])
keep_times = as.character(keep_times)
weartime = wide
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

# need to think about the
# threshold
res = multi_frag(
  counts = wide[1:10,],
  id = "biobank_id",
  weartime = weartime[1:10,],
  visit = NULL,
  thresh.lower = 30)
