###############################
# Subsetting demographics
###############################
rm(list = ls())
library(readr)
library(dplyr)
library(tidyr)
library(zoo)

pheno_dir = Sys.getenv("biobank")
fname = file.path(pheno_dir, "pheno.tab")
#############################
# Set up directories
#############################
root_dir = file.path(
  pheno_dir,
  "accelerometer")
tab_dir = file.path(root_dir,
  "tabular")
demog_file = file.path(
  tab_dir, 
  "demographics.csv.gz")

id_file = file.path(root_dir, 
                    "ids.txt")
biobank_ids = readLines(id_file)

rm_id_file = file.path(
  root_dir, 
  "removal_ids.txt")
rm_ids = readLines(rm_id_file)

demog_file = file.path(
  tab_dir, 
  "all_demographics_reformat.csv.gz")
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
demog = read_csv(demog_file,
  col_types = ctypes)

# remove withdrawn ids
demog = demog %>% 
  filter(!(biobank_id %in% rm_ids))

demog = demog %>% 
  arrange(biobank_id, visit, date_attend)

exvars = c("diabetes", "bmi", 
  "bmr", "smoke", "body_fat_pct",
  "walk_pace", "standing_height",
  "weight")
d = demog[, exvars]
demog$missdata = rowSums(!is.na(d)) == 0
rm(d)
demog = demog %>% 
  mutate(have_date = 
    !is.na(demog$date_attend))

table(have_date = demog$have_date, 
  missdata = demog$missdata)
demog = demog %>% 
  filter(have_date)

miss = demog %>% 
  filter(missdata)
miss = miss[, c("biobank_id", 
  "visit", "date_attend", 
    exvars)]


demog = demog %>% 
  mutate(
    age_recruitment = na.locf(
      age_recruitment, 
      na.rm = FALSE))  

