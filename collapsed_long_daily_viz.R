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

demog_file = file.path(
  tab_dir, 
  paste0("demographics_reformat", 
    "_first_visit.csv.gz"))

demog = read_csv(demog_file)
demog_ids = unique(demog$biobank_id)
demog$biobank_id = as.integer(
    demog$biobank_id)


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
# data = readRDS(pop_no_imp_activ_file)
data_ids = unique(data$biobank_id)


if (threshold) {
  data = data %>% 
    filter(above_threshold)
} 
data = data %>% 
  select(-above_threshold)

#############################
# Looking at time 
day_df = data %>% 
  select(biobank_id, day) %>% 
  distinct() %>% 
  arrange(biobank_id, day) %>% 
  group_by(biobank_id) %>% 
  slice(1)

day_df$date = ymd(day_df$day)
range_date = range(day_df$date)
breaks = seq(range_date[1], 
  range_date[2] + 
  as.period(3, "month"), 
  by = "3 month")
h = hist(day_df$date, breaks=breaks)

sub_df = df %>% 
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