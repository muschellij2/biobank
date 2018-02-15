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

prefix = "mean_"
inside = ""
tapp = ""

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

#############################
# Looking at day of visit 
#############################  
data = data %>% 
  select(biobank_id, day) 
data = data %>% 
  distinct() %>% 
  arrange(biobank_id, day) %>% 
  group_by(biobank_id) %>% 
  slice(1)

data = ungroup(data)

data = rename(data,
  accel_date = day)
# Output files
out_file = file.path(
  out_dir,
  "first_day.rds")
saveRDS(data, file = out_file)
