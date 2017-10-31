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
library(pheatmap)
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
demog = rename(demog, biobank_id = eid)
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
pop_activ_file = file.path(
  out_dir,
  paste0(prefix, 
         "pop_", inside,
         tapp,
         "activity_long.rds"))

# Output files
pop_activ_1440 = file.path(
  out_dir,
  paste0(prefix, 
         "pop_", inside,
         tapp,
         "activity_1440.rds"))

if (file.exists(pop_activ_1440)) {
  data = readRDS(pop_activ_file)
  # data = readRDS(pop_no_imp_activ_file)
  data_ids = unique(data$biobank_id)

  data = spread(data,
    key = minute,
    value = acceleration)
  saveRDS(data, file = pop_activ_1440)
} else {
  data = readRDS(pop_activ_1440) 
}




