# convert_1440
rm(list=ls())
library(biobankr)
library(dplyr)
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
coll_dir = file.path(root_dir,
                     "collapsed")
out_dir = file.path(root_dir,
                    "analysis")
# think about putting in biobankr
id_file = file.path(root_dir, 
                    "ids.txt")
biobank_ids = readLines(id_file)
n_ids = length(biobank_ids)
add_id = function(df, biobank_id) {
  df %>% 
    mutate(biobank_id = biobank_id)  %>% 
    select(biobank_id, everything())
}
# n_ids = 1000

daily = vector(
  mode = "list", length = n_ids)
names(daily) = biobank_ids


func = "mean"
prefix = paste0(func, "_")
insides = c("", "no_imputed_")

iscen = as.numeric(
  Sys.getenv("SGE_TASK_ID")
)
if (is.na(iscen)) {
  iscen = 1
}
inside = insides[iscen]
print(inside)

summarize_minute = function(df) {

  df = df %>% 
    mutate(sum = n * acceleration)

  df = df %>% 
    group_by(biobank_id) %>% 
    date_day_min(from_baseline = FALSE)

  df = df %>% 
    group_by(biobank_id, minute) %>% 
    summarize(sum = sum(sum),
      n = sum(n)) %>% 
    ungroup
  df = df %>% 
    mutate(acceleration = sum/n) %>% 
    select(-sum, -n)
  return(df)
}


iid = 1
pb = txtProgressBar(
  min = 1, 
  max = n_ids,
  initial = 1, char = ".", style = 3)

for (iid in seq(n_ids)) {
  setTxtProgressBar(pb = pb, value = iid)
  # print(paste0("iid is ", iid))
  
  biobank_id = biobank_ids[iid]
  # creating output directory
  id_out_dir = file.path(
    coll_dir,
    biobank_id)
  
  # Output files
  daily_file = file.path(
    id_out_dir,
    paste0(prefix,
           inside,
           "daily_", 
           "activity_long.rds"))

  out_files = c(daily_file)

  if (!all(file.exists(out_files))) {
    # organize the data
    message(
      paste0("Data for iid=", iid, 
             " not all available"))
  } else {
    df = readRDS(daily_file)
    df$biobank_id = NULL
    daily[[iid]] = df
  }
}

close(pb)

# Output files
pop_daily_file = file.path(
  out_dir,
  paste0(prefix, 
         "pop_", inside, 
         "daily_activity_long.rds"))

daily_df = bind_rows(daily, 
  .id = "biobank_id")
daily_df$biobank_id = as.integer(
  daily_df$biobank_id)
daily_df$acceleration = 
  as.numeric(daily_df$acceleration)


rm(list = c("daily"))
for (i in 1:10) {
  gc()
}

saveRDS(object = daily_df, 
        file = pop_daily_file)
