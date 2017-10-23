# convert_1440
rm(list = ls())
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
out_dir = file.path(root_dir,
                    "collapsed")
# think about putting in biobankr
id_file = file.path(root_dir, 
                    "ids.txt")
biobank_ids = readLines(id_file)
stub = "_90004_0_0.csv.gz"

iid = as.numeric(
  Sys.getenv("SGE_TASK_ID")
)
if (is.na(iid)) {
  iid = 25
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


func = "mean"
# prefix = ifelse(
#   func != "mean",
#               paste0(func, "_"), "")
prefix = paste0(func, "_")
# Output files
daily_file = file.path(
  id_out_dir,
  paste0(prefix, 
         "daily_activity_1440.rds"))
activ_file = file.path(
  id_out_dir,
  paste0(prefix, 
         "activity_1440.rds"))

no_imp_daily_file = file.path(
  id_out_dir,
  paste0(prefix,
         "no_imputed_daily_", 
         "activity_1440.rds"))
no_imp_activ_file = file.path(
  id_out_dir,
  paste0(prefix, 
         "no_imputed_activity_1440.rds"))

count_file = file.path(
  id_out_dir,
  "non_imputed_counts_1440.rds")

out_files = c(daily_file, activ_file,
              no_imp_daily_file, 
              no_imp_activ_file,
              count_file)
# bad id 1000888
if (!all(file.exists(out_files))) {
  # organize the data
  df = bb_read(infile)

  
  if (!file.exists(count_file)) {
    counts = bb_1440_count(
      df, long = TRUE,
      keep_imputed = FALSE)
    counts = add_id(counts, biobank_id)
    
    saveRDS(counts, file = count_file)
  }
  
  if (!file.exists(daily_file)) {
    means = bb_1440(
      df, 
      summarize_func = func
    )
    means = add_id(means, biobank_id)
    
    saveRDS(means, file = daily_file)
  }
  
  if (!file.exists(activ_file)) {
    
    means_noday = bb_1440(
      df, 
      summarize_func = func,
      summarize_over_day = TRUE
    )
    
    means_noday = add_id(means_noday, 
                         biobank_id)
    
    saveRDS(means_noday, file = activ_file)
  }
  
  if (!file.exists(no_imp_daily_file)) {
    
    no_imp_means = bb_1440(
      df, 
      summarize_func = func,
      keep_imputed = FALSE
    )
    
    no_imp_means = add_id(no_imp_means, 
                          biobank_id)
    
    saveRDS(no_imp_means, 
            file = no_imp_daily_file)
  }
  if (!file.exists(no_imp_daily_file)) {
    
    no_imp_means_noday = bb_1440(
      df, 
      summarize_func = func,
      summarize_over_day = TRUE,
      keep_imputed = FALSE
    )
    
    no_imp_means_noday = add_id(
      no_imp_means_noday, 
      biobank_id)
    
    saveRDS(no_imp_means_noday, 
            file = no_imp_activ_file)   
  } 
  
  
}

# }
