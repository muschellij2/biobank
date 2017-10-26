# convert_1440
rm(list = ls())
library(biobankr)
library(dplyr)
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
  iid = 72505
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
prefix = paste0(func, "_")
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

out_files = c(daily_file, 
              no_imp_daily_file
              )

filter_df = function(df) {
  df = df %>% 
    filter(n > 0)

  df = df %>% 
    filter(is.finite(acceleration))
  df = add_id(df, biobank_id)
  return(df)
} 

# bad id 1000888
# if (!all(file.exists(out_files))) {
  # organize the data
  xdf = bb_read(infile)
  xdf = xdf %>% mutate(
    bad = is.na(as.numeric(acceleration)) & 
      !is.na(acceleration))
  stopifnot(all(!xdf$bad))

  xdf$acceleration = 
    as.numeric(xdf$acceleration)
  df = bb_summarize_minute(xdf, 
    summarize_func = func,
    keep_imputed = TRUE,
    na.rm = TRUE    
    )

  df = filter_df(df)
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

# }