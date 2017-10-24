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

func = "mean"
prefix = paste0(func, "_")



#################################
# Not imputed data
#################################
inside = "no_imputed_"
insides = c("", "no_imputed_")
thresholds = c(FALSE, TRUE)
eg = expand.grid(inside = insides, 
  threshold = thresholds,
  stringsAsFactors = FALSE)


iscen = as.numeric(
  Sys.getenv("SGE_TASK_ID")
)
if (is.na(iscen)) {
  iscen = 1
}

# for (iscen in seq(nrow(eg))) {
  print(iscen)
  ieg = eg[iscen, ]
  inside = ieg$inside
  threshold = ieg$threshold
  
  pop_daily_file = file.path(
    out_dir,
    paste0(prefix, 
           "pop_", inside, 
           "daily_activity_long.rds"))

  # note the change in no_imp_daily
  df = readRDS(file = pop_daily_file)
  check = !any(is.na(df$biobank_id))
  stopifnot(check)

  df$biobank_id = as.integer(df$biobank_id)
  # Only one day
  pop_activ_file = file.path(
    out_dir,
    paste0(prefix, 
           "pop_", inside, 
           ifelse(threshold, 
            "_threshold", ""),
           "activity_long.rds"))


  date_vec = df$date
  df$date = NULL; gc()
  date_vec = as.POSIXlt(date_vec)
  df$minute = time_to_min(date_vec)

  if (threshold) {
    date_vec = yyyymmdd(date_vec)
    df$day = date_vec
    rm(date_vec); gc()

    df = df %>% 
      ungroup %>% 
      group_by(biobank_id, day) %>% 
      mutate(n_minutes = n())

    min_minutes = ceiling(1440*0.95)
    df = df %>% 
      filter(n >= min_minutes)
    df = df %>% 
      select( -n )
  } else {
    rm(date_vec); gc()
  }


  # don't need day anymore
  df$day = NULL
  # df = df %>% 
  #   rename(n = num_obs)

  df = df %>% 
    mutate(sum = n * acceleration)

  df = df %>% 
    select(-acceleration)


  df = df %>% 
    group_by(biobank_id, minute) %>% 
    summarize(sum = sum(sum),
      n = sum(n)) %>% 
    ungroup

  df = df %>% 
    mutate(acceleration = sum/n) %>% 
    select(-sum, -n)

  saveRDS(object = df, 
          file = pop_activ_file)
  rm(list = c("df"))
  for (i in 1:10) {
    gc()
  }
# }





