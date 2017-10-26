# convert_1440
rm(list=ls())
library(biobankr)
library(dplyr)
library(broom)
library(tidyr)
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
  iscen = 2
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
  df = ungroup(df)
  
  date_vec = df$date
  df$date = NULL; gc()
  date_vec = as.POSIXlt(date_vec)
  df$min = date_vec$min
  df$hour = date_vec$hour
  df$date = yyyymmdd(date_vec)
  rm(date_vec); gc()

  df = df %>% 
    select(-n)

  limits = c(3, 6)
  sub_df = df %>% 
  # less than 2 g
    filter(
      acceleration < 2000) %>% 
    mutate(
      keep = hour >= limits[1] &  
      hour <= limits[2]
      ) %>% 
  select(-hour, -date, -min)

  summ_df = sub_df %>% 
    group_by(biobank_id, keep) %>% 
    summarize(mean = mean(acceleration),
      median = median(acceleration))

  # sub_df %>% ggplot(
  #   aes(x= acceleration,
  #     colour = keep)) + 
  #   geom_line(stat = "density")


  probs = seq(0, 1, by = 0.01)
  qdf = sub_df %>% 
    group_by(keep) %>% 
    do( 
      tidy(
        t(quantile(.$acceleration, 
          probs = probs)
        )
      )
    )
  qdf = qdf %>% 
    gather(quantile, value,
    -keep)
  qdf = qdf %>% mutate(
    quantile = as.numeric(
      gsub("^X", "", quantile))
  )


  day_df = sub_df %>% 
    filter(!keep) %>% 
    select(-keep)
  sub_df = sub_df %>% 
    filter(keep) %>% 
    select(-keep)

  h = hist(
    sub_df$acceleration[ 
    sub_df$acceleration < 100],
    breaks = 2000)

  h_day = hist(
    day_df$acceleration[ 
    day_df$acceleration < 100],
    breaks = 2000)


  h0 = hist(
    sub_df$acceleration[ 
      sub_df$acceleration > 0],
    breaks = 2000)  

  cdf = ecdf(sub_df$acceleration)


  log_h = hist(
    log(sub_df$acceleration + 1),
    breaks = 2000)  


