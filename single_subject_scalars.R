# convert_1440
rm(list=ls())
library(biobankr)
library(dplyr)
library(broom)
library(tidyr)
library(ggplot2)
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
res_dir = file.path(root_dir,
    "results")


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
  df = df[ df$biobank_id %in% biobank_ids,]
  stopifnot(check)

  if (threshold) {

    df = df %>% 
      filter(above_threshold)
  } 

  df = df %>% 
    select(-above_threshold)

  df = df %>% 
    select(-n)

  hr_min = df$minute
  df$minute = NULL
  hr_min = min_to_hr_min(hr_min)
  df$min = hr_min[, "min"]
  df$hour = hr_min[, "hour"]
  rm(hr_min)

  # 3 am to 5 am
  limits = c(3, 5)
  sub_df = df %>% 
    select(-day, -min) %>% 
    mutate(
      night_time = hour >= limits[1] &  
      hour <= limits[2]
      ) %>% 
  select(-hour)

  day = sub_df[ !sub_df$night_time, ]
  night = sub_df[ sub_df$night_time, ]

  r = range(c(day$acceleration))
  r = range(c(r, 
    range(night$acceleration)
    ))

  ##########################
  # minute population levels
  ##########################
  min_pop_df = sub_df %>% 
    ungroup %>% 
    group_by(night_time) %>% 
    summarize(
      median = median(acceleration),
      q95 = quantile(acceleration, 
        probs = 0.95),
      mean = mean(acceleration))

  min_q95 = min_pop_df %>% 
    filter(night_time) %>%  # keep night
    select(q95) %>% unlist

  ##########################
  # Histograms of all the data
  ##########################
  pdfname = file.path(res_dir,
    paste0("day_night_all_", 
      inside, 
       ifelse(threshold, 
        "threshold", ""),      
      ".pdf")
    )
  pdfname = gsub("_.pdf", 
    ".pdf", pdfname)
  pdf(pdfname)
    hist(
      day$acceleration, 
      main = "day",
      breaks= 2000,
      xlim = r)
    hist(
      night$acceleration, 
      main = paste0("night: ", limits[1],
        " to ", limits[2]),
      breaks= 2000,      
      xlim = r)

    hist(
      day$acceleration[ 
        day$acceleration < 300], 
      main = "day",
      breaks= 2000,
      xlim = c(0, 300))
    abline(v = min_q95, col = "red")
    hist(
      night$acceleration[
        night$acceleration < 300], 
      main = paste0("night: ", limits[1],
        " to ", limits[2]),
      breaks= 2000,      
      xlim = c(0, 300))    
    abline(v = min_q95, col = "red")
  dev.off()
  rm(plot_df); gc()

  rm(list = c("day", "night")); 
  gc()


  # less than 2 g
  # 500mg is 99.9 quantile
  # actually 498.4
  # sub_df = sub_df %>% 
  #   filter(
  #     acceleration < 2000)

  sub_df = sub_df %>% 
    filter(
      acceleration < 500)

  ##########################
  # look at the txt file of the bad_ids
  ##########################
  odd_ids = c(2898965, 
    3853046, 
    5952814,
    1496134, 2005661L, 2967919L, 
    3462861L, 4476664L, 4992884L, 
    5849230L)

  bad_df = sub_df %>% 
    filter(biobank_id %in% odd_ids)


  ##########################
  # Summarize by person over
  # all day/night
  ##########################    
  summ_df = sub_df %>% 
    group_by(biobank_id, night_time) %>% 
    summarize(mean = mean(acceleration),
      median = median(acceleration),
      sd = sd(acceleration),
      n = n())

  # Only one per person
  summary_file = file.path(
    out_dir,
    paste0(prefix, 
           "pop_", inside, 
           ifelse(threshold, 
            "threshold_", ""),
           "summary.rds"))    
  saveRDS(summ_df, file = summary_file)

  ##########################
  # summarize over population
  ##########################
  pop_df = summ_df %>% 
    filter(n > 0) %>%   
    ungroup %>% 
    group_by(night_time) %>% 
    summarize(
      median = median(mean),
      q95 = quantile(mean, probs = 0.95),
      mean = mean(mean))
  ##########################
  # quantile of mean for night
  ##########################  
  q95 = pop_df %>% 
    filter(night_time) %>% 
    select(q95) %>% unlist
  
  ##########################
  # Plot day vs. night means
  ##########################  
  plot_df = summ_df %>% 
    filter(n > 0) %>% 
    select(-median, -n)

  plot_df = plot_df %>% 
    mutate(
      time = if_else(night_time, 
        "night", "day")
      ) %>% 
    select(-night_time)
  plot_df = plot_df %>% 
    spread(time, mean)

  g = plot_df %>% 
    ggplot(aes(x = night, y = day)) +
    geom_hex() + 
    geom_hline(yintercept = q95, 
      colour = "red")
  ##########################
  # all data
  ##########################      
  pngname = file.path(res_dir,
    paste0("day_night_means_all_",
      inside, 
      ifelse(threshold, 
            "threshold", ""),      
      ".png"))
  pngname = gsub("_.png", 
    ".png", pngname)  
  png(pngname)
    print(g)
  dev.off()

  ##########################
  # subset less than 100mg
  ##########################  
  pngname = file.path(res_dir,
    paste0("day_night_means_", 
      inside, 
      ifelse(threshold, 
            "threshold", ""),      
      ".png")
    )
  pngname = gsub("_.png", 
    ".png", pngname)    
  png(pngname)
    pp = plot_df  %>% 
      filter(day <= 100 & night <= 100) 
    print(g %+% pp)
  dev.off()  


  # plot_df %>% filter(day > 300)
  # sub_df %>% ggplot(
  #   aes(x= acceleration,
  #     colour = night_time)) + 
  #   geom_line(stat = "density")


  probs = seq(0, 1, by = 0.01)
  qdf = sub_df %>% 
    group_by(night_time) %>% 
    do( 
      tidy(
        t(quantile(.$acceleration, 
          probs = probs)
        )
      )
    )
  qdf = qdf %>% 
    gather(quantile, value,
    -night_time)
  qdf = qdf %>% mutate(
    quantile = as.numeric(
      gsub("^X", "", quantile))
  )


