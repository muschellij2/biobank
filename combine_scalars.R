# convert_1440
rm(list = ls())
library(biobankr)
library(dplyr)
library(readr)
library(tidyr)
library(pbapply)
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
out_dir = file.path(root_dir,
                    "collapsed")
result_dir = file.path(root_dir,
                    "analysis")
# think about putting in biobankr
id_file = file.path(root_dir, 
                    "ids.txt")
biobank_ids = readLines(id_file)


# creating output directory
id_out_dir = file.path(out_dir,
                       biobank_ids)

func = "mean"
prefix = paste0(func, "_")

measure_file = file.path(
  id_out_dir,
  paste0(prefix,
         "daily_", 
         "activity_measures.rds"))
names(measure_file) = biobank_ids

squant = function(quant, level = "minute") {
  # quant = spread(quant, names, x)
  if ("night_time" %in% colnames(quant)) {
    quant$time = ifelse(quant$night_time,
      "night", "day")
    quant$night_time = NULL
  } else {
    quant$time = "overall"
  }
  quant$level = level
  quant
}

measure_file = sample(measure_file, size = 100)
N = length(measure_file)


ouput_result_file = file.path(
  result_dir,
  "individual_quantiles.rds")

L = vector(mode = "list", 
  length = N)
names(L) = names(measure_file)

iid = 50
pb = txtProgressBar(style = 3, max = N)
for (iid in seq(N)) {
  d = try({
      read_rds(measure_file[iid])
  })

  L[[iid]] = d
  setTxtProgressBar(pb, value = iid)
}
close(pb)


keep = !sapply(L, inherits, "try-error")
L = L[keep]

d = L[[1]]

res = pblapply(L, function(d){

  runner = function(x, level = "minute") {
    x$quant = squant(x$quant, level = level)
    x$night_quant = squant(x$night_quant, 
      level = level)
    x$mode = data_frame(x = x$mode,
      names = "mode")

    x$daynight_mode$names = "mode"
    x$daynight_mode$x = x$daynight_mode$mode
    x$mode = squant(x$mode, level =level)

    x$daynight_mode = squant(x$daynight_mode, 
      level = level)
    x
  }

  d$five_sec = runner(d$five_sec, "5sec")
  d$minute = runner(d$minute, "minute")

  df = bind_rows(
    d$five_sec$quant,
    d$minute$quant,
    d$five_sec$night_quant,
    d$minute$night_quant,

    d$five_sec$mode,
    d$minute$mode,

    d$five_sec$daynight_mode,
    d$minute$daynight_mode
    )
  df$mode = NULL
  df
})

res = bind_rows(res, .id = "biobank_id")
res = rename(res, quant = names, value = x)


write_rds(res, 
  path = ouput_result_file, 
  compress = "xz")

modes = res %>% 
  filter(quant == "mode") 

modes %>% 
  ggplot(aes(x = value, colour = time)) +
  geom_line(stat = "density") + 
  facet_wrap(~ level)

modes = res %>% 
  filter(quant %in% c("mode", "50%", "10%")) 

modes %>% 
  ggplot(aes(x = value, colour = time)) +
  geom_line(stat = "density") + 
  facet_grid(quant ~ level)





# }