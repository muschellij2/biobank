# convert_1440
rm(list=ls())
library(biobankr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
options(digits.secs = 2)
# set to
# /dcl01/chatterj/data/
# ukbiobank/phenotype
pheno_dir = Sys.getenv("biobank")

#############################
# Set up directories
#############################
root_dir = file.path(
  pheno_dir,
  "accelerometer")
raw_dir = file.path(root_dir, 
  "raw_data")
res_dir = file.path(root_dir,
    "results") 


cwas = list.files(
  path = raw_dir,
  pattern = ".cwa", 
  full.names = TRUE)

icwa = 1
cwa = cwas[icwa]

x = read_cwa(cwa)

dat = x$data
dat = dat %>% 
  select(time, x, y, z) %>% 
  gather(direction, acceleration,
    x, y, z)

dd = head(dat, 1000)
dd = dd %>% 
  rename(date = "time")
# second(dd$date) = floor(second(dd$date))
dd = date_day_min(dd, 
  from_baseline = FALSE)
# dat = dat %>% 
#   arrange(time, direction)


