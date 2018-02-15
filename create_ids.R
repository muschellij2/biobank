# convert_1440
rm(list=ls())
library(biobankr)
library(dplyr)
library(DBI)
library(dbplyr)
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


############################
# these should be a w1771 format
############################
removal_files = list.files( 
  pattern ="^w.*.csv",
  path = pheno_dir,
  full.names = TRUE)

removal_ids = c(sapply(
  removal_files, readLines))

rm_id_file = file.path(
  root_dir, 
  "removal_ids.txt")
writeLines(removal_ids, con = rm_id_file)


############################
# get IDS with accelerometry
############################
files = list.files(
  pattern = ".*.gz$", 
  path = data_dir)
ids = strsplit(files, split = "_")
ids = sapply(ids, function(x) x[1])
ids = unique(ids)

ids = setdiff(ids, removal_ids)
id_file = file.path(root_dir, 
                    "ids.txt")

writeLines(ids, con = id_file)







