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
# think about putting in biobankr
id_file = file.path(root_dir, 
                    "ids.txt")
biobank_ids = readLines(id_file)
n_ids = length(biobank_ids)
stub = "_90004_0_0.csv.gz"

db_name = file.path(out_dir, 
    "data.db")

con <- dbConnect(RMariaDB::MariaDB(),
    dbname = db_name)

iid = as.numeric(
  Sys.getenv("SGE_TASK_ID")
)
if (is.na(iid)) {
  iid = 25
}

print(paste0("iid is ", iid))

biobank_id = biobank_ids[iid]
# original CSV.gz
infile = file.path(
  data_dir, 
  paste0(biobank_id, stub)
)

# organize the data
df = bb_read(infile)
add_id = function(df, biobank_id) {
  df %>% 
    mutate(biobank_id = biobank_id)  %>% 
    select(biobank_id, everything())
}
df  = add_id(df, biobank_id)

copy_to(con, df, "long",
  temporary = FALSE
)



dbDisconnect(con)