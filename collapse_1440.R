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
# n_ids = 100

daily = activ = list(
    mode = "list", length = n_ids)
no_imp_daily = no_imp_activ = daily

iid = 1
pb = txtProgressBar(
    min = 1, 
    max = n_ids,
    initial = 1, char = ".", style = 3)

for (iid in seq(n_ids)) {
    # setTxtProgressBar(pb = pb, value = iid)
    # print(paste0("iid is ", iid))

    biobank_id = biobank_ids[iid]
    # creating output directory
    id_out_dir = file.path(coll_dir,
        biobank_id)

    # Output files
    daily_file = file.path(id_out_dir,
        "daily_activity_1440.rds")
    activ_file = file.path(id_out_dir,
        "activity_1440.rds")

    no_imp_daily_file = file.path(id_out_dir,
        "no_imputed_daily_activity_1440.rds")
    no_imp_activ_file = file.path(id_out_dir,
        "no_imputed_activity_1440.rds")

    out_files = c(daily_file, activ_file,
        no_imp_daily_file, no_imp_activ_file)

    if (!all(file.exists(out_files))) {
        # organize the data
        message(
            paste0("Data for iid=", iid, 
                " not all available"))
    } 
    # else {
    #     daily[[iid]] = readRDS(daily_file)
    #     activ[[iid]] = readRDS(activ_file)
    #     no_imp_daily[[iid]] = readRDS(no_imp_daily_file)
    #     no_imp_activ[[iid]] = readRDS(no_imp_activ_file)
    # }
}

close(pb)

# Output files
pop_daily_file = file.path(out_dir,
    "pop_daily_activity_1440.rds")
pop_activ_file = file.path(out_dir,
    "pop_activity_1440.rds")

pop_no_imp_daily_file = file.path(out_dir,
    "pop_no_imputed_daily_activity_1440.rds")
pop_no_imp_activ_file = file.path(out_dir,
    "pop_no_imputed_activity_1440.rds")


daily_df = bind_rows(daily)

saveRDS(object = daily_df, 
    file = pop_daily_file)

rm(list = c("daily", "daily_df"))
gc()

activ_df = bind_rows(activ)
saveRDS(object = activ_df, 
    file = pop_activ_file)
rm(list = c("activ", "activ_df"))
gc()

no_imp_daily_df = bind_rows(no_imp_daily)
saveRDS(object = no_imp_daily_df, 
    file = pop_no_imp_daily_file)
rm(list = c("no_imp_daily", 
    "no_imp_daily_df"))
gc()


no_imp_activ_df = bind_rows(no_imp_activ)
saveRDS(object = no_imp_activ_df, 
    file = pop_no_imp_activ_file)
rm(list = c("no_imp_activ", 
    "no_imp_activ_df"))
gc()


