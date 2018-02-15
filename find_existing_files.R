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

no_data = which(!file.exists(measure_file))
print(no_data)
if (length(no_data) > 0) {
	writeLines(no_data, "missing_ids.txt")
} else {
	file.remove("missing_ids.txt")
}

