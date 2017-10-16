###############################
# Subsetting demographics
###############################
rm(list = ls())
library(readr)
library(dplyr)

pheno_dir = Sys.getenv("biobank")
fname = file.path(pheno_dir, "pheno.tab")
#############################
# Set up directories
#############################
root_dir = file.path(
  pheno_dir,
  "accelerometer")
tab_dir = file.path(root_dir,
  "tabular")
demog_file = file.path(
  tab_dir, 
  "demographics.csv.gz")

id_file = file.path(root_dir, 
                    "ids.txt")
biobank_ids = readLines(id_file)

if (!file.exists(demog_file)) {
  df = read_tsv(fname)
  
  stubs = c(
    "f.21001.", "f.31.", "f.52.", 
    "f.34.", "f.53.", "f.21003.", "f.21022.",
    "f.22001.", "f.23099.", "f.23105.", 
    "f.924.", "f.2443.", "f.6150.", 
    "f.21000.", "f.20160.", "f.6142.", 
    "f.40000.", "f.40005.", "f.40007.", 
    "f.40008.")
  vis = c(0:20)
  endings = c(0:300)
  eg = expand.grid(stub = stubs, 
                   visit = vis, end = endings)
  eg$cn = paste0(eg$stub, 
                 eg$visit, ".", eg$end)
  cn = eg$cn
  cn = c("f.eid", cn)
  keep_cn = intersect(colnames(df), cn)
  
  demog = df[, keep_cn]
  
  
  write_csv(demog, path = outfile)
  
} else {
  demog = read_csv(demog_file)
}


desc_file = file.path(tab_dir, 
  "variable_description.csv.gz")
vars = read_csv(desc_file)

run_vars = vars %>% 
  filter(varname %in% colnames(demog)) 
coded = run_vars %>% 
  filter(grepl("coding", tolower(Description)))
coded$coding = gsub(
  ".*data-coding (.*) comprises.*", 
  "\\1", coded$Description)

demog_accel = demog %>% 
  filter(f.eid %in% biobank_ids)


codings = list.files("coding.*[.]tsv",
  path = tab_dir, full.names = TRUE)
names(codings) = gsub(
  "coding(.*).tsv", "\\1",
  basename(codings))
codings = lapply(codings, read_tsv)

apply_coding = function(x, code_num) {
  code_num = as.character(code_num)
  coding = codings[[code_num]]
  code_factor = factor(x, 
    level = coding$coding,
    labels = coding$meaning)

}


demog[,coded$varname[1]]
