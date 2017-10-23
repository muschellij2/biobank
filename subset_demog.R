###############################
# Subsetting demographics
###############################
rm(list = ls())
library(readr)
library(dplyr)
library(tidyr)

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
    "f.34.", "f.53.", "f.21003.", 
    "f.21022.",
    "f.54.",
    "f.22001.", "f.23099.", "f.23105.", 
    "f.924.", "f.2443.", "f.6150.", 
    "f.21000.", "f.20160.", "f.6142.", 
    "f.40000.", "f.40005.", "f.40007.", 
    "f.40008.")
  vis = c(0:20)
  endings = c(0:300)
  eg = expand.grid(stub = stubs, 
                   visit = vis, 
                   end = endings)
  eg$cn = paste0(eg$stub, 
                 eg$visit, ".", eg$end)
  cn = eg$cn
  cn = c("f.eid", cn)
  keep_cn = intersect(colnames(df), cn)
  
  all_demog = df[, keep_cn]
  
  
  write_csv(all_demog, path = demog_file)
  
} else {
  all_demog = read_csv(demog_file)
}

demog = all_demog %>% 
  filter(f.eid %in% biobank_ids)

desc_file = file.path(tab_dir, 
  "variable_description.csv.gz")
vars = read_csv(desc_file)

run_vars = vars %>% 
  filter(varname %in% colnames(demog)) 
renaming_file = file.path(tab_dir,
  "demog_vars_timevarying.csv")
ren_df = read_csv(renaming_file)
sub_df = ren_df %>% 
  filter(time_varying)
non_tv = ren_df %>% 
  filter(!time_varying)
non_tv$non_timevar = paste0("f.", 
  non_tv$var_id, ".0.0")


run_vars = left_join(run_vars, ren_df, 
  by = "var_id")

coded = run_vars %>% 
  filter(!is.na(coding)) %>% 
  select(varname, coding, Description, 
    Type,
    everything())

#######################
# Grabbing labels
#######################
codings = list.files("coding.*[.]tsv",
  path = tab_dir, full.names = TRUE)
names(codings) = gsub(
  "coding(.*).tsv", "\\1",
  basename(codings))
codings = lapply(codings, read_tsv)

icol = 1
for (icol in seq(nrow(coded))) {
  cname = coded$varname[icol]
  code_num = coded$coding[icol]
  code_num = as.character(code_num)
  coding = codings[[code_num]]
  x = demog[, cname]
  x = unlist(x)
  x = unname(x)
  x = factor(x, 
    level = coding$coding,
    labels = coding$meaning)  
  demog[, cname] = x
}

###################################
# Reshaping to long for visit
###################################
d = demog
vis_numbers = c(0:5)
tvar = paste0(
  "f.", sub_df$var_id)
tvar = outer(tvar, vis_numbers, paste,
  sep = ".")
tvar = c(tvar)
tvar = outer(tvar, 0:40,
  paste, sep = ".")
tvar = c(tvar)
tvar = intersect(colnames(demog), tvar)
tvar = sort(tvar)

d = gather_(d, 
  key_col = "variable", 
  value_col = "value",
  tvar)

d = d %>% 
  separate(col = variable,
    into = c("f", "var_id", 
      "visit", "zero"),
    sep = "[.]") %>% 
  select(-f)

d$visit = as.numeric(d$visit)
d = left_join(d, ren_df, 
  by = "var_id") %>% 
  select(-time_varying)

stopifnot(!any(is.na(d$rename)))

d = d %>% 
  group_by(var_id) %>% 
  mutate(max_num = max(as.numeric(zero)),
    rename = ifelse(max_num > 0,
      paste0(rename, ".", zero),
      rename)) %>% 
  ungroup

d = d %>% 
  select(-var_id, -zero, -max_num)

# now should be n * visits data
d = d %>% 
  spread(key = rename, value = value)

# renaming the single time point columns
d = d %>% 
  rename(
    eid = f.eid,
    sex = f.31.0.0,
    yob = f.34.0.0,
    mob = f.52.0.0
    )

cancer = colnames(d)
cancer = grepl("40008|40005", cancer)
d = d[, !cancer]

outfile = file.path(
  tab_dir, 
  "demographics_reformat.csv.gz")
write_csv(d, path = outfile)

first = d %>% 
  filter(visit == 0)

outfile = file.path(
  tab_dir, 
  paste0("demographics_reformat", 
    "_first_visit.csv.gz"))
write_csv(first, path = outfile)


###############################
# remove cancer data 
###############################
