###############################
# Subsetting information on variables
###############################
rm(list = ls())
library(readr)
library(dplyr)
library(xml2)
library(rvest)
library(tidyr)

pheno_dir = Sys.getenv("biobank")
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


fname = file.path(pheno_dir, 
    "ukb8386.html")
xx = readLines(fname)
bad_string = "NEWLINE"
xx = gsub("<br>", bad_string, xx)
tfile = tempfile(fileext = ".html")
writeLines(xx, tfile)
doc = read_html(tfile)

tabs = html_nodes(doc, 
    xpath = "//table")
tab = tabs[[2]]

xdf = html_table(tab, trim = FALSE)

##############################
# Make the coding column
##############################
df = xdf
df$Description = gsub(
    bad_string, " ", df$Description)
df$coding = NA
have_coding = grepl("coding", df$Description)
df$coding[ have_coding ] = gsub(
  "(.*) (Uses data-coding.*)", 
  "\\2", df$Description[have_coding])
df$Description[ have_coding ] = gsub(
  "(.*) (Uses data-coding.*)", 
  "\\1", df$Description[ have_coding ])
df$coding = gsub(
  ".*data-coding (.*).*", 
  "\\1", df$coding)
df$coding = gsub(
  "(.*) comprises.*", 
  "\\1", df$coding)


##############################
# make variable names to 
# match pheno.tab
##############################
df$varname = gsub("-", ".", df$UDI)

df = df %>% 
  separate(varname,
    into = c("var_id", "visit", "level"),
    remove = FALSE)

df$varname = paste0("f.", df$varname)

outfile = file.path(tab_dir, 
  "variable_description.csv.gz")

write_csv(x = df, path = outfile)


