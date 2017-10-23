rm(list=ls())
library(rwunderground)
library(lubridate)
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


date_range = c("2006-03-13", 
    "2010-10-01")
date_range = lubridate::ymd(date_range)
date_seq = seq(
    date_range[1], 
    date_range[2], 
    by = "1 day")
date_seq = rev(date_seq)

cities = c("Bristol", "Leeds", "Reading", 
  "Nottingham", "Hounslow", "Croydon", 
    "Newcastle", "Sheffield", "Liverpool", 
    "Birmingham", "Bury", "Middlesborough", 
    "Edinburgh", "Oxford", "Cardiff", "Barts", 
    "Glasgow", "Stoke", "Manchester", 
    "Stockport", "Swansea", 
    "Wrexham")

outfile = file.path(tab_dir,
    "weather.csv") 

eg = expand.grid(
    city = cities, 
    date = date_seq)

if (!file.exists(outfile)) {
    i = 1
} else {
    df = read_csv(outfile)

    anti_join()
}


    idf = eg[i, ]
    loc = set_location(
        city = idf$city,
        territory = "GB"
        )
    date = as.character(idf$date)
    date = gsub("-", "", date)
    res = history(loc, date = date)
    alm = almanac(loc)

}