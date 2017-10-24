# convert_1440
rm(list=ls())
library(biobankr)
library(dplyr)
library(readr)
library(matrixStats)
library(ggplot2)
library(tidyr)
library(scales)
library(broom)
# set to
# /dcl01/chatterj/data/ukbiobank/phenotype
pheno_dir = Sys.getenv("biobank")

#############################
# Set up directories
#############################
root_dir = file.path(
  pheno_dir,
  "accelerometer")
tab_dir = file.path(root_dir,
  "tabular")
data_dir = file.path(
  root_dir,
  "intensity")
coll_dir = file.path(root_dir,
    "collapsed")
out_dir = file.path(root_dir,
    "analysis")
res_dir = file.path(root_dir,
    "results")

prefix = "mean_"
# inside = ""
# not imputed data
inside = "no_imputed_"
# Output files
pop_daily_file = file.path(
  out_dir,
  paste0(prefix, 
         "pop_", inside,
         "daily_activity_long.rds"))

pop_activ_file = file.path(
  out_dir,
  paste0(prefix, 
         "pop_", inside,
         "activity_long.rds"))

demog_file = file.path(
  tab_dir, 
  paste0("demographics_reformat", 
    "_first_visit.csv.gz"))

demog = read_csv(demog_file)
demog = rename(demog, biobank_id = eid)
demog_ids = unique(demog$biobank_id)
demog$biobank_id = as.character(
    demog$biobank_id)

data = readRDS(pop_activ_file)
# data = readRDS(pop_no_imp_activ_file)
data_ids = unique(data$biobank_id)

stopifnot(all(demog_ids %in% data_ids))
stopifnot(all(data_ids %in% demog_ids))

qcut = function(x, 
    na.rm = TRUE,
    ...) {
    q = quantile(x, na.rm = na.rm, ...)
    q = unname(q)
    x = cut(x, 
        breaks = q,
        include.lowest = TRUE)
    return(x)
}

###############################
# Running age
###############################
# varname = "age_assessment"
# continuous = TRUE

transparent_legend =  theme(
  legend.background = element_rect(
    fill = "transparent"),
  legend.key = element_rect(
    fill = "transparent", 
    color = "transparent")
)
run_vars = c("sex", 
    "age_assessment", "bmi",
    "smoke", 
    # "genetic_sex",
    "bmr", "body_fat_pct")
# varname = run_vars[1]
varname = "bmi"
demog = demog[, c("biobank_id", run_vars)]

pdfname = file.path(res_dir, 
    paste0("Median_", 
      inside,
      "plots.pdf"))
pdf(pdfname)

for (varname in run_vars) {

    xx = unlist(demog[, varname])
    continuous = is.numeric(xx)


    sub = demog %>% 
        select_("biobank_id", varname)
    if (continuous) {
        sub = sub %>% 
        mutate_(x = varname) %>% 
        mutate(qage = qcut(x)) 
        hist(xx, breaks = 100,
            main = varname)

    } else {
        sub = sub %>% mutate_(
            qage = varname)
    }
    sub = sub %>% 
        select(biobank_id, qage)

    sdata = left_join(data, sub)
    sdata = sdata %>% 
        filter(!is.na(qage))

    qmean = function(x) {
        mn = mean(x)
        q =  quantile(x,
                probs = c(0.25, 0.5, 0.75))
        c(mean = mn, q)
    }
    sdata = sdata %>% 
        group_by(qage, minute) %>% 
        do( tidy(t(qmean(.$acceleration))))
    sdata = sdata %>% 
        rename(q25 = X25.,
            median = X50.,
            q75 = X75.)


    df = sdata %>% 
        gather(key = measure,
            value = "value",
            -qage, -minute)
    df = df %>% 
        mutate(
            time = min_to_time(minute))

    cum_df = df %>%
        group_by(qage, measure) %>% 
        mutate(value = cumsum(value)) 

    xscale = scale_x_datetime(
        breaks=date_breaks("2 hour"), 
        labels=date_format("%H:%M"))


    this_guide = guides(
        colour = guide_legend(
        title = varname))
    g = ggplot(df, 
        aes(x = time, y = value,
            colour = qage)) +
        geom_line() + 
        facet_wrap( ~ measure)
    g = g + transparent_legend
    g = g + theme(
        legend.position = c(0.8, 0.9)) 
    g = g + this_guide
    g = g + xscale
    print(g)

    med = df %>% 
        filter(measure == "median")
    cmed = cum_df %>% 
        filter(measure == "median")    

    gmed = ggplot(med, 
        aes(x = time, y = value,
            colour = qage)) 
    gmed = gmed + xscale
    gmed = gmed + this_guide
    gmed = gmed + transparent_legend
    gmed = gmed + theme(
        legend.position = c(0.8, 0.9)) 
    gmed_line = gmed + geom_line()
    print(gmed_line)
    print(gmed_line %+% cmed)

    gm_smooth = gmed + 
        geom_smooth(se = FALSE)
    print(gm_smooth)
    # print(gm_smooth %+% cmed)

}

dev.off()
# sdata = data %>% 
#     select(-biobank_id) %>% 
#     group_by(qage) %>% 
#     summarise_all(
#         funs(mean = mean,
#         med = median)

#         )





