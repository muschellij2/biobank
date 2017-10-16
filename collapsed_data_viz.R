# convert_1440
rm(list=ls())
library(biobankr)
library(dplyr)
library(readr)
library(matrixStats)
library(ggplot2)
library(tidyr)
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

# Output files
pop_daily_file = file.path(out_dir,
    "pop_daily_activity_1440.rds")
pop_activ_file = file.path(out_dir,
    "pop_activity_1440.rds")

pop_no_imp_daily_file = file.path(out_dir,
    paste0("pop_no_imputed_daily_", 
        "activity_1440.rds"))
pop_no_imp_activ_file = file.path(out_dir,
    "pop_no_imputed_activity_1440.rds")


demog_file = file.path(
  tab_dir, 
  paste0("demographics_reformat", 
    "_first_visit.csv.gz"))

demog = read_csv(demog_file)
demog = rename(demog, biobank_id = eid)
demog_ids = demog$biobank_id
demog$biobank_id = as.character(
    demog$biobank_id)

data = readRDS(pop_activ_file)
data_ids = data$biobank_id

all(demog_ids %in% data_ids)
all(data_ids %in% demog_ids)

qcut = function(x, ...) {
    q = quantile(x, ...)
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
    "age_assessment", "bmi")
varname = run_vars[1]

pdfname = file.path(res_dir, 
    "Median_plots.pdf")

for (varname in run_vars) {

    x = unlist(demog[, varname])
    continuous = is.numeric(x)


    sub = demog %>% 
        select_("biobank_id", varname)
    if (continuous) {
        sub = sub %>% 
        mutate_(x = varname) %>% 
        mutate(qage = qcut(x)) 
    } else {
        sub = sub %>% mutate_(qage = varname)
    }
    sub = sub %>% 
        select(biobank_id, qage)

    sdata = left_join(data, sub)

    sdata = split(sdata, sdata$qage)
    sdata = lapply(sdata, function(x) {
        qage = unique(x$qage)
        x = x %>% 
            select(-biobank_id, -qage)
        cm = colMeans(x, na.rm = TRUE)
        x = as.matrix(x)
        cmed = colMedians(x, na.rm = TRUE)
        df = data.frame(
            time = as.numeric(colnames(x)),
            mean = cm,
            median = cmed)
        df$qage = qage
        df
    })
    sdata = bind_rows(sdata)


    df = sdata %>% 
        gather(key = measure,
            value = "value",
            mean, median)
    cum_df = sdata %>%
        group_by(qage) %>% 
        arrange(qage, time) %>%  
        mutate(mean = cumsum(mean),
            median = cumsum(median)) %>% 
        gather(key = measure,
            value = "value",
            mean, median)    

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
    print(g)

    med = df %>% 
        filter(measure == "median")
    cmed = cum_df %>% 
        filter(measure == "median")    

    gmed = ggplot(med, 
        aes(x = time, y = value,
            colour = qage)) 
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
    print(gm_smooth %+% cmed)

}

dev.off()
# sdata = data %>% 
#     select(-biobank_id) %>% 
#     group_by(qage) %>% 
#     summarise_all(
#         funs(mean = mean,
#         med = median)

#         )





