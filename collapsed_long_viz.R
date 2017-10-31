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

demog_file = file.path(
  tab_dir, 
  paste0("demographics_reformat", 
    "_first_visit.csv.gz"))

demog = read_csv(demog_file)
demog_ids = unique(demog$biobank_id)
demog$biobank_id = as.integer(
    demog$biobank_id)


prefix = "mean_"
#################################
# Not imputed data
#################################
insides = c("", "no_imputed_")
thresholds = c(FALSE, TRUE)
eg = expand.grid(inside = insides, 
  threshold = thresholds,
  stringsAsFactors = FALSE)


iscen = as.numeric(
  Sys.getenv("SGE_TASK_ID")
)
if (is.na(iscen)) {
  iscen = 2
}

inside = eg$inside[iscen]
threshold = eg$threshold[iscen]
tapp = ifelse(
  threshold,
  "threshold_", "")

# not imputed data
# inside = "no_imputed_"
# Output files
pop_activ_file = file.path(
  out_dir,
  paste0(prefix, 
         "pop_", inside,
         tapp,
         "activity_long.rds"))

data = readRDS(pop_activ_file)
# data = readRDS(pop_no_imp_activ_file)
data_ids = unique(data$biobank_id)

if (!all(demog_ids %in% data_ids)) {
  message(iscen)
  message(eg[iscen,])
  print(demog_ids[ !demog_ids %in% data_ids])
  warning("Not all IDs present")
}
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

# pdfname = file.path(res_dir, 
#     paste0("Median_", 
#       inside, tapp,
#       "plots.pdf"))
# pdf(pdfname)

# for (varname in run_vars) {

#     xx = unlist(demog[, varname])
#     continuous = is.numeric(xx)


#     sub = demog %>% 
#         select_("biobank_id", varname)
#     if (continuous) {
#         sub = sub %>% 
#         mutate_(x = varname) %>% 
#         mutate(qage = qcut(x)) 
#         hist(xx, breaks = 100,
#             main = varname)

#     } else {
#         sub = sub %>% mutate_(
#             qage = varname)
#     }
#     sub = sub %>% 
#         select(biobank_id, qage)

#     sdata = left_join(data, sub)
#     sdata = sdata %>% 
#         filter(!is.na(qage))

#     qmean = function(x) {
#         mn = mean(x)
#         q =  quantile(x,
#                 probs = c(0.25, 0.5, 0.75))
#         c(mean = mn, q)
#     }
#     sdata = sdata %>% 
#         group_by(qage, minute) %>% 
#         do( tidy(t(qmean(.$acceleration))))
#     sdata = sdata %>% 
#         rename(q25 = X25.,
#             median = X50.,
#             q75 = X75.)


#     df = sdata %>% 
#         gather(key = measure,
#             value = "value",
#             -qage, -minute)
#     df = df %>% 
#         mutate(
#             time = min_to_time(minute))

#     cum_df = df %>%
#         group_by(qage, measure) %>% 
#         mutate(value = cumsum(value)) 

#     xscale = scale_x_datetime(
#         breaks=date_breaks("2 hour"), 
#         labels=date_format("%H:%M"))


#     this_guide = guides(
#         colour = guide_legend(
#         title = varname))
#     g = ggplot(df, 
#         aes(x = time, y = value,
#             colour = qage)) +
#         geom_line() + 
#         facet_wrap( ~ measure)
#     g = g + transparent_legend
#     g = g + theme(
#         legend.position = c(0.8, 0.9)) 
#     g = g + this_guide
#     g = g + xscale
#     print(g)

#     med = df %>% 
#         filter(measure == "median")
#     cmed = cum_df %>% 
#         filter(measure == "median")    

#     gmed = ggplot(med, 
#         aes(x = time, y = value,
#             colour = qage)) 
#     gmed = gmed + xscale
#     gmed = gmed + this_guide
#     gmed = gmed + transparent_legend
#     gmed = gmed + theme(
#         legend.position = c(0.8, 0.9)) 
#     gmed_line = gmed + geom_line()
#     print(gmed_line)
#     print(gmed_line %+% cmed)

#     gm_smooth = gmed + 
#         geom_smooth(se = FALSE)
#     print(gm_smooth)
#     # print(gm_smooth %+% cmed)

# }

# dev.off()





sdata = sample_n(data, size = 500)
sdata = sdata %>% 
  as.data.frame

rownames(sdata) = sdata$biobank_id
sdata$biobank_id = NULL
sdata = as.matrix(sdata)

pngname = file.path(res_dir,
  paste0("heatmap_",
    inside, 
    tapp,     
    ".png"))

png(pngname)

pheatmap(sdata, 
  cluster_cols = FALSE,
  show_colnames = FALSE,
  show_rownames = FALSE
  )
dev.off()


pngname = file.path(res_dir,
  paste0("log10_heatmap_",
    inside, 
    tapp,     
    ".png"))

png(pngname)

pheatmap(log10(sdata + 1), 
  cluster_cols = FALSE,
  show_colnames = FALSE,
  show_rownames = FALSE
  )
dev.off()


size = 10
pngname = file.path(res_dir,
  paste0("spaghetti_",
    size, "_",
    inside, 
    tapp,     
    ".png"))
ldata = log10(sdata + 1)
r = range(ldata, na.rm = TRUE)
cm = colMeans(ldata, na.rm = TRUE)
cmed = colMedians(ldata, na.rm = TRUE)

picks = ldata %>% 
  as.data.frame %>% 
  sample_n(size = size)   %>% 
  as.matrix
times = min_to_time(as.integer(
  colnames(ldata)))

png(pngname)
plot(x = times, y = cm, 
  ylim = r, type = "n",
  xlab = "Time of the Day", 
  ylab = "Log 10 Acceleration (mg)")

for (irow in seq(size)) {
  col =  alpha(irow, 0.25)
  lines(x = times, picks[irow,], 
    col = col)
}
lines(x = times, cm)
dev.off()



