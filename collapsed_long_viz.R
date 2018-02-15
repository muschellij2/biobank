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
library(lubridate)
library(pheatmap)
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


id_file = file.path(root_dir, 
                    "ids.txt")
biobank_ids = readLines(id_file)

ctypes = cols(
  .default = col_character(),
  biobank_id = col_integer(),
  yob = col_integer(),
  weight_imaging = col_integer(),
  height_imaging = col_integer(),
  visit = col_integer(),
  age_assessment = col_integer(),
  age_death = col_double(),
  age_recruitment = col_integer(),
  bmi = col_double(),
  bmr = col_double(),
  body_fat_pct = col_double(),
  date_attend = col_date(format = ""),
  date_death = col_date(format = ""),
  manual_weight = col_double(),
  standing_height = col_double(),
  weight = col_double()
  )
demog_file = file.path(
  tab_dir, 
  paste0("demographics_reformat", 
    "_first_visit.csv.gz"))

demog = read_csv(demog_file, 
  col_types = ctypes)
demog = demog[ 
  demog$biobank_id %in% biobank_ids, ]
demog_ids = unique(demog$biobank_id)
demog$biobank_id = as.integer(
    demog$biobank_id)
if (!is.Date(demog$dob)) {
  demog$dob = ymd(demog$dob)
}


# Merging day of accelerometry
first_file = file.path(
  out_dir,
  "first_day.rds")
first_day = readRDS(first_file)

first_day = first_day[ 
  first_day$biobank_id %in% biobank_ids,]
first_day = first_day %>% 
  mutate(accel_date = ymd(accel_date))

# Age at accelerometry
demog = left_join(demog, first_day)
demog$age_accel = 
floor(
  as.numeric(demog$accel_date - 
    demog$dob) / 
  365.25
  )
demog = demog %>% 
  select(-accel_date)


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
  iscen = 4
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
data = data[ 
  data$biobank_id %in% biobank_ids,]

# data = readRDS(pop_no_imp_activ_file)
data_ids = unique(data$biobank_id)


if (!all(demog_ids %in% data_ids)) {
  message(iscen)
  message(eg[iscen,])
  print(demog_ids[ 
    !demog_ids %in% data_ids])
  warning("Not all IDs present")
}
stopifnot(all(data_ids %in% demog_ids))

wide = spread(data, 
  key = minute,
  value = acceleration)

##############################
# Number of missing counts
##############################
# joining so you have same n
full_wide = left_join(
  demog[, "biobank_id"],
  wide) %>% 
  arrange(biobank_id)
dim(full_wide)
full_wide$biobank_id = NULL


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
    "age_assessment", 
    "age_accel",
    "smoke"
    )
# , 
    # "bmi",
    # "genetic_sex",
    # "bmr", "body_fat_pct")
# varname = run_vars[1]
varname = "bmi"
demog = demog[, c("biobank_id", run_vars)]

q_attend = qcut(demog$age_assessment)
q_accel = qcut(demog$age_accel)
table(q_attend, q_accel)

xscale = scale_x_datetime(
    breaks=date_breaks("2 hour"), 
    date_labels="%H:%M")


pdfname = file.path(res_dir, 
    paste0("Median_", 
      inside, tapp,
      "plots.pdf"))
pdf(pdfname, width = 15,
    height = 8)

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


widepng = function(pngname) {
  png(pngname, 
    width = 1440 * 2,
    height = n_samp * 2,
    units = "px",
    res = 300,
    type = "cairo"
    )
}

###############################
# It's when they go to bed!
###############################
func = function(imeas, ...) {
  res = switch(
    imeas,
    median = colMedians(..., 
      na.rm = TRUE),
    mean = colMeans(..., na.rm = TRUE),
    q25 = colQuantiles(..., na.rm = TRUE,
      probs = 0.25),
    q75 = colQuantiles(..., na.rm = TRUE,
      probs = 0.75),
    q10 = colQuantiles(..., na.rm = TRUE,
      probs = 0.1),
    q90 = colQuantiles(..., na.rm = TRUE,
      probs = 0.9)
    )  
}
imeas = "q10"

wide2png = function(pngname) {
  png(pngname, 
    width = 15,
    height = 8,
    units = "in",
    res = 300,
    type = "cairo"
    )
}

for (imeas in c("median", 
  "mean", "q25", "q10", "q75", "q90")) {
  print(imeas)

  pngname = file.path(res_dir,
    paste0("pop_", imeas, "_",   
      inside, 
      tapp, 
      "plot",
      ".png"))
  
    wide2png(pngname)
    cm = func(
      imeas = imeas, 
      as.matrix(full_wide))
    plot(cm, type = "l", ylab = imeas)
    abline(v = which(0:1440 %% 60 == 0),
      col = "gray")

  dev.off()
}

n_miss = colSums(is.na(full_wide))
pct_miss = colMeans(is.na(full_wide))
n_miss = data.frame(n_miss = n_miss,
  time = as.integer(colnames(full_wide)),
  pct_miss = pct_miss
  )
n_miss = n_miss %>% 
  mutate(time = min_to_time(time))

g = n_miss %>% 
  ggplot(aes(
  x = time)) + 
  ggtitle(
    "Number of missing values") + 
  xlab("Time") + 
    xscale 
gpct = g + geom_line(aes(y = pct_miss)) +
  ylab("Percent of Subjects") 

g = g + geom_line(aes(y = n_miss)) +
  ylab("Number of Subjects")  

pngname = file.path(res_dir,
  paste0("missing_data_",   
    inside, 
    tapp, 
    "plot",
    ".png"))

wide2png(pngname)
print(g)
dev.off()

pngname = file.path(res_dir,
  paste0("pct_missing_data_",   
    inside, 
    tapp, 
    "plot",
    ".png"))
wide2png(pngname)
print(gpct)
dev.off()

probs = seq(0, 1, by =0.1)
samp_long = wide %>% 
  mutate(rmed = rowMedians(
    as.matrix(select(wide, -biobank_id)),
    na.rm = TRUE),
    rmed = qcut(rmed, probs = probs))

set.seed(20171102)
n_samp = 2000
samp_long = samp_long %>% 
  group_by(rmed) %>% 
  sample_n(size = n_samp/10) %>% 
  ungroup %>% 
  select(-rmed) %>% 
  gather(key = minute,
    value = acceleration,
    -biobank_id) %>% 
  filter(!is.na(acceleration)) 

samp_long = samp_long %>% 
  mutate(logged = log10(acceleration +1),
    minute = as.integer(minute)) %>% 
  arrange(biobank_id, minute)
samp_long$dt = min_to_time(samp_long$minute)

g = samp_long %>% 
  ggplot(aes(
  x = dt, 
  y = biobank_id)) + xscale +
  theme(axis.text.y=element_blank())
g = g + 
  scale_colour_distiller(
    palette = "Spectral")

glog = g + 
  geom_tile(aes(colour = logged))
g = g + 
  geom_tile(aes(colour = acceleration))
rm(samp_long)

# pngname = file.path(res_dir,
#   paste0("ggtile_",
#     inside, 
#     tapp, 
#     "logged",
#     ".png"))

# widepng(pngname)
# print(glog)
# dev.off()

# pngname = file.path(res_dir,
#   paste0("ggtile_",
#     inside, 
#     tapp, 
#     "raw",
#     ".png"))

# widepng(pngname)
# print(g)
# dev.off()

rmean = rowMeans(
  as.matrix(
  wide[, as.character(1310:1330)]),
  na.rm = TRUE
  )
ord = order(rmean)
ord = wide$biobank_id[ord]

first_day = first_day %>% 
  arrange(accel_date, biobank_id)
# ord = first_day$biobank_id
wide = wide %>% 
  mutate(
    biobank_id = factor(biobank_id,
      levels = ord)) %>% 
  arrange(biobank_id)



sdata = sample_n(wide, size = n_samp)
sdata = sdata %>% 
  as.data.frame

rownames(sdata) = sdata$biobank_id
sdata$biobank_id = NULL
sdata = as.matrix(sdata)

g1 = sdata[seq(n_samp/2),]
g2 = sdata[seq(n_samp/2+1, n_samp),]
corrs = diag(cor(g1, g2, 
  use="complete.obs"))

pngname = file.path(res_dir,
  paste0(
    "clustered_",   
    "heatmap_",
    inside, 
    tapp, 
    "plot",
    ".png"))
widepng(pngname)

pheatmap(sdata, 
  cluster_cols = FALSE,
  cluster_rows = TRUE,
  show_colnames = FALSE,
  show_rownames = FALSE
  )
dev.off()

pngname = gsub("clustered",
  "unclustered",
  pngname)
widepng(pngname)
pheatmap(sdata, 
  cluster_cols = FALSE,
  cluster_rows = FALSE,
  show_colnames = FALSE,
  show_rownames = FALSE
  )
dev.off()

ldata = log10(sdata + 1)

pngname = file.path(res_dir,
  paste0("clustered_",  
    "log10_heatmap_",
    inside, 
    tapp,  
    "plot", 
    ".png"))

widepng(pngname)

pheatmap(ldata,
  cluster_cols = FALSE,
  cluster_rows = TRUE,  
  show_colnames = FALSE,
  show_rownames = FALSE
  )
dev.off()


pngname = gsub("clustered",
  "unclustered",
  pngname)
widepng(pngname)


pheatmap(ldata,
  cluster_cols = FALSE,
  cluster_rows = FALSE,
  show_colnames = FALSE,
  show_rownames = FALSE
  )
dev.off()

# ord = ldata <= 1
ord = ldata > 1.5
class(ord) = "numeric"
nminutes = 5
ord = rowCumsums(ord) > nminutes
ord = max.col(ord, ties = "first")
ord = ord - nminutes
ord[ord < 1] = 1
ord = order(ord)
ldata = ldata[ord, ]

pngname = file.path(res_dir,
  paste0("ordered_",  
    "log10_heatmap_",
    inside, 
    tapp,  
    "plot", 
    ".png"))

widepng(pngname)
pheatmap(ldata, 
  cluster_cols = FALSE,
  cluster_rows = FALSE,  
  show_colnames = FALSE,
  show_rownames = FALSE
  )
dev.off()

ldata = apply(ldata, 2, 
  sort, na.last = FALSE)

pngname = file.path(res_dir,
  paste0("unclustered_",  
    "log10_heatmap_",
    inside, 
    tapp,  
    "plot_sorted", 
    ".png"))

widepng(pngname)

pheatmap(ldata, 
  cluster_cols = FALSE,
  cluster_rows = FALSE,  
  show_colnames = FALSE,
  show_rownames = FALSE
  )
dev.off()




size = 20
pngname = file.path(res_dir,
  paste0("spaghetti_",
    size, "_",
    inside, 
    tapp, 
    "plot",  
    ".png"))

r = range(ldata, na.rm = TRUE)
cm = colMeans(ldata, na.rm = TRUE)
cmed = colMedians(ldata, na.rm = TRUE)

picks = ldata %>% 
  as.data.frame %>% 
  sample_n(size = size)   %>% 
  as.matrix
cm_10 = colMeans(picks, na.rm = TRUE)

times = min_to_time(as.integer(
  colnames(ldata)))

wide2png(pngname)

plot(x = times, y = cm, 
  ylim = r, type = "n",
  xlab = "Time of the Day", 
  ylab = "Log 10 Acceleration (mg)")

for (irow in seq(size)) {
  col =  alpha(irow, 0.15)
  lines(x = times, picks[irow,], 
    col = col)
}
lines(x = times, cm_10, 
  col = alpha("blue", 0.5))
lines(x = times, cm)

dev.off()

# nruns = 10
nruns = 5
sub_means = lapply(seq(nruns), 
  function(r) {
  picks = ldata %>% 
    as.data.frame %>% 
    sample_n(size = size)   %>% 
    as.matrix
  cm_10 = colMeans(picks, na.rm = TRUE)
  data_frame(
    time = times,
    x = r, mean = cm_10)
})
sub_means = dplyr::bind_rows(sub_means)

df = data_frame(time = times,
  mean = cm,
  x = "Grand_Mean")

g = ggplot(aes(x= time, y =mean,
  colour = factor(x)), 
  data =sub_means) + 
  geom_line(alpha = 0.25) + 
  xscale + 
  guides(colour = FALSE) + 
  geom_line(data = df,
    colour = "black")
pngname = file.path(res_dir,
  paste0(
    "sub_sample_size_",
    size, "_",
    inside, 
    tapp, 
    "plot",
    ".png"))
wide2png(pngname)
g
dev.off()





