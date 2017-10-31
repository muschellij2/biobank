#############################################
# Biobank
#############################################
cd ${biobank}/accelerometer/code ;

# Rnosave convert_long.R -N CLONG \
#     -t 1-74999 \
#     -l mem_free=1G,h_vmem=1.5G \
#     -M noone@noone.com

# Rnosave convert_long.R -N CLONG \
#     -t 75000-103707 \
#     -l mem_free=1G,h_vmem=1.5G \
#     -M noone@noone.com

# Rnosave subset_demog.R -N DEMOG \
#     -l mem_free=30G,h_vmem=31G

# Rnosave collapse_long.R -N COLLAPSE \
# -l mem_free=99G,h_vmem=100G

Rnosave collapse_long.R -N COLLAPSE \
    -t 1-2 \
    -l mem_free=55G,h_vmem=56G


Rnosave collapse_across_days_long.R \
    -N DAILY \
    -t 1-4 \
    -hold_jid COLLAPSE \
    -l mem_free=55G,h_vmem=56G

Rnosave summary_measures_across_days.R \
    -N SUMM \
    -t 1-4 \
    -l mem_free=80G,h_vmem=81G    

