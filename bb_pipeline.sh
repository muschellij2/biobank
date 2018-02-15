#############################################
# Biobank
#############################################
cd ${biobank}/accelerometer/code ;

# Rnosave create_ids.R -N IDS \
#     -l mem_free=0.5G,h_vmem=1G

# Rnosave convert_long.R -N CLONG \
#     -t 1-74999 \
#     -l mem_free=0.5G,h_vmem=1.5G \
#     -m n

# Rnosave convert_long.R -N CLONG2 \
#     -t 75000-103707 \
#     -hold_jid CLONG \
#     -l mem_free=0.5G,h_vmem=1.5G \
#     -m n

Rnosave subset_demog.R -N DEMOG \
    -l mem_free=40G,h_vmem=41G

# Rnosave collapse_long.R -N COLLAPSE \
# -l mem_free=99G,h_vmem=100G

# Rnosave get_first_day.R -N FIRST \
# -l mem_free=30G,h_vmem=31G

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

Rnosave collapsed_long_viz.R \
    -N VIZ \
    -t 1-4 \
    -l mem_free=20G,h_vmem=21G   

Rnosave combine_scalars.R \
    -N SCALAR 
   

