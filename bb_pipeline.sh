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


Rnosave collapse_long.R -N COLLAPSE \
-l mem_free=99G,h_vmem=100G


Rnosave collapse_across_days_long.R \
    -N DAILY \
    -hold_jid COLLAPSE \
    -l mem_free=80G,h_vmem=82G