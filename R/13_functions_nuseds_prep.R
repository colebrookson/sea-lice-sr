########## 
##########
# Get NuSEDs data in the right form
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-09-16
##########
##########

nuseds_raw = read_csv(here::here(
  "./data/sr-data/NuSEDS/NuSEDS_20220309.csv"))
pink_exp = read_csv(here::here(
  "./data/sr-data/dfo-data/raw/pink/english-report-translated.csv"))
pink_recon = read_csv(here::here(
  "./data/sr-data/dfo-data/clean/pink-reconstructions.csv"))
pink_helper = read_csv(here::here(
  "./data/sr-data/dfo-data/raw/pink/helper-data-river-cu-match.csv"))