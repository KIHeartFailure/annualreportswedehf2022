# 1. Set options in config/global.dcf
# 2. Load packages listed in config/global.dcf
# 3. Import functions and code in lib directory
# 4. Load data in data directory
# 5. Run data manipulations in munge directory

ProjectTemplate::reload.project(
  reset = TRUE,
  data_loading = TRUE,
  munging = TRUE
)

# Run all files manually due to encoding problemas w Project Template but now working

# source("./munge/01-clean_missing.R")
# source("./munge/02-fixcenter.R")
# source("./munge/03-fixvars.R")
# source("./munge/04-qi.R")

ProjectTemplate::cache("rsdata")

ProjectTemplate::cache("qiinfo")
ProjectTemplate::cache("qiinfosv")

#ProjectTemplate::cache("tg_lan")
#ProjectTemplate::cache("tg_overtime")

labnams <- c("Year", "Upper target level", "Lower target level", "Unknown")
ProjectTemplate::cache("labnams")

labnamssv <- c("År", "Övre målnivå", "Lägre målnivå", "Okänd")
ProjectTemplate::cache("labnamssv")

shortttype <- c("Index", "3-month", "1-year", "2+-year")
ProjectTemplate::cache("shortttype")