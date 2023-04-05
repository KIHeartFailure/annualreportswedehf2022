
ProjectTemplate::reload.project(munge = F, data_loading = F)

datapath <- "./raw-data/årsrapport_2022/"

# Import data from UCR ----------------------------------------------------

newrs <- read_sas(paste0(datapath, "datauttag_arsrapport.sas7bdat"))

# Centre for new rs --------------------------------------------------------

center <- read_sas(paste0(datapath, "rikssvikt_ou.sas7bdat"))

# center <- clean_data(center)

sexage <- read_sas(paste0(datapath, "rikssvikt_pd_dodsdatum.sas7bdat"))

# Store as RData in /data folder ------------------------------------------

save(file = "./data/rawData_rs.RData", list = c(
  "newrs", "center", "sexage"
))

# tg
prevtime <- read.xlsx("./raw-data/tg/6137_2023 RiksSvikt - TG Prev mot Prev 2010-2022 - Huvuddiagnos 2023-04-05_klar_LB.xlsx",
  sheet = "Län per år"
)
inctime <- read.xlsx("./raw-data/tg/6137_2023 RiksSvikt - TG Inc mot Inc 2003-2021 - Registrering inom 1 år 2023-04-05_klar_LB.xlsx",
  sheet = "Län per år"
)
prev <- read.xlsx("./raw-data/tg/6137_2023 RiksSvikt - TG Prev mot Prev 2010-2022 - Huvuddiagnos 2023-04-05_klar_LB.xlsx",
  sheet = "2022"
)
inc <- read.xlsx("./raw-data/tg/6137_2023 RiksSvikt - TG Inc mot Inc 2003-2021 - Registrering inom 1 år 2023-04-05_klar_LB.xlsx",
  sheet = "2021"
)

prev <- prev %>% as_tibble(.name_repair = "unique")
inc <- inc %>% as_tibble(.name_repair = "unique")

save(file = "./data/rawData_tg.RData", list = c(
  "prevtime", "inctime", "prev", "inc"
))

# Get map data ------------------------------------------------------------

swedenmap <- getData("GADM", country = "SWE", level = 1)

saveRDS(swedenmap, file = "./data/swedenmap.rds")
