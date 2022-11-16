# default is to use tidyverse functions
select <- dplyr::select
rename <- dplyr::rename
filter <- dplyr::filter
mutate <- dplyr::mutate
complete <- tidyr::complete

global_cols <- c(
  "#213067",
  "#bb7cb3",
  "#e6007e",
  "#009fe3",
  "#33ac5a",
  "#95368c",
  "#878787",
  "#f29ec4",
  "#f7a600",
  "#a1daf8",
  "#89c58b",
  "#e6007e",
  "#854f46",
  "#9d9c9c",
  "#fdcb78",
  "#10bbef",
  "#ac8579"
)

global_colsgreymiss <- "#9d9c9c"

global_colslimit <- c("#33ac5a", "#f7a600")

# year of report

global_year <- 2022
global_startdtm <- ymd(paste0(global_year, "-01-01"))
global_stopdtm <- ymd(paste0(global_year, "-12-31"))
