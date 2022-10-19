
# Add centers -------------------------------------------------------------

rsdata <- left_join(rsdata,
  center %>%
    select(ID, ORG_UNIT_NAME, PARENT1, PARENT2),
  by = c("HEALTH_CARE_UNIT_REFERENCE" = "ID")
)


# Add region ------------------------------------------------------------

rsdata <- left_join(rsdata,
  center %>%
    filter(DEPTH == 1) %>%
    rename(region = ORG_UNIT_NAME) %>%
    select(ID, region),
  by = c("PARENT1" = "ID")
) %>%
  mutate(
    region = str_remove(region, "Region "),
    region = str_remove(region, " län"),
    region = str_remove(region, "sregionen"),
    region = if_else(region == "Jönköpings", "Jönköping", region),
    region = if_else(region == "Sörmanland", "Sörmland", region)
  )


# Add hospital --------------------------------------------------------------

# specialfix för Sus och KS, förkortning för Sahlgrenska

rsdata <- left_join(rsdata,
  center %>%
    filter(DEPTH == 2) %>%
    rename(tmp_center = ORG_UNIT_NAME) %>%
    select(ID, tmp_center),
  by = c("PARENT2" = "ID")
) %>%
  mutate(
    center = case_when(
      ORG_UNIT_LEVEL_NAME %in% c("Fristående hjärtmottagning", "Vårdcentral") ~ ORG_UNIT_NAME,
      #ORG_UNIT_NAME %in% c("H Hjärta och Kärl 2 Avd", "H Hjärta och Kärl Mott") ~ "Karolinska Huddinge",
      #ORG_UNIT_NAME %in% c("Karolinska AVA 1 Solna", "S Hjärta och Kärl 1 Avd", "S Hjärta och Kärl Mott") ~ "Karolinska Solna",
      ORG_UNIT_NAME %in% c(
        "Skånes universitetssjukhus avd 7 Lund", "Skånes universitetssjukhus avd 8 Lund",
        "Skånes universitetssjukhus hjärtmottagning Lund", "Skånes universitetssjukhus MAVA Lund",
        "Skånes universitetssjukhus medicinmottagning Lund",
        "Skånes universitetssjukhus Transplantation och avancerad svikt Lund"
      ) ~ "Sus Lund",
      ORG_UNIT_NAME %in% c(
        "Skånes universitetssjukhus Avdelning 2 Malmö", "Skånes universitetssjukhus Avdelning 3 Malmö",
        "Skånes universitetssjukhus Avdelning 4 Malmö", "Skånes universitetssjukhus kardiologavd Malmö",
        "Skånes universitetssjukhus kardiologmottagning Malmö",
        "Skånes universitetssjukhus Malmö medicinmottagning"
      ) ~ "Sus Malmö",
      TRUE ~ tmp_center
    ),
    center = case_when(
      center == "Sahlgrenska Universitetssjukhuset - Sahlgrenska" ~ "SU - Sahlgrenska",
      center == "Sahlgrenska Universitetssjukhuset - Östra" ~ "SU - Östra",
      center == "Sahlgrenska Universitetssjukhuset - Mölndal" ~ "SU - Mölndal",
      TRUE ~ center
    )
  )

# check so no more clinics at Sus
koll <- rsdata %>%
  count(center, tmp_center) %>%
  #filter(tmp_center %in% c("Karolinska", "Skånes universitetssjukhus"))
  filter(tmp_center %in% c("Skånes universitetssjukhus"))
#if (any(!koll$center %in% c("Sus Malmö", "Sus Lund", "Karolinska Solna", "Karolinska Huddinge"))) stop("More clinics at Sus or KS")
if (any(!koll$center %in% c("Sus Malmö", "Sus Lund"))) stop("More clinics at Sus")

# check so no more clinics at Sahlgrenska
koll2 <- rsdata %>%
  count(center, center) %>%
  filter(str_detect(center, "Sahlgrenska Universitetssjukhuset"))
if (nrow(koll2) > 0) stop("More clinics at SU")

# Group VC ect ------------------------------------------------------------

rsdata <- rsdata %>%
  mutate(
    vtype = factor(case_when(
      ORG_UNIT_LEVEL_NAME %in% c("Avdelning", "Fristående hjärtmottagning", "Mottagning") ~ 1,
      ORG_UNIT_LEVEL_NAME %in% c("Vårdcentral") ~ 2
    ), levels = 1:2, labels = c("Sjukhus", "Primärvård")) # ,
    # center = case_when(
    #  ORG_UNIT_LEVEL_NAME %in% c("Vårdcentral") ~ "",
    # ORG_UNIT_LEVEL_NAME %in% c("Fristående hjärtmottagning") ~ "Fristående enhet",
    #  TRUE ~ center
    #  )#,
    # i regioner ingår ej vc
    # regionvc = region,
    # region = case_when(
    #  ORG_UNIT_LEVEL_NAME %in% c("Vårdcentral") ~ "",
    #  TRUE ~ region
    # )
  )

rsdata <- rsdata %>%
  mutate(
    region = factor(region),
    center = factor(center)
  )
