
# Add centres -------------------------------------------------------------

rsdata <- left_join(rsdata,
  center %>%
    select(ID, ORG_UNIT_NAME, PARENT1, PARENT2),
  by = c("HEALTH_CARE_UNIT_REFERENCE" = "ID")
)


# Add county ------------------------------------------------------------

rsdata <- left_join(rsdata,
  center %>%
    filter(DEPTH == 1) %>%
    rename(county = ORG_UNIT_NAME) %>%
    select(ID, county),
  by = c("PARENT1" = "ID")
) %>%
  mutate(
    county = str_remove(county, "Region "),
    county = str_remove(county, " län"),
    county = str_remove(county, "sregionen"),
    county = if_else(county == "Jönköpings", "Jönköping", county),
    county = if_else(county == "Sörmanland", "Sörmland", county), 
    county = factor(county)
  )
