
inc <- inc %>%
  rename(
    ninc = Antal...2,
    totinc = Antal...8,
    pinc = `%...3`
  ) %>%
  select(X1, ninc, totinc, pinc)

prev <- prev %>%
  rename(
    nprev = Antal...2,
    totprev = Antal...4,
    pprev = `%`
  ) %>%
  select(X1, nprev, totprev, pprev)

tg_lan <- full_join(
  inc,
  prev,
  by = "X1"
) %>%
  mutate(isreg = str_detect(X1, "^\\d")) %>%
  filter(X1 == "RIKET" | isreg) %>%
  mutate(
    region = str_sub(X1, 4),
    region = case_when(
      region == "ET" ~ "Riket",
      region == "Södermanland" ~ "Sörmland",
      region == "Jämtland" ~ "Jämtland Härjedalen",
      TRUE ~ region
    )
  )

tmp_inc <- inctime %>%
  filter(X1 == "RIKET") %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "ar") %>%
  filter(V1 != "RIKET") %>%
  rename(inc = V1)

tmp_prev <- prevtime %>%
  filter(X1 == "RIKET") %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "ar") %>%
  filter(V1 != "RIKET") %>%
  rename(prev = V1)

tg_overtime <- full_join(tmp_inc, tmp_prev, by = "ar")
