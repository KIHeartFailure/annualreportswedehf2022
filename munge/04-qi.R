rsdata <- rsdata %>%
  mutate(
    qi_lvef = if_else(!is.na(LVEF_PERCENT) | !is.na(LVEF_SEMIQUANTITATIVE), 1, 0),
    qi_ntprobnp = if_else(!is.na(NT_PROBNP) | !is.na(NT_PROBNP_24H), 1, 0),
    qi_nyha = if_else(!is.na(FUNCTION_CLASS_NYHA), 1, 0),
    qi_qol = if_else(!is.na(LIFEQUALITY_SCORE), 1, 0),
    qi_uppfhf = case_when(
      FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(FOLLOWUP_HF_UNIT) ~ NA_real_,
      FOLLOWUP_HF_UNIT == "YES" ~ 1,
      FOLLOWUP_HF_UNIT == "NO" ~ 0
    ),
    qi_ras = case_when(
      FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(ef_catimp) | ef_catimp != "<40" ~ NA_real_,
      is.na(ACE_INHIBITOR) | is.na(A2_BLOCKER_ARB) | is.na(ARNI) ~ NA_real_,
      ACE_INHIBITOR == "YES" | A2_BLOCKER_ARB == "YES" | ARNI == "YES" ~ 1,
      TRUE ~ 0
    ),
    qi_arni = case_when(
      is.na(qi_ras) | qi_ras == 0 ~ NA_real_,
      ARNI == "YES" ~ 1,
      TRUE ~ 0
    ),
    qi_bbl = case_when(
      FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(ef_catimp) | ef_catimp != "<40" ~ NA_real_,
      is.na(BETA_BLOCKER) ~ NA_real_,
      BETA_BLOCKER == "YES" ~ 1,
      TRUE ~ 0
    ),
    qi_mra = case_when(
      FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(ef_catimp) | ef_catimp != "<40" ~ NA_real_,
      is.na(MRA) ~ NA_real_,
      MRA == "YES" ~ 1,
      TRUE ~ 0
    ),
    qi_trippel = case_when(
      is.na(qi_ras) | is.na(qi_bbl) | is.na(qi_mra) ~ NA_real_,
      qi_ras + qi_bbl + qi_mra == 3 ~ 1,
      TRUE ~ 0
    ),
    qi_sglt2 = case_when(
      FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(ef_catimp) | ef_catimp != "<40" ~ NA_real_,
      is.na(SGLT2) ~ NA_real_,
      SGLT2 == "YES" ~ 1,
      TRUE ~ 0
    ),

    # crt
    qi_crt = case_when(
      is.na(efcrt_catimp) | efcrt_catimp != "<40/<=35" ~ NA_real_,
      # is.na(QRS_WIDTHimp) | is.na(LEFT_BRANCH_BLOCKimp) | is.na(EKG_RHYTHMimp) ~ NA_real_,
      # FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(DEVICE_THERAPY) ~ NA_real_,
      DEVICE_THERAPY %in% c("CRT", "CRT_D") ~ 1,
      QRS_WIDTHimp > 130 & LEFT_BRANCH_BLOCKimp == "YES" ~ 0,
      TRUE ~ NA_real_
    ),

    # icd
    qi_icd = case_when(
      is.na(efcrt_catimp) | efcrt_catimp != "<40/<=35" ~ NA_real_,
      # FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(DEVICE_THERAPY) ~ NA_real_,
      DEVICE_THERAPY %in% c("ICD", "CRT_D") ~ 1,
      TRUE ~ 0
    ),
    qi_fys = case_when(
      # FOLLOWUP_UNIT == "DECEASED" ~ NA_real_,
      is.na(PARTICIPATION_HF_TRAINING) ~ NA_real_,
      PARTICIPATION_HF_TRAINING == "YES" ~ 1,
      TRUE ~ 0
    )
  )


# uppföljningar 3 mån

follow <- rsdata %>%
  filter(ttype == "3-month follow-up") %>%
  mutate(followup = 1) %>%
  select(patientreference, followup)

rsdata <- left_join(rsdata, follow, by = "patientreference")

rsdata <- rsdata %>%
  mutate(
    timedead = as.numeric(befdoddtm - indexdtm),
    qi_followreg3m = case_when(
      # FOLLOWUP_UNIT == "DECEASED" |
      TYPE != "INDEX" ~ NA_real_,
      timedead < 30.5 * 3 & !is.na(timedead) ~ NA_real_,
      indexdtm > ymd(paste0(global_year, "-07-01")) ~ NA_real_,
      followup == 1 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  select(-followup, -timedead)


rsdata <- rsdata %>%
  mutate(across(starts_with("qi_"), as.factor))


# Create info for qi variables with names and limits ----------------------

qivar <- names(rsdata %>% select(starts_with("qi_")))

qiinfo <- tibble(qivar = qivar)

qiinfo <- qiinfo %>%
  mutate(
    qishortname = case_when(
      qivar == "qi_lvef" ~ "LVEF",
      qivar == "qi_ntprobnp" ~ "NT-proBNP",
      qivar == "qi_nyha" ~ "NYHA",
      qivar == "qi_qol" ~ "QoL EQ-5D",
      qivar == "qi_uppfhf" ~ "HF clinic",
      qivar == "qi_ras" ~ "ACEI/ARB/ARNI",
      qivar == "qi_arni" ~ "Proportion ARNI",
      qivar == "qi_bbl" ~ "Beta-blockers",
      qivar == "qi_mra" ~ "MRA",
      qivar == "qi_trippel" ~ "Triple therapy",
      qivar == "qi_sglt2" ~ "SGLT2 inhibitor",
      qivar == "qi_crt" ~ "CRT",
      qivar == "qi_icd" ~ "ICD",
      qivar == "qi_fys" ~ "Physical exercise",
      qivar == "qi_followreg3m" ~ "3-month follow-up"
    ),
    qiname = case_when(
      qivar == "qi_lvef" ~ "documentation of LVEF at index",
      qivar == "qi_ntprobnp" ~ "documentation of NT-proBNP at index",
      qivar == "qi_nyha" ~ "documentation of NYHA class at index",
      qivar == "qi_qol" ~ "assessment of QoL EQ-5D at index",
      qivar == "qi_uppfhf" ~ "planned follow-up at a HF clinic",
      qivar == "qi_ras" ~ "prescribed ACEI/ARB/ARNI in patients with LVEF <40%",
      qivar == "qi_arni" ~ "proportion prescribed ARNI of ACEI/ARB/ARNI in patients with LVEF <40%",
      qivar == "qi_bbl" ~ "prescribed beta-blocker in patients with LVEF <40%",
      qivar == "qi_mra" ~ "prescribed MRA in patients with LVEF <40%",
      qivar == "qi_trippel" ~ "prescribed triple therapy in patients with LVEF <40%",
      qivar == "qi_sglt2" ~ "prescribed SGLT2 inhibitor in patients with LVEF <40%",
      qivar == "qi_crt" ~ "CRT implanted in patients with LVEF <40%, LBBB and QRS >130ms",
      qivar == "qi_icd" ~ "ICD implanted in patients with LVEF <40%",
      qivar == "qi_fys" ~ "participation in physical exercise for HF",
      qivar == "qi_followreg3m" ~ "follow-up visit at 3 months performed and documented"
    ),
    ll = case_when(
      qivar == "qi_lvef" ~ 0.8,
      qivar == "qi_ntprobnp" ~ 0.7,
      qivar == "qi_nyha" ~ 0.9,
      qivar == "qi_qol" ~ 0.7,
      qivar == "qi_uppfhf" ~ 0.8,
      qivar == "qi_ras" ~ 0.8,
      qivar == "qi_arni" ~ 0.1,
      qivar == "qi_bbl" ~ 0.8,
      qivar == "qi_mra" ~ 0.6,
      qivar == "qi_trippel" ~ 0.4,
      qivar == "qi_sglt2" ~ 0.3,
      qivar == "qi_crt" ~ 0.5,
      qivar == "qi_icd" ~ 0.5,
      qivar == "qi_fys" ~ 0.3,
      qivar == "qi_followreg3m" ~ 0.88
    ),
    ul = case_when(
      qivar == "qi_lvef" ~ 0.9,
      qivar == "qi_ntprobnp" ~ 0.8,
      qivar == "qi_nyha" ~ 0.95,
      qivar == "qi_qol" ~ 0.8,
      qivar == "qi_uppfhf" ~ 0.9,
      qivar == "qi_ras" ~ 0.9,
      qivar == "qi_arni" ~ 0.3,
      qivar == "qi_bbl" ~ 0.9,
      qivar == "qi_mra" ~ 0.7,
      qivar == "qi_trippel" ~ 0.5,
      qivar == "qi_sglt2" ~ 0.4,
      qivar == "qi_crt" ~ 0.6,
      qivar == "qi_icd" ~ 0.6,
      qivar == "qi_fys" ~ 0.4,
      qivar == "qi_followreg3m" ~ 0.92
    ),
    timepoint = case_when(
      qivar == "qi_lvef" ~ "Index",
      qivar == "qi_ntprobnp" ~ "Index",
      qivar == "qi_nyha" ~ "Index",
      qivar == "qi_qol" ~ "Index",
      qivar == "qi_uppfhf" ~ "Index",
      qivar == "qi_ras" ~ "3-month follow-up",
      qivar == "qi_arni" ~ "3-month follow-up",
      qivar == "qi_bbl" ~ "3-month follow-up",
      qivar == "qi_mra" ~ "3-month follow-up",
      qivar == "qi_trippel" ~ "3-month follow-up",
      qivar == "qi_sglt2" ~ "3-month follow-up",
      qivar == "qi_crt" ~ "3-month follow-up",
      qivar == "qi_icd" ~ "3-month follow-up",
      qivar == "qi_fys" ~ "3-month follow-up",
      qivar == "qi_followreg3m" ~ "Index"
    ),
    qino = case_when(
      qivar == "qi_lvef" ~ 1,
      qivar == "qi_ntprobnp" ~ 2,
      qivar == "qi_nyha" ~ 3,
      qivar == "qi_qol" ~ 4,
      qivar == "qi_uppfhf" ~ 5,
      qivar == "qi_ras" ~ 7,
      qivar == "qi_arni" ~ 8,
      qivar == "qi_bbl" ~ 9,
      qivar == "qi_mra" ~ 10,
      qivar == "qi_trippel" ~ 11,
      qivar == "qi_sglt2" ~ 12,
      qivar == "qi_crt" ~ 13,
      qivar == "qi_icd" ~ 14,
      qivar == "qi_fys" ~ 15,
      qivar == "qi_followreg3m" ~ 6
    )
  ) %>%
  arrange(qino)

qiinfosv <- qiinfo %>%
  mutate(
    qishortname = case_when(
      qivar == "qi_lvef" ~ "LVEF",
      qivar == "qi_ntprobnp" ~ "NT-proBNP",
      qivar == "qi_nyha" ~ "NYHA",
      qivar == "qi_qol" ~ "Hälsotillstånd",
      qivar == "qi_uppfhf" ~ "HF-mottagning",
      qivar == "qi_ras" ~ "ACEI/ARB/ARNI",
      qivar == "qi_arni" ~ "Andel ARNI",
      qivar == "qi_bbl" ~ "Betablockad",
      qivar == "qi_mra" ~ "MRA",
      qivar == "qi_trippel" ~ "Trippelbehandling",
      qivar == "qi_sglt2" ~ "SGLT2 hämmare",
      qivar == "qi_crt" ~ "CRT",
      qivar == "qi_icd" ~ "ICD",
      qivar == "qi_fys" ~ "Fysisk träning",
      qivar == "qi_followreg3m" ~ "Uppföljning 3 månader"
    ),
    qiname = case_when(
      qivar == "qi_lvef" ~ "LVEF, Vänsterkammarfunktion rapporterat vid index",
      qivar == "qi_ntprobnp" ~ "NT-proBNP rapporterat vid index",
      qivar == "qi_nyha" ~ "NYHA klass rapporterat vid index",
      qivar == "qi_qol" ~ "Hälsotillstånd rapporterat vid index",
      qivar == "qi_uppfhf" ~ "Planerad uppföljning vid hjärtsviktsmottagning",
      qivar == "qi_ras" ~ "ACEI/ARB/ARNI",
      qivar == "qi_arni" ~ "Andel ARNI av ACEI/ARB/ARNI",
      qivar == "qi_bbl" ~ "Betablockad",
      qivar == "qi_mra" ~ "MRA",
      qivar == "qi_trippel" ~ "Trippelbehandling",
      qivar == "qi_sglt2" ~ "SGLT2 hämmare",
      qivar == "qi_crt" ~ "CRT",
      qivar == "qi_icd" ~ "ICD",
      qivar == "qi_fys" ~ "Fysisk träning",
      qivar == "qi_followreg3m" ~ "Uppföljningsbesök vid 3 månader rapporterat"
    )
  ) %>%
  arrange(qino)