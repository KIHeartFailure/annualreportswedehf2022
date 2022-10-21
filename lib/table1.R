table1 <- function(data, stratvar, vars) {
  ns <- fnbm(c(data %>% count(!!!syms(stratvar)) %>% pull(n)))

  for (i in seq_along(vars)) {
    # for (i in 1:3) {
    tmp_tab1data <- data %>% filter(!is.na(!!sym(vars[i])))

    if (class(data %>% pull(!!vars[i])) == "numeric") {
      tmp_deskout <- contfunc(var = vars[i], stratvar = stratvar, data = tmp_tab1data)
    }
    if (class(data %>% pull(!!vars[i])) %in% c("factor", "character")) {
      tmp_deskout <- catfunc(var = vars[i], stratvar = stratvar, data = tmp_tab1data)
    }

    if (i == 1) {
      deskout <- rbind(c("Sweden", NA, ns), tmp_deskout)
    } else {
      deskout <- rbind(deskout, tmp_deskout)
    }
  }
  return(deskout)
}

contfunc <- function(var, stratvar, data, dec, nmiss) {
  deskstat <- data %>%
    group_by(!!!syms(stratvar)) %>%
    summarise(
      q1 = fnbm(quantile(!!sym(var), na.rm = TRUE, probs = .25)),
      med = fnbm(quantile(!!sym(var), na.rm = TRUE, probs = .5)),
      q3 = fnbm(quantile(!!sym(var), na.rm = TRUE, probs = .75)),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    mutate(ds = paste0(med, " [", q1, "-", q3, "]")) %>%
    select(!!!syms(stratvar), ds) %>%
    pivot_wider(names_from = all_of(stratvar), values_from = ds)

  deskstat <- cbind(var = var, level = NA, deskstat)
}

catfunc <- function(var, stratvar, data) {
  deskstat <- data %>%
    group_by(!!!syms(stratvar)) %>%
    count(!!sym(var), .drop = F) %>%
    mutate(
      p = fn(n / sum(n) * 100, 0),
      np = paste0(fnbm(n), " (", p, ")")
    ) %>%
    select(!!!syms(stratvar), !!sym(var), np) %>%
    pivot_wider(names_from = all_of(stratvar), values_from = np) %>%
    rename(level = !!sym(var))

  deskstat <- cbind(var = c(var, rep(NA, nlevels(data %>% pull(!!sym(var))) - 1)), deskstat)
}
