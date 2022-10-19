qifunc <- function(qi = qitmp, startime = global_startdtm, stoptime = global_stopdtm, type,
                   ll = lltmp, ul = ultmp, data = rsdata, unit = "center") {
  tmp <- data %>%
    filter(ttype %in% type &
      indexdtm >= startime &
      indexdtm <= stoptime &
      !is.na(!!sym(qi)))

  # riket
  riket <- tmp %>%
    count(!!sym(qi), .drop = F) %>%
    mutate(
      tot = sum(n),
      percent = as.numeric(fn(n / tot * 100, 0))
    ) %>%
    filter(!!sym(qi) == 1) %>%
    mutate(
      unit = "Riket",
      byvar = 1
    )

  # per hf duration
  hfdur <- tmp %>%
    filter(!is.na(hfdurindex)) %>%
    group_by(hfdurindex, .drop = F) %>%
    count(!!sym(qi), .drop = F) %>%
    mutate(
      tot = sum(n),
      percent = as.numeric(fn(n / tot * 100, 0))
    ) %>%
    ungroup() %>%
    filter(!!sym(qi) == 1) %>%
    filter(tot >= 10) %>%
    mutate(byvar = 2) %>%
    rename(unit = hfdurindex)

  if (unit == "region") {
    hfdur <- hfdur %>% mutate(unit = str_replace_all(unit, "Duration HF", "Dur HF"))
  }

  # per vtyp
  vtype <- tmp %>%
    filter(!is.na(vtype)) %>%
    group_by(vtype, .drop = F) %>%
    count(!!sym(qi), .drop = F) %>%
    mutate(
      tot = sum(n),
      percent = as.numeric(fn(n / tot * 100, 0))
    ) %>%
    ungroup() %>%
    filter(!!sym(qi) == 1) %>%
    filter(tot >= 10) %>%
    mutate(byvar = 3) %>%
    rename(unit = vtype)

  # per center/region
  unitdata <- tmp %>%
    filter(!is.na(!!sym(unit))) %>%
    group_by(!!sym(unit), .drop = F) %>%
    count(!!sym(qi), .drop = F) %>%
    mutate(
      tot = sum(n),
      percent = as.numeric(fn(n / tot * 100, 0))
    ) %>%
    ungroup() %>%
    filter(!!sym(qi) == 1) %>%
    filter(tot >= 10) %>%
    mutate(byvar = 4) %>%
    rename(unit = !!sym(unit))

  empty <- riket %>%
    mutate(
      n = 0,
      tot = 0,
      percent = 0,
      unit = "",
      byvar = NA
    )

  unitdata <- unitdata %>%
    arrange(desc(percent), unit)

  all <- bind_rows(riket, hfdur, vtype, empty, unitdata)

  all <- all %>%
    mutate(
      cols = case_when(
        byvar == 4 ~ global_colsblue[2],
        byvar %in% c(1, 2, 3) ~ global_colsblue[5],
        is.na(byvar) ~ "white"
      ),
      ntot = if_else(!is.na(byvar), paste0(n, " av ", tot), ""),
      per = if_else(!is.na(byvar), paste0(percent, "%"), ""),
      row = 1:n()
    ) %>%
    arrange(desc(row))

  all$unit <- forcats::fct_reorder(all$unit, all$row)
  all <- all %>%
    mutate(unit = factor(unit, levels = rev(levels(all$unit))))


  if (unit == "center") {
    cexmy <- .6
  }
  if (unit == "region") {
    cexmy <- .72
  }

  # c(bottom, left, top, right)
  par(mar = c(4.1, 11.2, .1, 1.5) + 0.1)

  b <- barplot(percent ~ unit,
    data = all,
    horiz = TRUE,
    axes = FALSE,
    xlab = "",
    ylab = "",
    xaxs = "i", yaxs = "i",
    col = all$cols,
    width = 0.1,
    border = "white",
    names.arg = NA,
    cex.lab = cexmy,
    xlim = c(0, 100)
  )

  axis(1, seq(0, 100, 20), cex.axis = cexmy)

  abline(v = ll * 100, col = global_colslimit[2], lty = 2, lwd = 1)
  abline(v = ul * 100, col = global_colslimit[1], lty = 2, lwd = 1)

  if (unit == "center") {
    axis(2, at = b, labels = all$unit, line = 2.6, tick = FALSE, cex.axis = cexmy, las = 2, gap.axis = -10000000)

    axis(2, at = b, labels = all$ntot, line = 0.7, tick = FALSE, cex.axis = cexmy, las = 2, hadj = 0.5, gap.axis = -10000000)

    axis(2, at = b, labels = all$per, line = -24, tick = FALSE, cex.axis = cexmy, las = 2, hadj = 0.5, gap.axis = -10000000)
  }

  if (unit == "region") {
    axis(2, at = b, labels = all$unit, line = 3.2, tick = FALSE, cex.axis = cexmy, las = 2, gap.axis = -10000000)

    axis(2, at = b, labels = all$ntot, line = 1, tick = FALSE, cex.axis = cexmy, las = 2, hadj = 0.5, gap.axis = -10000000)

    axis(2, at = b, labels = all$per, line = -24.1, tick = FALSE, cex.axis = cexmy, las = 2, hadj = 0.5, gap.axis = -10000000)
  }

  # mtext("n/N", side = 2, line = 1, at = last(b) + diff(tail(b, 2)), adj = 0.5, cex = cexmy, las = 2)
  axis(1, at = 50, cex.axis = cexmy, labels = "Procent", line = 1, tick = FALSE, hadj = .5)

  if (!is.null(ll)) {
    legend("bottom",
      inset = c(-0, -.125), xpd = NA,
      legend = labnams[2:3],
      lty = 2,
      col = global_colslimit,
      bty = "n",
      cex = cexmy,
      horiz = TRUE
    )
  }
}
