timefunc <- function(qi = qitmp, starttime = global_year - 4, stoptime = global_year, ll = lltmp, ul = ultmp,
                     data = rsdata, ylimmin = c(0, 100), onlyindex = FALSE, adjtext = NULL) {
  tmp <- data %>%
    filter(indexyear %in% paste(seq(starttime, stoptime, 1)) &
      !is.na(!!sym(qi)))

  if (onlyindex) {
    tmp <- tmp %>%
      filter(ttype == "Index")

    byvar <- "vtype"
    colsmy <- global_colsblue[c(5, 2)]
  }
  if (!onlyindex) {
    byvar <- "ttype"
    colsmy <- global_colsblue[c(6, 5, 4, 3)]
  }

  datafig <- tmp %>%
    filter(!is.na(!!sym(byvar))) %>%
    group_by(!!sym(byvar), indexyear, .drop = F) %>%
    count(!!sym(qi), .drop = F) %>%
    mutate(
      tot = sum(n),
      percent = n / tot * 100
    ) %>%
    ungroup() %>%
    filter(!!sym(qi) == 1) %>%
    filter(tot >= 10) %>%
    rename(unit = !!sym(byvar))

  datafig <- datafig %>%
    select(indexyear, percent, unit) %>%
    pivot_wider(names_from = unit, values_from = percent) %>%
    select(-indexyear) %>%
    as.matrix()

  cexmy <- 1
  # c(bottom, left, top, right) default c(5, 4, 4, 2) + 0.1.
  par(mar = c(5.5, 4, .5, 10) + 0.1, xpd = FALSE)

  matplot(datafig,
    type = "b",
    pch = 19,
    col = colsmy,
    lty = 1,
    lwd = 3,
    # cex = 1.5,
    axes = FALSE,
    xaxs = "i",
    yaxs = "i",
    ylim = ylimmin,
    xlim = c(1 - 0.1, stoptime - starttime + 1 + 0.1),
    ylab = "Procent",
    xlab = labnams[1],
    cex.lab = cexmy
  )

  box(bty = "l")

  if (ylimmin[2] - ylimmin[1] <= 22) {
    int <- 5
  } else {
    int <- 10
  }

  axis(2, seq(ylimmin[1], ylimmin[2], int), cex.axis = cexmy, las = 2)

  abline(h = ll * 100, col = global_colslimit[2], lty = 2, lwd = 1)
  abline(h = ul * 100, col = global_colslimit[1], lty = 2, lwd = 1)

  axis(1, at = 1:(stoptime - starttime + 1), labels = starttime:stoptime, cex.axis = cexmy)

  if (is.null(adjtext)) {
    adjtext <- rep(0, length(colnames(datafig)))
  }
  axis(2,
    at = datafig[stoptime - starttime + 1, ] + adjtext, labels = str_replace(colnames(datafig), "_", " "),
    line = -32.2,
    tick = FALSE, cex.axis = cexmy,
    las = 2,
    hadj = 0,
    gap.axis = -10000000
  )

  if (ylimmin[1] != 0) {
    par(xpd = NA)
    lines(x = c(0.8, 1), y = c(ylimmin[1], ylimmin[1] + 0.5))
    lines(x = c(0.8, 1), y = c(ylimmin[1] + 0.5, ylimmin[1] + 1))
  }

  if (!is.null(ll)) {
    legend("bottom",
      inset = c(-0, -0.21), xpd = NA,
      legend = labnams[2:3],
      lty = 2,
      col = global_colslimit,
      bty = "n",
      cex = cexmy,
      horiz = TRUE
    )
  }
}
