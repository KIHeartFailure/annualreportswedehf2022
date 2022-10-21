timefunc <- function(qi = qitmp, starttime = global_year - 5, stoptime = global_year, ll = lltmp, ul = ultmp,
                     data = rsdata, ylimmin = c(0, 100), onlyindex = FALSE, legplace = NULL) {
  tmp <- data %>%
    filter(indexyear %in% paste(seq(starttime, stoptime, 1)) &
      !is.na(!!sym(qi)))

  if (onlyindex) {
    tmp <- tmp %>%
      filter(ttype == "Index")

    byvar <- "vtype"
    colsmy <- global_cols[1:2]
    if (is.null(legplace)) legplace <- c(1, 20)
  }
  if (!onlyindex) {
    byvar <- "ttype"
    colsmy <- global_cols[1:4]
    if (is.null(legplace)) legplace <- c(1, 30)
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

  if (!all(ylimmin == c(0, 100))) {
    if (ylimmin[1] != 0) ylimmin[1] <- ylimmin[1] - 5
    if (ylimmin[2] != 100) ylimmin[2] <- ylimmin[2] + 5
  }

  cexmy <- 1.2
  # c(bottom, left, top, right) default c(5, 4, 4, 2) + 0.1.
  par(mar = c(5.9, 4, 0.5, 0.5) + 0.1, xpd = FALSE)

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
    ylab = "Percent",
    xlab = labnams[1],
    cex.lab = cexmy
  )

  box(bty = "l")

  if (ylimmin[2] - ylimmin[1] <= 22) {
    int <- 5
  } else {
    int <- 10
  }

  if (!all(ylimmin == c(0, 100))) {
    if (ylimmin[1] != 0) {
      axis(2, c(ylimmin[1], seq(ylimmin[1] + 5, (ylimmin[2]), int)),
        c(0, seq(ylimmin[1] + 5, ylimmin[2], int)),
        cex.axis = cexmy, las = 2
      )
      plotrix::axis.break(2, ylimmin[1] + 2.5, style = "slash")
    }
    if (ylimmin[2] != 100) {
      axis(2, c(seq(ylimmin[1], ylimmin[2] - 5, int), ylimmin[2]),
        c(seq(ylimmin[1], ylimmin[2], int), 100),
        cex.axis = cexmy, las = 2
      )
      plotrix::axis.break(2, ylimmin[2] - 2.5, style = "slash")
    }
  } else {
    axis(2, seq(ylimmin[1], ylimmin[2], int), cex.axis = cexmy, las = 2)
  }

  abline(h = ll * 100, col = global_colslimit[2], lty = 2, lwd = 1)
  abline(h = ul * 100, col = global_colslimit[1], lty = 2, lwd = 1)

  axis(1, at = 1:(stoptime - starttime + 1), labels = starttime:stoptime, cex.axis = cexmy)

  legend(
    x = legplace[1], y = legplace[2], legend = str_replace(colnames(datafig), "_", " "),
    bty = "n", col = colsmy, lwd = 3, pch = 19, cex = cexmy
  )

  if (!is.null(ll)) {
    legend("bottom",
      inset = c(-0, -0.23), xpd = NA,
      legend = labnams[2:3],
      lty = 2,
      col = global_colslimit,
      bty = "n",
      cex = cexmy,
      horiz = TRUE
    )
  }
}
