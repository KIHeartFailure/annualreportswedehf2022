barsummary <- function(qi = qitmp,
                       starttime = global_year - 2, stoptime = global_year,
                       ll = lltmp, ul = ultmp,
                       unit,
                       data = rsdata) {
  tmp <- data %>%
    filter(!is.na(!!sym(qi)) &
      vtype %in% unit)

  all <- tmp %>%
    group_by(ttype, indexyear, .drop = F) %>%
    count(!!sym(qi), .drop = F) %>%
    mutate(
      tot = sum(n),
      percent = as.numeric(fn(n / tot * 100, 0))
    ) %>%
    ungroup() %>%
    filter(!!sym(qi) == 1 &
      indexyear %in% paste(seq(starttime, stoptime, 1)))

  all <- all %>%
    mutate(
      cols = case_when(
        indexyear == starttime ~ global_colsblue[3],
        indexyear == starttime + 1 ~ global_colsblue[4],
        TRUE ~ global_colsblue[5]
      ),
      ntot = paste0(n, " av ", tot),
      per = paste0(percent, "%"),
      per = if_else(tot < 10, "", per),
      ntot = if_else(tot < 10, "", ntot),
      percent = if_else(tot < 10, 0, percent),
      row = 1:n(),
      indexyear = as.character(indexyear)
    )

  cexmy <- .9
  # c(bottom, left, top, right)
  par(mar = c(5, 4, 5, 0) + 0.1)

  b <- barplot(percent ~ indexyear + ttype,
    data = all,
    beside = TRUE,
    axes = FALSE,
    ylab = "Procent",
    xlab = "",
    col = all$cols,
    border = "white",
    names.arg = c(NA, NA, NA, NA),
    cex.lab = cexmy,
    ylim = c(0, 100)
  )

  axis(2, seq(0, 100, 20), cex.axis = cexmy, las = 2)

  abline(h = ll * 100, col = global_colslimit[2], lty = 2, lwd = 1)
  abline(h = ul * 100, col = global_colslimit[1], lty = 2, lwd = 1)

  axis(1, at = b, labels = all$indexyear, line = -.5, tick = FALSE, cex.axis = cexmy, gap.axis = -10000000, las = 2)

  axis(3, at = b, labels = all$ntot, line = -0.6, tick = FALSE, cex.axis = cexmy, hadj = 0, gap.axis = -10000000, las = 2)

  axis(1, at = b[2, ], labels = shortttype, line = 1.75, tick = FALSE, cex.axis = cexmy, gap.axis = -10000000)
  
  legend("bottom",
         inset = c(-0, -.28), xpd = NA,
         legend = labnams[2:3],
         lty = 2,
         col = global_colslimit,
         bty = "n",
         cex = cexmy,
         horiz = TRUE
  )
}
