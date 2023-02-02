barfunc <- function(var, startime = global_startdtm, stoptime = global_stopdtm, type = "Index",
                    data = rsdata) {
  tmp <- data %>%
    filter(ttype %in% type &
      indexdtm >= startime &
      indexdtm <= stoptime &
      !is.na(!!sym(var)))

  levs <- levels(tmp %>% pull(!!sym(var)))
  mycols <- global_cols[1:length(levs)]
  mycols[levs == "Unknown"] <- global_colsgreymiss

  # per county
  unitdata <- tmp %>%
    filter(county != "") %>%
    group_by(county) %>%
    count(!!sym(var), .drop = FALSE) %>%
    mutate(
      tot = sum(n),
      percent = n / tot * 100,
      unit = as.character(county),
      var = !!sym(var)
    ) %>%
    ungroup() %>%
    filter(tot >= 10) %>%
    mutate(
      row = 1:n()
    ) %>%
    arrange(desc(row))


  unitdata$unit <- forcats::fct_reorder(unitdata$unit, unitdata$row)
  unitdata <- unitdata %>%
    mutate(unit = factor(unit, levels = rev(levels(unitdata$unit))))

  percent <- unitdata %>%
    group_by(unit) %>%
    arrange(var) %>%
    summarize(percent = paste0(fn(percent, 0), collapse = "/")) %>%
    ungroup() %>%
    mutate(percent = paste0(percent, "%"))

  ntot <- unitdata %>%
    group_by(unit) %>%
    arrange(var) %>%
    summarize(ntot = paste0(fnbm(n), collapse = "/")) %>%
    ungroup() %>%
    mutate(ntot = paste0(ntot, " of ", fnbm(unitdata %>% group_by(unit) %>% slice(1) %>% ungroup() %>% pull(tot))))

  cexmy <- .75
  # c(bottom, left, top, right)
  par(mar = c(5, 12, .1, 3) + 0.1)

  b <- barplot(percent ~ var + unit,
    data = unitdata,
    horiz = TRUE,
    beside = F,
    axes = FALSE,
    xlab = "",
    ylab = "",
    xaxs = "i", yaxs = "i",
    col = mycols,
    width = 0.1,
    border = NA,
    las = 2,
    names.arg = rep(NA, length(unique(unitdata$unit))),
    cex.lab = cexmy,
    xlim = c(0, 100),
  )

  axis(1, seq(0, 100, 20), cex.axis = cexmy)

  axis(2, at = b, labels = unique(unitdata$unit), line = 4.8, tick = FALSE, cex.axis = cexmy, las = 2, gap.axis = -10000000)

  axis(2, at = b, labels = ntot$ntot, line = 1.9, tick = FALSE, cex.axis = cexmy, las = 2, hadj = 0.5, gap.axis = -10000000)

  axis(2, at = b, labels = percent$percent, line = -22.3, tick = FALSE, cex.axis = cexmy, las = 2, hadj = 0.5, gap.axis = -10000000)

  axis(1, at = 50, cex.axis = cexmy, labels = "Proportion (%)", line = 1, tick = FALSE, hadj = .5)

  legend("bottom",
    inset = c(-0, -.14), xpd = NA,
    levels(unitdata$var),
    fill = mycols,
    col = mycols,
    border = mycols,
    bty = "n",
    cex = cexmy,
    horiz = TRUE
  )

  # text(rep(c(97.5, 2.5), length(unitdata$percent) / 2), rep(b, each = 2), paste0(unitdata$percent, "%"), cex = cexmy)
}

barfunc3 <- function(var, startime = global_startdtm, stoptime = global_stopdtm, type = "Index",
                    data = rsdata) {
  tmp <- data %>%
    filter(ttype %in% type &
             indexdtm >= startime &
             indexdtm <= stoptime &
             !is.na(!!sym(var)))
  
  levs <- levels(tmp %>% pull(!!sym(var)))
  mycols <- global_cols[1:length(levs)]
  mycols[levs == "Unknown"] <- global_colsgreymiss
  
  # per county
  unitdata <- tmp %>%
    filter(county != "") %>%
    group_by(county) %>%
    count(!!sym(var), .drop = FALSE) %>%
    mutate(
      tot = sum(n),
      percent = n / tot * 100,
      unit = as.character(county),
      var = !!sym(var)
    ) %>%
    ungroup() %>%
    filter(tot >= 10) %>%
    mutate(
      row = 1:n()
    ) %>%
    arrange(desc(row))
  
  
  unitdata$unit <- forcats::fct_reorder(unitdata$unit, unitdata$row)
  unitdata <- unitdata %>%
    mutate(unit = factor(unit, levels = rev(levels(unitdata$unit))))
  
  percent <- unitdata %>%
    group_by(unit) %>%
    arrange(var) %>%
    summarize(percent = paste0(fn(percent, 0), collapse = "/")) %>%
    ungroup() %>%
    mutate(percent = paste0(percent, "%"))
  
  ntot <- unitdata %>%
    group_by(unit) %>%
    arrange(var) %>%
    summarize(ntot = paste0(fnbm(n), collapse = "/")) %>%
    ungroup() %>%
    mutate(ntot = paste0(ntot, " of ", fnbm(unitdata %>% group_by(unit) %>% slice(1) %>% ungroup() %>% pull(tot))))
  
  cexmy <- .75
  # c(bottom, left, top, right)
  par(mar = c(5, 12, .1, 3.5) + 0.1)
  
  b <- barplot(percent ~ var + unit,
               data = unitdata,
               horiz = TRUE,
               beside = F,
               axes = FALSE,
               xlab = "",
               ylab = "",
               xaxs = "i", yaxs = "i",
               col = mycols,
               width = 0.1,
               border = NA,
               las = 2,
               names.arg = rep(NA, length(unique(unitdata$unit))),
               cex.lab = cexmy,
               xlim = c(0, 100),
  )
  
  axis(1, seq(0, 100, 20), cex.axis = cexmy)
  
  axis(2, at = b, labels = unique(unitdata$unit), line = 5.8, tick = FALSE, cex.axis = cexmy, las = 2, gap.axis = -10000000)
  
  axis(2, at = b, labels = ntot$ntot, line = 2.2, tick = FALSE, cex.axis = cexmy, las = 2, hadj = 0.5, gap.axis = -10000000)
  
  axis(2, at = b, labels = percent$percent, line = -22, tick = FALSE, cex.axis = cexmy, las = 2, hadj = 0.5, gap.axis = -10000000)
  
  axis(1, at = 50, cex.axis = cexmy, labels = "Proportion (%)", line = 1, tick = FALSE, hadj = .5)
  
  legend("bottom",
         inset = c(-0, -.14), xpd = NA,
         levels(unitdata$var),
         fill = mycols,
         col = mycols,
         border = mycols,
         bty = "n",
         cex = cexmy,
         horiz = TRUE
  )
  
  # text(rep(c(97.5, 2.5), length(unitdata$percent) / 2), rep(b, each = 2), paste0(unitdata$percent, "%"), cex = cexmy)
}