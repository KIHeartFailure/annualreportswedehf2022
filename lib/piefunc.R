piefunc <- function(var, type = "Index",
                    startime = global_startdtm, stoptime = global_stopdtm,
                    data = rsdata, mycols) {
  tmp <- data %>%
    filter(
      indexdtm >= startime &
        indexdtm <= stoptime &
        ttype %in% type
    )

  levels(tmp[[var]]) <- c(levels(tmp[[var]]), labnams[4])

  riket <- tmp %>%
    count(!!sym(var)) %>%
    mutate(
      tot = sum(n),
      percent = n / tot * 100,
      !!sym(var) := replace_na(!!sym(var), labnams[4]),
      !!sym(var) := paste0(!!sym(var), "\n", fn(percent, 0), "%"),
      row = 1:n()
    )

  riket <- riket %>%
    mutate(varfac = factor(row, labels = riket %>% pull(!!sym(var))))

  p <- ggplot(data = riket, aes(x = "", y = percent, fill = varfac)) +
    geom_bar(stat = "identity") +
    theme_void() +
    theme(legend.position = "none") +
    geom_text(aes(x = 1.6, label = !!sym(var)), position = position_stack(vjust = 0.5), size = 5) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = mycols)

  return(p)
}
