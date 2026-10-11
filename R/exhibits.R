# Shared style and writers for figures, tables and manuscript macros.

colours <- c(a = "#2a78d6", b = "#eb6834", ink = "grey15")
figure_dpi <- 200

theme_paper <- function() {
  ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(4, 8, 4, 4)
    )
}

save_figure <- function(plot, name, width, height) {
  ggplot2::ggsave(figure_path(paste0(name, ".pdf")), plot, width = width, height = height, device = cairo_pdf)
  ggplot2::ggsave(figure_path(paste0(name, ".png")), plot, width = width, height = height, dpi = figure_dpi)
}

pct <- function(x, digits = 0) formatC(100 * x, format = "f", digits = digits)
num <- function(x, digits = 2) formatC(x, format = "f", digits = digits)
interval <- function(lo, hi, scale = 1, digits = 2) {
  f <- function(x) formatC(scale * x, format = "f", digits = digits)
  paste0("[", f(lo), ", ", f(hi), "]")
}
# A booktabs tabular for \input{} inside a table float. `groups` names consecutive
# blocks of rows by their sizes, as in kableExtra::pack_rows(index = ).
write_table <- function(df, path, align, groups = NULL) {
  tab <- knitr::kable(df, format = "latex", booktabs = TRUE, align = align, linesep = "")
  if (!is.null(groups)) {
    tab <- kableExtra::pack_rows(tab, index = groups, bold = FALSE, italic = TRUE)
  }
  writeLines(tab, path)
}

write_macros <- function(values, path) {
  stopifnot(!anyDuplicated(names(values)), all(grepl("^[A-Za-z]+$", names(values))))
  writeLines(sprintf("\\newcommand{\\%s}{%s}", names(values), unlist(values)), path)
}
