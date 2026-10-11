suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(ggplot2)
})

project_root <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION"))
project_file <- function(...) file.path(project_root, ...)
data_path <- function(...) project_file("data", ...)
table_path <- function(...) project_file("tabs", ...)
figure_path <- function(...) project_file("figs", ...)

for (f in list.files(project_file("R"), pattern = "\\.R$", full.names = TRUE)) source(f)
