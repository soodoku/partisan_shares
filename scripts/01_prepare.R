source("scripts/00_config.R")

truth <- truth_table()
s1 <- read_study1()
s2 <- read_study2()

readr::write_csv(study1_long(s1, truth), data_path("answers_study1.csv"))
readr::write_csv(wording_long(s2, truth), data_path("answers_wording.csv"))
readr::write_csv(timing_long(s2, truth), data_path("answers_timing.csv"))
readr::write_csv(truth, table_path("benchmarks.csv"))

# Sample composition: percent of respondents at each level, labelled for the table.
composition <- function(x, variable, levels, labels = levels) {
  tibble(variable, level = labels, value = map_dbl(levels, \(l) 100 * mean(x == l, na.rm = TRUE)))
}
party_labels <- c("Democrat (incl. leaners)", "Republican (incl. leaners)", "Independent")
race_labels <- c("White", "Black", "Asian", "Latino")
age_labels <- c("18-39", "40-64", "65+")

# Study 1 races are check-all-that-apply columns; Study 2 offered one choice.
s1_rows <- bind_rows(
  composition(s1$party, "Party", c("Democrat", "Republican", "Independent"), party_labels),
  composition(s1$gender, "Gender", c("Female", "Male")),
  tibble(
    variable = "Race and ethnicity", level = race_labels,
    value = map_dbl(select(s1, race_white, race_black, race_asian, race_latino), \(r) 100 * mean(!is.na(r)))
  ),
  composition(s1$education, "Education", c(
    "Less than high school", "High school", "Some college", "4-year degree", "Advanced degree"
  )),
  composition(s1$age, "Age", age_labels)
) |>
  mutate(study = "Study 1")

s2_rows <- bind_rows(
  composition(s2$pid3, "Party", c("Dem", "Rep", "Ind"), party_labels),
  composition(s2$sex, "Gender", c("Female", "Male")),
  composition(s2$race, "Race and ethnicity", c(
    "White or Caucasian", "Black or African American", "Asian or Pacific Islander", "Hispanic or Latino"
  ), race_labels),
  composition(cut(s2$age, c(-Inf, 40, 65, Inf), right = FALSE, labels = age_labels), "Age", age_labels)
) |>
  mutate(study = "Study 2")

bind_rows(
  s1_rows, s2_rows,
  tibble(variable = "N", level = "Respondents", value = c(nrow(s1), nrow(s2)), study = c("Study 1", "Study 2"))
) |>
  readr::write_csv(table_path("samples.csv"))
