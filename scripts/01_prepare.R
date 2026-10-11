source("scripts/00_config.R")

truth <- truth_table()
study1 <- read_study1()
# Study 2 rows from the two experiments analyzed here; the Bayesian-cues rows ship in
# data/ but belong to Ahler and Sood (2023).
study2 <- filter(read_study2(), experiment %in% c("wording", "timing"))

readr::write_csv(study1_long(study1, truth), data_path("answers_study1.csv"))
readr::write_csv(wording_long(study2, truth), data_path("answers_wording.csv"))
readr::write_csv(timing_long(study2, truth), data_path("answers_timing.csv"))
readr::write_csv(truth, table_path("benchmarks.csv"))

# Sample composition: percent of respondents at each level, labelled for the table.
composition <- function(x, variable, levels, labels = levels) {
  tibble(variable, level = labels, value = map_dbl(levels, \(l) 100 * mean(x == l, na.rm = TRUE)))
}
party_labels <- c("Democrat (incl. leaners)", "Republican (incl. leaners)", "Independent")
race_labels <- c("White", "Black", "Asian", "Latino")
age_labels <- c("18-39", "40-64", "65+")

# Study 1 races are check-all-that-apply columns; Study 2 offered one choice.
study1_rows <- bind_rows(
  composition(study1$party, "Party", c("Democrat", "Republican", "Independent"), party_labels),
  composition(study1$gender, "Gender", c("Female", "Male")),
  tibble(
    variable = "Race and ethnicity", level = race_labels,
    value = map_dbl(select(study1, race_white, race_black, race_asian, race_latino), \(r) 100 * mean(r))
  ),
  composition(study1$education, "Education", c(
    "Less than high school", "High school", "Some college", "4-year degree", "Advanced degree"
  )),
  composition(study1$age_group, "Age", age_labels)
) |>
  mutate(study = "Study 1")

study2_rows <- bind_rows(
  composition(study2$party_3, "Party", c("Dem", "Rep", "Ind"), party_labels),
  composition(study2$gender, "Gender", c("Female", "Male")),
  composition(study2$race, "Race and ethnicity", c(
    "White or Caucasian", "Black or African American", "Asian or Pacific Islander", "Hispanic or Latino"
  ), race_labels),
  composition(cut(study2$age, c(-Inf, 40, 65, Inf), right = FALSE, labels = age_labels), "Age", age_labels)
) |>
  mutate(study = "Study 2")

bind_rows(
  study1_rows, study2_rows,
  tibble(variable = "N", level = "Respondents", value = c(nrow(study1), nrow(study2)), study = c("Study 1", "Study 2"))
) |>
  readr::write_csv(table_path("samples.csv"))
