source("scripts/00_config.R")

truth <- truth_table()
s1 <- read_study1()
s2 <- read_study2()

readr::write_csv(study1_long(s1, truth), data_path("answers_study1.csv"))
readr::write_csv(wording_long(s2, truth), data_path("answers_wording.csv"))
readr::write_csv(timing_long(s2, truth), data_path("answers_timing.csv"))
readr::write_csv(truth, table_path("benchmarks.csv"))

# Sample composition. Study 1 races are check-all-that-apply; Study 2 offered one choice.
pct <- function(x, level) 100 * mean(x == level, na.rm = TRUE)
s1_rows <- tibble(
  variable = c(
    rep("Party", 3), rep("Gender", 2), rep("Race and ethnicity", 4), rep("Education", 5), rep("Age", 3)
  ),
  level = c(
    "Democrat (incl. leaners)", "Republican (incl. leaners)", "Independent", "Female", "Male",
    "White", "Black", "Asian", "Latino", "Less than high school", "High school", "Some college",
    "4-year degree", "Advanced degree", "18-39", "40-64", "65+"
  ),
  value = c(
    pct(s1$party, "Democrat"), pct(s1$party, "Republican"), pct(s1$party, "Independent"),
    pct(s1$gender, "Female"), pct(s1$gender, "Male"),
    100 * mean(!is.na(s1$race_white)), 100 * mean(!is.na(s1$race_black)),
    100 * mean(!is.na(s1$race_asian)), 100 * mean(!is.na(s1$race_latino)),
    pct(s1$education, "Less than high school"), pct(s1$education, "High school"),
    pct(s1$education, "Some college"), pct(s1$education, "4-year degree"),
    pct(s1$education, "Advanced degree"),
    pct(s1$age, "18-39"), pct(s1$age, "40-64"), pct(s1$age, "65+")
  ),
  study = "Study 1"
)
s2_rows <- tibble(
  variable = c(rep("Party", 3), rep("Gender", 2), rep("Race and ethnicity", 4), rep("Age", 3)),
  level = c(
    "Democrat (incl. leaners)", "Republican (incl. leaners)", "Independent", "Female", "Male",
    "White", "Black", "Asian", "Latino", "18-39", "40-64", "65+"
  ),
  value = c(
    pct(s2$pid3, "Dem"), pct(s2$pid3, "Rep"), pct(s2$pid3, "Ind"),
    pct(s2$sex, "Female"), pct(s2$sex, "Male"),
    pct(s2$race, "White or Caucasian"), pct(s2$race, "Black or African American"),
    pct(s2$race, "Asian or Pacific Islander"), pct(s2$race, "Hispanic or Latino"),
    100 * mean(s2$age < 40, na.rm = TRUE), 100 * mean(s2$age >= 40 & s2$age < 65, na.rm = TRUE),
    100 * mean(s2$age >= 65, na.rm = TRUE)
  ),
  study = "Study 2"
)
readr::write_csv(
  bind_rows(s1_rows, s2_rows) |>
    bind_rows(tibble(
      variable = "N", level = "Respondents", value = c(nrow(s1), nrow(s2)), study = c("Study 1", "Study 2")
    )),
  table_path("samples.csv")
)
