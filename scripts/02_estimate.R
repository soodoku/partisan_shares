source("scripts/00_config.R")

truth <- readr::read_csv(table_path("benchmarks.csv"), show_col_types = FALSE)
write_table_csv <- function(df, name) readr::write_csv(df, table_path(paste0(name, ".csv")))

# Signed error (the figure), plus absolute error and percentage bias.
read_answers <- function(name) {
  read_data(name) |>
    mutate(abs_error = abs(error), pct_error = 100 * error / true_share)
}
study1_answers <- read_answers("answers_study1.csv")
wording_answers <- read_answers("answers_wording.csv")
timing_answers <- read_answers("answers_timing.csv")

study2_effects <- function(outcome) {
  bind_rows(
    pooled_effect(timing_answers, "time_requirement", outcome = outcome, label = "Time requirement"),
    pooled_effect(wording_answers, "percent", outcome = outcome, label = "Percent wording")
  )
}

bind_rows(
  study1_effects(study1_answers) |> mutate(study = "Study 1"),
  study2_effects("error") |> mutate(study = "Study 2")
) |>
  write_table_csv("effects")

c("error", "abs_error", "pct_error") |>
  map(\(outcome) mutate(study2_effects(outcome), outcome = outcome)) |>
  list_rbind() |>
  write_table_csv("effects_alt_outcomes")

bind_rows(
  item_contrasts(timing_answers, "timing_condition", "time_pressure", "time_requirement") |>
    mutate(experiment = "Timing"),
  item_contrasts(wording_answers, "wording", "out_of_100", "percent") |>
    mutate(experiment = "Wording"),
  study1_pairs(study1_answers) |>
    imap(\(answers, treatment) item_contrasts(answers, "condition", "standard", treatment)) |>
    list_rbind() |>
    mutate(experiment = "Study 1")
) |>
  write_table_csv("item_contrasts")

write_table_csv(ks_tests(study1_answers), "ks_tests")

# Looking answers up: share of answers on the nose, by timing condition.
timing_on_the_nose <- on_the_nose(timing_answers)
bind_rows(
  timing_on_the_nose |>
    mutate(perceived = on_the_nose) |>
    item_contrasts("timing_condition", "time_pressure", "time_requirement"),
  pooled_effect(timing_on_the_nose, "time_requirement", outcome = "on_the_nose", label = "All items") |>
    transmute(
      item = "all", control_mean, treatment_mean, difference = estimate, conf_low, conf_high, p_value,
      control_n = n_answers
    )
) |>
  write_table_csv("on_the_nose")

write_table_csv(base_rate_comparison(read_study1(), truth), "base_rates")

thermometer <- thermometer_models(read_data("affect_2014.csv"))
write_table_csv(thermometer$by_item, "thermometer")
write_table_csv(thermometer$pooled, "thermometer_pooled")

study2 <- read_study2()
soda <- soda_experiment(study2)
write_table_csv(soda$means, "soda_means")
write_table_csv(soda$test, "soda_test")
write_table_csv(timing_compliance(study2), "timing_compliance")

# Sample flow for Study 2, all three experiments.
study2 |>
  count(experiment, timing_condition, soda_condition, bayes_condition) |>
  write_table_csv("study2_assignment")
