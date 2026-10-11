source("scripts/00_config.R")

truth <- readr::read_csv(table_path("benchmarks.csv"), show_col_types = FALSE)

# Signed error (the figure), plus absolute error and percentage bias.
read_answers <- function(name) {
  read_data(name) |>
    mutate(abs_err = abs(err), pct_err = 100 * err / truth)
}
l1 <- read_answers("answers_study1.csv")
lw <- read_answers("answers_wording.csv")
lt <- read_answers("answers_timing.csv")

effects <- bind_rows(
  study1_effects(l1) |> mutate(study = "Study 1"),
  pooled_effect(lt, "slow", label = "Time requirement") |> mutate(study = "Study 2"),
  pooled_effect(lw, "percent", label = "Percent wording") |> mutate(study = "Study 2")
)
readr::write_csv(effects, table_path("effects.csv"))

alt <- c("err", "abs_err", "pct_err") |>
  map(\(y) {
    bind_rows(
      pooled_effect(lt, "slow", outcome = y, label = "Time requirement"),
      pooled_effect(lw, "percent", outcome = y, label = "Percent wording")
    ) |>
      mutate(outcome = y)
  }) |>
  list_rbind()
readr::write_csv(alt, table_path("effects_alt_outcomes.csv"))

# Item-level contrasts.
readr::write_csv(
  bind_rows(
    item_contrasts(lt, "timing", c("fast", "slow")) |> mutate(experiment = "Timing"),
    item_contrasts(lw, "wording", c("out_of_100", "percent")) |> mutate(experiment = "Wording"),
    study1_pairs(l1) |>
      imap(\(d, cond) item_contrasts(d, "condition", c("simpleask", cond))) |>
      list_rbind() |>
      mutate(experiment = "Study 1")
  ),
  table_path("item_contrasts.csv")
)

readr::write_csv(ks_tests(l1), table_path("ks_tests.csv"))

# Looking answers up: share of answers on the nose, by timing condition.
lt_exact <- on_the_nose(lt)
readr::write_csv(
  bind_rows(
    item_contrasts(mutate(lt_exact, y = exact), "timing", c("fast", "slow")),
    pooled_effect(lt_exact, "slow", outcome = "exact", label = "All items") |>
      transmute(item = "all", mean_a = baseline, mean_b = treated, diff = estimate, lo, hi, p, n_a = n_answers)
  ),
  table_path("on_the_nose.csv")
)

readr::write_csv(base_rate_comparison(read_study1(), truth), table_path("base_rates.csv"))

thermo <- thermometer_models(read_data("affect_2014.csv"))
readr::write_csv(thermo$by_item, table_path("thermometer.csv"))
readr::write_csv(thermo$pooled, table_path("thermometer_pooled.csv"))

s2 <- read_study2()
soda <- soda_experiment(s2)
readr::write_csv(soda$means, table_path("soda_means.csv"))
readr::write_csv(soda$test, table_path("soda_test.csv"))

# Sample flow for Study 2.
readr::write_csv(count(s2, study, timing_cond, soda_cond), table_path("study2_assignment.csv"))
