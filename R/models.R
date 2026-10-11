# Estimators. Pooled effects regress an outcome on a treatment indicator with item
# fixed effects, so they compare answers to the same item, and with standard errors
# clustered by respondent (CR2, the bias-reduced estimator, with Bell-McCaffrey degrees
# of freedom; estimatr's default with clusters).

pooled_effect <- function(answers, treatment, outcome = "error", label = treatment) {
  fit <- estimatr::lm_robust(reformulate(treatment, outcome),
    fixed_effects = ~item, clusters = respondent_id, data = answers
  )
  arm_means <- answers |>
    summarise(mean = mean(.data[[outcome]]), .by = all_of(treatment)) |>
    deframe()
  estimatr::tidy(fit) |>
    filter(term == treatment) |>
    transmute(
      comparison = label, estimate, std_error = std.error, conf_low = conf.low, conf_high = conf.high,
      p_value = p.value, control_mean = arm_means[["0"]], treatment_mean = arm_means[["1"]],
      n_answers = nrow(answers), n_respondents = n_distinct(answers$respondent_id)
    )
}

# Each Study 1 treatment paired with standard estimation.
study1_pairs <- function(study1_answers) {
  names(study1_conditions)[-1] |>
    set_names() |>
    map(\(treatment) filter(study1_answers, condition %in% c("standard", treatment)))
}

study1_effects <- function(study1_answers) {
  study1_pairs(study1_answers) |>
    imap(\(answers, treatment) {
      answers |>
        mutate(treated = as.numeric(condition == treatment)) |>
        pooled_effect("treated", label = study1_conditions[[treatment]])
    }) |>
    list_rbind()
}

# Item-level means by arm with Welch tests of treatment minus control.
item_contrasts <- function(answers, arm, control, treatment) {
  answers |>
    mutate(arm = .data[[arm]]) |>
    summarise(
      control_mean = mean(perceived[arm == control]),
      control_se = std_error_of_mean(perceived[arm == control]),
      control_n = sum(arm == control),
      treatment_mean = mean(perceived[arm == treatment]),
      treatment_se = std_error_of_mean(perceived[arm == treatment]),
      treatment_n = sum(arm == treatment),
      difference = treatment_mean - control_mean,
      welch_test(perceived[arm == treatment], perceived[arm == control]),
      .by = item
    ) |>
    mutate(control = control, treatment = treatment)
}

std_error_of_mean <- function(x) sd(x) / sqrt(length(x))

welch_test <- function(treated, control) {
  broom::tidy(t.test(treated, control)) |>
    select(conf_low = conf.low, conf_high = conf.high, p_value = p.value)
}

# Two-sample Kolmogorov-Smirnov tests of each Study 1 treatment against standard
# estimation, with Benjamini-Hochberg adjustment within each treatment's eight tests.
# Answers are whole numbers, so p-values use the asymptotic distribution.
ks_tests <- function(study1_answers) {
  study1_pairs(study1_answers) |>
    imap(\(answers, treatment) {
      answers |>
        summarise(
          test = list(suppressWarnings(
            ks.test(perceived[condition == treatment], perceived[condition == "standard"], exact = FALSE)
          )),
          .by = item
        ) |>
        transmute(
          condition = study1_conditions[[treatment]], item,
          statistic = map_dbl(test, \(t) t$statistic), p_value = map_dbl(test, \(t) t$p.value),
          p_adjusted = p.adjust(p_value, "BH")
        )
    }) |>
    list_rbind()
}

# Study 1 standard-estimation respondents gave population base rates for a random
# subset of the eight groups after the party items. For each group: perceived party
# share minus perceived base rate, and the difference-in-differences
# (perceived party share - true party share) - (perceived base rate - true base rate),
# both within respondent.
base_rate_comparison <- function(study1, truth) {
  study1 |>
    filter(condition == "standard") |>
    select(respondent_id, paste0("perceived_", party_items), paste0("perceived_base_rate_", party_items)) |>
    pivot_longer(-respondent_id,
      names_to = c(".value", "item"), names_pattern = "^(perceived_base_rate|perceived)_(.+)$"
    ) |>
    rename(perceived_share = perceived) |>
    drop_na(perceived_share, perceived_base_rate) |>
    inner_join(select(truth, item, true_share, true_base_rate), by = "item") |>
    mutate(
      gap_each = perceived_share - perceived_base_rate,
      did_each = (perceived_share - true_share) - (perceived_base_rate - true_base_rate)
    ) |>
    summarise(
      n = n(), true_base_rate = first(true_base_rate), true_share = first(true_share),
      perceived_base_rate = mean(perceived_base_rate), perceived_share = mean(perceived_share),
      gap = mean(gap_each), one_sample_test(gap_each, "gap"),
      did = mean(did_each), one_sample_test(did_each, "did"),
      .by = item
    ) |>
    arrange(item_order(item))
}

# One-sample t-test against zero, as <prefix>_conf_low, <prefix>_conf_high, <prefix>_p_value.
one_sample_test <- function(x, prefix) {
  broom::tidy(t.test(x)) |>
    transmute(conf_low = conf.low, conf_high = conf.high, p_value = p.value) |>
    rename_with(\(n) paste(prefix, n, sep = "_"))
}

# April 2014 survey: perceived share of group g among out-party supporters regressed on
# the respondent's feeling thermometer rating of g. Each respondent rated all eight
# groups but estimated shares only for the party they do not lean toward.
thermometer_models <- function(affect) {
  groups <- c("black", "atheist", "lgb", "union", "rich", "evangelical", "southern", "young")
  ratings <- affect |>
    select(respondent_id, paste0("thermometer_", groups), paste0("perceived_", groups)) |>
    pivot_longer(-respondent_id, names_to = c(".value", "item"), names_pattern = "^(thermometer|perceived)_(.+)$") |>
    mutate(party = if_else(item %in% c("black", "atheist", "lgb", "union"), "Democrats", "Republicans")) |>
    drop_na(perceived, thermometer)
  by_item <- ratings |>
    nest(.by = c(item, party)) |>
    mutate(
      coefficient = map(data, \(d) thermometer_slope(estimatr::lm_robust(perceived ~ thermometer, d, se_type = "HC2"))),
      n = map_int(data, nrow)
    ) |>
    select(item, party, coefficient, n) |>
    unnest(coefficient) |>
    arrange(item)
  pooled <- estimatr::lm_robust(perceived ~ thermometer,
    fixed_effects = ~item, clusters = respondent_id, data = ratings
  )
  list(
    by_item = by_item,
    pooled = thermometer_slope(pooled) |> mutate(n = n_distinct(ratings$respondent_id))
  )
}

thermometer_slope <- function(fit) {
  estimatr::tidy(fit) |>
    filter(term == "thermometer") |>
    select(estimate, std_error = std.error, conf_low = conf.low, conf_high = conf.high)
}

# Diet soda: asking for the population base rate first, or not.
soda_experiment <- function(study2) {
  wording <- study2 |>
    filter(experiment == "wording") |>
    mutate(gap = soda_women - soda_men, asked_base_rate = soda_condition == "base_rate_first")
  means <- wording |>
    summarise(
      n = n(), women = mean(soda_women, na.rm = TRUE), men = mean(soda_men, na.rm = TRUE),
      gap = mean(gap, na.rm = TRUE), all = mean(soda_all, na.rm = TRUE),
      .by = asked_base_rate
    ) |>
    arrange(asked_base_rate)
  # t.test(gap ~ asked_base_rate) estimates FALSE minus TRUE; report asked minus not asked.
  test <- broom::tidy(t.test(gap ~ asked_base_rate, data = wording)) |>
    transmute(difference = -estimate, conf_low = -conf.high, conf_high = -conf.low, p_value = p.value)
  list(means = means, test = test)
}

# Timing compliance from the page timers: seconds from page load to submission.
timing_compliance <- function(study2) {
  study2 |>
    filter(experiment == "timing") |>
    select(timing_condition, matches("^timer_.+_page_submit$")) |>
    pivot_longer(-timing_condition, values_to = "seconds", values_drop_na = TRUE) |>
    summarise(
      pages = n(), median_seconds = median(seconds),
      under_15 = sum(seconds < 15), over_11 = sum(seconds > 11),
      .by = timing_condition
    ) |>
    arrange(timing_condition)
}

# Answers on either whole number bracketing the true share (8 or 9 for 8.7), among the
# items each respondent was actually shown. Spikes here would suggest looking answers up.
on_the_nose <- function(answers) {
  mutate(answers, on_the_nose = as.numeric(between(perceived, floor(true_share), ceiling(true_share))))
}
