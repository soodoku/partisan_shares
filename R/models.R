# Estimators. Pooled effects regress an outcome on a treatment indicator with item
# fixed effects and respondent-clustered standard errors (Stata's small-sample
# correction, as in the original analysis), so they compare answers to the same item.

pooled_effect <- function(long, treatment, outcome = "err", label = treatment) {
  fit <- estimatr::lm_robust(reformulate(treatment, outcome),
    fixed_effects = ~item, clusters = respondent_id, data = long, se_type = "stata"
  )
  arms <- long |>
    summarise(mean = mean(.data[[outcome]]), .by = all_of(treatment)) |>
    deframe()
  estimatr::tidy(fit) |>
    filter(term == treatment) |>
    transmute(
      comparison = label, estimate, se = std.error, lo = conf.low, hi = conf.high, p = p.value,
      baseline = arms[["0"]], treated = arms[["1"]],
      n_answers = nrow(long), n_respondents = n_distinct(long$respondent_id)
    )
}

# Each Study 1 treatment against standard estimation.
study1_pairs <- function(l1) {
  names(study1_conditions)[-1] |>
    set_names() |>
    map(\(cond) filter(l1, condition %in% c("simpleask", cond)))
}

study1_effects <- function(l1) {
  study1_pairs(l1) |>
    imap(\(d, cond) {
      pooled_effect(mutate(d, treat = as.numeric(condition == cond)), "treat", label = study1_conditions[[cond]])
    }) |>
    list_rbind()
}

# Item-level means by condition with Welch tests of the difference (b minus a).
item_contrasts <- function(long, group, levels) {
  long |>
    mutate(arm = .data[[group]]) |>
    summarise(
      mean_a = mean(y[arm == levels[1]]), se_a = se_mean(y[arm == levels[1]]), n_a = sum(arm == levels[1]),
      mean_b = mean(y[arm == levels[2]]), se_b = se_mean(y[arm == levels[2]]), n_b = sum(arm == levels[2]),
      diff = mean_b - mean_a,
      welch(y[arm == levels[2]], y[arm == levels[1]]),
      .by = item
    ) |>
    mutate(group_a = levels[1], group_b = levels[2])
}

se_mean <- function(x) sd(x) / sqrt(length(x))

welch <- function(b, a) {
  broom::tidy(t.test(b, a)) |> select(lo = conf.low, hi = conf.high, p = p.value)
}

# Two-sample Kolmogorov-Smirnov tests of each Study 1 treatment against standard
# estimation, with Benjamini-Hochberg adjustment within each treatment's eight tests.
# Answers are whole numbers, so p-values use the asymptotic distribution.
ks_tests <- function(l1) {
  study1_pairs(l1) |>
    imap(\(d, cond) {
      d |>
        summarise(
          ks = list(suppressWarnings(ks.test(y[condition == cond], y[condition == "simpleask"], exact = FALSE))),
          .by = item
        ) |>
        transmute(
          condition = study1_conditions[[cond]], item,
          d = map_dbl(ks, \(k) k$statistic), p = map_dbl(ks, \(k) k$p.value),
          p_bh = p.adjust(p, "BH")
        )
    }) |>
    list_rbind()
}

# Study 1 standard-estimation respondents gave population base rates for a random
# subset of the eight groups after the party items. For each group: perceived party
# share minus perceived base rate, and the difference-in-differences
# (party perception - party truth) - (base-rate perception - base-rate truth), both
# within respondent.
base_rate_comparison <- function(s1, truth) {
  s1 |>
    filter(condition == "simpleask") |>
    select(respondent_id, matches("^(dem|rep|br)_")) |>
    pivot_longer(-respondent_id, names_to = c("kind", "item"), names_pattern = "^(dem|rep|br)_(.+)$") |>
    mutate(kind = if_else(kind == "br", "base_perceived", "party_perceived")) |>
    pivot_wider(names_from = kind, values_from = value) |>
    drop_na(party_perceived, base_perceived) |>
    inner_join(select(truth, item, party_truth = truth, base_truth = base), by = "item") |>
    mutate(
      gap_i = party_perceived - base_perceived,
      dd_i = (party_perceived - party_truth) - (base_perceived - base_truth)
    ) |>
    summarise(
      n = n(), base_truth = first(base_truth), party_truth = first(party_truth),
      base_perceived = mean(base_perceived), party_perceived = mean(party_perceived),
      gap = mean(gap_i), t_summary(gap_i, "gap"),
      dd = mean(dd_i), t_summary(dd_i, "dd"),
      .by = item
    ) |>
    arrange(factor(item, party_items))
}

# One-sample t-test of `x` against zero, as <prefix>_lo, <prefix>_hi, <prefix>_p.
t_summary <- function(x, prefix) {
  broom::tidy(t.test(x)) |>
    transmute(lo = conf.low, hi = conf.high, p = p.value) |>
    rename_with(\(n) paste(prefix, n, sep = "_"))
}

# April 2014 survey: perceived share of group g among out-party supporters regressed on
# the respondent's feeling thermometer rating of g. Each respondent rated all eight
# groups but estimated shares only for the party they do not lean toward (d_* for
# Republicans rating Democrats' composition, r_* the reverse).
thermometer_models <- function(affect) {
  long <- affect |>
    select(respondent_id, matches("^(ft|d|r)_")) |>
    pivot_longer(-respondent_id, names_to = c(".value", "item"), names_pattern = "^(ft|d|r)_(.+)$") |>
    mutate(
      party = if_else(item %in% c("black", "aa", "lgb", "union"), "Democrats", "Republicans"),
      share = coalesce(d, r)
    ) |>
    drop_na(share, ft)
  by_item <- long |>
    nest(.by = c(item, party)) |>
    mutate(
      coef = map(data, \(df) ft_coefficient(estimatr::lm_robust(share ~ ft, data = df, se_type = "HC2"))),
      n = map_int(data, nrow)
    ) |>
    select(item, party, coef, n) |>
    unnest(coef) |>
    arrange(item)
  pooled <- estimatr::lm_robust(share ~ ft,
    fixed_effects = ~item, clusters = respondent_id, data = long, se_type = "stata"
  )
  list(
    by_item = by_item,
    pooled = ft_coefficient(pooled) |> mutate(n = n_distinct(long$respondent_id))
  )
}

ft_coefficient <- function(fit) {
  estimatr::tidy(fit) |>
    filter(term == "ft") |>
    select(estimate, se = std.error, lo = conf.low, hi = conf.high)
}

# Diet soda: asking for the population base rate first, or not.
soda_experiment <- function(s2) {
  d <- s2 |>
    filter(study == "wording") |>
    mutate(gap = soda_women - soda_men, asked_base = soda_cond == "br")
  means <- d |>
    summarise(
      n = n(), women = mean(soda_women, na.rm = TRUE), men = mean(soda_men, na.rm = TRUE),
      gap = mean(gap, na.rm = TRUE), base = mean(soda_br, na.rm = TRUE),
      .by = asked_base
    ) |>
    arrange(asked_base)
  # t.test(gap ~ asked_base) estimates FALSE minus TRUE; report asked minus not asked.
  test <- broom::tidy(t.test(gap ~ asked_base, data = d)) |>
    transmute(diff = -estimate, lo = -conf.high, hi = -conf.low, p = p.value)
  list(means = means, test = test)
}

# Timing compliance from the page timers: seconds from page load to submission.
timing_compliance <- function(s2) {
  s2 |>
    filter(study == "timing") |>
    select(matches("^timer_.+_page_submit$")) |>
    pivot_longer(everything(), names_to = "page", values_to = "seconds", values_drop_na = TRUE) |>
    mutate(condition = if_else(grepl("_fast_", page), "fast", "slow")) |>
    summarise(
      pages = n(), median = median(seconds), under_15 = sum(seconds < 15), over_11 = sum(seconds > 11),
      .by = condition
    ) |>
    arrange(condition)
}

# Answers on either whole number bracketing the true share (8 or 9 for 8.7), among the
# items each respondent was actually shown. Spikes here would suggest looking answers up.
on_the_nose <- function(long) {
  mutate(long, exact = as.numeric(between(y, floor(truth), ceiling(truth))))
}
