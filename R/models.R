# Estimators. Pooled effects regress an outcome on a treatment indicator with item
# fixed effects and respondent-clustered standard errors (Stata's small-sample
# correction, as in the original analysis), so they compare answers to the same item.

pooled_effect <- function(long, treatment, outcome = "err", label = treatment) {
  f <- estimatr::lm_robust(stats::reformulate(treatment, outcome),
    fixed_effects = ~item, clusters = respondent_id, data = long, se_type = "stata"
  )
  tibble(
    comparison = label, estimate = unname(f$coefficients[treatment]), se = unname(f$std.error[treatment]),
    lo = unname(f$conf.low[treatment]), hi = unname(f$conf.high[treatment]), p = unname(f$p.value[treatment]),
    baseline = mean(long[[outcome]][long[[treatment]] == 0]),
    treated = mean(long[[outcome]][long[[treatment]] == 1]),
    n_answers = nrow(long), n_respondents = length(unique(long$respondent_id))
  )
}

study1_effects <- function(l1) {
  bind_rows(lapply(names(study1_conditions)[-1], function(cond) {
    d <- l1 |>
      filter(condition %in% c("simpleask", cond)) |>
      mutate(treat = as.numeric(condition == cond))
    pooled_effect(d, "treat", label = study1_conditions[[cond]])
  }))
}

# Item-level means by condition with Welch tests of the difference.
item_contrasts <- function(long, group, levels) {
  long |>
    group_by(item) |>
    group_modify(function(df, key) {
      a <- df$y[df[[group]] == levels[1]]
      b <- df$y[df[[group]] == levels[2]]
      tt <- stats::t.test(b, a)
      tibble(
        mean_a = mean(a), se_a = stats::sd(a) / sqrt(length(a)), n_a = length(a),
        mean_b = mean(b), se_b = stats::sd(b) / sqrt(length(b)), n_b = length(b),
        diff = mean(b) - mean(a), lo = tt$conf.int[1], hi = tt$conf.int[2], p = tt$p.value
      )
    }) |>
    ungroup() |>
    mutate(group_a = levels[1], group_b = levels[2])
}

# Two-sample Kolmogorov-Smirnov tests of each Study 1 treatment against standard
# estimation, with Benjamini-Hochberg adjustment within each treatment's eight tests.
# Answers are whole numbers, so p-values use the asymptotic distribution.
ks_tests <- function(l1) {
  bind_rows(lapply(names(study1_conditions)[-1], function(cond) {
    bind_rows(lapply(party_items, function(k) {
      a <- l1$y[l1$item == k & l1$condition == "simpleask"]
      b <- l1$y[l1$item == k & l1$condition == cond]
      ks <- suppressWarnings(stats::ks.test(b, a, exact = FALSE))
      tibble(condition = study1_conditions[[cond]], item = k, d = unname(ks$statistic), p = ks$p.value)
    })) |>
      mutate(p_bh = stats::p.adjust(p, "BH"))
  }))
}

# Study 1 standard-estimation respondents gave population base rates for a random
# subset of the eight groups after the party items. For each group: perceived party
# share minus perceived base rate, and the difference-in-differences
# (party perception - party truth) - (base-rate perception - base-rate truth), both
# within respondent.
base_rate_comparison <- function(s1, truth) {
  sa <- filter(s1, condition == "simpleask")
  col <- setNames(paste0(ifelse(items$party[1:8] == "Democrats", "dem_", "rep_"), party_items), party_items)
  bind_rows(lapply(party_items, function(k) {
    p <- sa[[col[[k]]]]
    b <- sa[[paste0("br_", k)]]
    ok <- !is.na(p) & !is.na(b)
    t_row <- truth[truth$item == k, ]
    gap <- p[ok] - b[ok]
    dd <- (p[ok] - t_row$truth) - (b[ok] - t_row$base)
    tg <- stats::t.test(gap)
    td <- stats::t.test(dd)
    tibble(
      item = k, n = sum(ok), base_truth = t_row$base, party_truth = t_row$truth,
      base_perceived = mean(b[ok]), party_perceived = mean(p[ok]),
      gap = mean(gap), gap_lo = tg$conf.int[1], gap_hi = tg$conf.int[2], gap_p = tg$p.value,
      dd = mean(dd), dd_lo = td$conf.int[1], dd_hi = td$conf.int[2], dd_p = td$p.value
    )
  }))
}

# April 2014 survey: perceived share of group g among out-party supporters regressed on
# the respondent's feeling thermometer rating of g. Each respondent rated all eight
# groups but estimated shares only for the party they do not lean toward.
thermometer_models <- function(affect) {
  pairs <- tibble::tribble(
    ~item, ~share, ~ft, ~party,
    "black", "d_black", "ft_black", "Democrats",
    "union", "d_union", "ft_union", "Democrats",
    "lgb", "d_lgb", "ft_lgb", "Democrats",
    "aa", "d_aa", "ft_aa", "Democrats",
    "rich", "r_rich", "ft_rich", "Republicans",
    "evang", "r_evang", "ft_evang", "Republicans",
    "south", "r_south", "ft_south", "Republicans",
    "young", "r_young", "ft_young", "Republicans"
  )
  long <- bind_rows(lapply(seq_len(nrow(pairs)), function(i) {
    tibble(
      respondent_id = affect$respondent_id, item = pairs$item[i], party = pairs$party[i],
      share = affect[[pairs$share[i]]], ft = affect[[pairs$ft[i]]]
    )
  })) |>
    filter(!is.na(share), !is.na(ft))
  by_item <- long |>
    group_by(item, party) |>
    group_modify(function(df, key) {
      f <- estimatr::lm_robust(share ~ ft, data = df, se_type = "HC2")
      tibble(
        estimate = f$coefficients[["ft"]], se = f$std.error[["ft"]], lo = f$conf.low[["ft"]],
        hi = f$conf.high[["ft"]], n = nrow(df)
      )
    }) |>
    ungroup()
  pooled <- estimatr::lm_robust(share ~ ft,
    fixed_effects = ~item, clusters = respondent_id,
    data = long, se_type = "stata"
  )
  list(
    by_item = by_item,
    pooled = tibble(
      estimate = pooled$coefficients[["ft"]], se = pooled$std.error[["ft"]],
      lo = pooled$conf.low[["ft"]], hi = pooled$conf.high[["ft"]], n = length(unique(long$respondent_id))
    )
  )
}

# Diet soda: asking for the population base rate first, or not.
soda_experiment <- function(s2) {
  d <- s2 |>
    filter(study == "wording") |>
    mutate(gap = soda_women - soda_men, asked_base = soda_cond == "br")
  tt <- stats::t.test(gap ~ asked_base, data = d)
  means <- d |>
    group_by(asked_base) |>
    summarise(
      n = n(), women = mean(soda_women, na.rm = TRUE), men = mean(soda_men, na.rm = TRUE),
      gap = mean(gap, na.rm = TRUE), base = mean(soda_br, na.rm = TRUE), .groups = "drop"
    )
  list(
    means = means,
    test = tibble(
      diff = unname(diff(tt$estimate)), lo = -tt$conf.int[2], hi = -tt$conf.int[1], p = tt$p.value
    )
  )
}

# Answers on either whole number bracketing the true share (8 or 9 for 8.7), among the
# items each respondent was actually shown. Spikes here would suggest looking answers up.
on_the_nose <- function(long) {
  mutate(long, exact = as.numeric(y >= floor(truth) & y <= ceiling(truth)))
}
