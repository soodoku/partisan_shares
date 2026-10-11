source("scripts/00_config.R")

read_tab <- function(name) readr::read_csv(table_path(paste0(name, ".csv")), show_col_types = FALSE)

truth <- read_tab("benchmarks")
undocumented <- readr::read_csv(data_path("benchmarks.csv"), show_col_types = FALSE) |>
  filter(item == "undocumented") |>
  pull(value)
effects <- read_tab("effects")
alt <- read_tab("effects_alt_outcomes")
contrasts <- read_tab("item_contrasts")
ks <- read_tab("ks_tests")
nose <- read_tab("on_the_nose")
base_rates <- read_tab("base_rates")
thermo <- read_tab("thermometer")
thermo_pooled <- read_tab("thermometer_pooled")
soda_means <- read_tab("soda_means")
soda_test <- read_tab("soda_test")
samples <- read_tab("samples")
lt <- readr::read_csv(data_path("answers_timing.csv"), show_col_types = FALSE)

item_label <- setNames(paste0(items$party, ": ", items$label), items$item)
item_short <- setNames(
  c(
    "Dem. black", "Dem. atheist/agnostic", "Dem. LGB", "Dem. union", "Rep. $250K+", "Rep. evangelical",
    "Rep. Southern", "Rep. 65+", "U.S. foreign-born", "Poor who are black"
  ),
  items$item
)
p_stars <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))

# Figure 1: pooled effects of each elicitation change on signed error.
fig1 <- effects |>
  mutate(
    label = c(
      "Sum to 100 (vs. standard)", "Accuracy incentives (vs. standard)", "Base rates given (vs. standard)",
      "Time requirement (vs. time pressure)", "\"What percentage\" (vs. \"Out of every 100\")"
    ),
    label = factor(label, rev(label))
  ) |>
  ggplot(aes(x = estimate, y = label, xmin = lo, xmax = hi)) +
  geom_vline(xintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_errorbar(orientation = "y", width = 0, linewidth = 0.5, colour = colours[["ink"]]) +
  geom_point(size = 2, colour = colours[["ink"]]) +
  facet_grid(study ~ ., scales = "free_y", space = "free_y") +
  labs(x = "Change in signed error (percentage points), 95% CI", y = NULL) +
  theme_paper() +
  theme(strip.text.y = element_text(angle = 0, face = "bold"))
save_figure(fig1, "effects", 6.5, 3)

# Item-level means by condition with 95% intervals and the true share.
item_plot <- function(d, labels) {
  d <- d |>
    mutate(
      comparison = factor(labels[group_b], unname(labels)),
      item_name = factor(item_short[item], rev(item_short))
    )
  long <- bind_rows(
    d |> transmute(comparison, item, item_name, arm = "Baseline", mean = mean_a, se = se_a),
    d |> transmute(comparison, item, item_name, arm = "Treatment", mean = mean_b, se = se_b)
  ) |>
    mutate(lo = mean - 1.96 * se, hi = mean + 1.96 * se)
  tr <- d |>
    distinct(comparison, item, item_name) |>
    left_join(select(truth, item, truth), by = "item")
  ggplot(long, aes(x = mean, y = item_name, colour = arm, shape = arm)) +
    geom_point(
      data = tr, aes(x = truth, y = item_name), inherit.aes = FALSE, shape = 124, size = 3, colour = "grey30"
    ) +
    geom_errorbar(aes(xmin = lo, xmax = hi),
      orientation = "y", width = 0, linewidth = 0.4, position = position_dodge(width = 0.6)
    ) +
    geom_point(size = 1.8, position = position_dodge(width = 0.6)) +
    scale_colour_manual(values = c(Baseline = colours[["a"]], Treatment = colours[["b"]])) +
    scale_shape_manual(values = c(Baseline = 16, Treatment = 17)) +
    facet_wrap(~comparison, nrow = 1) +
    labs(x = "Mean reported share (%); | marks the true share", y = NULL) +
    theme_paper()
}
fig_s1 <- item_plot(
  filter(contrasts, experiment == "Study 1"),
  c(complexask = "Sum to 100", incentive = "Incentives", baserates_given = "Base rates given")
) + labs(caption = "Baseline: standard estimation")
save_figure(fig_s1, "items_study1", 7, 3.4)
fig_s2 <- item_plot(
  filter(contrasts, experiment != "Study 1"),
  c(
    slow = "Time requirement (baseline: time pressure)",
    percent = "\"What percentage\" (baseline: \"Out of every 100\")"
  )
)
save_figure(fig_s2, "items_study2", 7, 3.8)

# Timing experiment: distribution of answers relative to the truth. Looking answers
# up would show as a spike at zero in the time-requirement condition.
fig_density <- lt |>
  mutate(
    condition = if_else(timing == "slow", "Time requirement", "Time pressure"),
    item_name = factor(item_short[item], item_short)
  ) |>
  ggplot(aes(x = err, colour = condition, linetype = condition)) +
  geom_vline(xintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_density(linewidth = 0.5, bw = 3) +
  scale_colour_manual(values = c("Time pressure" = colours[["a"]], "Time requirement" = colours[["b"]])) +
  facet_wrap(~item_name, ncol = 5) +
  labs(x = "Reported minus true share (percentage points)", y = "Density") +
  theme_paper()
save_figure(fig_density, "timing_density", 7, 3.6)

# Tables.
write_tabular(
  samples |>
    filter(variable != "N") |>
    pivot_wider(names_from = study, values_from = value) |>
    transmute(
      ` ` = level,
      `Study 1 (2014)` = num(`Study 1`, 1),
      `Study 2 (2016)` = ifelse(is.na(`Study 2`), "--", num(`Study 2`, 1))
    ),
  table_path("samples.tex"), "lrr",
  groups = c(`1` = "Party", `4` = "Gender", `6` = "Race and ethnicity", `10` = "Education", `15` = "Age")
)

write_tabular(
  base_rates |>
    mutate(item = factor(item, party_items)) |>
    arrange(item) |>
    transmute(
      Group = item_short[as.character(item)], n = n,
      `Base, true` = num(base_truth, 1), `Base, perceived` = num(base_perceived, 1),
      `Party, true` = num(party_truth, 1), `Party, perceived` = num(party_perceived, 1),
      `Party - base` = paste0(num(gap, 1), p_stars(gap_p)),
      DiD = paste0(num(dd, 1), p_stars(dd_p)),
      `DiD 95% CI` = interval(dd_lo, dd_hi, digits = 1)
    ),
  table_path("base_rates.tex"), "lrrrrrrrr"
)

write_tabular(
  ks |>
    mutate(cell = paste0(num(d, 2), " (", num(p_bh, 2), ")")) |>
    select(condition, item, cell) |>
    pivot_wider(names_from = condition, values_from = cell) |>
    mutate(item = item_short[item]) |>
    rename(Group = item),
  table_path("ks.tex"), "llll"
)

write_tabular(
  thermo |>
    mutate(item = factor(item, c("black", "aa", "lgb", "union", "rich", "evang", "south", "young"))) |>
    arrange(item) |>
    transmute(
      `Party and group` = c(item_short[levels(item)[1:7]], "Rep. under 35")[as.integer(item)],
      Coefficient = num(estimate, 2), SE = num(se, 2), `95% CI` = interval(lo, hi), n = n
    ),
  table_path("thermometer.tex"), "lrrrr"
)

write_tabular(
  nose |>
    mutate(order = match(item, c(items$item, "all"))) |>
    arrange(order) |>
    transmute(
      Item = ifelse(item == "all", "All items (item fixed effects)", item_short[item]),
      `Time pressure` = pct(mean_a, 1), `Time requirement` = pct(mean_b, 1),
      Difference = pct(diff, 1), `95% CI` = interval(lo, hi, scale = 100, digits = 1), p = num(p, 2)
    ),
  table_path("on_the_nose.tex"), "lrrrrr"
)

outcome_names <- c(err = "Signed error", abs_err = "Absolute error", pct_err = "Percentage bias")
write_tabular(
  alt |>
    transmute(
      Outcome = outcome_names[outcome], Comparison = comparison,
      Baseline = num(baseline, 1), Effect = paste0(num(estimate, 1), p_stars(p)), SE = num(se, 1),
      Answers = n_answers, Respondents = n_respondents
    ),
  table_path("alt_outcomes.tex"), "llrrrrr"
)

write_tabular(
  truth |>
    transmute(
      Item = item_short[item], `True share` = num(truth, 1),
      `Population base rate` = ifelse(is.na(base), "--", num(base, 1))
    ),
  table_path("benchmarks.tex"), "lrr"
)

# Manuscript numbers.
eff <- function(cmp, col) effects[[col]][effects$comparison == cmp]
ic <- function(exp, b, it, col) {
  contrasts[[col]][contrasts$experiment == exp & contrasts$group_b == b & contrasts$item == it]
}
br <- function(it, col) base_rates[[col]][base_rates$item == it]
timing_items <- filter(contrasts, experiment == "Timing")
wording_items <- filter(contrasts, experiment == "Wording")
word_count <- c("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
soda_no <- soda_means[!soda_means$asked_base, ]
soda_yes <- soda_means[soda_means$asked_base, ]
nose_all <- nose[nose$item == "all", ]
n_samples <- samples$value[samples$variable == "N"]

write_macros(list(
  nStudyOne = n_samples[1], nStudyTwo = n_samples[2],
  nTiming = eff("Time requirement", "n_respondents"), nWording = eff("Percent wording", "n_respondents"),
  sumEffect = num(eff("Sum to 100", "estimate"), 1),
  standardBias = num(eff("Sum to 100", "baseline"), 1),
  sumBias = num(eff("Sum to 100", "treated"), 1),
  sumBlackAbs = num(abs(ic("Study 1", "complexask", "black", "diff")), 1),
  sumSouth = num(ic("Study 1", "complexask", "south", "diff"), 1),
  sumSouthMean = num(ic("Study 1", "complexask", "south", "mean_b"), 1),
  standardSouthMean = num(ic("Study 1", "complexask", "south", "mean_a"), 1),
  incEffect = num(eff("Incentives", "estimate"), 1),
  incLo = num(eff("Incentives", "lo"), 1), incHi = num(eff("Incentives", "hi"), 1),
  brEffect = num(eff("Base rates given", "estimate"), 1),
  brLo = num(eff("Base rates given", "lo"), 1), brHi = num(eff("Base rates given", "hi"), 1),
  brItemsUp = word_count[sum(filter(contrasts, group_b == "baserates_given")$diff > 0) + 1],
  timeEffect = num(abs(eff("Time requirement", "estimate")), 1),
  timeLo = num(abs(eff("Time requirement", "hi")), 1), timeHi = num(abs(eff("Time requirement", "lo")), 1),
  fastBias = num(eff("Time requirement", "baseline"), 1),
  slowBias = num(eff("Time requirement", "treated"), 1),
  timeDecline = pct(abs(eff("Time requirement", "estimate")) / eff("Time requirement", "baseline")),
  timeItemsLower = word_count[sum(timing_items$diff < 0) + 1],
  timeItemsFive = word_count[sum(timing_items$p < 0.05) + 1],
  timeItemsTen = word_count[sum(timing_items$p < 0.1) + 1],
  timePoorDiff = num(abs(ic("Timing", "slow", "blackpoor", "diff")), 2),
  wordEffect = num(abs(eff("Percent wording", "estimate")), 1),
  wordLo = num(eff("Percent wording", "lo"), 1), wordHi = num(eff("Percent wording", "hi"), 1),
  hundredBias = num(eff("Percent wording", "baseline"), 1),
  percentBias = num(eff("Percent wording", "treated"), 1),
  hundredLowerItems = word_count[sum(wording_items$diff > 0) + 1],
  wordImm = num(ic("Wording", "percent", "imm", "diff"), 1),
  wordLgb = num(abs(ic("Wording", "percent", "lgb", "diff")), 1),
  noseFast = pct(nose_all$mean_a, 1), noseSlow = pct(nose_all$mean_b, 1),
  noseDiff = pct(nose_all$diff, 1), noseP = num(nose_all$p, 2),
  absTimeEffect = num(abs(alt$estimate[alt$outcome == "abs_err" & alt$comparison == "Time requirement"]), 1),
  thermoPooled = num(abs(thermo_pooled$estimate), 2),
  thermoLo = num(thermo_pooled$lo, 2), thermoHi = num(thermo_pooled$hi, 2),
  thermoN = thermo_pooled$n,
  lgbBaseTrue = num(br("lgb", "base_truth"), 1), lgbBasePerceived = num(br("lgb", "base_perceived"), 1),
  richBaseTrue = num(br("rich", "base_truth"), 1), richBasePerceived = num(br("rich", "base_perceived"), 1),
  blackBaseTrue = num(br("black", "base_truth"), 1), blackBasePerceived = num(br("black", "base_perceived"), 1),
  gapSigCount = word_count[sum(base_rates$gap_p < 0.05) + 1],
  ddPosCount = word_count[sum(base_rates$dd_p < 0.05 & base_rates$dd > 0) + 1],
  ddNegCount = c("none", word_count[-1])[sum(base_rates$dd_p < 0.05 & base_rates$dd < 0) + 1],
  ddBlack = num(br("black", "dd"), 1),
  ddBlackLo = num(br("black", "dd_lo"), 1), ddBlackHi = num(br("black", "dd_hi"), 1),
  sodaNoWomen = num(soda_no$women, 1), sodaNoMen = num(soda_no$men, 1),
  sodaYesWomen = num(soda_yes$women, 1), sodaYesMen = num(soda_yes$men, 1),
  sodaYesBase = num(soda_yes$base, 1),
  sodaNoGap = num(soda_no$gap, 1), sodaYesGap = num(soda_yes$gap, 1),
  sodaDiff = num(soda_test$diff, 1), sodaLo = num(soda_test$lo, 1), sodaHi = num(soda_test$hi, 1),
  immTruth = num(truth$truth[truth$item == "imm"], 1),
  undocShare = num(undocumented, 1),
  poorTruth = num(truth$truth[truth$item == "blackpoor"], 1)
), table_path("macros.tex"))
