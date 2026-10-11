source("scripts/00_config.R")

read_result <- function(name) readr::read_csv(table_path(paste0(name, ".csv")), show_col_types = FALSE)

truth <- read_result("benchmarks")
undocumented_share <- read_data("benchmarks.csv") |>
  filter(item == "undocumented") |>
  pull(value)
effects <- read_result("effects")
alt_outcomes <- read_result("effects_alt_outcomes")
contrasts <- read_result("item_contrasts")
ks <- read_result("ks_tests")
on_the_nose <- read_result("on_the_nose")
base_rates <- read_result("base_rates")
thermometer <- read_result("thermometer")
thermometer_pooled <- read_result("thermometer_pooled")
soda_means <- read_result("soda_means")
soda_test <- read_result("soda_test")
samples <- read_result("samples")
compliance <- read_result("timing_compliance")
timing_answers <- read_data("answers_timing.csv")

short_label <- deframe(select(items, item, short_label))
p_stars <- function(p) case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.1 ~ "*", .default = "")

# Figure 1: pooled effects of each elicitation change on signed error.
effects_plot <- effects |>
  mutate(
    label = c(
      "Sum to 100 (vs. standard)", "Accuracy incentives (vs. standard)", "Base rates given (vs. standard)",
      "Time requirement (vs. time pressure)", "\"What percentage\" (vs. \"Out of every 100\")"
    ),
    label = factor(label, rev(label))
  ) |>
  ggplot(aes(x = estimate, y = label, xmin = conf_low, xmax = conf_high)) +
  geom_vline(xintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_errorbar(orientation = "y", width = 0, linewidth = 0.5, colour = colours[["ink"]]) +
  geom_point(size = 2, colour = colours[["ink"]]) +
  facet_grid(study ~ ., scales = "free_y", space = "free_y") +
  labs(x = "Change in signed error (percentage points), 95% CI", y = NULL) +
  theme_paper() +
  theme(strip.text.y = element_text(angle = 0, face = "bold"))
save_figure(effects_plot, "effects", 6.5, 3)

# Item-level means by arm with 95% intervals and the true share.
item_plot <- function(item_contrasts, panel_labels) {
  item_contrasts <- item_contrasts |>
    mutate(
      comparison = factor(panel_labels[treatment], unname(panel_labels)),
      item_name = factor(short_label[item], rev(short_label))
    )
  arm_means <- item_contrasts |>
    select(comparison, item, item_name, control_mean, control_se, treatment_mean, treatment_se) |>
    pivot_longer(
      c(control_mean, control_se, treatment_mean, treatment_se),
      names_to = c("arm", ".value"), names_sep = "_"
    ) |>
    mutate(
      arm = if_else(arm == "control", "Baseline", "Treatment"),
      conf_low = mean - 1.96 * se, conf_high = mean + 1.96 * se
    )
  true_shares <- item_contrasts |>
    distinct(comparison, item, item_name) |>
    left_join(select(truth, item, true_share), by = "item")
  ggplot(arm_means, aes(x = mean, y = item_name, colour = arm, shape = arm)) +
    geom_point(
      data = true_shares, aes(x = true_share, y = item_name),
      inherit.aes = FALSE, shape = 124, size = 3, colour = "grey30"
    ) +
    geom_errorbar(aes(xmin = conf_low, xmax = conf_high),
      orientation = "y", width = 0, linewidth = 0.4, position = position_dodge(width = 0.6)
    ) +
    geom_point(size = 1.8, position = position_dodge(width = 0.6)) +
    scale_colour_manual(values = c(Baseline = colours[["a"]], Treatment = colours[["b"]])) +
    scale_shape_manual(values = c(Baseline = 16, Treatment = 17)) +
    facet_wrap(~comparison, nrow = 1) +
    labs(x = "Mean reported share (%); | marks the true share", y = NULL) +
    theme_paper()
}
study1_items_plot <- item_plot(
  filter(contrasts, experiment == "Study 1"),
  c(sum_to_100 = "Sum to 100", incentive = "Incentives", base_rates_shown = "Base rates given")
) + labs(caption = "Baseline: standard estimation")
save_figure(study1_items_plot, "items_study1", 7, 3.4)
study2_items_plot <- item_plot(
  filter(contrasts, experiment != "Study 1"),
  c(
    time_requirement = "Time requirement (baseline: time pressure)",
    percent = "\"What percentage\" (baseline: \"Out of every 100\")"
  )
)
save_figure(study2_items_plot, "items_study2", 7, 3.8)

# Timing experiment: distribution of answers relative to the truth. Looking answers
# up would show as a spike at zero in the time-requirement condition.
timing_density_plot <- timing_answers |>
  mutate(
    condition = if_else(timing_condition == "time_requirement", "Time requirement", "Time pressure"),
    item_name = factor(short_label[item], short_label)
  ) |>
  ggplot(aes(x = error, colour = condition, linetype = condition)) +
  geom_vline(xintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_density(linewidth = 0.5, bw = 3) +
  scale_colour_manual(values = c("Time pressure" = colours[["a"]], "Time requirement" = colours[["b"]])) +
  facet_wrap(~item_name, ncol = 5) +
  labs(x = "Reported minus true share (percentage points)", y = "Density") +
  theme_paper()
save_figure(timing_density_plot, "timing_density", 7, 3.6)

# Tables.
samples |>
  filter(variable != "N") |>
  pivot_wider(names_from = study, values_from = value) |>
  transmute(
    ` ` = level,
    `Study 1 (2014)` = num(`Study 1`, 1),
    `Study 2 (2016)` = if_else(is.na(`Study 2`), "--", num(`Study 2`, 1))
  ) |>
  write_table(
    table_path("samples.tex"), "lrr",
    groups = c(Party = 3, Gender = 2, "Race and ethnicity" = 4, Education = 5, Age = 3)
  )

base_rates |>
  transmute(
    Group = short_label[item], n = n,
    `Base, true` = num(true_base_rate, 1), `Base, perceived` = num(perceived_base_rate, 1),
    `Party, true` = num(true_share, 1), `Party, perceived` = num(perceived_share, 1),
    `Party - base` = paste0(num(gap, 1), p_stars(gap_p_value)),
    DiD = paste0(num(did, 1), p_stars(did_p_value)),
    `DiD 95% CI` = interval(did_conf_low, did_conf_high, digits = 1)
  ) |>
  write_table(table_path("base_rates.tex"), "lrrrrrrrr")

ks |>
  mutate(cell = paste0(num(statistic, 2), " (", num(p_adjusted, 2), ")")) |>
  select(condition, item, cell) |>
  pivot_wider(names_from = condition, values_from = cell) |>
  mutate(Group = short_label[item], .before = 1) |>
  select(-item) |>
  write_table(table_path("ks.tex"), "llll")

thermometer |>
  mutate(item = factor(item, c("black", "atheist", "lgb", "union", "rich", "evangelical", "southern", "young"))) |>
  arrange(item) |>
  transmute(
    `Party and group` = coalesce(short_label[as.character(item)], "Rep. under 35"),
    Coefficient = num(estimate, 2), SE = num(std_error, 2), `95% CI` = interval(conf_low, conf_high), n = n
  ) |>
  write_table(table_path("thermometer.tex"), "lrrrr")

on_the_nose |>
  arrange(match(item, c(items$item, "all"))) |>
  transmute(
    Item = if_else(item == "all", "All items (item fixed effects)", short_label[item]),
    `Time pressure` = pct(control_mean, 1), `Time requirement` = pct(treatment_mean, 1),
    Difference = pct(difference, 1), `95% CI` = interval(conf_low, conf_high, scale = 100, digits = 1),
    p = num(p_value, 2)
  ) |>
  write_table(table_path("on_the_nose.tex"), "lrrrrr")

outcome_names <- c(error = "Signed error", abs_error = "Absolute error", pct_error = "Percentage bias")
alt_outcomes |>
  transmute(
    Outcome = outcome_names[outcome], Comparison = comparison,
    Baseline = num(control_mean, 1), Effect = paste0(num(estimate, 1), p_stars(p_value)), SE = num(std_error, 1),
    Answers = n_answers, Respondents = n_respondents
  ) |>
  write_table(table_path("alt_outcomes.tex"), "llrrrrr")

truth |>
  transmute(
    Item = short_label, `True share` = num(true_share, 1),
    `Population base rate` = if_else(is.na(true_base_rate), "--", num(true_base_rate, 1))
  ) |>
  write_table(table_path("benchmarks.tex"), "lrr")

# Manuscript numbers. `the()` pulls one cell and fails loudly if a lookup matches
# zero or several rows.
the <- function(df, col, ...) {
  value <- df |>
    filter(...) |>
    pull({{ col }})
  stopifnot(length(value) == 1)
  value
}
effect <- function(comparison_label, col) the(effects, {{ col }}, comparison == comparison_label)
contrast <- function(experiment_name, treatment_arm, item_name, col) {
  the(contrasts, {{ col }}, experiment == experiment_name, treatment == treatment_arm, item == item_name)
}
base_rate <- function(item_name, col) the(base_rates, {{ col }}, item == item_name)
timing_items <- filter(contrasts, experiment == "Timing")
wording_items <- filter(contrasts, experiment == "Wording")
base_rate_items <- filter(contrasts, treatment == "base_rates_shown")
number_words <- c("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
in_words <- function(n) number_words[n + 1]
soda_not_asked <- filter(soda_means, !asked_base_rate)
soda_asked <- filter(soda_means, asked_base_rate)
nose_all <- filter(on_the_nose, item == "all")
sample_sizes <- samples |>
  filter(variable == "N") |>
  pull(value)
true_share <- deframe(select(truth, item, true_share))
compliance_of <- function(condition, col) the(compliance, {{ col }}, timing_condition == condition)

write_macros(list(
  nStudyOne = sample_sizes[1], nStudyTwo = sample_sizes[2],
  nTiming = effect("Time requirement", n_respondents), nWording = effect("Percent wording", n_respondents),
  sumEffect = num(effect("Sum to 100", estimate), 1),
  standardBias = num(effect("Sum to 100", control_mean), 1),
  sumBias = num(effect("Sum to 100", treatment_mean), 1),
  sumBlackAbs = num(abs(contrast("Study 1", "sum_to_100", "black", difference)), 1),
  sumSouth = num(contrast("Study 1", "sum_to_100", "southern", difference), 1),
  sumSouthMean = num(contrast("Study 1", "sum_to_100", "southern", treatment_mean), 1),
  standardSouthMean = num(contrast("Study 1", "sum_to_100", "southern", control_mean), 1),
  incEffect = num(effect("Incentives", estimate), 1),
  incLo = num(effect("Incentives", conf_low), 1), incHi = num(effect("Incentives", conf_high), 1),
  brEffect = num(effect("Base rates given", estimate), 1),
  brLo = num(effect("Base rates given", conf_low), 1), brHi = num(effect("Base rates given", conf_high), 1),
  brItemsUp = c(paste(number_words[1:8], "of the eight"), "all eight")[sum(base_rate_items$difference > 0) + 1],
  timeEffect = num(abs(effect("Time requirement", estimate)), 1),
  timeLo = num(abs(effect("Time requirement", conf_high)), 1),
  timeHi = num(abs(effect("Time requirement", conf_low)), 1),
  fastBias = num(effect("Time requirement", control_mean), 1),
  slowBias = num(effect("Time requirement", treatment_mean), 1),
  timeDecline = pct(abs(effect("Time requirement", estimate)) / effect("Time requirement", control_mean)),
  timeItemsLower = in_words(sum(timing_items$difference < 0)),
  timeItemsFive = in_words(sum(timing_items$p_value < 0.05)),
  timeItemsTen = in_words(sum(timing_items$p_value < 0.1)),
  timePoorDiff = num(abs(contrast("Timing", "time_requirement", "black_poor", difference)), 2),
  wordEffect = num(abs(effect("Percent wording", estimate)), 1),
  wordLo = num(effect("Percent wording", conf_low), 1), wordHi = num(effect("Percent wording", conf_high), 1),
  hundredBias = num(effect("Percent wording", control_mean), 1),
  percentBias = num(effect("Percent wording", treatment_mean), 1),
  hundredLowerItems = in_words(sum(wording_items$difference > 0)),
  wordImm = num(contrast("Wording", "percent", "foreign_born", difference), 1),
  wordLgb = num(abs(contrast("Wording", "percent", "lgb", difference)), 1),
  slowPages = format(compliance_of("time_requirement", pages), big.mark = ","),
  slowUnder = compliance_of("time_requirement", under_15),
  slowMedian = num(compliance_of("time_requirement", median_seconds), 1),
  fastPages = format(compliance_of("time_pressure", pages), big.mark = ","),
  fastOver = compliance_of("time_pressure", over_11),
  fastMedian = num(compliance_of("time_pressure", median_seconds), 1),
  noseFast = pct(nose_all$control_mean, 1), noseSlow = pct(nose_all$treatment_mean, 1),
  noseDiff = pct(nose_all$difference, 1), noseP = num(nose_all$p_value, 2),
  absTimeEffect = num(abs(the(alt_outcomes, estimate, outcome == "abs_error", comparison == "Time requirement")), 1),
  thermoPooled = num(abs(thermometer_pooled$estimate), 2),
  thermoLo = num(thermometer_pooled$conf_low, 2), thermoHi = num(thermometer_pooled$conf_high, 2),
  thermoN = thermometer_pooled$n,
  lgbBaseTrue = num(base_rate("lgb", true_base_rate), 1),
  lgbBasePerceived = num(base_rate("lgb", perceived_base_rate), 1),
  richBaseTrue = num(base_rate("rich", true_base_rate), 1),
  richBasePerceived = num(base_rate("rich", perceived_base_rate), 1),
  blackBaseTrue = num(base_rate("black", true_base_rate), 1),
  blackBasePerceived = num(base_rate("black", perceived_base_rate), 1),
  gapSigCount = in_words(sum(base_rates$gap_p_value < 0.05)),
  ddPosCount = in_words(sum(base_rates$did_p_value < 0.05 & base_rates$did > 0)),
  ddNegCount = c("none", number_words[-1])[sum(base_rates$did_p_value < 0.05 & base_rates$did < 0) + 1],
  ddBlack = num(base_rate("black", did), 1),
  ddBlackLo = num(base_rate("black", did_conf_low), 1), ddBlackHi = num(base_rate("black", did_conf_high), 1),
  sodaNoWomen = num(soda_not_asked$women, 1), sodaNoMen = num(soda_not_asked$men, 1),
  sodaYesWomen = num(soda_asked$women, 1), sodaYesMen = num(soda_asked$men, 1),
  sodaYesBase = num(soda_asked$all, 1),
  sodaNoGap = num(soda_not_asked$gap, 1), sodaYesGap = num(soda_asked$gap, 1),
  sodaDiff = num(soda_test$difference, 1), sodaLo = num(soda_test$conf_low, 1), sodaHi = num(soda_test$conf_high, 1),
  immTruth = num(true_share[["foreign_born"]], 1),
  undocShare = num(undocumented_share, 1),
  poorTruth = num(true_share[["black_poor"]], 1)
), table_path("macros.tex"))
