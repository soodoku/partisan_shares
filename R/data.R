# Items, benchmarks and long-format answers. Every analysis works on one row per
# respondent x item, with `perceived` the reported share and `error` = perceived - true share.

items <- tribble(
  ~item, ~party, ~label, ~short_label,
  "black", "Democrats", "Black", "Dem. black",
  "atheist", "Democrats", "Atheist or agnostic", "Dem. atheist/agnostic",
  "lgb", "Democrats", "Gay, lesbian, or bisexual", "Dem. LGB",
  "union", "Democrats", "Union members", "Dem. union",
  "rich", "Republicans", "Earn $250,000+", "Rep. $250K+",
  "evangelical", "Republicans", "Evangelical", "Rep. evangelical",
  "southern", "Republicans", "Southern", "Rep. Southern",
  "over_65", "Republicans", "Age 65+", "Rep. 65+",
  "foreign_born", "U.S. residents", "Foreign-born", "U.S. foreign-born",
  "black_poor", "Poor Americans", "Black", "Poor who are black"
)
party_items <- items$item[1:8]
item_order <- function(item) factor(item, items$item)

read_data <- function(name) readr::read_csv(data_path(name), show_col_types = FALSE)

# Long answers in the paper's item order, with signed error against the truth.
add_error <- function(answers, truth) {
  answers |>
    inner_join(select(truth, item, true_share), by = "item") |>
    mutate(error = perceived - true_share) |>
    arrange(item_order(item), respondent_id)
}

# Weighted ANES 2012 shares, coded as in Ahler and Sood (2018): party shares among
# identifiers and leaners, and base rates among all adults. Using the same survey and
# coding for both keeps the difference-in-differences free of definitional gaps.
anes_benchmarks <- function(anes = read_data("anes_2012.csv")) {
  anes |>
    pivot_longer(-c(weight, party), names_to = "item", values_to = "member", values_drop_na = TRUE) |>
    inner_join(select(items, item, item_party = party), by = "item") |>
    summarise(
      true_share = 100 * weighted.mean(member[party %in% item_party], weight[party %in% item_party]),
      true_base_rate = 100 * weighted.mean(member, weight),
      .by = item
    )
}

truth_table <- function() {
  published <- read_data("benchmarks.csv") |>
    filter(item %in% items$item) |>
    pivot_wider(id_cols = item, names_from = quantity, values_from = value) |>
    transmute(item, true_share = coalesce(party_share, population_share), true_base_rate = base_rate)
  bind_rows(anes_benchmarks(), published) |>
    right_join(items, by = "item") |>
    arrange(item_order(item))
}

# Study 1 (November 2014). Standard estimation, incentives and base-rate respondents
# answered all eight party items; sum-to-100 respondents answered one party's four.
read_study1 <- function() read_data("study1_2014.csv")

study1_conditions <- c(
  standard = "Standard estimation", sum_to_100 = "Sum to 100",
  incentive = "Incentives", base_rates_shown = "Base rates given"
)

study1_long <- function(study1, truth) {
  study1 |>
    select(respondent_id, condition, paste0("perceived_", party_items)) |>
    pivot_longer(starts_with("perceived_"),
      names_to = "item", names_prefix = "perceived_", values_to = "perceived", values_drop_na = TRUE
    ) |>
    add_error(truth) |>
    mutate(condition = factor(condition, names(study1_conditions)))
}

# Study 2 (July 31, 2016).
read_study2 <- function() read_data("study2_2016.csv")

# Wording was randomized separately for three blocks within each respondent (the
# eight party items together, the foreign-born item, the poverty item), so a
# respondent can contribute answers under both wordings.
wording_long <- function(study2, truth) {
  study2 |>
    filter(experiment == "wording") |>
    select(respondent_id, matches("^perceived_.+_(percent|out_of_100)$")) |>
    pivot_longer(-respondent_id,
      names_to = c("item", "wording"), names_pattern = "^perceived_(.+)_(percent|out_of_100)$",
      values_to = "perceived", values_drop_na = TRUE
    ) |>
    add_error(truth) |>
    mutate(percent = as.numeric(wording == "percent"))
}

# Each timing respondent saw four of the eight party items, drawn at random, plus the
# foreign-born and poverty items. The diet-soda items asked of this group are not analyzed.
timing_long <- function(study2, truth) {
  study2 |>
    filter(experiment == "timing") |>
    select(respondent_id, timing_condition, starts_with("timed_")) |>
    pivot_longer(starts_with("timed_"),
      names_to = "item", names_prefix = "timed_", values_to = "perceived", values_drop_na = TRUE
    ) |>
    add_error(truth) |>
    mutate(time_requirement = as.numeric(timing_condition == "time_requirement")) |>
    relocate(item, .after = respondent_id)
}
