# Items, benchmarks and long-format data. Every analysis works on one row per
# respondent x item, with `y` the reported share and `err` = y - true share.

items <- tribble(
  ~item, ~party, ~label,
  "black", "Democrats", "Black",
  "aa", "Democrats", "Atheist or agnostic",
  "lgb", "Democrats", "Gay, lesbian, or bisexual",
  "union", "Democrats", "Union members",
  "rich", "Republicans", "Earn $250,000+",
  "evang", "Republicans", "Evangelical",
  "south", "Republicans", "Southern",
  "old", "Republicans", "Age 65+",
  "imm", "U.S. residents", "Foreign-born",
  "blackpoor", "Poor Americans", "Black"
)
party_items <- items$item[1:8]

read_data <- function(name) readr::read_csv(data_path(name), show_col_types = FALSE)

# Long answers in the paper's item order, with signed error against the truth.
add_error <- function(long, truth) {
  long |>
    inner_join(select(truth, item, truth), by = "item") |>
    mutate(err = y - truth) |>
    arrange(factor(item, items$item), respondent_id)
}

read_anes <- function() {
  read_data("anes_2012.csv") |>
    mutate(
      pid_x = if_else(pid_x %in% 1:7, pid_x, NA),
      party = case_when(pid_x <= 3 ~ "Democrats", pid_x >= 5 ~ "Republicans", pid_x == 4 ~ "Independents"),
      black = dem_racecps_black,
      union = as.numeric(dem_unionwho_r == 1),
      lgb = if_else(orientn_rgay %in% 1:3, as.numeric(orientn_rgay >= 2), NA),
      old = if_else(dem_agegrp_iwdate == -2, NA, as.numeric(dem_agegrp_iwdate >= 11)),
      rich = as.numeric(inc_incgroup_pre == 28)
    )
}

# Weighted ANES 2012 shares, coded as in Ahler and Sood (2018): party shares among
# identifiers and leaners, and base rates among all adults. Using the same survey and
# coding for both keeps the difference-in-differences free of definitional gaps.
anes_benchmarks <- function(anes = read_anes()) {
  long <- anes |>
    select(party, weight_full, black, union, lgb, south, old, rich) |>
    pivot_longer(-c(party, weight_full), names_to = "item", values_to = "member", values_drop_na = TRUE) |>
    inner_join(select(items, item, item_party = party), by = "item")
  long |>
    group_by(item) |>
    summarise(
      party_share = 100 * weighted.mean(member[party %in% item_party], weight_full[party %in% item_party]),
      base = 100 * weighted.mean(member, weight_full)
    )
}

truth_table <- function() {
  other <- read_data("benchmarks.csv") |>
    filter(item %in% items$item) |>
    pivot_wider(id_cols = item, names_from = quantity, values_from = value) |>
    transmute(item, party_share = coalesce(party, population), base)
  bind_rows(anes_benchmarks(), other) |>
    rename(truth = party_share) |>
    right_join(items, by = "item") |>
    arrange(factor(item, items$item))
}

# Study 1 (November 2014). Standard estimation, incentives and base-rate respondents
# answered all eight party items; sum-to-100 respondents answered one party's four.
read_study1 <- function() read_data("study1_2014.csv")

study1_conditions <- c(
  simpleask = "Standard estimation", complexask = "Sum to 100",
  incentive = "Incentives", baserates_given = "Base rates given"
)

study1_long <- function(s1, truth) {
  s1 |>
    select(respondent_id, condition, matches("^(dem|rep)_")) |>
    pivot_longer(matches("^(dem|rep)_"),
      names_to = "item", names_prefix = "(dem|rep)_", values_to = "y", values_drop_na = TRUE
    ) |>
    add_error(truth) |>
    mutate(condition = factor(condition, names(study1_conditions)))
}

# Study 2 (July 31, 2016).
read_study2 <- function() read_data("study2_2016.csv")

# Wording was randomized separately for three blocks within each respondent (the
# eight party items together, the foreign-born item, the poverty item), so a
# respondent can contribute answers under both wordings.
wording_long <- function(s2, truth) {
  s2 |>
    filter(study == "wording") |>
    select(respondent_id, matches("_(perc|100)$")) |>
    pivot_longer(-respondent_id,
      names_to = c("item", "wording"), names_pattern = "^(?:pcomp_(?:dem|rep)_)?(.+)_(perc|100)$",
      values_to = "y", values_drop_na = TRUE
    ) |>
    mutate(
      item = if_else(item == "black_poor", "blackpoor", item),
      wording = if_else(wording == "perc", "percent", "out_of_100")
    ) |>
    add_error(truth) |>
    mutate(percent = as.numeric(wording == "percent"))
}

# Each timing respondent saw four of the eight party items, drawn at random, plus the
# foreign-born and poverty items, under the screen version (fast or slow) matching
# their condition. The diet-soda items asked of this group are not analyzed.
timing_long <- function(s2, truth) {
  s2 |>
    filter(study == "timing") |>
    select(respondent_id, timing = timing_cond, matches("^timing_.+_(fast|slow)$")) |>
    pivot_longer(matches("^timing_"),
      names_to = "item", names_pattern = "^timing_(?:dem_|rep_)?(.+)_(?:fast|slow)$",
      values_to = "y", values_drop_na = TRUE
    ) |>
    add_error(truth) |>
    mutate(slow = as.numeric(timing == "slow")) |>
    relocate(item, .after = respondent_id)
}
