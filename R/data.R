# Items, benchmarks and long-format data. Every analysis works on one row per
# respondent x item, with `y` the reported share and `err` = y - true share.

items <- tibble::tribble(
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

read_anes <- function() {
  readr::read_csv(data_path("anes_2012.csv"), show_col_types = FALSE) |>
    mutate(
      pid_x = if_else(pid_x %in% 1:7, pid_x, NA_real_),
      party = case_when(pid_x <= 3 ~ "Democrats", pid_x >= 5 ~ "Republicans", pid_x == 4 ~ "Independents"),
      black = dem_racecps_black,
      union = as.numeric(dem_unionwho_r == 1),
      lgb = if_else(orientn_rgay %in% 1:3, as.numeric(orientn_rgay >= 2), NA_real_),
      old = if_else(dem_agegrp_iwdate == -2, NA_real_, as.numeric(dem_agegrp_iwdate >= 11)),
      rich = as.numeric(inc_incgroup_pre == 28)
    )
}

# Weighted ANES 2012 shares, coded as in Ahler and Sood (2018): party shares among
# identifiers and leaners, and base rates among all adults. Using the same survey and
# coding for both keeps the difference-in-differences free of definitional gaps.
anes_benchmarks <- function(anes = read_anes()) {
  share <- function(v, keep) {
    ok <- keep & !is.na(anes[[v]])
    100 * stats::weighted.mean(anes[[v]][ok], anes$weight_full[ok])
  }
  items |>
    filter(item %in% c("black", "union", "lgb", "south", "old", "rich")) |>
    rowwise() |>
    mutate(
      party_share = share(item, anes$party %in% party),
      base = share(item, rep(TRUE, nrow(anes)))
    ) |>
    ungroup() |>
    select(item, party_share, base)
}

truth_table <- function() {
  b <- readr::read_csv(data_path("benchmarks.csv"), show_col_types = FALSE)
  pick <- function(i, q) b$value[b$item == i & b$quantity == q]
  bind_rows(
    anes_benchmarks(),
    tibble(
      item = c("aa", "evang"), party_share = c(pick("aa", "party"), pick("evang", "party")),
      base = c(pick("aa", "base"), pick("evang", "base"))
    ),
    tibble(
      item = c("imm", "blackpoor"), party_share = c(pick("imm", "population"), pick("blackpoor", "population")),
      base = NA_real_
    )
  ) |>
    rename(truth = party_share) |>
    right_join(items, by = "item")
}

# Study 1 (November 2014). Standard estimation, incentives and base-rate respondents
# answered all eight party items; sum-to-100 respondents answered one party's four.
read_study1 <- function() readr::read_csv(data_path("study1_2014.csv"), show_col_types = FALSE)

study1_conditions <- c(
  simpleask = "Standard estimation", complexask = "Sum to 100",
  incentive = "Incentives", baserates_given = "Base rates given"
)

study1_long <- function(s1, truth) {
  col <- setNames(paste0(ifelse(items$party[1:8] == "Democrats", "dem_", "rep_"), party_items), party_items)
  bind_rows(lapply(party_items, function(k) {
    tibble(respondent_id = s1$respondent_id, condition = s1$condition, item = k, y = s1[[col[[k]]]])
  })) |>
    filter(!is.na(y)) |>
    left_join(select(truth, item, truth), by = "item") |>
    mutate(err = y - truth, condition = factor(condition, names(study1_conditions)))
}

# Study 2 (July 31, 2016).
read_study2 <- function() readr::read_csv(data_path("study2_2016.csv"), show_col_types = FALSE)

wording_stem <- c(
  black = "pcomp_dem_black", aa = "pcomp_dem_aa", lgb = "pcomp_dem_lgb", union = "pcomp_dem_union",
  rich = "pcomp_rep_rich", evang = "pcomp_rep_evang", south = "pcomp_rep_south", old = "pcomp_rep_old",
  imm = "imm", blackpoor = "black_poor"
)

# Wording was randomized separately for three blocks within each respondent (the
# eight party items together, the foreign-born item, the poverty item), so a
# respondent can contribute answers under both wordings.
wording_long <- function(s2, truth) {
  w <- filter(s2, study == "wording")
  bind_rows(lapply(names(wording_stem), function(k) {
    arm <- function(wording, suffix) {
      tibble(respondent_id = w$respondent_id, item = k, wording = wording, y = w[[paste0(wording_stem[[k]], suffix)]])
    }
    bind_rows(arm("percent", "_perc"), arm("out_of_100", "_100"))
  })) |>
    filter(!is.na(y)) |>
    left_join(select(truth, item, truth), by = "item") |>
    mutate(err = y - truth, percent = as.numeric(wording == "percent"))
}

timing_stem <- c(
  black = "dem_black", aa = "dem_aa", lgb = "dem_lgb", union = "dem_union",
  rich = "rep_rich", evang = "rep_evang", south = "rep_south", old = "rep_old",
  imm = "imm", blackpoor = "blackpoor"
)

# Each timing respondent saw four of the eight party items, drawn at random, plus the
# foreign-born and poverty items.
timing_long <- function(s2, truth) {
  t <- filter(s2, study == "timing")
  bind_rows(lapply(names(timing_stem), function(k) {
    tibble(
      respondent_id = t$respondent_id, item = k, timing = t$timing_cond,
      y = dplyr::coalesce(
        t[[paste0("timing_", timing_stem[[k]], "_fast")]], t[[paste0("timing_", timing_stem[[k]], "_slow")]]
      )
    )
  })) |>
    filter(!is.na(y)) |>
    left_join(select(truth, item, truth), by = "item") |>
    mutate(err = y - truth, slow = as.numeric(timing == "slow"))
}
