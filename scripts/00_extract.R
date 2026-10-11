# Writes the de-identified CSVs in data/ from the authors' earlier public replication
# repositories. Not part of the build: the sources live outside this repository.
#
#   Rscript scripts/00_extract.R <partisan_heads> <partisan_typecast>
#
# <partisan_heads> is a clone of github.com/soodoku/partisan_heads (commit d586407), whose
# data/ holds the 2014 MTurk studies and ANES 2012 used in Ahler and Sood (2018, Journal
# of Politics). <partisan_typecast> is a clone of github.com/soodoku/partisan_typecast
# (commit cee347e), whose data/timing.dta is the complete July 2016 survey file, also
# used in Ahler and Sood (2023, Political Behavior). Column names are rewritten to the
# scheme in data/codebook.md.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(haven)
})

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2)
heads_dir <- args[1]
typecast_dir <- args[2]
write_data <- function(df, name) readr::write_csv(df, file.path("data", name), na = "")

# Item stems in the source files and the item names used here.
item_names <- c(
  dem_black = "black", dem_aa = "atheist", dem_lgb = "lgb", dem_union = "union",
  rep_rich = "rich", rep_evang = "evangelical", rep_south = "southern", rep_old = "over_65",
  imm = "foreign_born", blackpoor = "black_poor", black_poor = "black_poor",
  soda_br = "soda_all", soda_men = "soda_men", soda_women = "soda_women",
  lgb = "lgb", union = "union", evang = "evangelical", rich = "rich"
)

# Qualtrics page timers come as four adjacent columns; `at` is the first of them.
timer_block <- function(df, at, prefix) {
  df |>
    select(at + 0:3) |>
    set_names(paste0(prefix, c("_first_click", "_last_click", "_page_submit", "_click_count")))
}

# Study 1 (November 2014) --------------------------------------------------------------
# The alternative-explanations experiment. Location (Qualtrics latitude and longitude),
# state and free-text party and ideology fields are dropped.
study1 <- read_dta(file.path(heads_dir, "data", "alt_exps_data.dta")) |>
  zap_labels() |>
  filter(experiment == "simple_complex")

# Incentive respondents saw the Democratic and Republican pages in random order; each
# page's timer follows its four answers. Which block belongs to which page was checked by
# matching each timer's missingness to the answer columns.
page_timers <- function(dem_first_col, rep_first_col, prefix) {
  map2(
    timer_block(study1, match(dem_first_col, names(study1)), prefix),
    timer_block(study1, match(rep_first_col, names(study1)), prefix),
    coalesce
  ) |>
    as_tibble()
}

study1 |>
  transmute(
    respondent_id = row_number(),
    condition = recode(condition,
      simpleask = "standard", complexask = "sum_to_100", incentive = "incentive",
      baserates_given = "base_rates_shown"
    ),
    incentive_page_order = recode(na_if(incentive_order, ""), dems_first = "dem_first", reps_first = "rep_first"),
    # Perceived share of the party in each group, whatever the condition.
    perceived_black = dem_black, perceived_atheist = dem_aa, perceived_lgb = dem_lgb,
    perceived_union = dem_union, perceived_rich = rep_rich, perceived_evangelical = rep_evang,
    perceived_southern = rep_south, perceived_over_65 = rep_old,
    # Standard-estimation respondents' perceived share of all Americans in the group.
    perceived_base_rate_black = simple_baserate_black,
    perceived_base_rate_atheist = simple_baserate_atheistoragnosti,
    perceived_base_rate_lgb = simple_baserate_gaylesbianorbise,
    perceived_base_rate_union = simple_baserate_unionmembers,
    perceived_base_rate_rich = simple_baserate_earningover25000,
    perceived_base_rate_evangelical = simple_baserate_evangelicalchris,
    perceived_base_rate_southern = simple_baserate_southerners,
    perceived_base_rate_over_65 = simple_baserate_65yearsorolder,
    # Sum-to-100 respondents divided one party among the categories of four groupings.
    allocation_dem_race_white = complex_dem_race_whitenotincludi,
    allocation_dem_race_black = complex_dem_race_black,
    allocation_dem_race_latino = complex_dem_race_hispanicorlatin,
    allocation_dem_race_other = complex_dem_race_other,
    allocation_dem_union_member = complex_dem_union_areunionmember,
    allocation_dem_union_nonmember = complex_dem_union_donotbelongtoa,
    allocation_dem_orientation_straight = complex_dem_lgb_heterosexualorst,
    allocation_dem_orientation_lgb = complex_dem_lgb_gaylesbianorbise,
    allocation_dem_religion_protestant = complex_dem_religion_protestant,
    allocation_dem_religion_catholic = complex_dem_religion_catholic,
    allocation_dem_religion_atheist = complex_dem_religion_atheistorag,
    allocation_dem_religion_other = complex_dem_religion_otherreligi,
    allocation_rep_religion_evangelical = complex_rep_religion_evangelical,
    allocation_rep_religion_catholic = complex_rep_religion_catholic,
    allocation_rep_religion_mainline = complex_rep_religion_mainlinepro,
    allocation_rep_religion_other = complex_rep_religion_other,
    allocation_rep_region_southern = complex_rep_south_southerners,
    allocation_rep_region_nonsouthern = complex_rep_south_nonsoutherners,
    allocation_rep_income_under_50k = complex_rep_income_lessthan49999,
    allocation_rep_income_50k_100k = complex_rep_income_between50000a,
    allocation_rep_income_100k_250k = complex_rep_income_between100000,
    allocation_rep_income_over_250k = complex_rep_income_morethan25000,
    allocation_rep_age_18_39 = complex_rep_age_1839yearsold,
    allocation_rep_age_40_64 = complex_rep_age_4064yearsold,
    allocation_rep_age_65_plus = complex_rep_age_65orolder,
    party = recode(pid_3, D = "Democrat", I = "Independent", R = "Republican"),
    party_7 = pid_7,
    # The export carries no value labels. Codes are mapped as in the original sample
    # table, whose percentages they reproduce exactly (e.g. 176/382 = 46.1% some college);
    # the gender mapping rests on that table alone.
    gender = recode(gender, `1` = "Female", `2` = "Male"),
    education = recode(education,
      `1` = "Less than high school", `2` = "High school", `3` = "Some college",
      `4` = "4-year degree", `5` = "Advanced degree"
    ),
    age_group = recode(age, `1` = "18-39", `2` = "40-64", `3` = "65+"),
    race_white = !is.na(racewhitecaucasian), race_black = !is.na(raceblackafricanamerican),
    race_asian = !is.na(raceasian), race_native = !is.na(racenativeamericanoralaskanative),
    race_pacific = !is.na(racenativehawaiianorotherpacific), race_latino = !is.na(racehispaniclatino),
    # Unlabelled codes as exported. ideology_3_code 1 and 3 are liberal and conservative
    # (each is followed by its own strength question); the other codes are undocumented.
    ideology_3_code = ideo_3, ideology_liberal_strength_code = lib_branch,
    ideology_conservative_strength_code = doyouconsideryourselfveryconserv,
    ideology_lean_code = doyouleaninaliberalorconservativ,
    religion_code = howwouldyoudescribeyourreligious, political_attention_code = political_attnnbsp
  ) |>
  bind_cols(
    page_timers("timingfirstclick", "v142", "incentive_dem_page"),
    page_timers("v125", "v134", "incentive_rep_page")
  ) |>
  write_data("study1_2014.csv")

# April 2014 survey ----------------------------------------------------------------------
# Group feeling thermometers and out-party composition estimates. The file is stacked
# four times (one row per policy item); one row per respondent is kept. Its policy-
# extremity perceptions belong to Ahler and Sood (2018, JOP) and stay in partisan_heads.
read_dta(file.path(heads_dir, "data", "extremity_exp_data.dta")) |>
  zap_labels() |>
  filter(!duplicated(responseid)) |>
  transmute(
    respondent_id = row_number(), condition = tolower(na_if(condition, "")),
    party_3_code = pid_3, party_7 = pid_7, ideology_7 = ideo_7,
    thermometer_black = ft_black, thermometer_atheist = ft_aa, thermometer_lgb = ft_lgb,
    thermometer_union = ft_union, thermometer_rich = ft_rich, thermometer_evangelical = ft_evang,
    thermometer_southern = ft_south, thermometer_young = ft_young,
    thermometer_democrats = ft_dems, thermometer_republicans = ft_reps,
    # Perceived share of the out-party in each group: Democratic groups from Republicans
    # and leaners, Republican groups from Democrats and leaners.
    perceived_black = d_comp_per_black, perceived_atheist = d_comp_per_aa,
    perceived_lgb = d_comp_per_lgb, perceived_union = d_comp_per_union,
    perceived_rich = r_comp_per_rich, perceived_evangelical = r_comp_per_evang,
    perceived_southern = r_comp_per_south, perceived_young = r_comp_per_young,
    # Unlabelled codes as exported.
    gender_code = gender, education_code = education, age_code = age, latino_code = latino,
    political_attention_code = political_attn
  ) |>
  write_data("affect_2014.csv")

# Study 2 (July 31, 2016) ----------------------------------------------------------------
# All three experiments fielded in the survey: the timing and wording experiments
# analyzed here and the Bayesian-cues experiment reported in Ahler and Sood (2023).
# Dropped: MTurk worker IDs, Qualtrics response IDs, timestamps, browser data, free-text
# comments and the free-text "other party" answer.
study2 <- read_dta(file.path(typecast_dir, "data", "timing.dta")) |>
  zap_labels() |>
  mutate(across(where(is.character), \(x) na_if(x, "")))

# Each respondent saw one timing condition, so the fast and slow versions of an item
# combine into one column. Each answer has its own page timer: the four columns after the
# answer under time pressure, the four before it under the time requirement. For the
# party items, which respondents saw at random, each timer's missingness matches its
# answer's exactly (tested).
timed_item <- function(stem) {
  versions <- map(c("fast", "slow"), \(speed) {
    answer_col <- paste0("timing_", stem, "_", speed)
    at <- match(answer_col, names(study2)) + if (speed == "fast") 1 else -4
    stopifnot(grepl("firstclick|^v[0-9]+$", names(study2)[at]))
    timer_block(study2, at, paste0("timer_", item_names[[stem]])) |>
      mutate("timed_{item_names[[stem]]}" := study2[[answer_col]], .before = 1)
  })
  map2(versions[[1]], versions[[2]], coalesce) |> as_tibble()
}
timing_stems <- c(
  "dem_black", "dem_aa", "dem_union", "dem_lgb", "rep_old", "rep_evang", "rep_south", "rep_rich",
  "soda_br", "soda_women", "soda_men", "imm", "blackpoor"
)
timed <- map(timing_stems, timed_item) |> list_cbind()

# Wording experiment: "What percentage" (perc) and "Out of every 100" (100) versions.
wording_name <- function(col) {
  stem <- sub("^(pcomp_)?(.+)_(perc|100)$", "\\2", col)
  version <- if_else(endsWith(col, "_perc"), "percent", "out_of_100")
  paste0("perceived_", item_names[stem], "_", version)
}

# Bayesian-cues experiment, by group: the respondent's estimates of the group's share of
# Americans and of its party split, recall of the figure shown, and the outcome.
bayes_name <- function(col) {
  col |>
    sub(pattern = "^bayes_(lgb|union|evang|rich)_br$", replacement = "bayes_estimate_\\1_base_rate") |>
    sub(pattern = "^bayes_(lgb|union|evang|rich)_comp_(dem|rep|ind)$", replacement = "bayes_estimate_\\1_\\2_share") |>
    sub(pattern = "^bayes_(lgb|union|evang|rich)_br_requiz$", replacement = "bayes_recall_\\1_base_rate") |>
    sub(pattern = "^bayes_(lgb|union|evang|rich)_(dem|rep)_requiz$", replacement = "bayes_recall_\\1_party_share") |>
    sub(pattern = "^bayes_(dem|rep)_(lgb|union|evang|rich)_dv$", replacement = "bayes_perceived_\\2") |>
    sub(pattern = "_evang_", replacement = "_evangelical_") |>
    sub(pattern = "_evang$", replacement = "_evangelical")
}

knowledge_topics <- c("gm", "blm", "first_amendment", "congress", "eu")

study2 |>
  transmute(
    respondent_id = row_number(),
    experiment = recode(study, main_measurement = "wording"),
    timing_condition = recode(timing_cond, fast = "time_pressure", slow = "time_requirement"),
    soda_condition = recode(soda_cond, br = "base_rate_first", no_br = "no_base_rate"),
    bayes_condition = recode(bayes_cond, group_comp = "group_composition"),
    bayes_party = recode(party, dem = "democrats", rep = "republicans"),
    wording_block_order = order,
    duration_seconds = durationinseconds,
    across(matches("^(pcomp_.+|imm|black_poor)_(perc|100)$"), .names = "{wording_name(.col)}"),
    soda_all = soda_br, soda_men, soda_women,
    across(matches("^bayes_.+_(br|comp_dem|comp_rep|comp_ind|requiz|dv)$"), .names = "{bayes_name(.col)}"),
    numeracy_1 = num1, numeracy_2 = num2, numeracy_3 = num3, numeracy_4 = num4,
    numeracy_1_correct = num1_01, numeracy_2_correct = num2_01, numeracy_3_correct = num3_01,
    numeracy_4_correct = num4_01, numeracy_index = num_index,
    knowledge_gm = pk1pk_gm, knowledge_blm = pk2pk_blm, knowledge_first_amendment = pk3pk_firstamd,
    knowledge_congress = pk4pk_congress, knowledge_eu = pk5pk_eu,
    # The knowledge battery was asked only in the Bayesian-cues experiment; the original
    # scoring gave everyone else 0.
    across(c(pk1, pk2, pk3, pk4, pk5, pk_index), \(x) if_else(study == "bayes", x, NA)),
    party_3 = pid3, party_7 = pid7, party_initial = pid3pid, party_strength_dem = strdempid_dem,
    party_strength_rep = strreppid_rep, party_lean = pidleanpid_ind,
    gender = sexgender, race,
    # One respondent typed a birth year (1986) instead of an age; fielded in 2016.
    age = if_else(age > 120, 2016 - age, age),
    state, religion = religionrelig, political_interest = polinterestpol_int
  ) |>
  rename_with(
    \(n) paste0("knowledge_", knowledge_topics[as.integer(sub("pk", "", n))], "_correct"),
    matches("^pk[1-5]$")
  ) |>
  # The source stores the knowledge index in single precision; it is the mean of the items.
  mutate(knowledge_index = rowMeans(pick(matches("^knowledge_.+_correct$")))) |>
  select(-pk_index) |>
  bind_cols(timed) |>
  write_data("study2_2016.csv")

# ANES 2012 --------------------------------------------------------------------------------
# The columns behind the party benchmarks and the population base rates, with the group
# indicators coded as in Ahler and Sood (2018, JOP).
read_dta(file.path(heads_dir, "data", "anes_timeseries_2012.dta"),
  col_select = c(
    weight_full, pid_x, dem_racecps_black, dem_unionwho_r, orientn_rgay, sample_state,
    dem_agegrp_iwdate, inc_incgroup_pre
  )
) |>
  zap_labels() |>
  transmute(
    weight = weight_full,
    party = case_when(pid_x %in% 1:3 ~ "Democrats", pid_x %in% 5:7 ~ "Republicans", pid_x == 4 ~ "Independents"),
    black = dem_racecps_black,
    union = as.numeric(dem_unionwho_r == 1),
    lgb = if_else(orientn_rgay %in% 1:3, as.numeric(orientn_rgay >= 2), NA),
    # States most respondents in a FiveThirtyEight/SurveyMonkey poll call Southern.
    southern = as.numeric(sample_state %in% c(
      "TX", "AR", "MS", "AL", "LA", "FL", "GA", "SC", "NC", "TN", "VA", "KY", "DC"
    )),
    over_65 = if_else(dem_agegrp_iwdate == -2, NA, as.numeric(dem_agegrp_iwdate >= 11)),
    rich = as.numeric(inc_incgroup_pre == 28)
  ) |>
  write_data("anes_2012.csv")
