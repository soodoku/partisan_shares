# Writes the de-identified CSVs in data/ from the authors' earlier public replication
# repositories. Not part of the build: the sources live outside this repository.
#
#   Rscript scripts/00_extract.R <partisan_heads> <partisan_typecast>
#
# <partisan_heads> is a clone of github.com/soodoku/partisan_heads (commit d586407), whose
# data/ holds the 2014 MTurk studies and ANES 2012 used in Ahler and Sood (2018, Journal
# of Politics). <partisan_typecast> is a clone of github.com/soodoku/partisan_typecast
# (commit cee347e), whose data/timing.dta is the complete July 2016 survey file, also
# used in Ahler and Sood (2022, Political Behavior).

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(haven)
})

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2)
heads <- args[1]
typecast <- args[2]
out <- function(name) file.path("data", name)
write_data <- function(df, name) readr::write_csv(df, out(name), na = "")

# Qualtrics page timers come as four adjacent columns; `at` is the first of them.
timer_block <- function(df, at, prefix) {
  df |>
    select(at + 0:3) |>
    set_names(paste0(prefix, c("_first_click", "_last_click", "_page_submit", "_click_count")))
}

# Study 1 (November 2014): the alternative-explanations experiment. Location (Qualtrics
# latitude and longitude), state and free-text party and ideology fields are dropped.
s1 <- read_dta(file.path(heads, "data", "alt_exps_data.dta")) |>
  zap_labels() |>
  filter(experiment == "simple_complex")
s1_names <- names(s1)

# Incentive respondents saw the Democratic and Republican pages in random order; each
# page's timer follows its four answers. Which block belongs to which page was checked by
# matching each timer's missingness to the answer columns.
page_timers <- function(dems_first, reps_first, prefix) {
  map2(
    timer_block(s1, match(dems_first, s1_names), prefix),
    timer_block(s1, match(reps_first, s1_names), prefix),
    coalesce
  ) |>
    as_tibble()
}
incentive_dem <- page_timers("timingfirstclick", "v142", "incentive_dem_page")
incentive_rep <- page_timers("v125", "v134", "incentive_rep_page")

s1 |>
  transmute(
    respondent_id = row_number(), condition, incentive_order = na_if(incentive_order, ""),
    dem_black, dem_aa, dem_lgb, dem_union, rep_rich, rep_evang, rep_south, rep_old,
    br_black = simple_baserate_black, br_aa = simple_baserate_atheistoragnosti,
    br_lgb = simple_baserate_gaylesbianorbise, br_union = simple_baserate_unionmembers,
    br_rich = simple_baserate_earningover25000, br_evang = simple_baserate_evangelicalchris,
    br_south = simple_baserate_southerners, br_old = simple_baserate_65yearsorolder,
    # Sum-to-100 respondents divided one party into the categories of four groupings.
    sum100_dem_race_white = complex_dem_race_whitenotincludi, sum100_dem_race_black = complex_dem_race_black,
    sum100_dem_race_latino = complex_dem_race_hispanicorlatin, sum100_dem_race_other = complex_dem_race_other,
    sum100_dem_union_member = complex_dem_union_areunionmember,
    sum100_dem_union_nonmember = complex_dem_union_donotbelongtoa,
    sum100_dem_lgb_straight = complex_dem_lgb_heterosexualorst, sum100_dem_lgb_lgb = complex_dem_lgb_gaylesbianorbise,
    sum100_dem_religion_protestant = complex_dem_religion_protestant,
    sum100_dem_religion_catholic = complex_dem_religion_catholic,
    sum100_dem_religion_atheist_agnostic = complex_dem_religion_atheistorag,
    sum100_dem_religion_other = complex_dem_religion_otherreligi,
    sum100_rep_religion_evangelical = complex_rep_religion_evangelical,
    sum100_rep_religion_catholic = complex_rep_religion_catholic,
    sum100_rep_religion_mainline = complex_rep_religion_mainlinepro,
    sum100_rep_religion_other = complex_rep_religion_other,
    sum100_rep_region_south = complex_rep_south_southerners,
    sum100_rep_region_nonsouth = complex_rep_south_nonsoutherners,
    sum100_rep_income_under_50k = complex_rep_income_lessthan49999,
    sum100_rep_income_50k_100k = complex_rep_income_between50000a,
    sum100_rep_income_100k_250k = complex_rep_income_between100000,
    sum100_rep_income_over_250k = complex_rep_income_morethan25000,
    sum100_rep_age_18_39 = complex_rep_age_1839yearsold, sum100_rep_age_40_64 = complex_rep_age_4064yearsold,
    sum100_rep_age_65_plus = complex_rep_age_65orolder,
    party = recode(pid_3, D = "Democrat", I = "Independent", R = "Republican"),
    pid_7,
    # The export carries no value labels. Codes are mapped as in the original sample
    # table, whose percentages they reproduce exactly (e.g. 176/382 = 46.1% some college);
    # the gender mapping rests on that table alone.
    gender = recode(gender, `1` = "Female", `2` = "Male"),
    education = recode(education,
      `1` = "Less than high school", `2` = "High school", `3` = "Some college",
      `4` = "4-year degree", `5` = "Advanced degree"
    ),
    age = recode(age, `1` = "18-39", `2` = "40-64", `3` = "65+"),
    race_white = racewhitecaucasian, race_black = raceblackafricanamerican, race_asian = raceasian,
    race_native = racenativeamericanoralaskanative, race_pacific = racenativehawaiianorotherpacific,
    race_latino = racehispaniclatino,
    # Unlabelled codes as exported. ideo_3 = 1 and 3 are liberal and conservative (each
    # is followed by its own strength question); the other codes are not documented.
    ideo_3, ideo_liberal_strength = lib_branch, ideo_conservative_strength = doyouconsideryourselfveryconserv,
    ideo_lean = doyouleaninaliberalorconservativ,
    religion_code = howwouldyoudescribeyourreligious, political_attention_code = political_attnnbsp
  ) |>
  bind_cols(incentive_dem, incentive_rep) |>
  write_data("study1_2014.csv")

# April 2014 survey with group feeling thermometers. The file is stacked four times
# (one row per policy item); one row per respondent is kept. Its policy-extremity
# perceptions belong to Ahler and Sood (2018, JOP) and are left in partisan_heads.
read_dta(file.path(heads, "data", "extremity_exp_data.dta")) |>
  zap_labels() |>
  filter(!duplicated(responseid)) |>
  transmute(
    respondent_id = row_number(), condition = na_if(condition, ""), pid_3, pid_7,
    ft_black, ft_aa, ft_lgb, ft_union, ft_rich, ft_evang, ft_south, ft_young, ft_dems, ft_reps,
    d_black = d_comp_per_black, d_aa = d_comp_per_aa, d_lgb = d_comp_per_lgb, d_union = d_comp_per_union,
    r_rich = r_comp_per_rich, r_evang = r_comp_per_evang, r_south = r_comp_per_south, r_young = r_comp_per_young,
    # Unlabelled codes as exported.
    gender_code = gender, education_code = education, age_code = age, latino_code = latino,
    ideo_7, political_attention_code = political_attn
  ) |>
  write_data("affect_2014.csv")

# Study 2 (July 31, 2016): all three experiments fielded in the survey, the timing and
# wording experiments analyzed here and the Bayesian-cues experiment reported in Ahler
# and Sood (2022). Dropped: MTurk worker IDs, Qualtrics response IDs, timestamps, browser
# data, free-text comments and the free-text "other party" answer.
s2 <- read_dta(file.path(typecast, "data", "timing.dta")) |>
  zap_labels() |>
  mutate(across(where(is.character), \(x) na_if(x, "")))
s2_names <- names(s2)

# In the timing experiment each answer has its own page timer: the four columns after
# the answer under time pressure, the four before it under the time requirement. For the
# party items, which respondents saw at random, each timer's missingness matches its
# answer's exactly.
timing_answers <- grep("^timing_.+_(fast|slow)$", s2_names, value = TRUE)
timers <- map(timing_answers, \(a) {
  at <- match(a, s2_names) + if (endsWith(a, "_fast")) 1 else -4
  stopifnot(grepl("firstclick|^v[0-9]+$", s2_names[at]))
  timer_block(s2, at, sub("^timing_", "timer_", a))
}) |>
  list_cbind()

answers <- grep("^(pcomp_.*_(perc|100)|imm_(perc|100)|black_poor_(perc|100))$", s2_names, value = TRUE)
s2 |>
  transmute(
    respondent_id = row_number(),
    study = recode(study, main_measurement = "wording"),
    timing_cond, soda_cond, bayes_cond, bayes_party = party, block_order = order,
    duration_seconds = durationinseconds,
    across(all_of(answers)), across(all_of(timing_answers)),
    soda_br, soda_men, soda_women,
    across(matches("^bayes_.+_(br|comp_dem|comp_rep|comp_ind|requiz|dv)$")),
    num1, num2, num3, num4, num1_01, num2_01, num3_01, num4_01, num_index,
    pk_gm = pk1pk_gm, pk_blm = pk2pk_blm, pk_firstamd = pk3pk_firstamd, pk_congress = pk4pk_congress,
    pk_eu = pk5pk_eu,
    # The knowledge battery was asked only in the Bayesian-cues experiment; the original
    # scoring gave everyone else 0.
    across(c(pk1, pk2, pk3, pk4, pk5, pk_index), \(x) if_else(study == "bayes", x, NA)),
    pid3, pid7, pid_initial = pid3pid, pid_strength_dem = strdempid_dem, pid_strength_rep = strreppid_rep,
    pid_lean = pidleanpid_ind,
    sex = sexgender, race,
    # One respondent typed a birth year (1986) instead of an age; fielded in 2016.
    age = if_else(age > 120, 2016 - age, age),
    state, religion = religionrelig, political_interest = polinterestpol_int
  ) |>
  bind_cols(timers) |>
  write_data("study2_2016.csv")

# ANES 2012 columns behind the party benchmarks and the population base rates.
read_dta(file.path(heads, "data", "anes_timeseries_2012.dta"),
  col_select = c(
    weight_full, pid_x, dem_racecps_black, dem_unionwho_r, orientn_rgay, sample_state,
    dem_agegrp_iwdate, inc_incgroup_pre
  )
) |>
  zap_labels() |>
  # States reduced to the Southern indicator used for the party benchmark (states
  # most respondents in a FiveThirtyEight/SurveyMonkey poll call Southern).
  mutate(south = as.numeric(sample_state %in% c(
    "TX", "AR", "MS", "AL", "LA", "FL", "GA", "SC", "NC", "TN", "VA", "KY", "DC"
  ))) |>
  select(-sample_state) |>
  write_data("anes_2012.csv")
