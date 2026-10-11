# Writes the de-identified CSVs in data/ from the original archive files. Not part of
# the build: the sources live outside this repository.
#
#   Rscript scripts/00_extract.R <archive> <partisan_heads>
#
# <archive> is the unzipped 2016-2017 project folder ("misinformation/"); <partisan_heads>
# is a clone of github.com/soodoku/partisan_heads (commit d586407), whose data/ holds the
# 2014 MTurk studies and ANES 2012 used in Ahler and Sood (2018, Journal of Politics).

suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
})

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2)
archive <- args[1]
heads <- args[2]
out <- function(name) file.path("data", name)

# Study 1 (November 2014): alternative-explanations experiment. Location, state and
# free-text party fields are dropped.
s1 <- read_dta(file.path(heads, "data", "alt_exps_data.dta")) |>
  zap_labels() |>
  filter(experiment == "simple_complex")
s1 |>
  transmute(
    respondent_id = row_number(), condition,
    dem_black, dem_aa, dem_lgb, dem_union, rep_rich, rep_evang, rep_south, rep_old,
    br_black = simple_baserate_black, br_aa = simple_baserate_atheistoragnosti,
    br_lgb = simple_baserate_gaylesbianorbise, br_union = simple_baserate_unionmembers,
    br_rich = simple_baserate_earningover25000, br_evang = simple_baserate_evangelicalchris,
    br_south = simple_baserate_southerners, br_old = simple_baserate_65yearsorolder,
    party = recode(pid_3, D = "Democrat", I = "Independent", R = "Republican"),
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
    race_latino = racehispaniclatino
  ) |>
  readr::write_csv(out("study1_2014.csv"), na = "")

# April 2014 survey with group feeling thermometers. The file is stacked four times
# (one row per policy item); one row per respondent is kept.
read_dta(file.path(heads, "data", "extremity_exp_data.dta")) |>
  zap_labels() |>
  filter(!duplicated(responseid)) |>
  transmute(
    respondent_id = row_number(), condition, pid_3,
    ft_black, ft_aa, ft_lgb, ft_union, ft_rich, ft_evang, ft_south, ft_young,
    d_black = d_comp_per_black, d_aa = d_comp_per_aa, d_lgb = d_comp_per_lgb, d_union = d_comp_per_union,
    r_rich = r_comp_per_rich, r_evang = r_comp_per_evang, r_south = r_comp_per_south, r_young = r_comp_per_young
  ) |>
  readr::write_csv(out("affect_2014.csv"), na = "")

# Study 2 (July 31, 2016). The same Qualtrics file holds a third, separate experiment
# (study == "bayes") that the chapter does not use; it is left out. Worker IDs, response
# IDs, timestamps, browser data, state and free-text comments are dropped.
s2 <- read_dta(file.path(archive, "data", "pcomp_mes_clean_labeled.dta")) |>
  zap_labels() |>
  filter(study %in% c("main_measurement", "timing"))
answers <- grep("^(pcomp_.*_(perc|100)|imm_(perc|100)|black_poor_(perc|100)|timing_.*_(fast|slow))$",
  names(s2),
  value = TRUE
)
s2 |>
  transmute(
    respondent_id = row_number(),
    study = if_else(study == "main_measurement", "wording", "timing"),
    timing_cond = na_if(timing_cond, ""), soda_cond = na_if(soda_cond, ""),
    across(all_of(answers)),
    soda_br, soda_men, soda_women,
    num_index, pid3 = na_if(pid3, ""), sex = na_if(sexgender, ""), race = na_if(race, ""),
    # One respondent typed a birth year (1986) instead of an age; fielded in 2016.
    age = if_else(age > 120, 2016 - age, age)
  ) |>
  readr::write_csv(out("study2_2016.csv"), na = "")

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
  readr::write_csv(out("anes_2012.csv"), na = "")
