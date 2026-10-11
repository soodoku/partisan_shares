# Codebook

One row per respondent in each survey file. Shares are percentages (0–100). Empty cells are
items a respondent was not shown or skipped. `scripts/00_extract.R` writes these files from
the authors' earlier public repositories, [partisan_heads](https://github.com/soodoku/partisan_heads)
(2014 surveys, ANES 2012) and [partisan_typecast](https://github.com/soodoku/partisan_typecast)
(2016 survey), renaming columns and dropping worker and response IDs, timestamps, location,
browser data and free text.

Item names are shared across files: `black`, `atheist` (atheist or agnostic), `lgb`, `union`
(Democratic groups); `rich` (earn $250,000+), `evangelical`, `southern`, `over_65`
(Republican groups); `foreign_born` (share of U.S. residents); `black_poor` (share of the poor
who are Black).

## `study1_2014.csv` — Study 1, November 2014 (382 respondents)

| Columns | Contents |
|---|---|
| `condition` | `standard`, `sum_to_100`, `incentive`, `base_rates_shown` |
| `incentive_page_order` | Incentive condition: `dem_first` or `rep_first` |
| `perceived_<item>` | Perceived share of the group's party in the group, whatever the condition |
| `perceived_base_rate_<item>` | Standard-estimation respondents' perceived share of Americans in the group (a random subset of groups each) |
| `allocation_<party>_<grouping>_<category>` | Sum-to-100 respondents' division of one party among a grouping's categories (e.g. `allocation_rep_income_*` across four brackets); each grouping adds to 100 |
| `incentive_{dem,rep}_page_*` | Incentive-condition page timers in seconds: first click, last click, page submit, and click count |
| `party`, `party_7` | Party with leaners; 7-point party ID (1–2 Democratic identifiers, 3 Democratic leaners, 4 independents, 5 Republican leaners, 6–7 Republican identifiers; which of each pair is "strong" is not recorded) |
| `gender`, `education`, `age_group` | Demographics |
| `race_*` | Check-all-that-apply race and ethnicity (`TRUE` if ticked) |
| `*_code` | Ideology, religion and political attention as exported, without value labels. `ideology_3_code` 1 and 3 are liberal and conservative (each has its own strength follow-up); other codes are undocumented |

## `affect_2014.csv` — April 2014 survey (1,036 rows)

| Columns | Contents |
|---|---|
| `condition` | Policy-extremity experiment arm (`ask`, `tell`, `control`); 34 rows are incomplete responses |
| `thermometer_<group>` | Feeling thermometers (0–100) toward eight groups (`young` is under 35) and the two parties |
| `perceived_<group>` | Perceived share of the out-party in the group: Democratic groups rated by Republicans and Republican leaners, Republican groups by Democrats and Democratic leaners |
| `party_3_code`, `party_7`, `ideology_7` | Party and ideology |
| `*_code` | Demographics and political attention as exported, without value labels |

The experiment's policy-extremity perceptions are analyzed in Ahler and Sood (2018, *JOP*) and stay in partisan_heads.

## `study2_2016.csv` — Study 2, July 31, 2016 (1,507 respondents)

| Columns | Contents |
|---|---|
| `experiment` | `wording` (312), `timing` (284), or `bayes` (911, the Bayesian-cues experiment in Ahler and Sood 2023) |
| `timing_condition` | `time_pressure` (10 seconds to answer) or `time_requirement` (15-second wait before answering) |
| `soda_condition` | `base_rate_first` (asked about all Americans first) or `no_base_rate` |
| `bayes_condition`, `bayes_party` | Bayesian-cues arm (`base_rate`, `group_composition`, `control`) and the party battery the respondent got |
| `wording_block_order`, `duration_seconds` | Order of the wording-experiment blocks; total survey time |
| `perceived_<item>_{percent,out_of_100}` | Wording experiment: "What percentage" or "Out of every 100" |
| `timed_<item>` | Timing experiment answers, including diet soda (`soda_all`, `soda_men`, `soda_women`) |
| `timer_<item>_*` | The page timer for each timed answer: first click, last click, page submit, click count. A page left untouched until it timed out has a timer but no answer |
| `soda_all`, `soda_men`, `soda_women` | Wording-experiment respondents' perceived share of Americans, men and women who drink diet soda |
| `bayes_estimate_<group>_base_rate`, `bayes_estimate_<group>_{dem,rep,ind}_share` | Bayesian-cues respondents' estimates of the group's share of Americans and of its party split |
| `bayes_recall_<group>_*` | Recall of the figure shown (paid a bonus if correct) |
| `bayes_perceived_<group>` | Outcome: perceived share of the party in the group (`lgb`, `union` for Democrats; `evangelical`, `rich` for Republicans) |
| `numeracy_<n>`, `numeracy_<n>_correct`, `numeracy_index` | Numeracy answers, scored items and their mean |
| `knowledge_<topic>`, `knowledge_<topic>_correct`, `knowledge_index` | Political knowledge answers, scored items and their mean; asked only in the Bayesian-cues experiment |
| `party_3`, `party_7`, `party_initial`, `party_strength_*`, `party_lean` | Party ID, constructed and as asked |
| `gender`, `race`, `age`, `state`, `religion`, `political_interest` | Demographics |

## `anes_2012.csv` (5,914 rows)

`weight` (full-sample weight), `party` (with leaners), and 0/1 indicators `black`, `union`
(union household), `lgb`, `southern`, `over_65`, and `rich` (household income $250,000+),
coded as in Ahler and Sood (2018, *JOP*).

## `benchmarks.csv`

Benchmarks not computed from the ANES, one row per `item` and `quantity` (`party_share`,
`base_rate` or `population_share`), with `source` and `note`.
