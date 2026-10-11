# Codebook

One row per respondent in each survey file. Shares are percentages (0–100). Empty cells are
items a respondent was not shown or skipped. `scripts/00_extract.R` writes these files from
the authors' earlier public repositories, [partisan_heads](https://github.com/soodoku/partisan_heads)
(2014 surveys, ANES 2012) and [partisan_typecast](https://github.com/soodoku/partisan_typecast)
(2016 survey), dropping worker and response IDs, timestamps, location, browser data and free text.

## `study1_2014.csv` — Study 1, November 2014 (382 respondents)

| Columns | Contents |
|---|---|
| `condition` | `simpleask` (standard estimation), `complexask` (sum to 100), `incentive`, `baserates_given` |
| `incentive_order` | Incentive condition: whether the Democratic or Republican page came first |
| `dem_black`, `dem_aa`, `dem_lgb`, `dem_union`, `rep_rich`, `rep_evang`, `rep_south`, `rep_old` | Estimated share of the party in the group, whatever the condition |
| `br_*` | Standard-estimation respondents' estimates of the group's share of all Americans (a random subset of groups each) |
| `sum100_*` | Sum-to-100 respondents' full division of one party across each grouping's categories (e.g. `sum100_rep_income_*` across four income brackets); each grouping adds to 100 |
| `incentive_{dem,rep}_page_*` | Incentive condition page timers in seconds: first click, last click, page submit, click count |
| `party`, `pid_7` | Party with leaners; 7-point party ID (1–2 Democratic identifiers, 3 Democratic leaners, 4 independents, 5 Republican leaners, 6–7 Republican identifiers; which of each pair is "strong" is not recorded) |
| `gender`, `education`, `age`, `race_*` | Demographics; race is check-all-that-apply (a value marks a box ticked) |
| `ideo_3`, `ideo_*_strength`, `ideo_lean`, `religion_code`, `political_attention_code` | Codes as exported, without value labels. `ideo_3` 1 and 3 are liberal and conservative (each has its own strength follow-up); other codes are undocumented |

## `affect_2014.csv` — April 2014 survey (1,036 rows)

| Columns | Contents |
|---|---|
| `condition` | Policy-extremity experiment arm (`Ask`, `Tell`, `Control`); 34 rows are incomplete responses |
| `ft_*` | Feeling thermometers (0–100) toward eight groups and the two parties |
| `d_*`, `r_*` | Estimated share of the out-party in each group (`d_*` from Republicans about Democrats, `r_*` the reverse) |
| `pid_3`, `pid_7`, `ideo_7` | Party and ideology |
| `*_code` | Demographics and political attention as exported, without value labels |

The experiment's policy-extremity perceptions are analyzed in Ahler and Sood (2018, *JOP*) and stay in partisan_heads.

## `study2_2016.csv` — Study 2, July 31, 2016 (1,507 respondents)

| Columns | Contents |
|---|---|
| `study` | `wording` (312), `timing` (284), or `bayes` (911, the Bayesian-cues experiment in Ahler and Sood 2023) |
| `timing_cond`, `soda_cond` | Timing arm (`fast` 10-second limit, `slow` 15-second wait); diet-soda arm (`br` base rate asked first, `no_br`) |
| `bayes_cond`, `bayes_party` | Bayesian-cues arm (`base_rate`, `group_comp`, `control`) and the party battery the respondent got |
| `block_order`, `duration_seconds` | Order of the wording-experiment blocks; total survey time |
| `pcomp_{dem,rep}_*_{perc,100}`, `imm_*`, `black_poor_*` | Wording experiment answers: "What percentage" (`perc`) or "Out of every 100" (`100`) |
| `timing_*_{fast,slow}` | Timing experiment answers, including the diet-soda items |
| `timer_*_{fast,slow}_*` | The page timer for each timing answer: first click, last click, page submit, click count |
| `soda_br`, `soda_men`, `soda_women` | Diet-soda estimates, wording-experiment respondents |
| `bayes_*_br`, `bayes_*_comp_*` | Bayesian-cues respondents' estimates of the group's population share and its party split |
| `bayes_*_requiz` | Recall check on the information shown (paid a bonus if correct) |
| `bayes_dem_*_dv`, `bayes_rep_*_dv` | Outcome: estimated share of the party in the group |
| `num1`–`num4`, `num*_01`, `num_index` | Numeracy items, scored items and their index |
| `pk_*`, `pk1`–`pk5`, `pk_index` | Political knowledge answers, scored items and their index |
| `pid3`, `pid7`, `pid_initial`, `pid_strength_*`, `pid_lean` | Party ID, constructed and as asked |
| `sex`, `race`, `age`, `state`, `religion`, `political_interest` | Demographics |

## `anes_2012.csv`

Weighted ANES 2012 columns behind the benchmarks: `weight_full`, 7-point party ID (`pid_x`), and
the group indicators (`dem_racecps_black`, `dem_unionwho_r`, `orientn_rgay`, `dem_agegrp_iwdate`,
`inc_incgroup_pre`, and `south`, the state reduced to a Southern indicator).
