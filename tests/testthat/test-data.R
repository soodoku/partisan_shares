study1 <- read_study1()
study2 <- read_study2()
truth <- truth_table()

test_that("samples and assignment match the design", {
  expect_equal(nrow(study1), 382L)
  expect_equal(
    as.vector(table(study1$condition)[c("standard", "sum_to_100", "incentive", "base_rates_shown")]),
    c(98L, 98L, 91L, 95L)
  )
  expect_equal(sum(study2$experiment == "wording"), 312L)
  expect_equal(as.vector(table(study2$timing_condition)), c(142L, 142L))
})

test_that("answer counts reflect who saw which item", {
  timing_answers <- timing_long(study2, truth)
  wording_answers <- wording_long(study2, truth)
  study1_answers <- study1_long(study1, truth)
  # Timing: four random party items plus two others for each of 284 respondents,
  # less eight skipped answers.
  expect_equal(nrow(timing_answers), 284L * 6L - 8L)
  expect_true(all(count(filter(timing_answers, item %in% party_items), respondent_id)$n <= 4))
  # Wording: ten items each, one skipped answer.
  expect_equal(nrow(wording_answers), 3119L)
  # Sum-to-100 respondents answered one party's four groups.
  expect_true(all(count(filter(study1_answers, condition == "sum_to_100"), respondent_id)$n == 4))
})

test_that("ANES benchmarks reproduce the published party shares", {
  published <- c(
    black = 23.99225, union = 10.51631, lgb = 6.284163, southern = 35.65226, over_65 = 21.300646, rich = 2.191437
  )
  benchmarks <- deframe(select(anes_benchmarks(), item, true_share))
  expect_equal(benchmarks[names(published)], published, tolerance = 1e-5)
})

test_that("population base rates come from the same ANES coding as the party shares", {
  # The 2017 code used Census and BLS figures with different definitions: 65+ as a
  # share of everyone including children (13%), union members as a share of workers
  # (11.3%). Among ANES adults, the share 65+ is near 19%.
  benchmarks <- anes_benchmarks()
  expect_gt(filter(benchmarks, item == "over_65")$true_base_rate, 18)
  union <- filter(benchmarks, item == "union")
  expect_lt(union$true_base_rate, union$true_share)
})

test_that("the poverty benchmark divides by all poor people", {
  # The 2017 code divided 10,755 by White + Black + Asian poor only (43,981), giving 24.5.
  expect_equal(filter(truth, item == "black_poor")$true_share, round(100 * 10755 / 46657, 1))
})

test_that("Study 2 ships all three experiments but the sample table describes two", {
  # Bayesian-cues arms as reported in Ahler and Sood (2023), n = 911.
  expect_equal(nrow(study2), 1507L)
  expect_equal(
    as.vector(table(study2$bayes_condition)[c("base_rate", "control", "group_composition")]),
    c(313L, 306L, 292L)
  )
  # The published appendix described all 1,507 records and listed 19.4% Latino;
  # among the 596 analyzed it is near 5%.
  samples <- readr::read_csv(table_path("samples.csv"), show_col_types = FALSE)
  expect_equal(filter(samples, variable == "N", study == "Study 2")$value, 596)
  analyzed <- filter(study2, experiment != "bayes")
  expect_lt(mean(analyzed$race == "Hispanic or Latino", na.rm = TRUE), 0.06)
})

test_that("knowledge scores are missing where the battery was not asked", {
  expect_true(all(is.na(filter(study2, experiment != "bayes")$knowledge_index)))
  expect_false(anyNA(filter(study2, experiment == "bayes")$knowledge_index))
})

test_that("each timing page timer belongs to its own answer", {
  # Party items were shown at random, so a timer filed under the wrong item would be
  # missing for different respondents than its answer. Every answer has its page's
  # timer; a timer without an answer is a page left untouched until it timed out.
  timing <- filter(study2, experiment == "timing")
  for (item in party_items) {
    shown <- !is.na(timing[[paste0("timer_", item, "_page_submit")]])
    answered <- !is.na(timing[[paste0("timed_", item)]])
    expect_true(all(shown[answered]), label = item)
    expect_true(all(timing[[paste0("timer_", item, "_click_count")]][shown & !answered] == 0), label = item)
  }
  compliance <- timing_compliance(study2)
  expect_lt(filter(compliance, timing_condition == "time_pressure")$over_11, 5)
  expect_lt(filter(compliance, timing_condition == "time_requirement")$under_15, 10)
})

test_that("Study 1 sum-to-100 allocations add to 100", {
  totals <- study1 |>
    filter(condition == "sum_to_100") |>
    transmute(
      dem_race = rowSums(pick(starts_with("allocation_dem_race"))),
      rep_income = rowSums(pick(starts_with("allocation_rep_income")))
    )
  expect_true(all(c(totals$dem_race, totals$rep_income) %in% c(100, NA)))
})

test_that("Study 1 Republicans and independents are not swapped", {
  # The published appendix listed 22.3% Republican and 18.9% independent.
  expect_equal(round(100 * mean(study1$party == "Republican"), 1), 18.8)
  expect_equal(round(100 * mean(study1$party == "Independent"), 1), 22.3)
})
