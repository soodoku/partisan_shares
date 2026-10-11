s1 <- read_study1()
s2 <- read_study2()
truth <- truth_table()

test_that("samples and assignment match the design", {
  expect_equal(nrow(s1), 382L)
  counts <- table(s1$condition)[c("simpleask", "complexask", "incentive", "baserates_given")]
  expect_equal(as.vector(counts), c(98L, 98L, 91L, 95L))
  expect_equal(sum(s2$study == "wording"), 312L)
  expect_equal(as.vector(table(s2$timing_cond)), c(142L, 142L))
})

test_that("answer counts reflect who saw which item", {
  lt <- timing_long(s2, truth)
  lw <- wording_long(s2, truth)
  l1 <- study1_long(s1, truth)
  # Timing: four random party items plus two others for each of 284 respondents,
  # less eight skipped answers.
  expect_equal(nrow(lt), 284L * 6L - 8L)
  per_resp <- table(lt$respondent_id[lt$item %in% party_items])
  expect_true(all(per_resp <= 4))
  # Wording: ten items each, one skipped answer.
  expect_equal(nrow(lw), 3119L)
  # Sum-to-100 respondents answered one party's four groups.
  expect_true(all(table(l1$respondent_id[l1$condition == "complexask"]) == 4))
})

test_that("ANES benchmarks reproduce the published party shares", {
  b <- anes_benchmarks()
  published <- c(black = 23.99225, union = 10.51631, lgb = 6.284163, south = 35.65226, old = 21.300646, rich = 2.191437)
  expect_equal(b$party_share[match(names(published), b$item)], unname(published), tolerance = 1e-5)
})

test_that("population base rates come from the same ANES coding as the party shares", {
  # The 2017 code used Census and BLS figures with different definitions: 65+ as a
  # share of everyone including children (13%), union members as a share of workers
  # (11.3%). Among ANES adults, the share 65+ is near 19%.
  b <- anes_benchmarks()
  expect_gt(b$base[b$item == "old"], 18)
  expect_lt(b$base[b$item == "union"], b$party_share[b$item == "union"])
})

test_that("the poverty benchmark divides by all poor people", {
  # The 2017 code divided 10,755 by White + Black + Asian poor only (43,981), giving 24.5.
  expect_equal(truth$truth[truth$item == "blackpoor"], round(100 * 10755 / 46657, 1))
})

test_that("Study 2 ships all three experiments but the sample table describes two", {
  # Bayesian-cues arms as reported in Ahler and Sood (2023), n = 911.
  expect_equal(nrow(s2), 1507L)
  expect_equal(as.vector(table(s2$bayes_cond)[c("base_rate", "control", "group_comp")]), c(313L, 306L, 292L))
  # The published appendix described all 1,507 records and listed 19.4% Latino;
  # among the 596 analyzed it is near 5%.
  samples <- readr::read_csv(table_path("samples.csv"), show_col_types = FALSE)
  expect_equal(samples$value[samples$variable == "N" & samples$study == "Study 2"], 596)
  analyzed <- filter(s2, study != "bayes")
  expect_lt(mean(analyzed$race == "Hispanic or Latino", na.rm = TRUE), 0.06)
})

test_that("knowledge scores are missing where the battery was not asked", {
  expect_true(all(is.na(s2$pk_index[s2$study != "bayes"])))
  expect_false(anyNA(s2$pk_index[s2$study == "bayes"]))
})

test_that("each timing page timer belongs to its own answer", {
  # Party items were shown at random, so a timer filed under the wrong item would be
  # missing for different respondents than its answer.
  party_answers <- grep("^timing_(dem|rep)_.+_(fast|slow)$", names(s2), value = TRUE)
  for (a in party_answers) {
    submit <- s2[[paste0(sub("^timing_", "timer_", a), "_page_submit")]]
    expect_gt(mean(is.na(submit) == is.na(s2[[a]])), 0.998, label = a)
  }
  compliance <- timing_compliance(s2)
  expect_lt(compliance$over_11[compliance$condition == "fast"], 5)
  expect_lt(compliance$under_15[compliance$condition == "slow"], 10)
})

test_that("Study 1 sum-to-100 allocations add to 100", {
  totals <- s1 |>
    filter(condition == "complexask") |>
    mutate(
      dem_race = rowSums(pick(starts_with("sum100_dem_race"))),
      rep_income = rowSums(pick(starts_with("sum100_rep_income")))
    )
  expect_true(all(c(totals$dem_race, totals$rep_income) %in% c(100, NA)))
})

test_that("Study 1 Republicans and independents are not swapped", {
  # The published appendix listed 22.3% Republican and 18.9% independent.
  expect_equal(round(100 * mean(s1$party == "Republican"), 1), 18.8)
  expect_equal(round(100 * mean(s1$party == "Independent"), 1), 22.3)
})
