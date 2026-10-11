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

test_that("Study 2 demographics exclude the separate experiment", {
  # The published appendix described all 1,507 records, including a third
  # experiment, and listed 19.4% Latino; among the 596 analyzed it is near 5%.
  expect_equal(nrow(s2), 596L)
  expect_lt(mean(s2$race == "Hispanic or Latino", na.rm = TRUE), 0.06)
})

test_that("Study 1 Republicans and independents are not swapped", {
  # The published appendix listed 22.3% Republican and 18.9% independent.
  expect_equal(round(100 * mean(s1$party == "Republican"), 1), 18.8)
  expect_equal(round(100 * mean(s1$party == "Independent"), 1), 22.3)
})
