effects <- read_table("effects.csv")
contrasts <- read_table("item_contrasts.csv")
nose <- read_table("on_the_nose.csv")
base_rates <- read_table("base_rates.csv")

test_that("pooled treatment effects reproduce the 2017 Stata estimates", {
  # Treatment effects compare answers within item, so they do not depend on the
  # benchmarks except through which items each arm answered.
  est <- setNames(effects$estimate, effects$comparison)
  expect_equal(est[["Incentives"]], 0.8909929, tolerance = 1e-6)
  expect_equal(est[["Base rates given"]], 7.171227, tolerance = 1e-6)
  expect_equal(est[["Time requirement"]], -4.335181, tolerance = 1e-6)
  expect_equal(est[["Percent wording"]], -1.248022, tolerance = 1e-6)
  expect_equal(est[["Sum to 100"]], -0.52, tolerance = 0.01)
})

test_that("the 'What percentage' baseline is the percent arm, not the regression constant", {
  w <- effects[effects$comparison == "Percent wording", ]
  expect_lt(w$treated, w$baseline)
  expect_equal(round(w$treated, 1), 17.7)
})

test_that("the lookup test counts only items respondents saw", {
  # The 2017 version scored unseen items as not on the nose (n = 2,840).
  all_items <- nose[nose$item == "all", ]
  expect_equal(all_items$n_a, 1696)
  expect_gt(all_items$mean_a, 0.03)
})

test_that("the Black-Democrat difference-in-differences subtracts the true shares", {
  # The 2017 code wrote `x - 24 / 24`, which subtracts 1, so the true shares
  # dropped out and the estimate was the raw perceived gap (+4.6).
  b <- base_rates[base_rates$item == "black", ]
  expect_equal(b$dd, b$gap - (b$party_truth - b$base_truth), tolerance = 1e-9)
  expect_lt(b$dd, 0)
})

test_that("sum to 100 raised the Southern Republican item", {
  s1 <- contrasts[contrasts$experiment == "Study 1" & contrasts$group_b == "complexask", ]
  south <- s1[s1$item == "south", ]
  expect_gt(south$diff, 15)
  expect_lt(south$p, 0.001)
})
