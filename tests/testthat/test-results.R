effects <- read_table("effects.csv")
contrasts <- read_table("item_contrasts.csv")
on_the_nose <- read_table("on_the_nose.csv")
base_rates <- read_table("base_rates.csv")

test_that("pooled treatment effects match the 2017 estimates", {
  # Treatment effects compare answers within item, so they do not depend on the
  # benchmarks except through which items each arm answered.
  estimate <- deframe(select(effects, comparison, estimate))
  expect_equal(estimate[["Incentives"]], 0.8909929, tolerance = 1e-6)
  expect_equal(estimate[["Base rates given"]], 7.171227, tolerance = 1e-6)
  expect_equal(estimate[["Time requirement"]], -4.335181, tolerance = 1e-6)
  expect_equal(estimate[["Percent wording"]], -1.248022, tolerance = 1e-6)
  expect_equal(estimate[["Sum to 100"]], -0.52, tolerance = 0.01)
})

test_that("the 'What percentage' baseline is the percent arm, not the regression constant", {
  wording <- filter(effects, comparison == "Percent wording")
  expect_lt(wording$treatment_mean, wording$control_mean)
  expect_equal(round(wording$treatment_mean, 1), 17.7)
})

test_that("the lookup test counts only items respondents saw", {
  # The 2017 version scored unseen items as not on the nose (n = 2,840).
  all_items <- filter(on_the_nose, item == "all")
  expect_equal(all_items$control_n, 1696)
  expect_gt(all_items$control_mean, 0.03)
})

test_that("the Black-Democrat difference-in-differences subtracts the true shares", {
  # The 2017 code wrote `x - 24 / 24`, which subtracts 1, so the true shares
  # dropped out and the estimate was the raw perceived gap (+4.6).
  black <- filter(base_rates, item == "black")
  expect_equal(black$did, black$gap - (black$true_share - black$true_base_rate), tolerance = 1e-9)
  expect_lt(black$did, 0)
})

test_that("sum to 100 raised the Southern Republican item", {
  southern <- filter(contrasts, experiment == "Study 1", treatment == "sum_to_100", item == "southern")
  expect_gt(southern$difference, 15)
  expect_lt(southern$p_value, 0.001)
})
