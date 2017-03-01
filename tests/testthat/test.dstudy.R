context("D-study")

#' Test D study with Table 6.5 on page 198.
test_that("Synthetic dataset, valid args", {
  g <- gstudy(synthetic4)
  d <- dstudy(g, alpha = .1) # 80% 2-tailed confidence interval

  expect_equal(d$call$queries, 12)

  expect_equal(d$Erho2, .729, tolerance = 1e-3)
  expect_equal(d$Erho2.lwr, .541, tolerance = 1e-3)
  expect_equal(d$Erho2.upr, .876, tolerance = 1e-3)

  expect_equal(d$Phi, .672, tolerance = 1e-3)
  expect_equal(d$Phi.lwr, .457, tolerance = 1e-3)
  expect_equal(d$Phi.upr, .845, tolerance = 1e-3)
})

test_that("Synthetic dataset, invalid args", {
  g <- gstudy(synthetic4)

  expect_error(dstudy(1:3))
  expect_error(dstudy(g, queries = -1))
  expect_error(dstudy(g, stability = c(.1, -.1)))
  expect_error(dstudy(g, stability = c(.9, 1)))
  expect_error(dstudy(g, alpha = c(-.01, .01)))
  expect_error(dstudy(g, alpha = c(.01, 1.01)))
})
