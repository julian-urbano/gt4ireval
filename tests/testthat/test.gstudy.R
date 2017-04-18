context("G-study")

#' Test G study with Table 6.2 on page 182.
test_that("Synthetic dataset, valid args", {
  g <- gstudy(synthetic4)

  expect_equal(g$n.s, 10)
  expect_equal(g$n.q, 12)

  expect_equal(g$var.s, .6258, tolerance = 1e-4)
  expect_equal(g$var.q, .8840, tolerance = 1e-4)
  expect_equal(g$var.e, 2.7872, tolerance = 1e-4)

  expect_equal(g$em.s, 10.2963, tolerance = 1e-4)
  expect_equal(g$em.q, 11.6273, tolerance = 1e-4)
  expect_equal(g$em.e, 2.7872, tolerance = 1e-4)

  g <- gstudy(synthetic4, alpha = .1)
  expect_equal(g$var.s.lwr, .078, tolerance = 1e-3)
  expect_equal(g$var.s.upr, 1.174, tolerance = 1e-3)
  expect_equal(g$var.q.lwr, .169, tolerance = 1e-3)
  expect_equal(g$var.q.upr, 1.599, tolerance = 1e-3)
  expect_equal(g$var.e.lwr, 2.070, tolerance = 1e-3)
  expect_equal(g$var.e.upr, 3.505, tolerance = 1e-3)

  g <- gstudy(synthetic4, drop = .3)
  expect_equal(g$n.s, 7) # 2nd and 3rd are tied
  expect_equal(g$n.q, 12)

  g <- gstudy(synthetic4, drop = .2)
  expect_equal(g$n.s, 9) # 2nd and 3rd are tied
  expect_equal(g$n.q, 12)

  expect_warning(g <- gstudy(synthetic4[4:7,4:7], ensurePositive = TRUE))
  expect_equal(g$var.q, 0)
  expect_warning(g <- gstudy(synthetic4[4:7,4:7], ensurePositive = FALSE))
  expect_lt(g$var.q, 0)
})

test_that("Synthetic dataset, invalid args", {
  expect_error(gstudy(1:4))
  expect_error(gstudy(synthetic4, drop = -.1))
  expect_error(gstudy(synthetic4, drop = 1.1))
  expect_error(gstudy(synthetic4, alpha = c(-.01, .01)))
  expect_error(gstudy(synthetic4, alpha = c(.01, .55)))
})
