context("G-study")

#' Test G study with Table 6.2 on page 182.
test_that("Synthetic dataset, valid args", {
  g <- gstudy(synthetic4)

  expect_equal(g$n.s, 10)
  expect_equal(g$n.q, 12)

  expect_equal(g$var.s, .6258,tolerance = 1e-4)
  expect_equal(g$var.q, .8840,tolerance = 1e-4)
  expect_equal(g$var.e, 2.7872,tolerance = 1e-4)

  expect_equal(g$em.s, 10.2963,tolerance = 1e-4)
  expect_equal(g$em.q, 11.6273,tolerance = 1e-4)
  expect_equal(g$em.e, 2.7872,tolerance = 1e-4)

  g <- gstudy(synthetic4, drop = .3)
  expect_equal(g$n.s, 7) # 2nd and 3rd are tied
  expect_equal(g$n.q, 12)

  g <- gstudy(synthetic4, drop = .2)
  expect_equal(g$n.s, 9) # 2nd and 3rd are tied
  expect_equal(g$n.q, 12)
})

test_that("Synthetic dataset, invalid args", {
  expect_error(gstudy(1:4))
  expect_error(gstudy(synthetic4, -.1))
  expect_error(gstudy(synthetic4, 1.1))
})
