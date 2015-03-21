## Copyright (C) 2015  Julián Urbano <urbano.julian@gmail.com>
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see http://www.gnu.org/licenses/.

## Unit tests based on example from:
##   R. L. Brennan, "Generalizability Theory". Springer, 2001.

rm(list = ls(all = T))
library(RUnit)

## Synthetic dataset no. 4 from Table 3.2 on page 73, recasted as a p X i design, as required on page 182.
df <- structure(list(p1 = c(5L, 6L, 5L, 5L, 5L, 3L, 4L, 5L, 6L, 7L, 3L, 3L),
                     p2 = c(9L, 3L, 7L, 7L, 7L, 5L, 5L, 5L, 7L, 7L, 5L, 2L),
                     p3 = c(3L, 4L, 3L, 3L, 5L, 3L, 3L, 5L, 6L, 5L, 1L, 6L),
                     p4 = c(7L, 5L, 5L, 3L, 3L, 1L, 4L, 3L, 5L, 3L, 3L, 5L),
                     p5 = c(9L, 2L, 9L, 7L, 7L, 7L, 3L, 7L, 2L, 7L, 5L, 3L),
                     p6 = c(3L, 4L, 3L, 5L, 3L, 3L, 6L, 3L, 4L, 5L, 1L, 2L),
                     p7 = c(7L, 3L, 7L, 7L, 7L, 5L, 5L, 7L, 5L, 5L, 5L, 4L),
                     p8 = c(5L, 8L, 5L, 7L, 7L, 5L, 5L, 4L, 3L, 2L, 1L, 1L),
                     p9 = c(9L, 9L, 8L, 8L, 6L, 6L, 6L, 5L, 5L, 8L, 1L, 1L),
                     p10 = c(4L, 4L, 4L, 3L, 3L, 5L, 6L, 5L, 5L, 7L, 1L, 1L)),
                .Names = c("p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10"),
                class = "data.frame", row.names = c(NA, -12L))

source("gt4ireval.R")

## Test G study with Table 6.2 on page 182
test.g.study <- function() {
  g <- g.study(data = df, drop = 0)

  # Degrees of freedom
  checkEqualsNumeric(10, g$n.s)
  checkEqualsNumeric(12, g$n.q)

  # Variance components
  checkEqualsNumeric(.6258, g$var.s, tolerance = 1e-4)
  checkEqualsNumeric(.8840, g$var.q, tolerance = 1e-4)
  checkEqualsNumeric(2.7872, g$var.e, tolerance = 1e-4)

  # Mean squares
  checkEqualsNumeric(10.2963, g$em.s, tolerance = 1e-4)
  checkEqualsNumeric(11.6273, g$em.q, tolerance = 1e-4)
  checkEqualsNumeric(2.7872, g$em.e, tolerance = 1e-4)
}

## Test D study with Table 6.5 on page 198
test.d.study <- function() {
  g <- g.study(data = df, drop = 0)
  d <- d.study(gdata = g, alpha = .1) # 80% confidence intervals

  # Erho2
  checkEqualsNumeric(.729, d$Erho2, tolerance = 1e-3)
  checkEqualsNumeric(.541, d$Erho2.lwr, tolerance = 1e-3)
  checkEqualsNumeric(.876, d$Erho2.upr, tolerance = 1e-3)
  # Phi
  checkEqualsNumeric(.672, d$Phi, tolerance = 1e-3)
  checkEqualsNumeric(.457, d$Phi.lwr, tolerance = 1e-3)
  checkEqualsNumeric(.845, d$Phi.upr, tolerance = 1e-3)
}

# runTestFile("gt4ireval.test.R")
