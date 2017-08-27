[![Travis-CI Build Status](https://travis-ci.org/julian-urbano/gt4ireval.svg?branch=develop)](https://travis-ci.org/julian-urbano/gt4ireval)
[![License](https://img.shields.io/badge/license-MIT-brightgreen.svg)](https://opensource.org/licenses/MIT)
[![CRAN version](http://www.r-pkg.org/badges/version/gt4ireval?color=blue)](https://cran.r-project.org/package=gt4ireval) 
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/gt4ireval?color=blue)](https://cran.r-project.org/package=gt4ireval) 

# gt4ireval

Provides tools to measure the reliability of an Information Retrieval test collection. It allows users to estimate reliability using Generalizability Theory and map those estimates onto well-known indicators such as Kendall tau correlation or sensitivity.

For a full background please refer to *Julián Urbano, Mónica Marrero and Diego Martín, "[On the Measurement of Test Collection Reliability](http://julian-urbano.info/files/publications/055-measurement-test-collection-reliability.pdf)", ACM SIGIR, 2013*.

## Installation

You may install the stable release from CRAN

```r
install.packages("gt4ireval")
```

or the latest development version from GitHub

```r
devtools::install_github("julian-urbano/gt4ireval", ref = "develop")
```

## Usage

A full user manual in available in the [package vignette](https://cran.r-project.org/web/packages/gt4ireval/vignettes/gt4ireval.html).

As a very simple example, we can analyze the TREC-3 Ad hoc data:

```r
head(adhoc3)
#     sys1   sys2   sys3   sys4   sys5   sys6   sys7 ...
# 1 0.2830 0.5163 0.4810 0.5737 0.5184 0.4945 0.5013 ...
# 2 0.0168 0.5442 0.3987 0.2964 0.6115 0.2354 0.1689 ...
# ...
```

We first run a G-study,

```r
ah3.g <- gstudy(adhoc3, drop = 0.25)
ah3.g
# Summary of G-Study
# 
#                  Systems     Queries Interaction
#              ----------- ----------- -----------
# Variance       0.0028117    0.028093    0.010152
# Variance(%)       6.8482      68.425      24.727
# ---
# Mean Sq.         0.15074     0.85296    0.010152
# Sample size           30          50        1500
```

then a D-study,

```r
dstudy(ah3.g, queries = c(50, 100, 150))
# Summary of D-Study
# 
# Call:
#     queries = 50 
#   stability = 0.95 
#       alpha = 0.025 
# 
# Stability:
#                                            Erho2                                   Phi
#              -----------------------------------   -----------------------------------
#      Queries    Expected       Lower       Upper      Expected       Lower       Upper
#  ----------- ----------- ----------- -----------   ----------- ----------- -----------
#           50     0.93265     0.89311     0.96287       0.78613     0.66141     0.88039 
#          100     0.96515     0.94354     0.98109       0.88026     0.79621     0.93639 
#          150     0.97649     0.96164     0.98731       0.91686     0.85423     0.95668 
# 
# Required number of queries:
#                                            Erho2                                   Phi
#              -----------------------------------   -----------------------------------
#    Stability    Expected       Lower       Upper      Expected       Lower       Upper
#  ----------- ----------- ----------- -----------   ----------- ----------- -----------
#         0.95          69          37         114           259         130         487
```

and possibly map onto AP correlation, for instance,

```r 
gt2tauAP(Erho2 = c(0.93, 0.95, 0.98))
# [1] 0.7487836 0.8150692 0.9226192
```

## License and Citation

`gt4ireval` is released under the terms of the [MIT License](https://opensource.org/licenses/MIT).

If you use this code in your work, please cite the following paper:

```latex
@inproceedings{Urbano2013measurement,
  author = {Urbano, Juli\'{a}n and Marrero, M\'{o}nica and Mart\'{\i}n, Diego},
  booktitle = {International ACM SIGIR Conference on Research and Development in Information Retrieval},
  pages = {393--402},
  title = {{On the Measurement of Test Collection Reliability}},
  year = {2013}
}
```

## Acknowledgements

> This work is supported by an A4U postdoctoral grant and a Juan de la Cierva postdoctoral fellowship.
