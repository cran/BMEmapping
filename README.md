
<!-- README.md is generated from README.Rmd. Please edit that file -->

### Spatial Interpolation for data comprising hard and soft-interval forms

The **Bayesian Maximum Entropy (BME)** framework provides a flexible and
principled approach to space-time data analysis by combining Bayesian
inference with the maximum entropy principle. It supports optimal
estimation using both precise (hard) and uncertain (soft) data, such as
intervals or probability distributions—making it ideal for complex,
real-world datasets. The **BMEmapping** R package implements core BME
methods for spatial interpolation, enabling the integration of
heterogeneous data, variogram-based modeling, and uncertainty
quantification.

## Installation

You can install the development version of **BMEmapping** from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("KinsprideDuah/BMEmapping")
```

## Functions

`prob_zk` - computes and optionally plots the posterior density estimate
at a single unobserved location.

`bme_predict` - predicts the posterior mean or mode and the associated
variance at an unobserved location.

`bme_cv` - performs a cross-validation on the hard data to assess model
performance.

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/KinsprideDuah/BMEmapping/issues).

## Author

Kinspride Duah

## License

MIT + file LICENSE
