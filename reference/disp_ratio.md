# Dispersion ratio

This function calculates the dispersion ratio of fitted models. Aimed at
binomial and Poisson family models where The fixed variance assumption
commonly leads to over- dispersion in the real world.

## Usage

``` r
disp_ratio(model, ...)
```

## Arguments

- model:

  a fitted regression model object that has a relevant pearson residual
  to be extracted

- ...:

  dots

## Value

A dispersion ratio, where 1 is equidispersion (as expected), \> 1 is
over-dispersion and \<1 is under-dispersion

## Examples

``` r
library(NHSRdatasets)
data(LOS_model)

mod1 <- glm(Death ~ Age * LOS, data=LOS_model, family="binomial")

disp_ratio(mod1)
#> [1] 1.13377
```
