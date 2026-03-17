# Exact Poisson limit for an SMR / small count (Ulm)

Exact poisson limit used for small counts and SMRs, based on Poisson
distribution. This uses Ulm's method, exploiting the link to the chisq
distribution. This approach is used for un adjusted limits in the
FunnelPlotR package.

## Usage

``` r
exact_SMR_ci(o, n, ci = 0.95)
```

## Arguments

- o:

  observed or numerator value

- n:

  expected or denominator value

- ci:

  confidence interval coverage required. Default is 0.95 for 95\\
  confidence interval

## Value

A vector o/n, lower confidence interval limit, and upper confidence
interval limit

## Examples

``` r
# For a rate of 50 / 100
exact_SMR_ci(50, 120)
#>        Rate  LowerCI   UpperCI
#> 1 0.4166667 0.309258 0.5493231
```
