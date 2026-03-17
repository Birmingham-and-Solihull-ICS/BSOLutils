# Wilson score binomial confidence interval for proportions

A Wilson score-based CI calculation for a proportion. Based on guidance
by PHE

## Usage

``` r
prop_ci(o, n, ci = 0.95)
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

A data.frame with Rate = o/n, lower confidence interval limit, and upper
confidence interval limit

## Examples

``` r
prop_ci(50,120)
#>        Rate   LowerCI   UpperCI
#> 1 0.4166667 0.3323835 0.5061197
```
