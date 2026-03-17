# Byar's confidence interval

Byar's confidence interval for counts, crude rates or indirectly
standardised ratios

## Usage

``` r
byars_ci(o, n, ci = 0.95)
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
byars_ci(50, 120)
#>        Rate   LowerCI   UpperCI
#> 1 0.4166667 0.3092342 0.5493346
```
