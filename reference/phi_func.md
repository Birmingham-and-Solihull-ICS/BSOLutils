# Calculate overdispersion ratio of z-scores

Internal function to perform the transformations for data types.

## Usage

``` r
phi_func(n, zscores)
```

## Arguments

- n:

  Single numeric value for the count of the number of groups (and
  therefore z-scores)

- zscores:

  Vector of z-scores z-scores to be used. Commonly, this might be
  'winsorised' first to remove impact of extreme outliers.

## Value

A numeric phi value

## Examples

``` r
phi_func(3, c(1.3,0.75, 1.5))
#> [1] 1.500833
```
