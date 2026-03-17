# Binomial thinning

Binomial thinning

## Usage

``` r
thin_binomial(x, p)
```

## Arguments

- x:

  vector of counts (e.g. arrivals per day)

- p:

  vector of probabilities (e.g. arrivals get admitted)

## Value

vector of thinned counts

## Examples

``` r
set.seed(2024)
x_counts <- c(0, 3, 10, 5, 2)
p        <- 0.4
thin_binomial(x_counts, p)
#> [1] 0 2 3 2 1
```
