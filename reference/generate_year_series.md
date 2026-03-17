# Generate Year Windows

Creates a sequence of year windows of fixed length.

## Usage

``` r
generate_year_series(
  min_year,
  max_year,
  span_years = 3L,
  overlapping = TRUE,
  strict = TRUE
)
```

## Arguments

- min_year:

  Integer. First year in the range.

- max_year:

  Integer. Last year in the range.

- span_years:

  Integer \>= 1. Window size in years.

- overlapping:

  Logical. If TRUE (default), uses sliding windows. If FALSE, uses
  non-overlapping windows.

- strict:

  Logical. If TRUE, windows must be fully inside the min_year:max_year
  range. If FALSE, partial windows at the end are allowed (for
  non-overlapping mode).

## Value

A data.frame with columns: - from: starting year of window - to: ending
year of window - k: span_years

## Examples

``` r
generate_year_series(2014, 2024, 3)
#>   from   to k
#> 1 2014 2016 3
#> 2 2015 2017 3
#> 3 2016 2018 3
#> 4 2017 2019 3
#> 5 2018 2020 3
#> 6 2019 2021 3
#> 7 2020 2022 3
#> 8 2021 2023 3
#> 9 2022 2024 3
generate_year_series(2014, 2024, 3, overlapping = FALSE)
#>   from   to k
#> 1 2014 2016 3
#> 2 2017 2019 3
#> 3 2020 2022 3
```
