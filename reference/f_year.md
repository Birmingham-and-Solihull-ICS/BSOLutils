# Financial year from date

Financial year from date

## Usage

``` r
f_year(d, short = FALSE)
```

## Arguments

- d:

  a date, or vector of dates.

- short:

  TRUE/FALSE value, where TRUE returns 25/26 format, and FALSE returns
  2025/26. Default is FALSE.

## Value

a character value, or vector, of the form 24/25 for fiscal year 2024/25

## Examples

``` r
f_year(Sys.Date())
#> [1] "2025/26"
f_year(Sys.Date(), short = TRUE)
#> [1] "25/26"
```
