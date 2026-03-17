# Derive SQL Data Types

Derive equivalent SQL data types from an R dataframe

## Usage

``` r
derive_sql_data_types(df, buffer = 0)
```

## Arguments

- df:

  R dataframe

- buffer:

  use to increase size of varchar fields - default is 0.

## Value

Creates a vector of column names and derived SQL data types. Can be used
within data types parameter of dbWriteTable() from DBI package.

## Examples

``` r
derive_sql_data_types(mtcars)
#>     mpg     cyl    disp      hp    drat      wt    qsec      vs      am    gear 
#> "float" "float" "float" "float" "float" "float" "float" "float" "float" "float" 
#>    carb 
#> "float" 
```
