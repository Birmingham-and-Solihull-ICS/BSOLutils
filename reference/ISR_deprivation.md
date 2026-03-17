# Calculate Indirectly Standardised ratio for deprivation quintiles, standardised by age and/or sex

This function uses negative binomial regression, to counter
overdispersion, to adjust data by calculating and expected rate of
events, which is compared to the observed event: observed / expected. A
ratio of 1 means observed = expected, \<1 means observed \< expected,
and \>1 means observed \> expected.

## Usage

``` r
ISR_deprivation(.dt, age = TRUE, sex = FALSE)
```

## Arguments

- .dt:

  A data.frame contain data you need to calculate the table with the
  following column names:

  - numerator - the count of events in question

  - denominator - the population or group at risk, or group/segment

  - imd_code - numeric encoded IMD quintile, with unknown coded as 999

  - age_group_code - age bands used for standardisation. Bin/age band
    size is not important provided it is consistent.

  - sex_group_code - numeric encoded sex

- age:

  TRUE/FALSE whether to include age in the standardisation.

- sex:

  TRUE/FALSE whether to include sex in the standardisation.

## Value

a data.frame with

## Examples

``` r
data(ISR_example)
ISR_deprivation(ISR_example)
#> Waiting for profiling to be done...
#>               imd_quintile     ratio   lowerCI  upperCI
#> imd_code_f2              2 1.0342909 0.9213015 1.161235
#> imd_code_f3              3 1.0805487 0.9650033 1.210195
#> imd_code_f4              4 1.0284717 0.9190492 1.151211
#> imd_code_f5              5 1.0600902 0.9486446 1.185006
#> imd_code_f999          999 0.9437151 0.8443600 1.055087
```
