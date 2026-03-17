# Prepare ISR deprivation output for reporting

This function enriches the output from
[`ISR_deprivation()`](https://birmingham-and-solihull-ics.github.io/BSOLutils/reference/ISR_deprivation.md)
by adding interpretation and statistical significance columns. The
resulting dataset can be used for reporting tables, for example when
creating formatted tables with
[`ISR_deprivation_table()`](https://birmingham-and-solihull-ics.github.io/BSOLutils/reference/ISR_deprivation_table.md).

## Usage

``` r
create_ISR_deprivation_output(data)
```

## Arguments

- data:

  A data frame produced by
  [`ISR_deprivation()`](https://birmingham-and-solihull-ics.github.io/BSOLutils/reference/ISR_deprivation.md)
  containing the following columns:

  imd_quintile

  :   IMD quintile (2, 3, 4, 5, and 999 representing All Persons).

  ratio

  :   Rate ratio comparing each IMD quintile against IMD 1 (reference
      group).

  lowerCI

  :   Lower bound of the confidence interval for the rate ratio.

  upperCI

  :   Upper bound of the confidence interval for the rate ratio.

## Value

A data frame containing the following columns:

- IMD Quintile:

  Deprivation quintile group.

- Ratio:

  Rate ratio compared with IMD 1.

- Lower CI:

  Lower bound of the confidence interval.

- Upper CI:

  Upper bound of the confidence interval.

- Interpretation:

  Text interpretation describing the percentage difference from IMD 1
  and the associated confidence interval.

- Statistical Significance:

  Indicates whether the difference compared with IMD 1 is statistically
  significant.

## Details

The function:

- Rounds rate ratios and confidence intervals to two decimal places

- Calculates the percentage difference from the reference group (IMD 1)

- Generates an interpretation sentence describing the comparison with
  IMD 1

- Determines statistical significance based on whether the confidence
  interval crosses 1

Statistical significance is determined as follows:

- If the lower confidence interval is greater than 1, the rate is
  considered significantly higher than IMD 1.

- If the upper confidence interval is less than 1, the rate is
  considered significantly lower than IMD 1.

- Otherwise, the difference is not statistically significant.

## See also

[`ISR_deprivation`](https://birmingham-and-solihull-ics.github.io/BSOLutils/reference/ISR_deprivation.md),
[`ISR_deprivation_table`](https://birmingham-and-solihull-ics.github.io/BSOLutils/reference/ISR_deprivation_table.md)

## Examples

``` r
data(ISR_example)

raw_output <- ISR_deprivation(ISR_example)
#> Waiting for profiling to be done...

report_table <- create_ISR_deprivation_output(raw_output)
```
