# Create a formatted gt table for ISR deprivation output

This function converts the prepared output from
[`create_ISR_deprivation_output()`](https://birmingham-and-solihull-ics.github.io/BSOLutils/reference/create_ISR_deprivation_output.md)
into a formatted `gt` table for reporting. It applies consistent styling
to improve readability and interpretation of deprivation comparisons
against IMD 1.

## Usage

``` r
ISR_deprivation_table(data)
```

## Arguments

- data:

  A data frame produced by
  [`create_ISR_deprivation_output()`](https://birmingham-and-solihull-ics.github.io/BSOLutils/reference/create_ISR_deprivation_output.md)
  containing the following columns:

  IMD Quintile

  :   Deprivation quintile group.

  Ratio

  :   Rate ratio compared with IMD 1.

  Lower CI

  :   Lower bound of the confidence interval.

  Upper CI

  :   Upper bound of the confidence interval.

  Interpretation

  :   Narrative interpretation of the comparison with IMD 1.

  Statistical Significance

  :   Significance category used for cell shading.

## Value

A `gt_tbl` object that can be printed in Quarto, R Markdown, or other
reporting workflows.

## Details

The function:

- Adds alternating row shading

- Centres all columns

- Sets a wider width for the Interpretation column

- Highlights rate ratios above 1 in red

- Highlights rate ratios below 1 in green

- Applies background colours to the Statistical Significance column

- Bolds all column headers

The `Ratio` column is styled as follows:

- Values greater than 1 are shown with red text and a light red
  background, indicating a higher rate than IMD 1.

- Values less than 1 are shown with dark green text and a light green
  background, indicating a lower rate than IMD 1.

- Values equal to 1 retain the default table styling, indicating no
  difference from IMD 1.

The `Statistical Significance` column is shaded according to category:

- `"Significantly higher than IMD 1"` = light red

- `"Significantly lower than IMD 1"` = light green

- `"Not statistically significant"` = light grey

## See also

[`ISR_deprivation`](https://birmingham-and-solihull-ics.github.io/BSOLutils/reference/ISR_deprivation.md),
[`create_ISR_deprivation_output`](https://birmingham-and-solihull-ics.github.io/BSOLutils/reference/create_ISR_deprivation_output.md)

## Examples

``` r
data(ISR_example)

raw_output <- ISR_deprivation(ISR_example)
#> Waiting for profiling to be done...
table_data <- create_ISR_deprivation_output(raw_output)

ISR_deprivation_table(table_data)


  









IMD Quintile
```

Ratio

Lower CI

Upper CI

Interpretation

Statistical Significance

2

1.03

0.92

1.16

3.0% ↑ Higher than IMD 1 (95% CI: -8.0% to 16.0%)

Not statistically significant

3

1.08

0.97

1.21

8.0% ↑ Higher than IMD 1 (95% CI: -3.0% to 21.0%)

Not statistically significant

4

1.03

0.92

1.15

3.0% ↑ Higher than IMD 1 (95% CI: -8.0% to 15.0%)

Not statistically significant

5

1.06

0.95

1.19

6.0% ↑ Higher than IMD 1 (95% CI: -5.0% to 19.0%)

Not statistically significant

999

0.94

0.84

1.06

6.0% ↓ Lower than IMD 1 (95% CI: -16.0% to 6.0%)

Not statistically significant
