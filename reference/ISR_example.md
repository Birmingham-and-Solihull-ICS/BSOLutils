# ISR example dataset

An artificial dataset with 6 age bads, 2 sex bands and 5 IMD quintiles
plus and unknown coded as 999. This is here to test and demonstrate the
function

## Usage

``` r
data(ISR_example)
```

## Format

### `ISR_example`

A data frame with 192 rows and 5 columns:

- age_group_code:

  Numerically coded age-bands

- sex_group_code:

  Numerically coded sex groups

- imd_code:

  Index of multiple deprivation qunitiles, 1-5, 999 = unknown

- numerator:

  Numerator of a given indicator

- denominator:

  Denominator of a given indicator

## Source

Constructed from code
<https://github.com/Birmingham-and-Solihull-ICS/BSOLutils/blob/main/data-raw/build_example_data.R>

## Examples

``` r
data(ISR_example)
```
