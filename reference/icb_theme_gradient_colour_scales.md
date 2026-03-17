# ICB Gradient colour Scale

Generates a colour gradient

## Usage

``` r
scale_colour_icb(
  palette = c("cluster", "bsol", "alternative", "ics_blue", "ics_orange", "ics_green",
    "ics_purple", "ics_navy"),
  discrete = TRUE,
  reverse = FALSE,
  ...
)

scale_color_icb(
  palette = c("cluster", "bsol", "alternative", "ics_blue", "ics_orange", "ics_green",
    "ics_purple", "ics_navy"),
  discrete = TRUE,
  reverse = FALSE,
  ...
)

scale_fill_icb(
  palette = c("cluster", "bsol", "alternative", "ics_blue", "ics_orange", "ics_green",
    "ics_purple", "ics_navy"),
  discrete = TRUE,
  reverse = FALSE,
  ...
)
```

## Arguments

- palette:

  The name of palette to use

- discrete:

  Optional: boolean to indicate that this scale is a discrete scale.
  Defaults to TRUE

- reverse:

  Optional: boolean to reverse the direction of the scale. Defaults to
  FALSE

- ...:

  additional arguments passed to the ggplot functions

## Value

a scale object to be used with a ggplot object
