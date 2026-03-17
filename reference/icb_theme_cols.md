# ICB Theme Colours

This function returns the colours that are used in the ICB theme as a
named vector of RGB values in hexadecimal form.

## Usage

``` r
icb_theme_cols(
  ...,
  palette = c(NA, "cluster", "bsol", "alternative", "ics_blue", "ics_orange",
    "ics_green", "ics_purple", "ics_navy")
)
```

## Arguments

- ...:

  individual colours that you wish to get the values of

- palette:

  a name of a palette to select the colours of

## Value

a named vector of RGB colours in hexadecimal form

## Details

If no arguments are passed to the function, then all of the colours are
returned. If only specific colours are required, then the names of the
colours that are required can be passed as strings to the function, and
only those colours will be returned.

If a "palette" is selected then no arguments should be passed to ...

Only one palette can be selected at a time.

## Examples

``` r
# show all of the colours
icb_theme_cols()
#>               green          light_blue              orange           deep_navy 
#>           "#8cedab"           "#4fbff0"           "#fc8700"           "#031d44" 
#>              purple            nhs_blue         light_slate            charcoal 
#>           "#b88ce3"           "#005EB8"           "#b2b7b9"           "#2c2825" 
#>               white      cluster_green1      cluster_green2   cluster_turquoise 
#>           "#ffffff"           "#28A745"           "#1FA766"           "#1AA6A0" 
#>   cluster_lightblue        cluster_blue    cluster_darkblue      cluster_purple 
#>           "#1A92C7"           "#2E6FCA"           "#4E54B0"           "#6C4993" 
#>  cluster_purplepink cluster_pinkorgange      cluster_orange 
#>           "#8C4475"           "#B8484E"           "#F16522" 
# or, just show some colours
icb_theme_cols("green", "orange", "deep_navy", "nhs_blue")
#>     green    orange deep_navy  nhs_blue 
#> "#8cedab" "#fc8700" "#031d44" "#005EB8" 
# or, select a single palette
icb_theme_cols(palette = "ics_orange")
#>    orange     white deep_navy 
#> "#fc8700" "#ffffff" "#031d44" 
```
