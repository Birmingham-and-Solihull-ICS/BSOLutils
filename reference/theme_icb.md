# Automatic ggplot2 ICB theme

A theme designed to complement the ICB colour palette. Uses a light,
neutral background and minimalist styling.

## Usage

``` r
theme_icb(base_size = 12, base_family = "Open Sans")
```

## Arguments

- base_size:

  Base font size.

- base_family:

  Base font family.

## Value

A ggplot2 theme object.

## Examples

``` r
library(ggplot2)
ggplot(mtcars, aes(mpg, wt)) +
  geom_point(aes(colour = mpg)) +
  scale_colour_icb(discrete = FALSE) +
  theme_icb()

```
