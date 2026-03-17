# Calculate the between group standard error (tau2) using a dispersion factor, and within

Function to calculate between group variance (tau2) to add to within
group variance (S2). NOTE: the S input, is the within group standard
error (the square root of the variance).

## Usage

``` r
tau_func(n, phi, S)
```

## Arguments

- n:

  The number of groups for data items, e.g. hospitals trusts that
  z-scores are calculated at.

- phi:

  The dispersion ratio, where \> 1 means overdispersion

- S:

  Standard error (within cluster, calculated in z-score process)

## Value

A numeric Tau2 (between group variance) value
