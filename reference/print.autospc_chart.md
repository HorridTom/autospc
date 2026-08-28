# Print a summary of the analysis

An `autospc_chart` is the analysis, not the drawing, so printing one
summarises what the algorithm did: the calculation periods it formed,
where it re-established limits, which points it excluded, and why it
stopped.
[`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md)
returns an `autospc_plot`, which draws.

## Usage

``` r
# S3 method for class 'autospc_chart'
print(x, ...)
```

## Arguments

- x:

  An `autospc_chart`.

- ...:

  Ignored, for consistency with the generic.

## Value

`x`, invisibly.
