# The analysis behind an autospc_plot

The result of each chart the plot holds, in one table.

## Usage

``` r
# S3 method for class 'autospc_plot'
as.data.frame(x, ...)
```

## Arguments

- x:

  An `autospc_plot`.

- ...:

  Ignored, for consistency with the generic.

## Value

A data frame.

## Details

An XmR pair is one analysis of one series shown as two charts, so it
goes out wide: the moving range and its limits join the X columns as
`mr`, `amr`, `url` and `lrl`. Several charts of the same type are
separate analyses, so they stack long, with `stage` identifying which
each row came from - the same column `facet_stages(plot_chart = FALSE)`
returns, and the same name as the facet variable, because
[`facet_stages()`](https://horridtom.github.io/autospc/reference/facet_stages.md)
is the only thing that produces several charts of one type.

This is the analytic result, not the table `autospc(plot_chart = FALSE)`
returns: it carries the columns the algorithm produced, and not the
columns `add_plot_columns()` adds for drawing.
