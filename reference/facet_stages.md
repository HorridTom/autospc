# facet_stages

facet_stages

## Usage

``` r
facet_stages(df, split_rows, plotChart = TRUE, ...)
```

## Arguments

- df:

  A data frame. For an XMR, C or C' chart, must have columns for:

  - the subgrouping variable, to be plotted on the horizontal axis, (x);

  - the variable of interest to be plotted on the vertical axis (y);

  - and optionally, a title and subtitle for the plot.

    
  For a P or P' chart, must have columns for:

  - the subgrouping variable, to be plotted on the horizontal axis, (x);

  - the total count or denominator (n);

  - the count meeting criteria, or numerator (y);

  - and optionally, a title and subtitle for the plot.

- split_rows:

  A vector of row numbers specifying the stages to display results at.
  Names specify facet strip labels.

- plotChart:

  Boolean specifying whether to plot the chart. If not, the data is
  returned with centre line, control limits and other analytic output
  appended as columns.

- ...:

  Arguments passed to
  [`plot_auto_SPC()`](https://horridtom.github.io/autospc/reference/plot_auto_SPC.md)

## Value

Faceted plot showing results of
[`plot_auto_SPC()`](https://horridtom.github.io/autospc/reference/plot_auto_SPC.md)
at different stages as specified by split_rows

## Examples

``` r
# Show progression of C' chart for count of monthly attendances over time
facet_stages(
  ed_attendances_monthly,
  split_rows = c(30L, 60L, 90L),
  chartType = "C'",
  x = Month_Start,
  y = Att_All, 
  x_break = 365
)
#> Registered S3 methods overwritten by 'ggpp':
#>   method                  from   
#>   heightDetails.titleGrob ggplot2
#>   widthDetails.titleGrob  ggplot2

```
