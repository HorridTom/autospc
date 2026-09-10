# Plot SPC charts at successive stages of a series

`facet_stages()` analyses the same series in stages, each time using
more of it, and plots the results side by side - one facet per stage.
Each facet is what
[`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md)
would have drawn from the data available at that point, so the set of
them shows how the chart, and the control limits, developed as the data
arrived.

## Usage

``` r
facet_stages(data, split_at, plot_chart = TRUE, ..., split_rows = deprecated())
```

## Arguments

- data:

  A data frame. For column requirements by chart type, see
  [`vignette("data-requirements", package = "autospc")`](https://horridtom.github.io/autospc/articles/data-requirements.md).

- split_at:

  A vector of positions in the analysed series, which holds one point
  per subgroup in x order, specifying the stages to display results at.
  Names specify facet strip labels.

- plot_chart:

  Boolean specifying whether to plot the chart. If not, the
  subgroup-aggregated data is returned with centre line, control limits
  and other analytic output appended as columns.

- ...:

  Arguments passed to
  [`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md)

- split_rows:

  **\[deprecated\]** Use `split_at` instead. The positions it takes are
  now counted in the analysed series rather than in the data as
  supplied.

## Value

With `plot_chart = TRUE` (the default), an `autospc_plot`: one ggplot,
faceted by stage, which also carries the analysed chart behind each
facet and the parameters it was drawn with.

With `plot_chart = FALSE`, a data frame holding every stage, with
`stage` saying which each row belongs to.

## Examples

``` r
# Show progression of C' chart for count of monthly attendances over time
facet_stages(
  ed_attendances_monthly,
  split_at = c(30L, 60L, 90L),
  chart_type = "C'",
  x = month_start,
  y = att_all,
  x_break = 365
)

```
