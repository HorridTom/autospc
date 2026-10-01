# Changelog

## autospc 0.2.0.9001

### U and U’ charts

`chart_type = "U"` draws a U chart of rates: a count of events `y` over
the area of opportunity `n` in which they could occur, such as
infections per 1,000 central line days. Its control limits vary with
`n`. `chart_type = "U'"` draws Laney’s U’ chart (Laney, 2002), whose
limits are also widened where the rates vary from subgroup to subgroup
by more than a Poisson count would. The chart plots `y / n`, so the unit
of `n` sets the unit of the rate, and `n` need not be a whole number.
Rows that share an `x` are summed into one subgroup. See
[`vignette("data-requirements")`](https://horridtom.github.io/autospc/articles/data-requirements.md)
for the data each needs.

### Documentation

- `mr_screen_max_loops` applies to the screening of moving ranges in C’
  and P’ charts as well as X charts, and now to U’ charts too. Its
  documentation said only X charts; it now says which, and that 0 turns
  the screening off.
- The `autospc.rounded_constants` option documentation now names C’ and
  P’ charts, which use d2, and D4 in their screening of moving ranges.

## autospc 0.2.0

This release adds the XbarS chart, improves handling of missing values,
and returns the same table columns for a given chart type whatever the
(valid) data. It also announces that the default of `max_exclusions`
changes to 0 in 0.3.0. It brings together development versions
0.1.0.9001 to 0.1.0.9028.

### Lifecycle changes

#### Breaking changes

##### The table `plot_chart = FALSE` returns

- **The analysed values have a column of their own.** `series` holds the
  values the algorithm analyses and the chart plots: the `y` values as
  supplied on a C, C’ or X chart, the moving ranges on an MR chart, and
  percentages on a P or P’ chart. **`y` now holds what the caller
  supplied**, aggregated where the chart type aggregates; on an MR chart
  it held the moving ranges, and on a P or P’ chart the percentages.
  **`y_numerator` is gone**: on a P or P’ chart the count is now `y`.
  Code using the analysed values or `y_numerator` from an MR, P or P’
  chart needs `series` in place of `y`, and `y` in place of
  `y_numerator`. The columns are ordered `x`, `series`, `y`, and then
  the denominator `n` where the chart type has one.

- **`autospc(plot_chart = FALSE)` and
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) on a
  plot return the same table.**

  - `cl_label`, `annotation_level` and `annotation_curvature` are no
    longer included. They are properties of the drawing rather than of
    the analysis.
  - `highlight` now marks only the rules a point breaks. The rule
    highlight of an excluded point was overwritten by the exclusion
    mark, masking the rule it broke. The plot is unchanged.
  - The floating median and the rows `extend_limits_to` adds are part of
    the analysis, so
    [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) on a
    plot carries them.
  - `show_limits` no longer changes the table.
  - A new column, `limit_extension`, is TRUE on the rows
    `extend_limits_to` adds beyond the end of the data and FALSE on
    every other row. Those rows no longer copy the last subgroup’s
    denominator and numerator; they hold the limits, the period they
    continue, and nothing else.

- **A given chart type returns the same columns, in the same order and
  of the same types, whatever the data and the other arguments.** A
  series too short to establish limits used to be returned with only the
  columns the data preparation had added; it now has every column, with
  NA where the analysis would have recorded a result. `limit_extension`
  is FALSE there rather than NA. An XmR chart’s two halves are joined
  whether or not limits were established. The `median` column is always
  returned, holding no value where no floating median was drawn.

- **Every chart type returns a new column, `sd_estimate`**, an estimate
  of the standard deviation of a single observation on the same scale as
  the centre line - for example `sqrt(cl)` for a C chart, and the mean
  moving range over d2 for an X chart.

- **P and P’ charts no longer return `constant`, `pbar`, `ucl_display`
  and `lcl_display`.** They held working values from extending limits
  over a display period. `pbar` is `cl`, and the limits at any
  denominator `n` sit `3 * sd_estimate / sqrt(n)` either side of the
  centre line.

##### Results that change

- **Missing values.** The analysis now proceeds as though a point with
  no `y` were not there, rather than walking over it as a row. This
  changes results for any series that has one.

  - A calculation period now holds `period_min` points, not `period_min`
    rows. Where a series had missing values inside the first period,
    limits were previously calculated from fewer points than asked for.
  - Control limits now carry across a gap, as the centre line already
    did.
  - No limits are drawn before the first point or after the last.
  - A missing point no longer silently splits a run, which had made a
    shift rule break disappear. The new argument `na_ends_run` controls
    this, and defaults to `TRUE`, the previous behaviour. A missing
    point may have continued the run before it or been on the other side
    of the centre line, and the data cannot say which: `TRUE` minimises
    the risk of a false positive shift rule break arising from missing
    data, `FALSE` minimises the risk of a false negative.
  - An MR chart now shows its control limits at the first point as well
    as its centre line. The limits themselves are unchanged.
  - On a P or P’ chart, the limits at a point with missing `y` are
    calculated from that point’s own denominator where the data supplied
    one, and the denominator is reported in the table. Where the
    denominator is missing or zero the limits are drawn at the mean
    denominator of the point’s period, leaving out excluded points and
    subgroups with no observation.
  - The period columns are now filled in at a point with missing `y`,
    and the centre line and limits at such a point inside a display
    period match the rest of that period.

- **A point on the centre line no longer ends the run it sits in.** A
  point within `centre_line_tolerance` of the centre line neither
  commences a run, ends one, nor counts towards the length of the one it
  sits in, which is the conventional treatment of a point that is
  neither above the line nor below it.

- **The exact values of d2 and D4 are used.** They were written into the
  code as the published values to three decimal places, 1.128 and 3.267.
  The limits of an X, C’ or P’ chart sit 0.034% closer to the centre
  line than before, and an MR chart’s upper limit is 0.014% lower. These
  are the ratios of the exact to the rounded constants. Where a moving
  range or a point lies within that margin of a limit, the screening of
  moving ranges or a rule 1 break can differ as well. Centre lines, and
  C and P charts, are unchanged.
  `options(autospc.rounded_constants = TRUE)` restores the rounded
  values.

- **Control limits are constrained to the range the statistic can
  take**. A P or P’ chart’s upper limit is constrained to at most 100%,
  previously this applied only in display periods. A percentage chart’s
  vertical axis follows the limits and the points where they reach
  outside 0 to 100. This only affected uninformative limits, and data
  points outside the valid range.
  `options(autospc.constrain_limits = FALSE)` draws unconstrained
  limits, where the calculation puts them.

- **Limits extended beyond the data.**

  - The extension starts where the next subgroup would have been: one
    median gap between consecutive subgroups past the last, capped at
    half the extension, rather than one unit of `x`. On a chart whose
    `x` is spaced about one unit apart nothing changes; on monthly
    `Date` data the first row of the extension moves from one day past
    the last subgroup to one month past it.
  - A P’ chart’s extended limits use the standard deviation estimate of
    the period they extend. Previously, they were recalculated for the
    extension from that period’s data, with the z score of each point in
    the period standardised at the period’s mean denominator rather than
    at the point’s own, and with the moving ranges formed differently
    around a subgroup with no observation. Where the period’s
    denominators vary, the extended limits were previously too wide, and
    are now narrower.
  - On a P chart, the centre line of extension rows, and the limits
    around it, are those of the period being extended. Previously, Where
    a point had been excluded from that period, the extension
    recalculated the centre line from averaged denominators, which gave
    a different value.
  - The extension of a P or P’ chart uses the final calculation period’s
    mean denominator, leaving out excluded points and subgroups with no
    observation.

  Only rows added by `extend_limits_to` are affected by these changes,
  so they do not affect centre lines, control limits or rule breaks
  within the data.

##### Data that is refused

- **X, MR and XMR charts reject a repeated `x`.** Each point on these
  charts is one row, so a repeated `x` has no place to be plotted. It
  was previously accepted, and multiplied in the output table. The error
  names the values that are repeated. The other chart types combine the
  rows that share an `x` into one subgroup, as before.

- **A P or P’ chart refuses counts it cannot plot.** `y` must be a count
  from 0 to `n`, and `n` cannot be negative. The error names the rows at
  fault and their values, up to five of them. A subgroup with `n = 0`
  and `y = 0` is still drawn as a gap. A series whose counts are valid
  is unaffected.

##### Removed

- `autospc(override_annotation_dist)` and
  `autospc(override_annotation_dist_P)`, defunct since 0.1.0, are gone
  from the signature. Supplying one is now R’s own “unused argument”
  error. Use `upper_annotation_sf` and `lower_annotation_sf` instead:
  `override_annotation_dist = 10` becomes `upper_annotation_sf = 1.1`.

#### Deprecations

- **The default of `max_exclusions` will change from 3 to 0 in 0.3.0**,
  so that points are excluded from a period’s limits only where you ask
  for it. Wherever points are excluded the change moves the limits, and
  can change where they are re-established. Until then,
  [`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md)
  and
  [`facet_stages()`](https://horridtom.github.io/autospc/reference/facet_stages.md)
  warn when `max_exclusions` is left unset and the analysis excluded at
  least one point. Set `max_exclusions = 3` to keep the current results,
  or `max_exclusions = 0` to adopt the new default now; either stops the
  warning. The warning has the class
  `"autospc_max_exclusions_default_warning"` (#282).

- **`facet_stages(split_rows)` is deprecated in favour of `split_at`**,
  which counts points in the analysed series - one point per subgroup,
  in `x` order - rather than rows of the data as supplied. Where the
  data already holds one row per subgroup, in `x` order, nothing
  changes. Supplying `split_rows` warns, and its value is taken as
  `split_at`. It will be removed in 0.3.0.

- **Setting `no_regrets = TRUE` with `overhanging_reversions = FALSE` is
  deprecated**, and will be an error in 0.3.0. `no_regrets` requires
  consideration of overhanging reversions, so the combination does not
  make sense. It still warns and changes `overhanging_reversions` to
  TRUE.

- **`autospc(show_mr)`, `facet_stages(show_mr)` and
  `autospc(write_table)`**, deprecated since 0.1.0, still warn, and will
  be removed in 0.3.0.

### New features

- **XbarS charts.** `chart_type = "XbarS"` draws an Xbar chart of
  subgroup means above an S chart of subgroup standard deviations, as
  specified e.g. in Provost and Murray, *The Health Care Data Guide*.
  `chart_type = "Xbar"` and `chart_type = "S"` draw either chart on its
  own, and
  [`facet_stages()`](https://horridtom.github.io/autospc/reference/facet_stages.md)
  facets an XbarS request as its Xbar chart.

  - Data may have one row per measurement, with `y` the measurement, or
    rows that each summarise some measurements, with `y` their mean, `n`
    their number and the new argument `s` their sample standard
    deviation. In either form, rows that share an `x` are combined into
    one subgroup, so e.g. with `x = month`, data with one row per
    practice per month gives one subgroup per month. See
    [`vignette("data-requirements")`](https://horridtom.github.io/autospc/articles/data-requirements.md).
  - As is standard, the Xbar centre line is the mean of the subgroup
    means weighted by subgroup size, the S centre line is the mean of
    the subgroup standard deviations weighted in the same way, and the
    limits of both charts vary with each subgroup’s size. The S chart’s
    lower limit is zero for subgroups of fewer than six.
  - A subgroup of one is plotted on the Xbar chart and counts towards
    its centre line, but has no control limits. It has no standard
    deviation, so it is not plotted on the S chart.
  - The two charts re-establish their limits independently, as the X and
    MR charts of an XMR chart do.
  - `plot_chart = FALSE` returns the Xbar chart’s table with the S
    chart’s columns beside it: `subgroup_s`, `s_cl`, `s_ucl` and
    `s_lcl`. The Xbar table carries `n`, `s` and `sbar`.

- **`override_y_lim` takes the lower end of the vertical axis as well.**
  A single number still specifies the upper end. A vector of two numbers
  gives the lower and upper ends, and `NA` in either position leaves
  that end as the chart would have set it. The axis now zooms rather
  than clips, so nothing is dropped from the drawing: a limit or a
  centre line annotation outside the axis sits outside the panel. A
  range that would leave a data point outside the axis is an error.

- **`aggregation_na_rm`** controls what a missing (`NA`) observation
  does to the subgroup it is aggregated into. `FALSE`, the default,
  makes the whole subgroup missing, as before. `TRUE` discards the
  observation and forms the subgroup from the rest.

- **Three package options**, documented in `?autospc-package`:
  `autospc.rounded_constants`, `autospc.constrain_limits` and
  `autospc.warn_missing_x`.

- **Arguments are checked.** `title`, `subtitle`, `override_x_title`,
  `override_y_title` and `log_file_path` must be a single string or
  NULL; `r1_col` and `r2_col` must be a colour; `x_date_format` must
  hold at least one `%` code. A column named by `x`, `y` or `n` that is
  not in the data is named in the error along with the argument that
  named it, and a chart with no `x` says so.

### Bug fixes

- [`facet_stages()`](https://horridtom.github.io/autospc/reference/facet_stages.md)
  accepts a paired chart type (`"XMR"` or `"XbarS"`) held in a variable
  or given as an expression, e.g. `chart_type = my_type`. It previously
  failed with “No autospc_chart class for chart_type” (#302).

- A floating median is drawn on a series too short for control limits,
  and with `show_limits = FALSE`. `floating_median = "auto"` no longer
  errors where a point in the median window is missing.

- A series shorter than `floating_median_n` no longer warns “no
  non-missing arguments to max”, and draws no floating median.
  `floating_median = "yes"` warns when the series is too short for the
  median it asked for.

- [`facet_stages()`](https://horridtom.github.io/autospc/reference/facet_stages.md)
  no longer fails when no stage has enough points for control limits, or
  with `show_limits = FALSE`, or when asked for a single stage. A split
  point beyond the end of the series is taken as the last point, with a
  warning, rather than repeating a stage.
  `facet_stages(plot_chart = FALSE)` always returns a `stage` column.

- In a faceted chart where some stages have limits and some do not, the
  points of a stage without limits are drawn black rather than the grey
  of an excluded point.

- A chart drawn without control limits honours `x_break`,
  `x_date_format` and `x_pad_end`.

- The rows `extend_limits_to` adds no longer change the type of the `x`
  and `y` columns, and name the period they continue where the series
  ends with a subgroup that has no observation.

- `plot_period` is missing on the rows before the first point and after
  the last, where it read `NANA`.

- Rows with no `x` are excluded before the analysis rather than after,
  with a warning that `options(autospc.warn_missing_x = FALSE)` turns
  off. One such row could previously add a subgroup of its own.

- The error raised when `extend_limits_to` is not beyond the end of the
  data names the argument, and is raised before the chart is computed.

- The `month_start` column of `ed_attendances_monthly` is the first day
  of each month; it was sometimes the last day of the month before.
  Charts drawn from this dataset shift by up to a day on the x axis. Its
  help page now lists its 7 columns correctly.

## autospc 0.1.0

### Lifecycle changes

#### Breaking changes

- Seven columns of the table `autospc(plot_chart = FALSE)` and
  `facet_stages(plot_chart = FALSE)` return have been renamed from
  camelCase to snake_case, so that every column of the table is named
  the same way:

  | Was              | Is now              |
  |------------------|---------------------|
  | `periodType`     | `period_type`       |
  | `breakPoint`     | `break_point`       |
  | `aboveOrBelowCl` | `above_or_below_cl` |
  | `runStart`       | `run_start`         |
  | `limitChange`    | `limit_change`      |
  | `periodStart`    | `period_start`      |
  | `plotPeriod`     | `plot_period`       |

  The values are unchanged. Code that reads these columns by name —
  filtering on `periodType == "calculation"`, for one — needs the new
  name. The columns that were already snake_case (`cl_change`,
  `cl_label`, `annotation_level`, `annotation_curvature`, `highlight`,
  `excluded`) are unaffected, as are `x`, `y`, `n`, `cl`, `ucl`, `lcl`
  and the moving range columns `mr`, `amr`, `url` and `lrl`.

- `autospc(override_annotation_dist)` and
  `autospc(override_annotation_dist_P)` are now defunct. They have
  warned since 0.0.0.9010; supplying either is now an error. Use
  `upper_annotation_sf` and `lower_annotation_sf` instead — the
  equivalent scale factor is `1 + 1/x`, so
  `override_annotation_dist = 10` becomes `upper_annotation_sf = 1.1`,
  and `lower_annotation_sf` defaults to its mirror image,
  `2 - upper_annotation_sf`. The two arguments remain in the signature
  so that the error can name their replacement, and will be removed in a
  later release.

  `upper_annotation_sf` and `lower_annotation_sf` apply to every chart
  type, so there is no replacement specific to P and P′ charts.

- `create_SPC_auto_limits_table()` is no longer exported. It was an
  internal step of
  [`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md)
  that had been made public without a documented reason, and holding it
  to a public interface was preventing the simplification of the
  package’s internals.

  [`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md)
  does the same work and is the supported way to do it. For the results
  as data rather than a plot, use `autospc(plot_chart = FALSE)`, which
  returns the same limits, rule breaks and period boundaries with the
  additional columns needed for plotting.

  If you were calling `create_SPC_auto_limits_table()` directly and
  `autospc(plot_chart = FALSE)` does not meet your needs, please open an
  issue at <https://github.com/HorridTom/autospc/issues> — we are happy
  to help you move across.

#### Deprecations

- `autospc(write_table)` is deprecated, and no longer writes a file.
  Save the results yourself instead: `autospc(plot_chart = FALSE)`
  returns them as a data frame, and
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) on a
  chart does the same. This is more flexible, giving you the choice of
  what type of file, how to save it, where to save it, etc.

  Note that there was also a bug that meant the feature did not work:
  `write_table = TRUE` failed. Supplying the argument now warns, and
  returns the results as `plot_chart = FALSE` would.

- `autospc(show_mr)` is deprecated. Use `chart_type` instead:
  `chart_type = "X"` draws the X chart on its own, which is what
  `show_mr = FALSE` did, and `chart_type = "XMR"` draws the pair.
  Supplying `show_mr` still works and still does what it did, but now
  warns.

  Note that the caption names the chart type, so a chart drawn with
  `chart_type = "X"` is captioned “X Shewhart Chart” where the same
  chart drawn with `chart_type = "XMR", show_mr = FALSE` was captioned
  “XMR Shewhart Chart”.

- `facet_stages(show_mr)` is deprecated for the same reason.
  [`facet_stages()`](https://horridtom.github.io/autospc/reference/facet_stages.md)
  has never drawn the moving range chart, so an `XMR` request is now
  faceted as an `X` chart — including its caption, which changes from
  “XMR Shewhart Chart” to “X Shewhart Chart”. Nothing else about the
  chart changes.

#### Other changes

- A chart has no x axis title unless you give one. The default was
  `"Day"`, whatever the x column held, so a chart of monthly or weekly
  data was labelled “Day” as well. Use `override_x_title` for a title of
  your own. The y axis is unchanged: where you give no
  `override_y_title` it still takes one from the chart type.

- `chart_type = "X"` draws the X chart on its own, without the moving
  range chart beneath it. It gives the same result as
  `chart_type = "XMR"` with `show_mr = FALSE`.

- [`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md)
  returns a plot object that is still a ggplot — printing, `ggsave()`
  and adding ggplot2 layers all work as before — and additionally
  carries the analysed chart it was drawn from.
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) on it
  returns the analysis.

  An XmR chart carries both halves of the pair, the X chart first and
  the moving range chart second.
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) on one
  returns them joined side by side, with the moving range and its limits
  as `mr`, `amr`, `url` and `lrl` — the same shape
  `autospc(plot_chart = FALSE)` returns.

  [`facet_stages()`](https://horridtom.github.io/autospc/reference/facet_stages.md)
  returns the same kind of object, carrying one analysed chart per facet
  in stage order. Where `split_rows` is named, the charts take those
  names. [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  on one stacks the facets, with `stage` saying which each row came from
  — the same column `facet_stages(plot_chart = FALSE)` returns.

- A `title` or `subtitle` column in the data is no longer repeated over
  the moving range chart of an XmR pair. The pair is one chart in two
  panels, so its title is drawn once, above the X chart. A title given
  as an argument was already drawn once; this makes the two agree.

- `autospc(plot_chart = FALSE, show_limits = FALSE)` now returns the
  four columns describing the periods — `limit_change`, `period_start`,
  `plot_period` and `cl_change` — which it previously returned only when
  `show_limits` was `TRUE`.

- `autospc(plot_chart = FALSE)` and `facet_stages(plot_chart = FALSE)`
  always return a plain data frame. Previously the class depended on the
  chart type: given a data frame, C and C′ charts returned a tibble and
  the other chart types returned a data frame; given a tibble, every
  chart type returned a tibble. The same now applies to the frames a
  chart object carries — the analysis in `$result$table` and the tables
  in `$history`. `$data_original`, which is the data as you passed it,
  is unchanged and keeps its class.

  Add
  [`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  to the result if you want a tibble.

- The analysed columns keep the type they were supplied with. `y`, and
  `n` for a P or P′ chart, were coerced to double inside the algorithm,
  so integer counts came back as doubles; they now come back as
  integers. The limits — `cl`, `ucl` and `lcl` — are doubles as before,
  whatever the counts they were computed from. Values are unchanged.

- Columns other than those a chart uses are now dropped consistently.
  Previously the aggregation step was skipped entirely when no `x` value
  was repeated, so extra columns survived into the output for a series
  with one row per subgroup and were dropped for one without. They are
  now dropped in both cases.

- `autospc(keep_candidate_tables)` is a new argument, `FALSE` by
  default. The algorithm considers a candidate calculation period at
  each point where it might re-establish the limits, and records each
  one it forms. Setting this to `TRUE` additionally records, for each
  candidate, the full table of limits it would have produced. It is off
  by default because those tables are several times the size of
  everything else the chart holds: for a 600-point chart that
  re-establishes its limits nine times, the chart object measured 111 KB
  with the default and 534 KB with `keep_candidate_tables = TRUE`.

- The default values of `period_min` and `max_exclusions` are now the
  integers `21L` and `3L`, where they were the doubles `21` and `3`.
  Both are counts of data points. Passing a double still works.

- A rounding warning is no longer given for data that is then rejected.
  A P or P′ chart given a numerator with fractional values and a
  denominator of the wrong type warned that it was rounding the
  numerator before erroring on the denominator; it now raises the error
  without the warning.

### Bug fixes

- [`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md)
  no longer warns where a column named `x`, `y` or `n` is present in the
  data and the matching argument is also given (#85). The warning said
  that the column named in the argument would be used, so it told the
  caller only that the package had done as it was asked. It warned even
  where the argument named the same column — `x = x` on data whose
  column is already called `x` — which is the case the issue reported.
  There were three of these, one each for `x`, `y` and `n`. All three
  have gone, including where the argument names a different column from
  the one already called `x`: the column named in the argument is the
  one used, and `$data_original` holds every column exactly as it was
  passed.

- `log_file_path` now writes one file per call, holding every chart the
  call analysed, with a `chart` column saying which each entry came
  from. An XmR run wrote the file twice — once for the X chart and once
  for the moving range chart, the second overwriting the first — so the
  X chart’s log was lost. A faceted run did the same once per facet,
  leaving only the last stage. For an XmR pair `chart` holds the chart
  type, `"X"` or `"MR"`; for a faceted chart it holds the stage, named
  from `split_rows` where it has names and numbered where it does not. A
  run of a single chart writes the same shape, with one value in that
  column.

  The log written to file is also a plain data frame now, where it was a
  rowwise tibble.

- The console log of the X chart of an XmR pair is headed `X` rather
  than `XMR`. It is the X chart’s log; the moving range chart’s log
  follows it, headed `MR`.

- Setting `no_regrets = TRUE` with `overhanging_reversions = FALSE`
  warns once per call. An XmR run warned twice, and a faceted run once
  per facet, because the check ran once per chart constructed.

- An x column of a type the chart cannot be drawn against — anything
  other than `Date`, `POSIXct`, numeric or integer — warns once per
  call. A faceted run warned once per facet and once more for the series
  as a whole.

- Data holding a column called `x`, `y` or `n` no longer prevents a
  different column being used for that argument.
  `autospc(data, x = month, y = count)`, where `data` also has a column
  called `x`, failed with `Names must be unique`; the column named in
  the argument is now used and the column called `x` is dropped, along
  with the other columns the analysis does not use. The data as supplied
  is unaffected.
  [`facet_stages()`](https://horridtom.github.io/autospc/reference/facet_stages.md)
  warned in the same situation and then failed the same way; it now
  behaves as
  [`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md)
  does.

- [`facet_stages()`](https://horridtom.github.io/autospc/reference/facet_stages.md)
  now labels its axes. It resolved the axis titles and then never passed
  them to the drawing, so a faceted chart had no axis labels at all,
  where the same data through
  [`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md)
  was labelled. A `title` or `subtitle` column in the data was dropped
  the same way and now reaches the chart. Titles given as arguments were
  unaffected and still win.

- [`facet_stages()`](https://horridtom.github.io/autospc/reference/facet_stages.md)
  without a `chart_type` now says so. It failed with
  `argument is of length zero` before reaching the check that names the
  argument and lists the chart types available.

- [`facet_stages()`](https://horridtom.github.io/autospc/reference/facet_stages.md)
  now uses the same annotation positioning as
  [`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md)
  on R below 4.3. `basic_annotations` defaults to
  `getRversion() < "4.3.0"` in
  [`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md),
  but the faceted path never passed the default on, so a faceted chart
  fell back to the positioning that needs ggrepel and ggpp. On R 4.3 and
  later nothing changes.

- `chart_type = "XMR"` no longer fails when
  [`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md)
  is called from a wrapper that forwards its arguments —
  `function(...) autospc(...)` — which raised
  `'...' used in an incorrect context`. The XmR pair was the only chart
  type that re-invoked
  [`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md)
  through the call it had been given, and it no longer does: both halves
  are analysed directly.

- The series is now sorted by `x` before limits are calculated. The
  algorithm works through the points in order, so data supplied out of
  `x` order produced different limits from the same data supplied in
  order, and was plotted against a scrambled series. Charts that
  aggregate over `x` — C, C′, P and P′ — were usually protected by the
  aggregation step, which sorts as a side effect; X and MR charts were
  affected whatever the data. Results will change for any series that
  was not already in `x` order.

- P and P′ charts accepting individual binary observations no longer
  fail when every subgroup holds exactly one observation. The run
  stopped with `object 'n' not found`, because the denominator is
  materialised during aggregation and aggregation was skipped when no
  `x` value was repeated. Note that such a series is degenerate — every
  proportion is 0% or 100%.

## autospc 0.0.0.9040

### Lifecycle changes

#### Breaking changes

- [`plot_auto_SPC()`](https://horridtom.github.io/autospc/reference/plot_auto_SPC.md)
  has been renamed to
  [`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md).
  Therefore
  [`plot_auto_SPC()`](https://horridtom.github.io/autospc/reference/plot_auto_SPC.md)
  is now deprecated. Many of this function’s arguments have also been
  renamed, in line with the [Tidyverse style
  guide](https://style.tidyverse.org/syntax.html#sec-objectnames). The
  table below provides details of all name changes implemented in this
  change.

| What           | Before                     | After                      | Change |
|----------------|----------------------------|----------------------------|--------|
| Version number | 0.0.0.9039                 | 0.0.0.9040                 | Yes    |
| Function       | plot_auto_SPC()            | autospc()                  | Yes    |
| Argument       | df                         | data                       | Yes    |
| Argument       | x                          | x                          | No     |
| Argument       | y                          | y                          | No     |
| Argument       | n                          | n                          | No     |
| Argument       | chartType                  | chart_type                 | Yes    |
| Argument group | \## Algorithm Parameters   | \## Algorithm Parameters   | No     |
| Argument       | periodMin                  | period_min                 | Yes    |
| Argument       | baseline                   | baseline_length            | Yes    |
| Argument       | runRuleLength              | shift_rule_threshold       | Yes    |
| Argument       | noRecals                   | baseline_only              | Yes    |
| Argument       | recalEveryShift            | establish_every_shift      | Yes    |
| Argument       | noRegrets                  | no_regrets                 | Yes    |
| Argument       | overhangingReversions      | overhanging_reversions     | Yes    |
| Argument group | \## SPC Parameters         | \## SPC Parameters         | No     |
| Argument       | maxNoOfExclusions          | max_exclusions             | Yes    |
| Argument       | highlightExclusions        | highlight_exclusions       | Yes    |
| Argument       | mr_screen_max_loops        | mr_screen_max_loops        | No     |
| Argument       | rule2Tolerance             | centre_line_tolerance      | Yes    |
| Argument       | floatingMedian             | floating_median            | Yes    |
| Argument       | floatingMedian_n           | floating_median_n          | Yes    |
| Argument group | \## Output Type            | \## Output Type            | No     |
| Argument       | plotChart                  | plot_chart                 | Yes    |
| Argument       | showLimits                 | show_limits                | Yes    |
| Argument       | showMR                     | show_mr                    | Yes    |
| Argument       | writeTable                 | write_table                | Yes    |
| Argument       | verbosity                  | verbosity                  | No     |
| Argument       | log_file_path              | log_file_path              | No     |
| Argument group | \## Chart Appearance       | \## Chart Appearance       | No     |
| Argument       | title                      | title                      | No     |
| Argument       | subtitle                   | subtitle                   | No     |
| Argument       | use_caption                | use_caption                | No     |
| Argument       | override_x_title           | override_x_title           | No     |
| Argument       | override_y_title           | override_y_title           | No     |
| Argument       | override_y_lim             | override_y_lim             | No     |
| Argument       | x_break                    | x_break                    | No     |
| Argument       | x_date_format              | x_date_format              | No     |
| Argument       | x_pad_end                  | x_pad_end                  | No     |
| Argument       | extend_limits_to           | extend_limits_to           | No     |
| Argument       | r1_col                     | r1_col                     | No     |
| Argument       | r2_col                     | r2_col                     | No     |
| Argument       | point_size                 | point_size                 | No     |
| Argument       | line_width_sf              | line_width_sf              | No     |
| Argument       | includeAnnotations         | include_annotations        | Yes    |
| Argument       | basicAnnotations           | basic_annotations          | Yes    |
| Argument       | annotation_size            | annotation_size            | No     |
| Argument       | align_labels               | align_labels               | No     |
| Argument       | flip_labels                | flip_labels                | No     |
| Argument       | upper_annotation_sf        | upper_annotation_sf        | No     |
| Argument       | lower_annotation_sf        | lower_annotation_sf        | No     |
| Argument       | annotation_arrows          | annotation_arrows          | No     |
| Argument       | annotation_arrow_curve     | annotation_arrow_curve     | No     |
| Argument       | override_annotation_dist   | override_annotation_dist   | No     |
| Argument       | override_annotation_dist_P | override_annotation_dist_P | No     |
