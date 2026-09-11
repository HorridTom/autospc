# Changelog

## autospc 0.1.0.9017

### Smaller changes

- The error raised when `extend_limits_to` is not beyond the end of the
  data now names the argument, and is raised before the chart is
  computed rather than at the end of the analysis.

## autospc 0.1.0.9016

### A short series no longer warns about taking a maximum of nothing

A floating median is taken over the last `floating_median_n` non-missing
points, and `floating_median_n` defaults to 12. Where a series held
fewer points than that, the position the median window starts at was
worked out before ascertaining whether there was a median to draw,
resulting in [`max()`](https://rdrr.io/r/base/Extremes.html) of no
values warning and giving `-Inf`. Every chart of fewer than twelve
points therefore emitted “no non-missing arguments to max; returning
-Inf”, whatever `floating_median` was set to.

- **A series with too few points now draws no floating median.** `-Inf`
  had meant that `floating_median = "yes"` took the median over the
  whole series instead, and that `"auto"` looked for a shift rule break
  across the whole series rather than over the last `floating_median_n`
  points.

- **`floating_median = "yes"` warns** when it asked for a median and the
  series is too short for one, naming both counts.

- **`"auto"` and `"no"` are silent.** `"auto"` simply does not draw a
  median line where there are not sufficient data to do so; `"no"` never
  draws one.

## autospc 0.1.0.9015

### `facet_stages(split_rows)` is now `facet_stages(split_at)`

`split_rows` counted rows of the data as supplied. Where the data held
several observations per subgroup, that was not the number of points on
the chart: thirty-six observations of twelve subgroups, split at row 18,
gave a first stage of six points rather than eighteen. Where the data
was not in `x` order the split did not respect that ordering either.

- **`split_at` counts points in the analysed series**, which holds one
  point per subgroup in `x` order. Where the data already holds one row
  per subgroup, in `x` order, nothing changes.

- **`split_rows` is deprecated.** Supplying it warns, and its value is
  taken as `split_at` — so it takes the new meaning rather than keeping
  the old one for a release. Where both are given, `split_at` is used.

- The warning about split points beyond the end of the series now refers
  to the analysed series.

## autospc 0.1.0.9014

### Deprecations

- `autospc(override_annotation_dist)` and
  `autospc(override_annotation_dist_P)` are gone. They were deprecated
  in 0.0.0.9010 and became defunct in 0.1.0, and stayed in the signature
  only so that supplying one gave an error naming its replacement.
  Supplying one is now R’s own “unused argument” error. Use
  `upper_annotation_sf` and `lower_annotation_sf` instead — the
  equivalent scale factor is `1 + 1/x`, so
  `override_annotation_dist = 10` becomes `upper_annotation_sf = 1.1`.

## autospc 0.1.0.9013

### Deprecations

- Setting `no_regrets = TRUE` with `overhanging_reversions = FALSE` is
  now deprecated, and will be an error in a future version. `no_regrets`
  requires consideration of overhanging reversions, so the combination
  does not make sense. It has always warned and changed
  `overhanging_reversions` to TRUE, and still does; the warning now says
  that the change is going away. Set `overhanging_reversions = TRUE`, or
  leave it at its default, or set `no_regrets = FALSE`.

## autospc 0.1.0.9012

### Where an extension of the limits begins

`extend_limits_to` adds rows past the end of the data and draws the
final period’s limits across them. The first of those rows sat one unit
along the horizontal axis from the last subgroup. One unit is a step in
whatever units `x` is expressed in rather than a subgroup, so on finely
spaced data it was far too long: with readings ten milliseconds apart
and `x` in seconds, one unit is a hundred subgroups, and a short
extension put the first row past the second.

- **The step is now the median gap between consecutive subgroups**, so
  the extension starts where the next subgroup would have been.

- **It is capped at half the extension**, so that both rows fall inside
  an extension shorter than one subgroup. The limits then slope over the
  first half of that extension rather than holding level, although this
  is by definition over only a small fraction of the x-axis.

- **It is rounded up on an axis of whole units** — an integer column, or
  a `Date` — because a fractional step there lands on a value the column
  cannot tell from the one before it.

- **On an axis of whole units the step is also held to the length of the
  extension**, because rounding up can otherwise take it back past the
  cap. Where that leaves the step landing on `extend_limits_to` itself —
  an extension of one whole unit or less — the extension is a single row
  rather than two, and the limits slope across it.

On a chart whose `x` is spaced about one unit apart nothing moves. On
other spacings the first row of the extension moves: on monthly `Date`
data, from one day past the last subgroup to one month past it.

## autospc 0.1.0.9011

### The analysed values have a column of their own

**This changes what `y` means in the table
[`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md)
returns**, for the chart types whose analysed series is derived from
column(s) the caller passed. C, C’ and X charts are unaffected: what
they analyse is `y` as supplied.

- **`series` is new**, and holds the values the algorithm analyses and
  the chart plots: the `y` values as supplied on a C, C’ or X chart, the
  moving ranges on an MR chart, and percentages on a P or P’ chart.

- **`y` now holds what the caller supplied**, aggregated where the chart
  type aggregates. It held the analysed series before, so on an MR chart
  it held the moving ranges and now holds the values they were measured
  between, and on a P or P’ chart it held percentages and now holds the
  count.

- **`y_numerator` is gone.** It existed only because `y` held
  percentages on a P or P’ chart and the count had nowhere else to go.

- The columns are ordered `x`, `series`, `y`, and then the denominator
  `n` where the chart type has one.

The plot itself is unchanged. Note that for MR, P and P’ charts this is
a breaking change for code taking the output of
[`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md)
with `plot_chart = FALSE` and using the analysed values or
`y_numerator`. The fix is straightforward however, simply replace the
`y` with `series` and `y_numerator` with `y` in the legacy code.

## autospc 0.1.0.9010

### The table the package returns

`autospc(plot_chart = FALSE)` and
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) on a plot
returned different tables. They are now the same table, providing the
results of the analysis.

- **The columns that place the centre line labels on a plot have left
  the returned table.** `cl_label`, `annotation_level` and
  `annotation_curvature` say where a label and its arrow are drawn,
  which is a property of the drawing rather than of the analysis. They
  were in the table `plot_chart = FALSE` returned and were never in the
  table [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  gave.

- **`highlight` now marks only the rules a point breaks.** The rule
  highlight of an excluded point was overwritten by the exclusion mark,
  masking the rule it broke. The exclusion mark that
  `highlight_exclusions` asks for is now added when the plot is drawn,
  so the plot is unchanged.

- **The floating median and the rows `extend_limits_to` adds are now
  part of the analysis**, so
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) on a
  plot carries them. Both were produced when a plot was drawn, so
  `plot_chart = FALSE` had them and
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) did
  not.

- **`show_limits` no longer changes the table.** It says whether the
  limits are drawn, and the table now holds the analysis whether or not
  they are.

- **New column `limit_extension`**, TRUE on the rows `extend_limits_to`
  adds beyond the end of the data and FALSE on every row that holds a
  subgroup.

- **The rows `extend_limits_to` adds no longer copy the last subgroup’s
  values.** They carried its denominator and numerator, at a point on
  the axis where there is no subgroup at all; they now hold the limits,
  the period they continue, and nothing else.

### Bug fixes

- **The rows `extend_limits_to` adds no longer change the type of the
  `x` and `y` columns.** A column of whole numbers became a column of
  numbers, and the `x_max` the plot records followed it. A point on the
  axis that falls between two whole numbers is still taken as given, and
  the column gives way to it.

- **`plot_period` is now missing on the rows before the first point and
  after the last**, which have no limits and so belong to no period. It
  read `NANA` there.

- **The rows `extend_limits_to` adds now name the period they continue**
  where the series ends with a subgroup that holds no observation. They
  took the period of the last row of the table, which in that case
  belongs to no period, so they were labelled `displayNA`.

## autospc 0.1.0.9009

### Bug fixes

- **A point on the centre line no longer ends the run it sits in.** A
  point within `centre_line_tolerance` of the centre line was treated as
  a side of its own, so it split the run it fell in. It now neither
  commences a run, ends one, nor counts towards the length of the one it
  sits in, which is the conventional treatment of a point that is
  neither above the line nor below it. A run still commences only at a
  point that is above or below the line, so points on the line before
  any run belong to none.

## autospc 0.1.0.9008

### Data that cannot be charted

- **X, MR and XMR charts now reject a repeated `x`.** Each point on
  these charts is one row, so a repeated `x` has no place to be plotted.
  It was previously accepted, and multiplied in the output table. The
  error names the values that are repeated. C, C’, P and P’ charts are
  unchanged, summing the rows that share an `x` into one subgroup as
  before.

### Missing values

The analysis now proceeds as though a point with no `y` were not there,
rather than walking over it as a row. This changes results for any
series that has one.

- **A calculation period now holds `period_min` points, not `period_min`
  rows.** Where a series had missing values inside the first period,
  limits were previously calculated from fewer points than asked for.

- **An MR chart now shows its control limits at the first point** as
  well as its centre line, which it already showed. The first row of an
  MR chart holds no moving range, because there is no earlier point to
  measure one against; that is not a missing value, and the limits there
  are defined. The limits themselves are unchanged.

- **Control limits now carry across a gap.** They were drawn only where
  a point was, so they broke at every missing value; the centre line
  carried across but the control limits did not.

- **No limits are drawn before the first point or after the last.** The
  centre line previously ran to both edges of the chart whether or not
  there was anything there.

- **A missing point no longer silently splits a run**, which had made a
  shift rule break disappear. `na_ends_run` now controls this, and
  defaults to `TRUE`, which is the previous behaviour. A missing point
  may have continued the run before it or been on the other side of the
  centre line, and the data cannot say which: `TRUE` minimises the risk
  of a false positive shift rule break arising from missing data,
  `FALSE` minimises the risk of a false negative.

- Rows with no `x` are excluded before the analysis rather than after
  it. One such row could previously add a subgroup of its own, which
  counted towards the minimum needed for limits: 20 subgroups plus one
  row with no `x` drew limits that 20 subgroups alone correctly refused.
  `options(autospc.warn_missing_x = FALSE)` turns off the warning.

- The warning given when a series is too short now says how many points
  it has.

- **`aggregation_na_rm` controls what an observation with no value does
  to the subgroup it is aggregated into.** `FALSE`, the default, makes
  the whole subgroup missing, as the package has always done. `TRUE`
  discards the observation and forms the subgroup from the rest. A row
  is discarded when either its `y` or its `n` has no value, so a
  subgroup’s numerator and denominator always count the same
  observations. A subgroup that loses every observation stays on the
  chart as a missing point rather than disappearing. `aggregation_na_rm`
  has no effect on data that is already one row per subgroup, or on X
  and MR charts, which do not aggregate.

- **The limits at a point with missing `y` on a P or P’ chart are now
  calculated from that point’s own denominator**, where the data
  supplied one. The denominator of such a point is also now reported in
  the returned table. Limits at a point whose denominator is missing or
  zero are drawn at the mean denominator of its period.

- **P and P’ charts now return a `limit_width` column.** It holds the
  distance the limits sit from the centre line at a denominator of 1, so
  that the limits at any denominator are the centre line plus and minus
  `limit_width` over the square root of that denominator. It takes one
  value for a calculation period and the display period that follows it.

- **P and P’ charts no longer return the `constant`, `pbar`,
  `ucl_display` and `lcl_display` columns.** They held working values
  from the extension of limits over a display period. `constant` and
  `pbar` are now `limit_width` and `cl`, which hold the same values on
  every row rather than on the display rows alone, and the other two
  held the display limits before they were held within 0 and 100, which
  were not used.

- **Period columns are now correctly populated for a point with missing
  `y`.** `period_type`, `period_start`, `plot_period`, `limit_change`,
  `cl_change` and, on a P or P’ chart, `limit_width` were all missing at
  such a point, though its centre line and limits were filled in from
  the period it sits in.

- **The centre line and limits at a point with missing `y` inside a
  display period now match the rest of that period.** Previously they
  were calculated afresh from the display period’s own values instead of
  being carried forward with the rest of the period’s. No data point is
  plotted at such a point, so it is only the lines that were affected.

## autospc 0.1.0.9003

### Bug fixes

- [`facet_stages()`](https://horridtom.github.io/autospc/reference/facet_stages.md)
  no longer fails when `split_rows` asks for a single stage. Passing the
  last row of the data, as in
  `facet_stages(data, split_rows = nrow(data))`, failed on drawing with
  `At least one layer must contain all faceting variables`. It now draws
  one facet.

- A `split_rows` value beyond the end of the data is now taken as the
  last row, and warns. It previously produced a repeated stage: on 43
  rows of data, `split_rows = 44` gave two facets of the same series and
  `split_rows = c(44, 45)` gave three.

- `facet_stages(plot_chart = FALSE)` now always returns a `stage`
  column. Where there was a single stage it returned a table with no
  `stage` column at all.

## autospc 0.1.0.9002

### Bug fixes

- The `month_start` column of `ed_attendances_monthly` is now the first
  day of each month, for 109 consecutive months from June 2015. It
  previously wrote each month either as its first day or as the last day
  of the month before, so the dates looked as though they drifted
  through the calendar. The counts are unchanged and no row has moved to
  a different month. Charts drawn from this dataset shift by up to a day
  on the x axis.
- The help page for `ed_attendances_monthly` said the data had 9 columns
  where it has 7, and listed `e_adm_via_ed` before `e_adm_over_4h` while
  the data has them the other way round. Both are corrected.

## autospc 0.1.0.9001

### Bug fixes

- [`facet_stages()`](https://horridtom.github.io/autospc/reference/facet_stages.md)
  no longer fails when no stage has enough points for control limits. It
  draws each stage as a plain time series, which is what
  [`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md)
  draws for a series with no limits. The same fault stopped
  `facet_stages(show_limits = FALSE)` drawing at all; that works now
  too.

- In a faceted chart where some stages have limits and some do not, the
  points of a stage without limits were drawn grey, which is the colour
  of a point excluded from the limits calculation. They are now drawn
  black, like the points of any series shown without limits.

- A chart drawn without control limits now honours `x_break`,
  `x_date_format` and `x_pad_end`. They were ignored, because the plain
  time series was drawn without formatting its x axis.

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
