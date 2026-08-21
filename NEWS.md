# autospc (development version)

## Lifecycle changes

### Breaking changes

* `autospc(override_annotation_dist)` and `autospc(override_annotation_dist_P)`
  are now defunct. They have warned since 0.0.0.9010; supplying either is now an
  error. Use `upper_annotation_sf` and `lower_annotation_sf` instead — the
  equivalent scale factor is `1 + 1/x`, so `override_annotation_dist = 10`
  becomes `upper_annotation_sf = 1.1`, and `lower_annotation_sf` defaults to its
  mirror image, `2 - upper_annotation_sf`. The two arguments remain in the
  signature so that the error can name their replacement, and will be removed in
  a later release.

  `upper_annotation_sf` and `lower_annotation_sf` apply to every chart type, so
  there is no replacement specific to P and P′ charts.

* `create_SPC_auto_limits_table()` is no longer exported. It was an internal
  step of `autospc()` that had been made public without a documented reason,
  and holding it to a public interface was preventing the simplification of the
  package's internals.

  `autospc()` does the same work and is the supported way to do it. For the
  results as data rather than a plot, use `autospc(plot_chart = FALSE)`, which
  returns the same limits, rule breaks and period boundaries with the
  additional columns needed for plotting.

  If you were calling `create_SPC_auto_limits_table()` directly and
  `autospc(plot_chart = FALSE)` does not meet your needs, please open an issue
  at <https://github.com/HorridTom/autospc/issues> — we are happy to help you
  move across.

### Deprecations

* `autospc(show_mr)` is deprecated. Use `chart_type` instead: `chart_type = "X"`
  draws the X chart on its own, which is what `show_mr = FALSE` did, and
  `chart_type = "XMR"` draws the pair. Supplying `show_mr` still works and still
  does what it did, but now warns.

  Note that the caption names the chart type, so a chart drawn with
  `chart_type = "X"` is captioned "X Shewhart Chart" where the same chart drawn
  with `chart_type = "XMR", show_mr = FALSE` was captioned "XMR Shewhart Chart".

* `facet_stages(show_mr)` is deprecated for the same reason. `facet_stages()` has
  never drawn the moving range chart, so an `XMR` request is now faceted as an
  `X` chart — including its caption, which changes from "XMR Shewhart Chart" to
  "X Shewhart Chart". Nothing else about the chart changes.

### Other changes

* `chart_type = "X"` draws the X chart on its own, without the moving range
  chart beneath it. It gives the same result as `chart_type = "XMR"` with
  `show_mr = FALSE`.

* `autospc()` returns a plot object that is still a ggplot — printing, `ggsave()`
  and adding ggplot2 layers all work as before — and additionally carries the
  analysed chart it was drawn from. `as.data.frame()` on it returns the analysis.

  An XmR chart carries both halves of the pair, the X chart first and the moving
  range chart second. `as.data.frame()` on one returns them joined side by side,
  with the moving range and its limits as `mr`, `amr`, `url` and `lrl` — the same
  shape `autospc(plot_chart = FALSE)` returns.

  `facet_stages()` returns the same kind of object, carrying one analysed chart
  per facet in stage order. Where `split_rows` is named, the charts take those
  names. `as.data.frame()` on one stacks the facets, with `stage` saying which
  each row came from — the same column `facet_stages(plot_chart = FALSE)`
  returns.

* A `title` or `subtitle` column in the data is no longer repeated over the
  moving range chart of an XmR pair. The pair is one chart in two panels, so its
  title is drawn once, above the X chart. A title given as an argument was
  already drawn once; this makes the two agree.

* `autospc(plot_chart = FALSE, show_limits = FALSE)` now returns the four
  columns describing the periods — `limitChange`, `periodStart`, `plotPeriod`
  and `cl_change` — which it previously returned only when `show_limits` was
  `TRUE`.

* Columns other than those a chart uses are now dropped consistently. Previously
  the aggregation step was skipped entirely when no `x` value was repeated, so
  extra columns survived into the output for a series with one row per subgroup
  and were dropped for one without. They are now dropped in both cases.

* `autospc(keep_candidate_tables)` is a new argument, `FALSE` by default. The
  algorithm considers a candidate calculation period at each point where it might
  re-establish the limits, and records each one it forms. Setting this to `TRUE`
  additionally records, for each candidate, the full table of limits it would
  have produced. It is off by default because those tables are several times the
  size of everything else the chart holds: for a 600-point chart that
  re-establishes its limits nine times, the chart object measured 111 KB with
  the default and 534 KB with `keep_candidate_tables = TRUE`.

* The default values of `period_min` and `max_exclusions` are now the integers
  `21L` and `3L`, where they were the doubles `21` and `3`. Both are counts of
  data points. Passing a double still works.

* A rounding warning is no longer given for data that is then rejected. A P or P′
  chart given a numerator with fractional values and a denominator of the wrong
  type warned that it was rounding the numerator before erroring on the
  denominator; it now raises the error without the warning.

## Bug fixes

* Data holding a column called `x`, `y` or `n` no longer prevents a different
  column being used for that argument. `autospc(data, x = month, y = count)`,
  where `data` also has a column called `x`, failed with `Names must be unique`;
  the column named in the argument is now used and the column called `x` is
  dropped, along with the other columns the analysis does not use. The data as
  supplied is unaffected. `facet_stages()` warned in the same situation and then
  failed the same way; it now behaves as `autospc()` does.

* `facet_stages()` now labels its axes. It resolved the axis titles and then
  never passed them to the drawing, so a faceted chart had no axis labels at
  all, where the same data through `autospc()` was labelled. A `title` or
  `subtitle` column in the data was dropped the same way and now reaches the
  chart. Titles given as arguments were unaffected and still win.

* `facet_stages()` without a `chart_type` now says so. It failed with
  `argument is of length zero` before reaching the check that names the
  argument and lists the chart types available.

* `facet_stages()` now uses the same annotation positioning as `autospc()` on
  R below 4.3. `basic_annotations` defaults to `getRversion() < "4.3.0"` in
  `autospc()`, but the faceted path never passed the default on, so a faceted
  chart fell back to the positioning that needs ggrepel and ggpp. On R 4.3 and
  later nothing changes.

* `chart_type = "XMR"` no longer fails when `autospc()` is called from a
  wrapper that forwards its arguments — `function(...) autospc(...)` — which
  raised `'...' used in an incorrect context`. The XmR pair was the only chart
  type that re-invoked `autospc()` through the call it had been given, and it no
  longer does: both halves are analysed directly.

* The series is now sorted by `x` before limits are calculated. The algorithm
  works through the points in order, so data supplied out of `x` order produced
  different limits from the same data supplied in order, and was plotted against
  a scrambled series. Charts that aggregate over `x` — C, C′, P and P′ — were
  usually protected by the aggregation step, which sorts as a side effect; X and
  MR charts were affected whatever the data. Results will change for any series
  that was not already in `x` order.

* P and P′ charts accepting individual binary observations no longer fail when
  every subgroup holds exactly one observation. The run stopped with
  `object 'n' not found`, because the denominator is materialised during
  aggregation and aggregation was skipped when no `x` value was repeated.
  Note that such a series is degenerate — every proportion is 0% or 100%.


# autospc 0.0.0.9040

## Lifecycle changes

### Breaking changes

* `plot_auto_SPC()` has been renamed to `autospc()`. Therefore `plot_auto_SPC()`
  is now deprecated. Many of this function's arguments have also been renamed,
  in line with the [Tidyverse style guide](https://style.tidyverse.org/syntax.html#sec-objectnames).
  The table below provides details of all name changes implemented in this
  change.
  
  
|What          |Before                    |After                     |Change|
|--------------|--------------------------|--------------------------|------|
|Version number|0.0.0.9039                |0.0.0.9040                |Yes   |
|Function      |plot_auto_SPC()           |autospc()                 |Yes   |
|Argument      |df                        |data                      |Yes   |
|Argument      |x                         |x                         |No    |
|Argument      |y                         |y                         |No    |
|Argument      |n                         |n                         |No    |
|Argument      |chartType                 |chart_type                |Yes   |
|Argument group|## Algorithm Parameters   |## Algorithm Parameters   |No    |
|Argument      |periodMin                 |period_min                |Yes   |
|Argument      |baseline                  |baseline_length           |Yes   |
|Argument      |runRuleLength             |shift_rule_threshold      |Yes   |
|Argument      |noRecals                  |baseline_only             |Yes   |
|Argument      |recalEveryShift           |establish_every_shift     |Yes   |
|Argument      |noRegrets                 |no_regrets                |Yes   |
|Argument      |overhangingReversions     |overhanging_reversions    |Yes   |
|Argument group|## SPC Parameters         |## SPC Parameters         |No    |
|Argument      |maxNoOfExclusions         |max_exclusions            |Yes   |
|Argument      |highlightExclusions       |highlight_exclusions      |Yes   |
|Argument      |mr_screen_max_loops       |mr_screen_max_loops       |No    |
|Argument      |rule2Tolerance            |centre_line_tolerance     |Yes   |
|Argument      |floatingMedian            |floating_median           |Yes   |
|Argument      |floatingMedian_n          |floating_median_n         |Yes   |
|Argument group|## Output Type            |## Output Type            |No    |
|Argument      |plotChart                 |plot_chart                |Yes   |
|Argument      |showLimits                |show_limits               |Yes   |
|Argument      |showMR                    |show_mr                   |Yes   |
|Argument      |writeTable                |write_table               |Yes   |
|Argument      |verbosity                 |verbosity                 |No    |
|Argument      |log_file_path             |log_file_path             |No    |
|Argument group|## Chart Appearance       |## Chart Appearance       |No    |
|Argument      |title                     |title                     |No    |
|Argument      |subtitle                  |subtitle                  |No    |
|Argument      |use_caption               |use_caption               |No    |
|Argument      |override_x_title          |override_x_title          |No    |
|Argument      |override_y_title          |override_y_title          |No    |
|Argument      |override_y_lim            |override_y_lim            |No    |
|Argument      |x_break                   |x_break                   |No    |
|Argument      |x_date_format             |x_date_format             |No    |
|Argument      |x_pad_end                 |x_pad_end                 |No    |
|Argument      |extend_limits_to          |extend_limits_to          |No    |
|Argument      |r1_col                    |r1_col                    |No    |
|Argument      |r2_col                    |r2_col                    |No    |
|Argument      |point_size                |point_size                |No    |
|Argument      |line_width_sf             |line_width_sf             |No    |
|Argument      |includeAnnotations        |include_annotations       |Yes   |
|Argument      |basicAnnotations          |basic_annotations         |Yes   |
|Argument      |annotation_size           |annotation_size           |No    |
|Argument      |align_labels              |align_labels              |No    |
|Argument      |flip_labels               |flip_labels               |No    |
|Argument      |upper_annotation_sf       |upper_annotation_sf       |No    |
|Argument      |lower_annotation_sf       |lower_annotation_sf       |No    |
|Argument      |annotation_arrows         |annotation_arrows         |No    |
|Argument      |annotation_arrow_curve    |annotation_arrow_curve    |No    |
|Argument      |override_annotation_dist  |override_annotation_dist  |No    |
|Argument      |override_annotation_dist_P|override_annotation_dist_P|No    |



