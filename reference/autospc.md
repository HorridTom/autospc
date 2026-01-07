# Plot SPC charts with automated limit recalculation

`autospc()` creates a statistical process control chart from a
dataframe, applying the Stable Shift Algorithm to automate recalculation
of control limits.

## Usage

``` r
autospc(
  data,
  x,
  y,
  n,
  chart_type = NULL,
  period_min = 21,
  baseline_length = NULL,
  shift_rule_threshold = 8,
  baseline_only = FALSE,
  establish_every_shift = FALSE,
  no_regrets = TRUE,
  overhanging_reversions = TRUE,
  max_exclusions = 3,
  highlight_exclusions = TRUE,
  mr_screen_max_loops = 1L,
  centre_line_tolerance = 0,
  floating_median = "no",
  floating_median_n = 12L,
  plot_chart = TRUE,
  show_limits = TRUE,
  show_mr = TRUE,
  write_table = FALSE,
  verbosity = 0L,
  log_file_path = NULL,
  title = NULL,
  subtitle = NULL,
  use_caption = TRUE,
  override_x_title = NULL,
  override_y_title = NULL,
  override_y_lim = NULL,
  x_break = NULL,
  x_date_format = "%Y-%m-%d",
  x_pad_end = NULL,
  extend_limits_to = NULL,
  r1_col = "orange",
  r2_col = "steelblue3",
  point_size = 2,
  line_width_sf = 1,
  include_annotations = TRUE,
  basic_annotations = getRversion() < "4.3.0",
  annotation_size = 3,
  align_labels = FALSE,
  flip_labels = FALSE,
  upper_annotation_sf = NULL,
  lower_annotation_sf = NULL,
  annotation_arrows = FALSE,
  annotation_arrow_curve = 0.3,
  override_annotation_dist = NULL,
  override_annotation_dist_P = NULL
)
```

## Arguments

- data:

  A data frame. For an XMR, C or C' chart, must have columns for:

  - the subgrouping variable, to be plotted on the horizontal axis, (x);

  - the variable of interest to be plotted on the vertical axis (y);

  - and optionally, a title and subtitle for the plot.

    
  For a P or P' chart, must have columns for:

  - the subgrouping variable, to be plotted on the horizontal axis, (x);

  - the total count or denominator (n);

  - the count meeting criteria, or numerator (y);

  - and optionally, a title and subtitle for the plot.

- x:

  Name of column (passed using tidyselect semantics) to use as subgroups
  on the horizontal axis of the chart.

- y:

  Name of column (passed using tidyselect semantics) to use as:

  - the variable to be plotted for XMR charts,

  - count (plotted on the vertical axis) for C and C' charts,

  - numerator of the proportion (plotted on the vertical axis) for P and
    P' charts.

- n:

  Name of column (passed using tidyselect semantics) to use as
  denominator for P and P' charts.

- chart_type:

  The type of chart you wish to plot. Available options are: "XMR",
  "MR", "C", "C'", "P", "P'".

  ### Algorithm Parameters

  Parameters that control behaviour of the algorithm used to
  re-establish control limits.

- period_min:

  The minimum number of points (subgroups) per period, i.e. the minimum
  number of points required to form control limits.

- baseline_length:

  Integer, overrides period_min for the first calculation period only,
  if specified

- shift_rule_threshold:

  The minimum number of consecutive points above or below the centre
  line constituting a shift (or "rule 2") break.

- baseline_only:

  Boolean - if TRUE, do not recalculate control limits, instead extend
  limits calculated from the first period_min points.

- establish_every_shift:

  Boolean - whether to bypass the Stable Shift Algorithm and simply
  re-establish limits at every shift rule break (respecting period_min)

- no_regrets:

  Boolean signifying which version of the algorithm should be used.
  Defines whether limits can change as more data is added or not.

- overhanging_reversions:

  Boolean determining whether rule breaks in the opposite direction to a
  rule break triggering a candidate recalculation prevent recalculation
  even if they overhang the end of the candidate calculation period. Set
  to FALSE only with no_regrets = FALSE.

  ### SPC Parameters

  Parameters that control how cetnre line and control limits are
  established for each period, and details of how SPC rules are applied

- max_exclusions:

  The maximum number of extreme points to exclude from limit
  calculations.

- highlight_exclusions:

  Boolean signifying whether excluded points are greyed out.

- mr_screen_max_loops:

  Integer or Inf specifying maximum number of times to recursively
  ignore mr values above the upper range limit when calculating xmr
  limits. Note this does not affect the calculation of the upper range
  limit on the mr chart.

- centre_line_tolerance:

  Minimum difference between a point's vertical position and the centre
  line to count as "on the centre line" for the purposes ofshift rule
  breaks

- floating_median:

  Whether to add a floating median line to the chart, calculated based
  on the final floating_median_n data points on the chart: "no" - do not
  display a floating median, "yes" - display a floating median, "auto" -
  display a floating median if and only if there is at least one point
  that is part of a shift rule break in the final floating_median_n data
  points on the chart.

- floating_median_n:

  The number of points to use for calculation of the floating median, if
  present.

  ### Output Type

  Arguments that control how the result is outputted

- plot_chart:

  Boolean specifying whether to plot the chart. If not, the data is
  returned with centre line, control limits and other analytic output
  appended as columns.

- show_limits:

  Boolean controlling whether or not to display centre line and control
  limits

- show_mr:

  Logical controlling whether the moving range chart is included in XMR
  chart

- write_table:

  Boolean specifying whether to save the data as a CSV (useful for doing
  lots of charts at a time).

- verbosity:

  Integer 0-2 specifying how talkative the algorithm is in the standard
  output log; the higher the number the more information is provided,
  none if 0.

- log_file_path:

  if not NULL (the default), path to save log file to. The file
  extension provided (.rds or .csv) determines the type of file the log
  data is saved to. Full log data is saved, regardless of verbosity.

  ### Chart Appearance

  Arguments that control aspects of chart visualisation

- title:

  Optional string specifying chart title. Overrides df\$title.

- subtitle:

  Optional string specifying subtitle. Overrides df\$subtitle.

- use_caption:

  Boolean controlling whether the caption is displayed.

- override_x_title:

  String specifying horizontal axis label.

- override_y_title:

  String specifying vertical axis label.

- override_y_lim:

  Optional numeric specifying upper limit of the vertical axis.

- x_break:

  Optional numeric specifying spacing of horizontal axis breaks.

- x_date_format:

  Optional string format for date labels on horizontal axis. Passed to
  scales::date_format.

- x_pad_end:

  Optional, specifies a minimum end point for the horizontal axis.

- extend_limits_to:

  Optional, specifies a point on the horizontal axis to extend the final
  limits out to

- r1_col:

  Highlight colour for breaks of rule 1 (points outside the control
  limits)

- r2_col:

  Highlight colour for breaks of rule 2 (shifts)

- point_size:

  Size of plot points, defaults to 2. See
  [aes_linetype_size_shape](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html)
  for more details.

- line_width_sf:

  Numeric scale factor for plot line widths.

- include_annotations:

  Boolean specifying whether to show centre line labels

- basic_annotations:

  Boolean specifying whether to force use of basic annotation
  positioning. When TRUE, suggested packages ggrepel and ggpp are not
  required, but annotation arrows are not supported. Defaults to TRUE
  for R versions prior to 4.3, FALSE otherwise.

- annotation_size:

  Text size for centre line labels

- align_labels:

  Boolean specifying whether or not to align centre line labels at a
  fixed vertical position

- flip_labels:

  Boolean specifying whether or not to place centre line labels on
  different sides of the centre line depending on the direction of
  change from the previous period

- upper_annotation_sf:

  Numeric scale factor specifying upper vertical position of centre line
  labels as a multiple of the upper control limit

- lower_annotation_sf:

  Numeric scale factor specifying lower vertical position of centre line
  labels as a multiple of the lower control limit

- annotation_arrows:

  Boolean specifying whether or not to display arrows connecting centre
  line labels to the centre line they refer to

- annotation_arrow_curve:

  Numeric curvature of the annotation arrows

- override_annotation_dist:

  Deprecated

- override_annotation_dist_P:

  Deprecated

## Value

An SPC ggplot or corresponding data

## Examples

``` r
# Using a C' chart to track changes in the count of monthly attendance 
autospc(
  ed_attendances_monthly, 
  chart_type = "C'", 
  x = month_start, 
  y = att_all
)
#> Registered S3 methods overwritten by 'ggpp':
#>   method                  from   
#>   heightDetails.titleGrob ggplot2
#>   widthDetails.titleGrob  ggplot2

   
#Using a P' chart to track changes in the percentage admitted within 4 hours
autospc(
  ed_attendances_monthly, 
  chart_type = "P'", 
  x = month_start, 
  y = within_4h, 
  n = att_all
)


#using a shift_rule_threshold of 7 when tracking monthly attendance
autospc(
  ed_attendances_monthly, 
  chart_type = "C'", 
  x = month_start, 
  y = att_all,
  shift_rule_threshold = 7
)

```
