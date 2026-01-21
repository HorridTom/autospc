# Automatically recalculate SPC control limits

`create_SPC_auto_limits_table` applies the Stable Shift Algorithm to
automate recalculation of control limits.

## Usage

``` r
create_SPC_auto_limits_table(
  data,
  chart_type,
  period_min,
  baseline_length,
  shift_rule_threshold,
  max_exclusions,
  no_regrets,
  verbosity,
  baseline_only,
  establish_every_shift,
  centre_line_tolerance,
  show_limits,
  overhanging_reversions,
  mr_screen_max_loops
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

- chart_type:

  The type of chart you wish to plot. Must must have length one.
  Available options are: "XMR", "MR", "C", "C'", "P", "P'".

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

- max_exclusions:

  The maximum number of extreme points to exclude from limit
  calculations.

- no_regrets:

  Boolean signifying which version of the algorithm should be used.
  Defines whether limits can change as more data is added or not.

- verbosity:

  Integer 0-2 specifying how talkative the algorithm is in the standard
  output log; the higher the number the more information is provided,
  none if 0.

- baseline_only:

  Boolean - if TRUE, do not recalculate control limits, instead extend
  limits calculated from the first period_min points.

- establish_every_shift:

  Boolean - whether to bypass the Stable Shift Algorithm and simply
  re-establish limits at every shift rule break (respecting period_min)

- centre_line_tolerance:

  Minimum difference between a point's vertical position and the centre
  line to count as "on the centre line" for the purposes ofshift rule
  breaks

- show_limits:

  Boolean controlling whether or not to display centre line and control
  limits

- overhanging_reversions:

  Boolean determining whether rule breaks in the opposite direction to a
  rule break triggering a candidate recalculation prevent recalculation
  even if they overhang the end of the candidate calculation period. Set
  to FALSE only with no_regrets = FALSE.

  ### SPC Parameters

  Parameters that control how cetnre line and control limits are
  established for each period, and details of how SPC rules are applied

- mr_screen_max_loops:

  Integer or Inf specifying maximum number of times to recursively
  ignore mr values above the upper range limit when calculating xmr
  limits. Note this does not affect the calculation of the upper range
  limit on the mr chart.

## Value

data frame with limits, rule breaks and additional info needed for
plotting

## Examples

``` r
# Calculate limts for a C' chart for count of monthly attendances

df <- ed_attendances_monthly %>%
        dplyr::rename(x = month_start,
                      y = att_all)

limits_table <- create_SPC_auto_limits_table(
  df,
  chart_type = "C'",
  period_min = 21,
  baseline_length = NULL,
  shift_rule_threshold = 8,
  max_exclusions = 3,
  no_regrets = TRUE,
  verbosity = 1L,
  baseline_only = FALSE,
  establish_every_shift = FALSE,
  centre_line_tolerance = 0,
  show_limits = TRUE,
  overhanging_reversions = TRUE,
  mr_screen_max_loops = 1L
)

head(limits_table)
#> # A tibble: 6 × 14
#>   x              y    ucl   lcl    cl periodType excluded breakPoint rule1 rule2
#>   <date>     <dbl>  <dbl> <dbl> <dbl> <chr>      <lgl>    <lgl>      <lgl> <lgl>
#> 1 2015-05-31 12178 10689. 8233. 9461. calculati… TRUE     NA         TRUE  FALSE
#> 2 2015-06-30 12888 10689. 8233. 9461. calculati… TRUE     FALSE      TRUE  FALSE
#> 3 2015-07-31 12360 10689. 8233. 9461. calculati… TRUE     FALSE      TRUE  FALSE
#> 4 2015-08-31 11232 10689. 8233. 9461. calculati… FALSE    FALSE      TRUE  FALSE
#> 5 2015-09-30 11445 10689. 8233. 9461. calculati… FALSE    FALSE      TRUE  FALSE
#> 6 2015-11-01  9330 10689. 8233. 9461. calculati… FALSE    FALSE      FALSE FALSE
#> # ℹ 4 more variables: aboveOrBelowCl <int>, highlight <chr>, runStart <lgl>,
#> #   log <chr>
```
