# autospc (development version)

# autospc 0.0.0.9040

## Lifecycle changes

### Breaking changes

* `plot_auto_SPC()` has been renamed to `autospc()`. Therefore `plot_auto_SPC()`
  is now deprecated. Many of this function's arguments have also been renamed,
  in line with the [Tidyverse style guide](https://style.tidyverse.org/syntax.html#sec-objectnames)
  The table below provides details of all name changes implemented in this
  change.
  
  
|What                   |Before                    |After                     |Change|
|-----------------------|--------------------------|--------------------------|------|
|Version number         |0.0.0.9039                |0.0.0.9040                | TRUE |
|Function name          |plot_auto_SPC()           |autospc()                 | TRUE |
|Function argument      |df                        |data                      | TRUE |
|Function argument      |x                         |x                         |FALSE |
|Function argument      |y                         |y                         |FALSE |
|Function argument      |n                         |n                         |FALSE |
|Function argument      |chartType                 |chart_type                | TRUE |
|Function argument group|## Algorithm Parameters   |## Algorithm Parameters   |FALSE |
|Function argument      |periodMin                 |period_min                | TRUE |
|Function argument      |baseline                  |baseline_length           | TRUE |
|Function argument      |runRuleLength             |shift_rule_threshold      | TRUE |
|Function argument      |noRecals                  |baseline_only             | TRUE |
|Function argument      |recalEveryShift           |establish_every_shift     | TRUE |
|Function argument      |noRegrets                 |no_regrets                | TRUE |
|Function argument      |overhangingReversions     |overhanging_reversions    | TRUE |
|Function argument group|## SPC Parameters         |## SPC Parameters         |FALSE |
|Function argument      |maxNoOfExclusions         |max_exclusions            | TRUE |
|Function argument      |highlightExclusions       |highlight_exclusions      | TRUE |
|Function argument      |mr_screen_max_loops       |mr_screen_max_loops       |FALSE |
|Function argument      |rule2Tolerance            |centre_line_tolerance     | TRUE |
|Function argument      |floatingMedian            |floating_median           | TRUE |
|Function argument      |floatingMedian_n          |floating_median_n         | TRUE |
|Function argument group|## Output Type            |## Output Type            |FALSE |
|Function argument      |plotChart                 |plot_chart                | TRUE |
|Function argument      |showLimits                |show_limits               | TRUE |
|Function argument      |showMR                    |show_mr                   | TRUE |
|Function argument      |writeTable                |write_table               | TRUE |
|Function argument      |verbosity                 |verbosity                 |FALSE |
|Function argument      |log_file_path             |log_file_path             |FALSE |
|Function argument group|## Chart Appearance       |## Chart Appearance       |FALSE |
|Function argument      |title                     |title                     |FALSE |
|Function argument      |subtitle                  |subtitle                  |FALSE |
|Function argument      |use_caption               |use_caption               |FALSE |
|Function argument      |override_x_title          |override_x_title          |FALSE |
|Function argument      |override_y_title          |override_y_title          |FALSE |
|Function argument      |override_y_lim            |override_y_lim            |FALSE |
|Function argument      |x_break                   |x_break                   |FALSE |
|Function argument      |x_date_format             |x_date_format             |FALSE |
|Function argument      |x_pad_end                 |x_pad_end                 |FALSE |
|Function argument      |extend_limits_to          |extend_limits_to          |FALSE |
|Function argument      |r1_col                    |r1_col                    |FALSE |
|Function argument      |r2_col                    |r2_col                    |FALSE |
|Function argument      |point_size                |point_size                |FALSE |
|Function argument      |line_width_sf             |line_width_sf             |FALSE |
|Function argument      |includeAnnotations        |include_annotations       | TRUE |
|Function argument      |basicAnnotations          |basic_annotations         | TRUE |
|Function argument      |annotation_size           |annotation_size           |FALSE |
|Function argument      |align_labels              |align_labels              |FALSE |
|Function argument      |flip_labels               |flip_labels               |FALSE |
|Function argument      |upper_annotation_sf       |upper_annotation_sf       |FALSE |
|Function argument      |lower_annotation_sf       |lower_annotation_sf       |FALSE |
|Function argument      |annotation_arrows         |annotation_arrows         |FALSE |
|Function argument      |annotation_arrow_curve    |annotation_arrow_curve    |FALSE |
|Function argument      |override_annotation_dist  |override_annotation_dist  |FALSE |
|Function argument      |override_annotation_dist_P|override_annotation_dist_P|FALSE |



