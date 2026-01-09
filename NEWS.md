# autospc (development version)

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



