#' Deprecated
#' 
#' `plot_auto_SPC()` 
#' 
#' This function was renamed. Please use [autospc::autospc()], plot_auto_SPC()
#' is deprecated.
#' 
#' @export
plot_auto_SPC <- function(df,
                          x,
                          y,
                          n,
                          chartType = NULL,
                          ## Algorithm Parameters
                          periodMin = 21,
                          baseline = NULL,
                          runRuleLength = 8,
                          noRecals = FALSE,
                          recalEveryShift = FALSE,
                          noRegrets = TRUE,
                          overhangingReversions = TRUE,
                          ## SPC Parameters
                          maxNoOfExclusions = 3,
                          highlightExclusions = TRUE,
                          mr_screen_max_loops = 1L,
                          rule2Tolerance = 0,
                          floatingMedian = "no",
                          floatingMedian_n = 12L,
                          ## Output Type
                          plotChart = TRUE,
                          showLimits = TRUE,
                          showMR = TRUE,
                          writeTable = FALSE,
                          verbosity = 0L,
                          log_file_path = NULL,
                          ## Chart Appearance
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
                          includeAnnotations = TRUE,
                          basicAnnotations = getRversion() < '4.3.0',
                          annotation_size = 3,
                          align_labels = FALSE,
                          flip_labels = FALSE,
                          upper_annotation_sf = NULL,
                          lower_annotation_sf = NULL,
                          annotation_arrows = FALSE,
                          annotation_arrow_curve = 0.3,
                          override_annotation_dist = NULL,
                          override_annotation_dist_P = NULL
) {
  
  lifecycle::deprecate_stop("0.0.0.9040", "plot_auto_SPC()", "autospc()")
  
}
