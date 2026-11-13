# Functions to populate and interpret algorithm log

interpret_log_entry <- function(entry,
                                verbosity) {
  # Deal with counter overflow marker
  if (stringr::str_detect(entry, "\\|")) {
    entry <- stringr::str_sub(
      stringr::str_extract(entry, "\\|.*$"),
      2L,
      -1L
    )
  }

  # Extract components from log entry
  step <- stringr::str_sub(
    entry,
    1L,
    2L
  )

  branch <- stringr::str_sub(
    entry,
    3L,
    4L
  )

  entry_data <- stringr::str_sub(
    entry,
    5L,
    -1L
  )
  if (stringr::str_length(entry_data) == 0L) {
    entry_data <- NA_character_
  }

  # Form log entry interpretation string based on log entry components
  switch(step,
    "00" = {
      if (branch == "00") {
        interpretation <- "Restart forced by force_restart_at."
      } else if (branch == "10") {
        interpretation <- paste(
          "Restart after a gap of at least restart_after_gap points with no",
          "value."
        )
      } else {
        interpretation <- "Undefined branch at step 00."
      }
    },
    "01" = {
      interpretation <- "Counter initialised to 1."
    },
    "02" = {
      if (branch == "00") {
        interpretation <- "Sufficient data to form at least one period."
      } else if (branch == "10") {
        interpretation <- "Insufficient data to form control limits."
      } else {
        interpretation <- "Undefined branch at step 02."
      }
    },
    "03" = {
      interpretation <- "Main algorithm loop commenced."
    },
    "04" = {
      if (stringr::str_sub(
        branch,
        1L,
        1L
      ) == "0") {
        interpretation <- "Sufficient data to proceed."
      } else if (stringr::str_sub(
        branch,
        1L,
        1L
      ) == "1") {
        interpretation <- paste(
          "Insufficient remaining data for further",
          "re-establishment of limits."
        )
      } else {
        interpretation <- "Undefined branch at step 04."
      }

      if (!is.na(entry_data) & stringr::str_sub(
        branch,
        2L,
        2L
      ) == "1") {
        interpretation <- paste0(
          interpretation,
          " Moving counter to the next shift rule break,",
          " commencing at point ",
          entry_data,
          "."
        )
      }
    },
    "05" = {
      if (branch == "00") {
        interpretation <- "There is a shift rule break commencing here,"

        switch(entry_data,
          "01" = {
            interpretation <- paste(
              interpretation,
              "downwards from the current centre line."
            )
          },
          "10" = {
            interpretation <- paste(
              interpretation,
              "upwards from the current centre line."
            )
          },
          {
            interpretation <- paste(
              interpretation,
              "information on its direction is missing."
            )
          }
        )
      } else if (branch == "10") {
        interpretation <- paste(
          "There are no subsequent shift rule breaks."
        )
      } else {
        interpretation <- "Undefined branch at step 05."
      }
    },
    "06" = {
      if (branch == "00") {
        if (verbosity > 1) {
          interpretation <- "Sufficient data to proceed."
        } else {
          interpretation <- ""
        }
        interpretation <- paste(
          interpretation,
          "Forming candidate limits."
        )
      } else if (branch == "10") {
        interpretation <- paste(
          "Insufficient remaining data for further",
          "re-establishment of limits."
        )
      } else {
        interpretation <- "Undefined branch at step 06."
      }

      if (!is.na(entry_data)) {
        opp <- as.logical(as.integer(stringr::str_sub(
          entry_data,
          1L,
          1L
        )))
        frp <- as.logical(as.integer(stringr::str_sub(
          entry_data,
          2L,
          2L
        )))
        opp_str <- if (opp) {
          paste(
            "There is a shift rule break back towards",
            "the prevailing centre line."
          )
        } else if (verbosity > 1) {
          paste(
            "There is no shift rule break back towards the",
            "prevailing centre line."
          )
        } else {
          ""
        }

        frp_str <- if (frp) {
          paste(
            "The final run in the candidate calculation period may",
            "become a shift rule break back towards the prevailing",
            "centre line."
          )
        } else if (verbosity > 1) {
          paste(
            "The final run in the candidate calculation period",
            "cannot become a shift rule break back towards the",
            "prevailing centre line."
          )
        } else {
          ""
        }

        interpretation <- paste(
          interpretation,
          opp_str,
          frp_str
        )
      }
    },
    "07" = {
      if (branch == "00") {
        interpretation <- paste(
          "Candidate limits accepted, limits",
          "re-established."
        )
      } else if (branch == "10") {
        interpretation <- paste(
          "Candidate limits rejected, prevailing",
          "limits retained."
        )
      } else if (branch == "20") {
        interpretation <- paste(
          "Candidate limits rejected: insufficient data to establish a",
          "stable new process. The final run of the candidate calculation",
          "period is not complete."
        )
      } else if (branch == "30") {
        interpretation <- paste(
          "Not considered: in a range given by",
          "prevent_establishment_in."
        )
      } else {
        interpretation <- "Undefined branch at step 07."
      }
    },
    {
      interpretation <- "Undefined log entry"
    }
  )

  return(interpretation)
}


create_log_dataframe <- function(table,
                                 verbosity) {
  table <- table %>%
    dplyr::select(x,
      log_entry = log
    ) %>%
    tibble::rowid_to_column("counter") %>%
    dplyr::filter(!is.na(log_entry)) %>%
    tidyr::separate_longer_delim(log_entry,
      delim = ";"
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(interpretation = interpret_log_entry(log_entry,
      verbosity = verbosity
    )) %>%
    dplyr::ungroup() %>%
    as.data.frame()


  return(table)
}


interpret_log <- function(table,
                          verbosity) {
  log_table <- create_log_dataframe(table,
    verbosity = verbosity
  )

  log_table <- log_table %>%
    dplyr::filter(!(trimws(interpretation) == "")) %>%
    dplyr::group_by(counter) %>%
    dplyr::mutate(interpretation = stringr::str_wrap(interpretation,
      width = 60L
    )) %>%
    dplyr::summarise(
      x = dplyr::first(x),
      interpretation = paste(interpretation,
        collapse = "\n- "
      ),
      interpretation = paste0("- ", interpretation),
      .groups = "drop"
    )

  log_txt <- log_table %>%
    dplyr::mutate(log_txt = paste0(
      "Counter at ",
      counter,
      ", ",
      x,
      ":\n",
      interpretation
    )) %>%
    dplyr::summarise(log_txt = paste0(log_txt,
      collapse = "\n\n"
    )) %>%
    dplyr::pull(log_txt)

  return(log_txt)
}


#' Report what the analysis found
#'
#' One warning for the call where any chart was short of points, the log of each
#' chart printed, and one log file for the call. Called by `autospc()` and by
#' `facet_stages()`, between the analysis and the output.
#'
#' @param charts A list of analysed `autospc_chart` objects.
#' @param labels One label per chart, naming it in the log file and in the
#'   warning. The chart type by default; `facet_stages()` passes the stages.
#' @param short_message A function taking the labels of the charts that were
#'   short of points, and giving the warning to raise about them.
#'
#' @return invisible NULL
#' @noRd
report_analysis <- function(charts,
                            show_limits,
                            verbosity,
                            log_file_path,
                            labels = vapply(
                              charts,
                              function(chart) {
                                chart_type_label(chart)
                              },
                              character(1L)
                            ),
                            short_message = series_short_message) {
  short <- vapply(
    charts,
    function(chart) !enough_data_for_limits(chart),
    logical(1L)
  )

  if (show_limits && any(short)) {
    warning(short_message(labels[short], charts[short]))
  }

  for (chart in charts) {
    log_output(chart$result$table,
      verbosity = verbosity,
      chart_type = chart_type_label(chart)
    )
  }

  logs <- lapply(charts, function(chart) chart$result$table)
  names(logs) <- labels

  write_log_file(
    logs = logs,
    log_file_path = log_file_path
  )

  invisible(NULL)
}


#' The warning where a series has too few points for limits
#'
#' The halves of a pair are short together, so neither is named. The count
#' is of points the analysis can use, which is fewer than the rows supplied
#' where any of them hold no observation.
#'
#' @param charts The charts that are short of points.
#'
#' @return A string.
#' @noRd
series_short_message <- function(labels,
                                 charts) {
  chart <- location_component(charts)

  if (length(restart_rows(chart)) > 0L) {
    return(too_few_points_message(
      "Each segment of the series between restarts has"
    ))
  }

  points <- n_effective_points(chart, data = chart$data)

  return(too_few_points_message(
    paste("The input data has", points, "points,")
  ))
}


#' The warning where the stages of a faceted chart have too few points for
#' limits
#'
#' @param labels The stages that are short of points.
#'
#' @return A string.
#' @noRd
stages_short_message <- function(labels,
                                 charts) {
  subject <- paste("Stages", paste(labels, collapse = ", "), "have")

  if (length(labels) == 1L) {
    subject <- paste("Stage", labels, "has")
  }

  return(too_few_points_message(subject))
}


#' The sentence both warnings end with
#'
#' @param subject What the sentence is about, ending in has or have.
#'
#' @return A string.
#' @noRd
too_few_points_message <- function(subject) {
  return(paste(
    subject, "fewer than the minimum number of points needed to",
    "calculate one period. Timeseries data without limits has been",
    "displayed."
  ))
}


#' Print the log for one chart
#'
#' Called once per chart, so an XmR pair prints two logs and a faceted chart one
#' per facet. `write_log_file()` writes the file, once for the whole call.
#'
#' @return invisible TRUE
#' @noRd
log_output <- function(table,
                       verbosity,
                       chart_type) {
  if (verbosity > 0) {
    log_text <- interpret_log(table,
      verbosity = verbosity
    )
    cat(paste0(
      "\n",
      chart_type,
      ":\n\n"
    ))
    cat(log_text)
    cat("\n\n")
  }

  invisible(TRUE)
}


#' Write the log file for one call to autospc() or facet_stages()
#'
#' One file per call, holding every chart the call analysed. `chart` says which
#' each entry came from: the chart type for the two halves of an XmR pair, and
#' the stage for a faceted chart. A call that analyses one chart writes the same
#' shape, with one value in that column.
#'
#' The file holds the full log whatever `verbosity` says, as it always has.
#'
#' @param logs A named list of analysis tables, one per chart, named for the
#'   value `chart` should take.
#'
#' @return invisible TRUE if a file was written, invisible FALSE if not
#' @noRd
write_log_file <- function(logs,
                           log_file_path) {
  if (is.null(log_file_path)) {
    return(invisible(FALSE))
  }

  log_table <- lapply(logs,
    create_log_dataframe,
    verbosity = 2L
  )

  log_table <- as.data.frame(dplyr::bind_rows(log_table,
    .id = "chart"
  ))

  fext <- tools::file_ext(log_file_path)

  if (tolower(fext) == "rds") {
    tryCatch(
      expr = {
        saveRDS(log_table,
          file = log_file_path
        )
      },
      error = function(cnd) {
        message("Unable to save log file.")
        print(cnd)
      }
    )
  } else if (tolower(fext) == "csv") {
    tryCatch(
      expr = {
        utils::write.csv(log_table,
          file = log_file_path
        )
      },
      error = function(cnd) {
        message("Unable to save log file.")
        print(cnd)
      }
    )
  } else {
    warning("Invalid extension in log_file_path. Log file not written.")

    return(invisible(FALSE))
  }

  invisible(TRUE)
}


#' Build the log column from the chart's history
#'
#' Entries past the end of the table are held at the last row, prefixed
#' `co@N|` with the row they belong to.
#'
#' @return character vector, one element per row of the table, NA where the
#'   algorithm recorded nothing
#' @noRd
render_log <- function(chart) {
  n_rows <- nrow(chart$result$table)
  segments <- segment_rows(chart)

  rows <- integer(0)
  codes <- character(0)

  for (segment in seq_along(segments)) {
    entries <- segment_log_entries(chart,
      segment = segment,
      segments = segments
    )
    rows <- c(rows, entries$rows)
    codes <- c(codes, entries$codes)
  }

  return(collect_log_entries(rows, codes = codes, n_rows = n_rows))
}


#' The log code at the first row of a segment
#'
#' @param chart The chart, with its series prepared.
#' @param segment Which segment, counting from 1.
#' @param row The segment's first row.
#'
#' @return `0100` for the first segment, `0000` for a segment after a forced
#'   restart, and `0010` for one after a gap.
#' @noRd
restart_log_code <- function(chart,
                             segment,
                             row) {
  if (segment == 1L) {
    return("0100")
  }

  if (row %in% forced_restart_rows(chart)) {
    return("0000")
  }

  return("0010")
}


#' Log entries for one segment of the series
#'
#' The first segment starts with `0100`, each segment after a forced restart
#' with `0000` at its first row, and each segment after a gap with `0010`. Each
#' segment's entries are those its own history records: its first period, the
#' rule breaks and candidates whose counter lies in it, and why the algorithm
#' stopped looking in it - `stopped` for the last segment, and the restart
#' record that follows it for any other. An entry past the end of a segment
#' other than the last is held at its last row, marked as
#' `collect_log_entries()` marks one past the end of the series.
#'
#' @param chart The chart, with its history and table set.
#' @param segment Which segment, counting from 1.
#' @param segments The rows of each segment, as `segment_rows()` returns them.
#'
#' @return A list of `rows` and `codes`, in the order they were recorded.
#' @noRd
segment_log_entries <- function(chart,
                                segment,
                                segments) {
  this_segment <- segments[[segment]]
  first_row <- if (length(this_segment) > 0L) this_segment[[1L]] else 1L
  last_segment <- segment == length(segments)
  last_row <- if (last_segment) Inf else this_segment[[length(this_segment)]]
  history <- chart$history

  rows <- integer(0)
  codes <- character(0)
  add <- function(row, code) {
    rows <<- c(rows, as.integer(row))
    codes <<- c(codes, code)
  }

  held <- function() {
    beyond <- rows > last_row
    codes[beyond] <- paste0("co@", rows[beyond], "|", codes[beyond])
    rows[beyond] <- last_row

    return(list(rows = rows, codes = codes))
  }

  add(first_row, restart_log_code(chart, segment = segment, row = first_row))

  if (!enough_data_for_new_period(
    data = segment_data(chart,
      rows = this_segment,
      first_segment = segment == 1L
    ),
    counter = 1L,
    chart = chart
  )) {
    add(first_row, "0210")
    return(held())
  }

  add(first_row, "0200")

  stopped <- if (last_segment) {
    history$stopped
  } else {
    list(
      counter = history$restarts$counter[[segment]],
      reason = history$restarts$previous_segment_ended[[segment]]
    )
  }

  if (identical(stopped$reason, "baseline only")) {
    return(held())
  }

  in_segment <- function(row) {
    return(!is.na(row) & row >= first_row & row <= last_row)
  }

  path <- history$counter_path
  first_period <- path$to[
    path$reason == "first period established" & in_segment(path$from)
  ]
  add(first_period[[1L]], "0300")

  breaks <- history$breaks
  breaks <- breaks[in_segment(breaks$counter), , drop = FALSE]
  candidates <- Filter(
    function(candidate) in_segment(candidate$counter),
    history$candidates
  )

  for (i in seq_len(NROW(breaks))) {
    add(
      breaks$counter[i],
      paste0(
        if (breaks$already_at_break[i]) "0400" else "0401",
        breaks$position[i]
      )
    )

    if (is.na(breaks$position[i])) {
      next
    }

    add(breaks$position[i], paste0("0500", sign_chr(breaks$direction[i])))

    if (i <= length(candidates)) {
      candidate <- candidates[[i]]

      # a candidate in a prevented range is not formed, so has no checks
      if (!isTRUE(candidate$prevented)) {
        add(
          candidate$counter,
          paste0(
            "0600",
            as.integer(candidate$opposite_break),
            as.integer(candidate$final_run_prevents)
          )
        )
      }

      add(candidate$counter, candidate_decision_code(candidate))
    }
  }

  stop_code <- switch(stopped$reason,
    "not enough data for a further period" = "0410",
    "no further shift rule breaks" = "0510",
    "too few points after the shift rule break" = "0610",
    NULL
  )
  if (!is.null(stop_code)) {
    add(stopped$counter, stop_code)
  }

  return(held())
}


#' Log code for the decision on one candidate
#'
#' 0700 accepted; 0710 rejected because of an opposing rule break; 0720
#' rejected because the final run of the candidate calculation period is not
#' complete and may still become an opposing rule break, which ends the
#' algorithm; 0730 not formed, because it starts in a range given by
#' `prevent_establishment_in`.
#'
#' @return character, one log code
#' @noRd
candidate_decision_code <- function(candidate) {
  if (isTRUE(candidate$prevented)) {
    return("0730")
  }

  if (candidate$accepted) {
    return("0700")
  }

  if (!candidate$opposite_break && candidate$final_run_prevents) {
    return("0720")
  }

  return("0710")
}


#' Join log entries into one column, in the order they were recorded
#'
#' @return character vector of length n_rows
#' @noRd
collect_log_entries <- function(rows, codes, n_rows) {
  # entries past the end of the table are held at the last row
  overflow <- rows > n_rows
  codes[overflow] <- paste0("co@", rows[overflow], "|", codes[overflow])
  rows[overflow] <- n_rows

  log_column <- rep(NA_character_, n_rows)
  for (row in unique(rows)) {
    log_column[row] <- paste(codes[rows == row], collapse = ";")
  }

  return(log_column)
}
