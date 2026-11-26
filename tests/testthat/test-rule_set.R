# The rules of a set other than rule 1 and the shift rule, one row per rule, so
# that a named set can be compared with its source's rules as a table
rules_of <- function(set) {
  field <- function(name, missing_value) {
    return(vapply(
      set$rules,
      function(rule) if (is.null(rule[[name]])) missing_value else rule[[name]],
      missing_value
    ))
  }

  return(data.frame(
    id = vapply(set$rules, function(rule) rule$id, integer(1L)),
    kind = vapply(set$rules, function(rule) rule$kind, character(1L)),
    k = vapply(set$rules, function(rule) rule$k, integer(1L)),
    m = field("m", NA_integer_),
    beyond = field("beyond", NA_real_),
    within = field("within", NA_real_),
    side = field("side", NA_character_)
  ))
}


# the named sets, against the rules of their sources


test_that("the Provost and Murray set holds their rules 3 to 5, with a shift of 8 as rule 2", {
  set <- rule_set("provost_murray")

  expect_identical(set$shift_id, 2L)
  expect_identical(set$shift_length, 8L)
  expect_identical(set$label, "Provost and Murray")
  expect_identical(
    rules_of(set),
    data.frame(
      id = 3:5,
      kind = c("trend", "zone", "zone"),
      k = c(6L, 2L, 15L),
      m = c(NA, 3L, 15L),
      beyond = c(NA, 2, NA),
      within = c(NA, NA, 1),
      side = c(NA, "either", "either")
    )
  )
})


test_that("the Western Electric set holds tests 2 and 3, with a shift of 8 as test 4", {
  set <- rule_set("western_electric")

  expect_identical(set$shift_id, 4L)
  expect_identical(set$shift_length, 8L)
  expect_identical(set$label, "Western Electric")
  expect_identical(
    rules_of(set),
    data.frame(
      id = 2:3,
      kind = c("zone", "zone"),
      k = c(2L, 4L),
      m = c(3L, 5L),
      beyond = c(2, 1),
      within = c(NA_real_, NA_real_),
      side = c("same", "same")
    )
  )
})


test_that("the Nelson set holds tests 3 to 8, with a shift of 9 as test 2", {
  set <- rule_set("nelson")

  expect_identical(set$shift_id, 2L)
  expect_identical(set$shift_length, 9L)
  expect_identical(set$label, "Nelson")
  expect_identical(
    rules_of(set),
    data.frame(
      id = 3:8,
      kind = c("trend", "alternating", "zone", "zone", "zone", "zone"),
      k = c(6L, 14L, 2L, 4L, 15L, 8L),
      m = c(NA, NA, 3L, 5L, 15L, 8L),
      beyond = c(NA, NA, 2, 1, NA, 1),
      within = c(NA, NA, NA, NA, 1, NA),
      side = c(NA, NA, "same", "same", "either", "either")
    )
  )
})


test_that("every named set has a label for each rule", {
  for (name in autospc_rule_set_names()) {
    labels <- rule_set_table(rule_set(name))$label

    expect_false(anyNA(labels), info = name)
    expect_false(any(labels == ""), info = name)
  }
})


test_that("the name of a set is not given with shift_id, shift_length or label", {
  expect_error(rule_set("nelson", shift_id = 2), "not given with the name")
  expect_error(rule_set("nelson", shift_length = 9), "not given with the name")
  expect_error(rule_set("nelson", label = "N"), "not given with the name")
})


test_that("a name that is not a named set is an error naming the sets", {
  expect_error(
    rule_set("nelsen"),
    "\"nelsen\" is not the name of a rule set. The named sets are"
  )
})


# a set of your own


test_that("shift_id and shift_length must be given", {
  expect_error(rule_set(trend_rule(k = 6), shift_length = 8), "`shift_id`")
  expect_error(rule_set(trend_rule(k = 6), shift_id = 2), "`shift_length`")
})


test_that("shift_id cannot be 1", {
  expect_error(
    rule_set(trend_rule(k = 6), shift_id = 1, shift_length = 8),
    "`shift_id` must not be 1"
  )
})


test_that("shift_id and shift_length are whole numbers of one or more", {
  expect_error(
    rule_set(trend_rule(k = 6), shift_id = 2.5, shift_length = 8),
    "`shift_id` must be a whole number"
  )
  expect_error(
    rule_set(trend_rule(k = 6), shift_id = 2, shift_length = 0),
    "`shift_length` must be a whole number"
  )
})


test_that("each rule of a set is made by a rule function", {
  expect_error(
    rule_set(list(k = 6), shift_id = 2, shift_length = 8),
    "must be made by `zone_rule\\(\\)`"
  )
})


test_that("a set may hold no rules besides rule 1 and the shift rule", {
  set <- rule_set(shift_id = 2, shift_length = 8)

  expect_identical(rule_set_table(set)$id, 1:2)
})


test_that("rules given no id take the free numbers from 2, leaving out shift_id", {
  set <- rule_set(
    trend_rule(k = 6),
    alternating_rule(k = 14),
    zone_rule(k = 2, m = 3, beyond = 2),
    shift_id = 3,
    shift_length = 8
  )

  expect_identical(rules_of(set)$id, c(2L, 4L, 5L))
})


test_that("rules given ids keep them, with gaps", {
  set <- rule_set(
    zone_rule(k = 2, m = 3, beyond = 2, side = "same", id = 5),
    shift_id = 2,
    shift_length = 9
  )

  expect_identical(rule_set_table(set)$id, c(1L, 2L, 5L))
})


test_that("either every rule is given an id or none is", {
  expect_error(
    rule_set(trend_rule(k = 6, id = 3), alternating_rule(k = 14),
      shift_id = 2, shift_length = 8
    ),
    "Either every rule of a set is given an `id`, or none is"
  )
})


test_that("no rule may take id 1 or the shift rule's id", {
  expect_error(
    rule_set(trend_rule(k = 6, id = 1), shift_id = 2, shift_length = 8),
    "No rule of a set may have id 1"
  )
  expect_error(
    rule_set(trend_rule(k = 6, id = 2), shift_id = 2, shift_length = 8),
    "No rule of a set may have id 1"
  )
})


test_that("no two rules may share an id", {
  expect_error(
    rule_set(trend_rule(k = 6, id = 3), alternating_rule(k = 14, id = 3),
      shift_id = 2, shift_length = 8
    ),
    "No two rules of a set may have the same id"
  )
})


test_that("rules are listed in increasing order of id", {
  expect_error(
    rule_set(trend_rule(k = 6, id = 4), alternating_rule(k = 14, id = 3),
      shift_id = 2, shift_length = 8
    ),
    "increasing order of id"
  )
})


test_that("a rule given no label takes its description", {
  set <- rule_set(
    trend_rule(k = 5),
    zone_rule(k = 2, m = 3, beyond = 2, label = "Near a limit"),
    shift_id = 2,
    shift_length = 8
  )

  expect_identical(
    vapply(set$rules, function(rule) rule$label, character(1L)),
    c("5 increasing or decreasing", "Near a limit")
  )
})


test_that("rule 1 and the shift rule have fixed descriptions and labels", {
  rules <- rule_set_table(rule_set("western_electric"))

  expect_identical(
    rules[rules$id %in% c(1L, 4L), c("kind", "description", "label")],
    data.frame(
      kind = c("outside_limits", "shift"),
      description = c(
        "1 point outside the limits",
        "8 in a row on one side of the centre line"
      ),
      label = c("Outside limits", "Shift")
    ),
    ignore_attr = TRUE
  )
})


# the rule functions


test_that("zone_rule takes k of m with k at most m", {
  expect_error(zone_rule(k = 4, m = 3, beyond = 1), "`k` must be at most `m`")
  expect_error(zone_rule(k = 1.5, m = 3, beyond = 1), "`k` must be")
})


test_that("zone_rule needs at least one of beyond and within", {
  expect_error(zone_rule(k = 2, m = 3), "At least one of `beyond` and `within`")
})


test_that("zone_rule's beyond is less than its within", {
  expect_error(
    zone_rule(k = 2, m = 3, beyond = 2, within = 2),
    "`beyond` must be less than `within`"
  )
  expect_error(zone_rule(k = 2, m = 3, beyond = -1), "`beyond` must be")
  expect_error(zone_rule(k = 2, m = 3, within = 0), "`within` must be")
})


test_that("zone_rule's side is either or same, and defaults to either", {
  expect_identical(zone_rule(k = 2, m = 3, beyond = 2)$side, "either")
  expect_error(zone_rule(k = 2, m = 3, beyond = 2, side = "above"), "side")
})


test_that("trend_rule needs at least two points, alternating_rule three", {
  expect_error(trend_rule(k = 1), "`k` must be at least 2")
  expect_error(alternating_rule(k = 2), "`k` must be at least 3")
})


test_that("a rule's id is a whole number and its label a single string", {
  expect_error(trend_rule(k = 6, id = 0), "`id` must be")
  expect_error(trend_rule(k = 6, label = 1), "`label` must be a single string")
})


# descriptions


test_that("a zone rule is described by its count, zone and side", {
  describe <- function(...) rule_description(zone_rule(...))

  expect_identical(
    describe(k = 2, m = 3, beyond = 2, side = "same"),
    "2 of 3 beyond 2 SD, on one side"
  )
  expect_identical(
    describe(k = 15, m = 15, within = 1),
    "15 in a row within 1 SD, either side"
  )
  expect_identical(
    describe(k = 3, m = 4, beyond = 1, within = 2),
    "3 of 4 between 1 and 2 SD, either side"
  )
  expect_identical(
    describe(k = 2, m = 3, beyond = 1.5),
    "2 of 3 beyond 1.5 SD, either side"
  )
})


test_that("beyond 3 SD reads outside the limits, and a single point has no side", {
  expect_identical(
    rule_description(zone_rule(k = 1, m = 1, beyond = 3)),
    "1 point outside the limits"
  )
  expect_identical(
    rule_description(zone_rule(k = 2, m = 3, beyond = 3, side = "same")),
    "2 of 3 outside the limits, on one side"
  )
})


test_that("a zone from the centre line with no outer end reads of the centre line", {
  expect_identical(
    rule_description(zone_rule(k = 8, m = 8, beyond = 0, side = "same")),
    "8 in a row on one side of the centre line"
  )
})


test_that("trend and alternating rules are described by their count", {
  expect_identical(
    rule_description(trend_rule(k = 6)),
    "6 increasing or decreasing"
  )
  expect_identical(
    rule_description(alternating_rule(k = 14)),
    "14 alternating up and down"
  )
})


# printing


test_that("a rule set prints each rule's column, description and label", {
  expect_output(print(rule_set("nelson")), "<autospc_rule_set> Nelson")
  expect_output(
    print(rule_set("nelson")),
    "rule2  9 in a row on one side of the centre line \\(shift\\)  Shift"
  )
  expect_output(
    print(rule_set("nelson")),
    "rule4  14 alternating up and down"
  )
})


test_that("printing a rule set or a rule returns it invisibly", {
  set <- rule_set("provost_murray")
  rule <- trend_rule(k = 6)

  expect_output(expect_invisible(returned_set <- print(set)))
  expect_identical(returned_set, set)
  expect_output(expect_invisible(returned_rule <- print(rule)))
  expect_identical(returned_rule, rule)
})


test_that("a rule prints its description", {
  expect_output(
    print(trend_rule(k = 6)),
    "<autospc_rule> 6 increasing or decreasing"
  )
})
