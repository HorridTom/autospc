# Data Requirements for \*autospc\* Analysis

``` r

library(autospc)
library(dplyr)
```

For each type of Statistical Process Control (SPC) chart supported by
*autospc*, certain data columns must be specified, and those columns
must meet certain requirements. This article sets out these requirements
for each chart type.

There are two factors influencing the nature of these requirements.
First, the statistical theory behind SPC charts places restrictions on
the type of data each chart supports. For instance, the theory behind
C-charts draws on the Poisson distribution, which is a discrete
distribution defined on the non-negative integers. Therefore the
variable of interest for a C-chart must be a non-negative integer ( \\0,
1, 2, \dots\\). Second, the types of objects used to represent data in
**R**. For instance, a non-negative integer is most naturally
represented as an object of type `integer`, but there may also be
situations in which it is appropriate to use an object of type `double`.

The type requirements of *autospc* are intended to be a flexible as
possible, whilst reflecting the natural usage of the **R**’s object
types. For example, whilst technically an object of type `integer` can
be coerced to type `logical`, *autospc* does not allow `integer` data to
be used in place of `logical` in observation-level data for a P-chart,
since the risk of confusion with aggregated data is too great.

Below is an example call to
[`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md).
Note that as is often the case, the first argument (`data`) is passed by
position, and the rest are passed as named arguments.

``` r

autospc(
  ed_attendances_monthly, 
  chart_type = "C'", 
  x = month_start, 
  y = att_all
)
```

In what follows, we shall refer to the columns of the passed data (in
this example, the data `ed_attendances_monthly`) by the names of the
arguments they are passed to. So in this example, in the sentence “The
`y` column must be either an `integer` or `double`”, what is really
meant is “The `att_all` column must be either an `integer` or `double`”.

## 1 The subgrouping variable, `x`

The column specified as `x` will be used both in aggregating data into
subgroups for plotting (where this is relevant), and as the variable
plotted on the horizontal axis (i.e. ordering the data) on the chart.
Object classes that are currently supported for `x` are: `Date`,
`POSIXct`, `numeric`, and `integer`.

## 2 XMR charts

The data columns required for the XMR chart are as follows:

- The subgrouping variable, to be plotted on the horizontal axis, `x`
- The variable of interest, to be plotted on the vertical axis, `y`.
  This must be of type `integer` or `double`.

Unlike C/C’ charts (see Section [3](#c-and-c-charts)), XMR charts place
no further restriction on `y`: non-whole-number doubles are accepted
without modification or warning, since XMR charts are suitable for
continuous measurements as well as counts.

## 3 C and C’ charts

The data columns required for C and C’ charts are as follows:

- The subgrouping variable, to be plotted on the horizontal axis, `x`
- The count of events, to be plotted on the vertical axis, `y`. This
  must be of type `integer` or `double`.

Since C and C’ charts are for count data, `y` must consist of whole
numbers only. *autospc* handles two R types that can represent whole
numbers differently:

- **`integer`**: accepted without modification or warning.

- **`double`**: *autospc* checks whether all values of `y` are whole
  numbers (up to machine precision). If they are, the column is accepted
  without modification or warning. If at least one value has a non-zero
  fractional part, the values are rounded to the nearest whole number
  and a warning is issued:

  > At least one element of y has non-zero fractional part. Rounding to
  > the nearest whole number. C and C’ charts are for count data, i.e.
  > whole numbers only.

Any other type for `y`, including `logical`, will cause an error:

> For a C or C’ chart, y must be of type integer or double.

## 4 P and P’ charts

P and P’ charts are for proportions. They require a numerator (the count
meeting some criterion) and a denominator (the total count). *autospc*
supports two ways of supplying this information, depending on the form
in which the data are available: observation-level data using a
`logical` `y` column, or aggregated data using numeric `y` and `n`
columns. In both cases the `x` column is required as for the above chart
types.

### 4.1 Observation-level data (no `n` specified)

If `n` is not specified, *autospc* expects `y` to be a column of type
`logical`, where each row represents an individual observation and the
value of `y` indicates whether that observation meets the criterion of
interest (e.g. `TRUE` if a patient attending an emergency department was
discharged, admitted or transferred within 4 hours, `FALSE` otherwise).
In this case, *autospc* internally computes the numerator and
denominator by aggregating over subgroups defined by `x`.

Any type for `y` other than `logical` when `n` is absent will cause an
error:

> n is not specified and y is not of type logical. For P and P’ charts,
> if n is not specified, y must be of type logical.

### 4.2 Aggregated data (`n` specified)

If `n` is specified, *autospc* expects both `y` (the numerator) and `n`
(the denominator) to be counts, i.e. whole numbers. Both columns must be
of type `integer` or `double`, and the same whole-number checking logic
described for C/C’ charts in Section [3](#c-and-c-charts) applies
independently to each:

- **`integer`**: accepted without modification or warning.

- **`double` with all whole-number values**: accepted without
  modification or warning.

- **`double` with at least one non-whole-number value**: values are
  rounded to the nearest whole number and a warning is issued. For `y`
  the warning reads:

  > At least one element of y has non-zero fractional part. Rounding to
  > the nearest whole number. P and P’ charts with n specified require y
  > to be a count, i.e. whole numbers only.

  For `n` the analogous warning reads:

  > At least one element of n has non-zero fractional part. Rounding to
  > the nearest whole number. P and P’ charts with n specified require n
  > to be a count, i.e. whole numbers only.

Any other type for `y` when `n` is present, including `logical`, will
cause an error:

> For a P or P’ chart with n specified, y must be of type integer or
> double.

Similarly, any type for `n` other than `integer` or `double` will cause
an error:

> For a P or P’ chart with n specified, n must be of type integer or
> double.

## 5 Summary

Table [5.1](#tab:summary-table) summarises the column requirements for
each chart type.

| Chart type | y type(s) accepted | n type(s) accepted |
|:---|:---|:---|
| XMR / MR | `integer`, `double` | not used |
| C / C’ | `integer`, `double` (whole numbers only; non-integer doubles are rounded with a warning) | not used |
| P / P’ (observation level) | `logical` | not used |
| P / P’ (aggregated) | `integer`, `double` (whole numbers only; non-integer doubles are rounded with a warning) | `integer`, `double` (whole numbers only; non-integer doubles are rounded with a warning) |

Table 5.1: Summary of data column requirements by chart type. {.table}
