# Data Requirements for autospc Analysis

``` r

library(autospc)
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
  y = att_all,
  max_exclusions = 3L
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

## 2 XMR charts (and their components, X and MR charts)

The data columns required for XMR, X and MR charts are as follows:

- The subgrouping variable, to be plotted on the horizontal axis, `x`
- The variable of interest, to be plotted on the vertical axis, `y`.
  This must be of type `integer` or `double`.

Unlike C/C’ charts (see Section [4](#c-and-c-charts)), XMR charts place
no further restriction on `y`: non-whole-number doubles are accepted
without modification or warning, since they are suitable for continuous
measurements as well as counts.

## 3 XbarS charts (and their components, Xbar and S charts)

Xbar and S charts are for measurements taken in subgroups. The Xbar
chart plots each subgroup’s mean and the S chart each subgroup’s sample
standard deviation. For example, [Adeleke et
al. (2019)](https://doi.org/10.1136/openhrt-2019-001086) used an XbarS
chart as a balancing measure in an initiative to improve the care of
patients with atrial fibrillation (AF) in 48 general practices in
Hounslow, London. Each subgroup was a month, and each measurement the
CHA₂DS₂-VASc stroke risk score of a patient newly diagnosed with AF in
that month, so the chart showed whether the stroke risk of the patients
being diagnosed changed as the initiative went on. Its supplementary
file 6 shows the Xbar chart, whose limits vary from month to month with
the number of patients diagnosed.

*autospc* supports two ways of supplying the data: observation-level
data, with one row per measurement, such as one row per newly diagnosed
patient; or aggregated data, with each row giving the mean, number and
standard deviation of some measurements, such as one row per practice
per month. In both cases the `x` column is required as for the above
chart types, and rows that share a value of `x` form one subgroup, so
`x` may be repeated.

### 3.1 Observation-level data (no `n` or `s` specified)

If neither `n` nor `s` is specified, each row is one measurement, and
`y` is the measurement. `y` must be of type `integer` or `double`, and
*autospc* computes each subgroup’s mean, size and standard deviation
from the rows that share its `x`.

### 3.2 Aggregated data (`n` and `s` specified)

If `n` and `s` are specified, each row summarises some measurements: `y`
is their mean, `n` their number and `s` their sample standard deviation.
All three must be of type `integer` or `double`. Where several rows
share an `x`, as where each row holds one practice’s patients in a
month, they are combined into one subgroup whose mean, size and standard
deviation are those of all their measurements together.

The two must be specified together. Specifying only one causes an error:

> For Xbar, S and XbarS charts given one row per subgroup, n and s must
> both be specified: y is then the subgroup mean, n its size and s its
> sample standard deviation.

A column called `n` or `s` in the data is used for that argument without
being specified, as `n` is for P and P’ charts.

Neither `n` nor `s` may be negative:

> For Xbar, S and XbarS charts, n and s cannot be negative.

The subgroup size `n` must be a whole number. Where it is of type
`double` with at least one non-whole-number value, the values are
rounded to the nearest whole number and a warning is issued:

> At least one element of n has non-zero fractional part. Rounding to
> the nearest whole number. Xbar, S and XbarS charts require n to be a
> subgroup size, i.e. whole numbers only.

### 3.3 Subgroups of one

A subgroup of one has a mean but no standard deviation. It is plotted on
the Xbar chart and counts towards its centre line, but has no control
limits there. It is not plotted on the S chart, and has no control
limits there either.

An aggregated row with a mean and a size of two or more, but no standard
deviation, is plotted on the Xbar chart with control limits, and is not
plotted on the S chart.

## 4 C and C’ charts

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

## 5 P and P’ charts

P and P’ charts are for proportions. They require a numerator (the count
meeting some criterion) and a denominator (the total count). *autospc*
supports two ways of supplying this information, depending on the form
in which the data are available: observation-level data using a
`logical` `y` column, or aggregated data using numeric `y` and `n`
columns. In both cases the `x` column is required as for the above chart
types.

### 5.1 Observation-level data (no `n` specified)

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

### 5.2 Aggregated data (`n` specified)

If `n` is specified, *autospc* expects both `y` (the numerator) and `n`
(the denominator) to be counts, i.e. whole numbers. Both columns must be
of type `integer` or `double`, and the same whole-number checking logic
described for C/C’ charts in Section [4](#c-and-c-charts) applies
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

## 6 Summary

Table [6.1](#tab:summary-table) summarises the column requirements for
each chart type.

| Chart type | y type(s) accepted | n type(s) accepted | s type(s) accepted |
|:---|:---|:---|:---|
| X / MR / XMR | `integer`, `double` | not used | not used |
| Xbar / S / XbarS (observation level) | `integer`, `double` | not used | not used |
| Xbar / S / XbarS (aggregated) | `integer`, `double` (the subgroup mean) | `integer`, `double` (the subgroup size; whole numbers only; non-integer doubles are rounded with a warning) | `integer`, `double` (the subgroup’s sample standard deviation) |
| C / C’ | `integer`, `double` (whole numbers only; non-integer doubles are rounded with a warning) | not used | not used |
| P / P’ (observation level) | `logical` | not used | not used |
| P / P’ (aggregated) | `integer`, `double` (whole numbers only; non-integer doubles are rounded with a warning) | `integer`, `double` (whole numbers only; non-integer doubles are rounded with a warning) | not used |

Table 6.1: Summary of data column requirements by chart type. {.table}
