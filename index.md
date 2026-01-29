# autospc

## Overview

autospc provides a rigorous and consistent means of re-establishing
limits on Shewhart charts (also known as control charts), using the
*Stable Shift Algorithm*.¹

The main function is
[`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md),
which plots Shewhart charts of various types appropriate for different
types of data, with limits established using the algorithm.

## Installation

autospc can be installed using the [pak](https://pak.r-lib.org/)
package.

Install pak if you have not already done so

`install.packages("pak")`

Then you can install autospc

`pak::pkg_install("HorridTom/autospc")`

This may then give you the option to install other packages that
`autospc` is dependent on.

Note that you may need to install the necessary toolchains for building
R packages from source:
[RTools](https://cran.r-project.org/bin/windows/Rtools/) for Windows,
[Xcode and a Fortran
compiler](https://cran.r-project.org/bin/macosx/tools/) for Mac OS X.

Once installed, you can load autospc as usual with

[`library(autospc)`](https://horridtom.github.io/autospc/)

## Usage

The example dataset `ed_attendances_monthly`, provided with autospc, is
typical of the time series data that the Stable Shift Algorithm is
designed for. The example code below uses this dataset, and can be
executed once autospc is loaded.

For a **C, C’ or XmR** chart the data must have the following columns:

- Your x-axis variable: date, POSIXct, numeric or integer type. The name
  of this column can be specified in the `x` argument of
  [`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md).

- Your y-axis variable : numeric or integer type. The name of this
  column can be specified in the `y` argument of the
  [`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md)
  function.

For example:

    autospc(ed_attendances_monthly,
                    chart_type = "C'",
                    x = month_start,
                    y = att_all)

For a **P or P’** chart the data must have the following columns:

- Your x-axis variable: date, POSIXct, numeric or integer type. The name
  of this column can be specified in the `x` argument of
  [`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md).

- The denominator or total count (e.g. number of ED attendances):
  numeric or integer type. The name of this column can be specified in
  the `n` argument of
  [`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md).

- The numerator (e.g. number of ED attendances less than 4 hours in
  duration): numeric or integer type. The name of this column can be
  specified in the `y` argument of
  [`autospc()`](https://horridtom.github.io/autospc/reference/autospc.md).

For example:

    autospc(ed_attendances_monthly,
                    chart_type = "P'",
                    x = month_start,
                    y = within_4h,
                    n = att_all)

The parameters of the Stable Shift Algorithm, and the appearance of the
chart, can be configured through various arguments. Use
[`?autospc::autospc`](https://horridtom.github.io/autospc/reference/autospc.md)
to find out more.

### Analysis output as a table

In addition to the default plot output, analysis results can be obtained
in table format using `plot_chart = FALSE`, as follows:

    limits_table <- autospc(ed_attendances_monthly,
                                  chart_type = "P'",
                                  x = month_start,
                                  y = within_4h,
                                  n = att_all,
                                  plot_chart = FALSE)

    head(limits_table,
          n = 5L)

## Getting help

If you encounter a clear bug, please file an issue with a [minimal
reproducible
example](https://forum.posit.co/t/faq-how-to-do-a-minimal-reproducible-example-reprex-for-beginners/23061)
on [GitHub](https://github.com/HorridTom/autospc/issues).

## References

1.  Woodcock T, O’Connor I, Bell D. Re-establishing control limits in
    statistical process control analyses: the stable shift algorithm.
    *BMJ Quality & Safety* Published Online First: 30 November 2025.
    doi:
    [10.1136/bmjqs-2025-019263](https://dx.doi.org/10.1136/bmjqs-2025-019263)
