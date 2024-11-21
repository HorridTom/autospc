# autospc

## Overview

autospc provides a rigorous and consistent means of re-establishing limits on
Shewhart charts (also known as control charts), using the
_Stable Shift Algorithm_.

The main function is `plot_auto_SPC()`, which plots Shewhart charts of various
types appropriate for different types of data, with limits established using the
algorithm.

## Installation

autospc can be installed using the [devtools](https://cran.r-project.org/web/packages/devtools/index.html) package.

Install devtools if you have not already done so

`install.packages("devtools")`

Then you can install autospc

`devtools::install_github("HorridTom/autospc")`

This may then give you the option to install other packages that `autospc` is 
dependent on.

Once installed, you can load autospc as usual with

`library(autospc)`

## Usage

The example dataset `ed_attendances_monthly`, provided with autospc, is typical
of the time series data that the Stable Shift Algorithm is designed for. The
example code below uses this dataset, and can be executed once autospc is
loaded.

For a **C, C' or XmR** chart the data must have the following columns:

* Your x-axis variable: date, POSIXct, numeric or integer type. The name of this
column can be specified in the `x` argument of `plot_auto_SPC()`.

* Your y-axis variable : numeric or integer type. The name of this column can be
specified in the `y` argument of the `plot_auto_SPC()` function.

For example:

```
plot_auto_SPC(ed_attendances_monthly,
                chartType = "C'",
                x = Month_Start,
                y = Att_All)
```

For a **P or P'** chart the data must have the following columns:

* Your x-axis variable: date, POSIXct, numeric or integer type. The name of this
column can be specified in the `x` argument of `plot_auto_SPC()`.

* The denominator or total count (e.g. number of ED attendances): numeric or
integer type. The name of this column can be specified in the `n` argument of
`plot_auto_SPC()`.

* The numerator (e.g. number of ED attendances less than 4 hours in duration): 
numeric or integer type. The name of this column can be specified in the `y`
argument of `plot_auto_SPC()`.

For example:

```
plot_auto_SPC(ed_attendances_monthly,
                chartType = "P'",
                x = Month_Start,
                y = Within_4h,
                n = Att_All)
```

The parameters of the Stable Shift Algorithm, and the appearance of the chart,
can be configured through various arguments. Use `?autospc::plot_auto_SPC` to
find out more.

### Analysis output as a table

In addition to the default plot output, analysis results can be obtained in
table format using `plotChart = FALSE`, as follows:

```
limits_table <- plot_auto_SPC(ed_attendances_monthly,
                              chartType = "P'",
                              x = Month_Start,
                              y = Within_4h,
                              n = Att_All,
                              plotChart = FALSE)
                              
head(limits_table,
      n = 5L)
```

## Getting help

If you encounter a clear bug, please file an issue with a [minimal reproducible
example](https://forum.posit.co/t/faq-how-to-do-a-minimal-reproducible-example-reprex-for-beginners/23061)
on [GitHub](https://github.com/HorridTom/autospc/issues).
