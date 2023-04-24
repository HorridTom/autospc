# autospc
## Installation
Make sure you have the `devtools` package installed.

`install.packages("devtools")`

Then you can install the `autospc` package.

`devtools::install_github("HorridTom/autospc")`

This may then give you the option to install other packages that `autospc` is 
dependent on.

## Input data format
For a **C or C'** chart the data must have the following columns:

* Your x-axis variable which should be a date, POSIXct, numeric or integer type. The name of this
column can be specified in the `x` argument of the `plot_auto_SPC`.

* Your y-axis variable which should be a numeric or integer type. The name of this
column can be specified in the `y` argument of the `plot_auto_SPC` function, e.g. 
`plot_auto_spc(data, x = "x_column_name", y = "y_column_name")`

For a **P or P'** chart the data must have the following columns:

* Your x-axis variable which should be a date, POSIXct, numeric or integer type. The name of this
column can be specified in the `x` argument of the `plot_auto_SPC` function.

* The total count data or denominator (e.g. number of ED attendances) which should be a numeric or integer type. The name of this column can be specified in the `n` argument of the `plot_auto_SPC` function.

* The numerator (e.g. number of 4hr breaches) which should be a numeric or integer type. The name of this
column can be specified in the `y` argument of the `plot_auto_SPC` function, e.g. 
`plot_auto_spc(data, x = "x_column_name", n = "denominator_column_name", y = "numerator_column_name", chartType = "P'")`


## Running the charts
Run the following code on a `dataframe` that fits the above format with the columns "month", "breaches" and "attendances".

`autospc::plot_auto_SPC(dataframe, x = "month", y = "breaches")`

The default chart that will be plotted is a C' chart. This may be changed using the `chartType` argument. E.g.
`autospc::plot_auto_SPC(dataframe, x = "month", y = "breaches", n = "attendances", chartType = "P")`

There are various arguments that can be specified in this function call to change
the appearance of the chart. Use `??autospc::plot_auto_spc` to find out more.

## Getting the data table
Run the following code on a `dataframe` in the above format to return the data 
with control limits and other information.

`autospc::plot_auto_SPC(dataframe, plotChart = FALSE)`

`limits_table <- autospc::plot_auto_SPC(dataframe, , x = "month", y = "breaches", plotChart = FALSE)`

This stores the returned data in a variable called `limits_table`
