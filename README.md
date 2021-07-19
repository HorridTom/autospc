# autospc
## Installation
Make sure you have the `devtools` package installed.
`install.packages("devtools")

Then you can install the `autospc` package.
`devtools::install_github("HorridTom/autospc")`

This may then give you the option to install other packages that `autospc` is 
dependent on.

## Input data format
For a **C or C'** chart the data must have the following columns:
* "x" - your independent variable which should be a date
* "y" - your dependent variable which should be a count 
* "subtitle" - An optional column filled with a string of the subtitle that you 
want the chart to display (e.g. "Lanarkshire" for every row). Do not include this 
column if no subtitle is needed.
* "title"" - An optional column filled with a string of the subtitle that you 
want the chart to display (e.g. "ED Attendances per Day" for every row). Do not include this 
column if no title is needed.

For a **P or P'** chart the data must have the following columns:
* "x" - your independent variable which should be a date
* "n" - the total count data (e.g. number of ED attendances)
* "b" - the numerator (e.g. number of 4hr breaches) 
* "subtitle" - An optional column filled with a string of the subtitle that you 
want the chart to display (e.g. "Lanarkshire" for every row). Do not include this 
column if no subtitle is needed.
* "title"" - An optional column filled with a string of the subtitle that you 
want the chart to display (e.g. "ED Four Hour Performance" for every row). Do not include this 
column if no title is needed.

## Running the charts
Run the following code on a `dataframe` that fits the above format.
`autospc::plot_auto_spc(dataframe)`
There are various arguments that can be specified in this function call to change
the appearance of the chart. Use `??autospc::plot_auto_spc` to find out more.

## Getting the data
Run the following code on a `dataframe` in the above format to return the data 
with control limits and other information.
`autospc::plot_auto_spc(dataframe, plot_chart = F)`

`limits_table <- autospc::plot_auto_spc(dataframe, plot_chart = F)`
This stores the returned data in a variable called `limits_table`