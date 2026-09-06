# autospc: Automatically Partitioned SPC Charts

Creates spc charts with control limits and centre line calculations
partitioned into distinct periods.

## Options

- `autospc.warn_missing_x`:

  Whether to warn when rows are excluded because `x` is `NA`. `TRUE`
  unless set otherwise. Set `options(autospc.warn_missing_x = FALSE)` to
  omit this warning, for instance when drawing many charts to avoid many
  warnings. The warning carries the class `"autospc_missing_x_warning"`,
  so it can also be handled on its own with
  [`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html).

## See also

Useful links:

- <https://horridtom.github.io/autospc/>

## Author

**Maintainer**: Thomas Woodcock <woodcock.thomas@gmail.com>
([ORCID](https://orcid.org/0000-0002-4735-4856))

Authors:

- Imogen O'Connor <imogen.connor-helleur@imperial.ac.uk>
