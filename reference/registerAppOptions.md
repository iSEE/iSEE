# Set and get app-level options

Set and get global options for the
[`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md) application.
These are options that do not correspond to any
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) slot and
cannot be changed by the user after initialization.

## Usage

``` r
registerAppOptions(se, ...)

getAppOption(name, se, default = NULL)

getAllAppOptions(se)
```

## Arguments

- se:

  The SummarizedExperiment object to be supplied to
  [`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md).

- ...:

  Named options to register. Alternatively a single named list
  containing the options to register.

- name:

  String containing the name of the option to retrieve.

- default:

  Value to return if `name` is not present in the available options.

## Value

`registerAppOptions` will return `se`, modified with the
application-level options.

`getAppOption` will return the value of the specified option, or
`default` if that option is not available.

`getAllAppOptions` will return a named list of all registered options.

## Details

`registerAppOptions` provides an alternative mechanism for setting
global options, separate from
[`panelDefaults`](https://isee.github.io/iSEE/reference/panelDefaults.md).
The primary difference is that `registerAppOptions` allows tuning of
options that do not have a corresponding slot in any
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) subclass.
This makes it useful for parameters that the user should not or cannot
change within the application, as well as for fine-tuning parameters
that are too rarely used to have their own interface elements.

Known options include:

- `panel.color`:

  Named character vector of colors. The names of the vector should be
  set to the name of class to be overridden; if a class is not named
  here, its default color is used. It is highly recommended to define
  colors as hex color codes (e.g., `"#1e90ff"`), for full compatibility
  with both HTML elements and R plots.

- `color.maxlevels`:

  Maximum number of levels for a categorical variable used for coloring.
  Variables with more levels are coerced to numeric to avoid problems
  with an overly-large legend. Defaults to 24.

- `factor.maxlevels`:

  Maximum number of levels for a categorical variable to be used
  anywhere in the app. Variables with more levels are coerced to numeric
  to avoid rendering delays. Defaults to 100.

- `RowTable.select.details`:

  A function that takes a string containing the name of a feature (i.e.,
  the current selection in the
  [RowTable](https://isee.github.io/iSEE/reference/RowTable-class.md))
  and returns a HTML element with more details.

- `ColumnTable.select.details`:

  A function that takes a string containing the name of a sample (i.e.,
  the current selection in the
  [ColumnTable](https://isee.github.io/iSEE/reference/ColumnTable-class.md))
  and returns a HTML element with more details.

- `tooltip.signif`:

  Number of *significant* digits to display in the tooltip. Defaults to
  6.

The registered options are stored in the SummarizedExperiment to ensure
that we can recover the application state with the combination of the
SummarizedExperiment and list of Panel settings. By comparison, if we
had used a global cache as in
[`panelDefaults`](https://isee.github.io/iSEE/reference/panelDefaults.md),
we would need to save them separately to ensure that we can recover a
particular application state.

By default, `registerAppOptions` will add or replace individual
arguments specified by `...`. This means that users can call the
function multiple times to accumulate registered options in `se`. The
exception is if `...` contains a single list, in which case the entire
set of options is directly replaced by that list. For example, one could
supply a single empty list to clear `se` of all existing options.

## For developers

Developers of Panel subclasses can add arbitrary options to `...` to
help control the behavior of their Panel instances. We recommend
prefixing any options with the name of the package in the form of
`<PACKAGE>_<OPTION>`, so as to avoid conflicts with other options (in
the base classes, or in other downstream packages) that have the same
name.

For calls to `getAppOption` that occur after the
[`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md) app has started,
it is not actually necessary to supply `se`. The options in `se` are
transferred to a global option store when the app starts, allowing us to
call `getAppOption` without `se` in various Panel methods. This is
useful for some generics where `se` is not part of the function
signature. Developers can mimic this state (e.g., for unit testing) by
calling `.activateAppOptionRegistry` on the SummarizedExperiment
produced by `registerAppOptions`. Conversely, calling
`.deactivateAppOptionRegistry` will reset the global option store.

## Author

Aaron Lun

## Examples

``` r
se <- SummarizedExperiment()
se <- registerAppOptions(se, factor.maxlevels=10, color.maxlevels=10)

getAppOption("factor.maxlevels", se)
#> [1] 10
getAppOption("color.maxlevels", se)
#> [1] 10
getAppOption("random.other.thing", se, default=10)
#> [1] 10

getAllAppOptions(se)
#> $factor.maxlevels
#> [1] 10
#> 
#> $color.maxlevels
#> [1] 10
#> 

# For developers: you don't actually need to pass 'se' to the getters
# if they are being called inside Panel methods.
.activateAppOptionRegistry(se)
getAppOption("factor.maxlevels")
#> [1] 10
getAppOption("color.maxlevels")
#> [1] 10
.deactivateAppOptionRegistry()

# Wiping out all options.
se <- registerAppOptions(se, list())
getAllAppOptions(se)
#> list()
```
