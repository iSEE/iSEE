# Set default slot values

A utility function to set slots to default values if their values are
not provided to `initialize` methods.

## Usage

``` r
.emptyDefault(args, field, default)
```

## Arguments

- args:

  A named list of arguments to pass to the `initialize` method for a
  given class.

- field:

  String specifying the field to set.

- default:

  The default value of the slot in `field`.

## Value

`args` is returned with the named `field` set to `default` if it was
previously absent.

## Details

A more natural approach would be to have the default values in the
arguments of the `initialize` method. However, this would require us to
hard-code the slot names in the function signature, which would break
our current DRY model of only specifying the slot names once.

## Author

Aaron Lun, Kevin Rue-Albrecht

## Examples

``` r
showMethods("initialize", classes = "ReducedDimensionPlot", includeDefs = TRUE)
#> Function: initialize (package methods)
#> .Object="ReducedDimensionPlot"
#> function (.Object, ...) 
#> {
#>     args <- list(...)
#>     args <- .emptyDefault(args, .redDimType, NA_character_)
#>     args <- .emptyDefault(args, .redDimXAxis, 1L)
#>     args <- .emptyDefault(args, .redDimYAxis, 2L)
#>     do.call(callNextMethod, c(list(.Object), args))
#> }
#> 
#> 
#> 
```
