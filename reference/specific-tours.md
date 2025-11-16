# UI-specific tour management utilities

Utilities to manage the tours specific to individual UI elements for
each [Panel](https://isee.github.io/iSEE/reference/Panel-class.md). This
is done via a global tour cache that is updated by each Panel's
interface-generating methods, so that developers can easily put the UI
documentation next to the element definition.

## Usage

``` r
.addSpecificTour(cls, field, fun, force = FALSE)

.getSpecificTours(cls)

.clearSpecificTours()
```

## Arguments

- cls:

  String containing the name of the
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) class
  containing the relevant UI element.

- field:

  String containing the slot of the `cls` class that is controlled by
  the UI element.

- fun:

  Function that accepts a string containing the encoded Panel name, and
  returns a data.frame compatible with rintrojs, i.e., with a `element`
  and `intro` column.

- force:

  Logical scalar indicating whether `fun` should forcibly overwrite an
  existing function in the tour cache for `cls` and `field`.

## Value

`.addSpecificTour` registers the provided function in the tour cache for
the provided `cls` and `field`. A `NULL` is invisibly returned.

`.getSpecificTours` returns a list of registered tour-generating
functions for the specified `cls`.

`.clearSpecificTours` removes all functions in the tour cache.

## Details

By default, `.addSpecificTour` will have no effect if a function is
already registered for a particular combination of `cls` and `field`.
However, users can force a replacement with `force=TRUE`.

`.clearSpecificTours` is intended for use by the iSEE app itself and
should not be used by Panel methods.

## See also

[`.selectInput.iSEE`](https://isee.github.io/iSEE/reference/interface-wrappers.md)
and friends, which provide modified UI elements that can be clicked to
launch a helpful tour.

## Author

Aaron Lun
