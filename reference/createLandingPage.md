# Create a landing page

Define a function to create a landing page in which users can specify or
upload SummarizedExperiment objects.

## Usage

``` r
createLandingPage(
  seUI = NULL,
  seLoad = NULL,
  initUI = NULL,
  initLoad = NULL,
  requireButton = TRUE
)
```

## Arguments

- seUI:

  Function that accepts a single `id` argument and returns a UI element
  for specifying the SummarizedExperiment.

- seLoad:

  Function that accepts the input value of the UI element from `seUI`
  and returns a SummarizedExperiment object.

- initUI:

  Function that accepts a single `id` argument and returns a UI element
  for specifying the initial state.

- initLoad:

  Function that accepts the input value of the UI element from `initUI`
  and returns a list of
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md)s.

- requireButton:

  Logical scalar indicating whether the app should require an explicit
  button press to initialize, or if it should initialize upon any
  modification to the UI element in `seUI`.

## Value

A function that generates a landing page upon being passed to
[`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md) as the
`landingPage` argument.

## Details

By default, this function creates a landing page in which users can
upload an RDS file containing a SummarizedExperiment, which is
subsequently read by [`readRDS`](https://rdrr.io/r/base/readRDS.html) to
launch an instance of
[`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md). However, any
source of SummarizedExperiment objects can be used; for example, we can
retrieve them from databases by modifying `seUI` and `seLoad`
appropriately.

The default landing page also allows users to upload a RDS file
containing a list of
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md)s that
specifies the initial state of the
[`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md) instance (to be
used as the `initial` argument in
[`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md)). Again, any
source can be used to create this list if `initUI` and `initLoad` are
modified appropriately.

The UI elements for the SummarizedExperiment and the initial state are
named `"se"` and `"initial"` respectively. This can be used in Shiny
bookmarking to initialize an
[`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md) in a desired
state by simply clicking a link, provided that `requireButton=FALSE` so
that reactive expressions are immediately triggered upon setting `se=`
and `initial=` in the URL. We do not use bookmarking to set all
individual [`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md)
parameters as we will run afoul of URL character limits.

## Defining a custom landing page

We note that `createLandingPage` is just a limited wrapper around the
landing page API. In
[`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md), `landingPage`
can be any function that accepts the following arguments:

- `FUN`, a function to initialize the
  [`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md) observer
  architecture. This function expects to be passed:

  - `SE`, a SummarizedExperiment object.

  - `INITIAL`, a list of
    [Panel](https://isee.github.io/iSEE/reference/Panel-class.md)
    objects describing the initial application state. If `NULL`, the
    initial state from `initial` in the top-level
    [`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md) call is used
    instead.

  - `TOUR`, a data.frame containing a tour to be attached to the app -
    see
    [`defaultTour`](https://isee.github.io/iSEE/reference/defaultTour.md)
    for an example. If `NULL` (the default), no tour is added.

  - `COLORMAP`, an
    [ExperimentColorMap](https://isee.github.io/iSEE/reference/ExperimentColorMap-class.md)
    object that defines the colormaps to use in the application.

- `input`, the Shiny input list.

- `output`, the Shiny output list.

- `session`, the Shiny session object.

The `landingPage` function should define a
[`renderUI`](https://rdrr.io/pkg/shiny/man/renderUI.html) expression
that is assigned to `output$allPanels`. This should define a UI that
contains all widgets necessary for a user to set up an
[`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md) session
interactively. We suggest that all UI elements have IDs prefixed with
`"initialize_INTERNAL"` to avoid conflicts.

The function should also define observers to respond to user
interactions with the UI elements. These are typically used to define a
SummarizedExperiment object and an input state as a list of
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md)s; any one
of these observers may then call `FUN` on those arguments to launch the
main [`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md) instance.

Note that, once the main app is launched, the UI elements constructed
here are lost and observers will never be called again. There is no
explicit “unload” mechanism to return to the landing page from the main
app, though a browser refresh is usually sufficient.

## Author

Aaron Lun

## Examples

``` r
createLandingPage()
#> function (FUN, input, output, session) 
#> {
#>     output$allPanels <- renderUI({
#>         tagList(seUI(.initializeSE), initUI(.initializeInitial), 
#>             if (requireButton) 
#>                 actionButton(.initializeLaunch, label = "Launch", 
#>                   style = .actionbutton_biocstyle))
#>     })
#>     target <- if (requireButton) 
#>         .initializeLaunch
#>     else .initializeSE
#>     observeEvent(input[[target]], {
#>         se2 <- try(seLoad(input[[.initializeSE]]))
#>         if (is(se2, "try-error")) {
#>             showNotification("invalid SummarizedExperiment supplied", 
#>                 type = "error")
#>         }
#>         else {
#>             init <- try(initLoad(input[[.initializeInitial]]))
#>             if (is(init, "try-error")) {
#>                 showNotification("invalid initial state supplied", 
#>                   type = "warning")
#>                 init <- NULL
#>             }
#>             FUN(SE = se2, INITIAL = init)
#>         }
#>     }, ignoreNULL = TRUE, ignoreInit = TRUE)
#>     invisible(NULL)
#> }
#> <bytecode: 0x559f01a3f6e0>
#> <environment: 0x559f01a451f8>

# Alternative approach, to create a landing page
# that opens one of the datasets from the scRNAseq package.
library(scRNAseq)
all.data <- ls("package:scRNAseq")
all.data <- all.data[grep("Data$", all.data)]

lpfun <- createLandingPage(
    seUI=function(id) selectInput(id, "Dataset:", choices=all.data),
    seLoad=function(x) get(x, as.environment("package:scRNAseq"))()
)

app <- iSEE(landingPage=lpfun)
if (interactive()) {
  shiny::runApp(app, port=1234)
}
```
