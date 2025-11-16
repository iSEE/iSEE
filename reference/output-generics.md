# Generics for Panel outputs

An overview of the generics for defining the panel outputs, along with
recommendations on their implementation.

## Defining the output element

`.defineOutput(x)` defines the output element of the panel (e.g., a plot
or table widget), given an instance of a
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) subclass
in `x`.

Methods for this generic are expected to return a HTML element
containing the visual output of the panel, such as the return value of
[`plotOutput`](https://rdrr.io/pkg/shiny/man/plotOutput.html) or
[`dataTableOutput`](https://rdrr.io/pkg/shiny/man/renderDataTable.html).
This element will be shown in the iSEE interface above the parameter
boxes for `x`. Multiple elements can be returned via a
[`tagList`](https://rstudio.github.io/htmltools/reference/tagList.html).

The IDs of the output elements are expected to be prefixed with the
panel name from
[`.getEncodedName`](https://isee.github.io/iSEE/reference/getEncodedName.md)`(x)`
and an underscore, e.g., `"ReducedDimensionPlot1_someOutput"`. One of
the output elements may simply have the ID set to `PANEL` alone; this is
usually the case for simple panels with one primary output like a
[DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md).

## Defining the rendered output

`.renderOutput(x, se, ..., output, pObjects, rObjects)` will create an
expression to render the panel's output. The following arguments are
required:

- `x`, an instance of a
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) class.

- `se`, a SummarizedExperiment object containing the current dataset.

- `...`, further arguments that may be used by specific methods.

- `output`, the Shiny output object from the server function.

- `pObjects`, an environment containing global parameters generated in
  the [`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md) app.

- `rObjects`, a reactive list of values generated in the
  [`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md) app.

It is expected to attach one or more reactive expressions to `output` to
render the output element(s) defined by `.defineOutput`. This is
typically done by calling shiny rendering functions like
[`renderPlot`](https://rdrr.io/pkg/shiny/man/renderPlot.html) or the
most appropriate equivalent for the panel's output. The return value of
this generic is not used; only the side-effect of the reactive output
set-up is relevant.

The rendering expression inside the chosen rendering function is
expected to:

1.  Call `force(rObjects[[PANEL]])`, where `PANEL` is the output of
    [`.getEncodedName`](https://isee.github.io/iSEE/reference/getEncodedName.md)`(x)`.
    This ensures that the output is rerendered upon requesting changes
    in
    [`.requestUpdate`](https://isee.github.io/iSEE/reference/requestUpdate.md).

2.  Call `.generateOutput` to generate the output content to be
    rendered.

3.  Fill `pObjects$contents[[PANEL]]` with some content related to the
    displayed output that allows cross-referencing with single/multiple
    selection structures. This will be used in other generics like
    [`.multiSelectionCommands`](https://isee.github.io/iSEE/reference/multi-select-generics.md)
    and
    [`.singleSelectionValue`](https://isee.github.io/iSEE/reference/single-select-generics.md)
    to determine the identity of the selected point(s). As a result, it
    is only strictly necessary if the panel is a potential transmitter,
    as determined by the return value of
    [`.multiSelectionDimension`](https://isee.github.io/iSEE/reference/multi-select-generics.md).

4.  Fill `pObjects$commands[[PANEL]]` with a character vector of
    commands required to produce the displayed output. This should
    minimally include the commands required to generate
    `pObjects$contents[[PANEL]]`; for plotting panels, the vector should
    also include code to create the plot.

5.  Fill `pObjects$varname[[PANEL]]` with a string containing the R
    expression in `pObjects$commands[[PANEL]]` that holds the contents
    stored in `pObjects$contents[[PANEL]]`. This is used for code
    reporting, and again, is only strictly necessary if the panel is a
    potential transmitter.

We strongly recommend calling
[`.retrieveOutput`](https://isee.github.io/iSEE/reference/retrieveOutput.md)
within the rendering expression, which will automatically perform all of
the tasks above, rather than calling `.generateOutput` manually. By
doing so, the only extra work required of the rendering expression is to
actually render the output (e.g., by `print`ing a
[ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object).
Of course, the rendering expression must itself be encapsulated by an
appropriate rendering function assigned to `output`.

Developers should not attempt to modify `x` in any rendering expression.
This does not have pass-by-reference semantics and any changes will not
propagate to other parts of the application. Similarly, the rendering
expression should treat `pObjects$memory` as read-only. Any adjustment
of parameters should be handled elsewhere, e.g., by the observer
expressions in
[`.createObservers`](https://isee.github.io/iSEE/reference/observer-generics.md).

## Generating content

`.generateOutput(x, se, all_memory, all_contents)` actually generates
the panel's output to be used in the rendering expression. The following
arguments are required:

- `x`, an instance of a
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) class.

- `se`, a SummarizedExperiment object containing the current dataset.

- `all_memory`, a named list containing
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) objects
  parameterizing the current state of the app.

- `all_contents`, a named list containing the contents of each panel.

Methods for this generic should return a list containing:

- `contents`, some arbitrary content for the panel (usually a
  data.frame). The values therein are used by
  [`.multiSelectionCommands`](https://isee.github.io/iSEE/reference/multi-select-generics.md)
  to determine the multiple row/column selection in `x` to be
  transmitted to other (child) panels. The app will ensure that the
  `pObjects$contents` of each panel is populated before attempting to
  render their children. `contents` may be set to `NULL` if `x` does not
  transmit, i.e.,
  [`.multiSelectionDimension`](https://isee.github.io/iSEE/reference/multi-select-generics.md)
  returns `"none"`.

- `commands`, a list of character vectors of R commands that, when
  executed, produces the contents of the panel and any displayed output
  (e.g., a [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
  object). Developers should write these commands as if the evaluation
  environment only contains the SummarizedExperiment `se` and
  ExperimentColorMap `colormap`. It may also contain `col_selected`, if
  a multiple column selection is being transmitted to `x`; and possibly
  `row_selected`, if a multiple row selection is being transmitted to
  `x`.

- `varname`, a string specifying the name of the variable in `commands`
  used to generate `contents`. This is used to fulfill code tracking
  obligations. If the current panel is not a transmitter, this may be
  set to `NULL` instead.

The output list may contain any number of other fields that can be used
by `.renderOutput` but are otherwise ignored.

We suggest implementing this method using
[`eval`](https://rdrr.io/r/base/eval.html)`(`[`parse`](https://rdrr.io/r/base/parse.html)`(text=...))`
calls, which enables easy construction and evaluation of the commands
and contents at the same time. A convenient wrapper for this call is
provided by the
[`.textEval`](https://isee.github.io/iSEE/reference/manage_commands.md)
utility.

The `all_memory` and `all_contents` arguments are provided for the sole
purpose of determining what multiple selections are being received by
`x`. We strongly recommend passing them onto
[`.processMultiSelections`](https://isee.github.io/iSEE/reference/processMultiSelections.md)
to do the heavy lifting. It would be unusual and inadvisable to use
these arguments for any other information sharing across panels.

## Exporting content

`.exportOutput(x, se, all_memory, all_contents)` converts the panel
output into a downloadable form. The following arguments are required:

- `x`, an instance of a
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) class.

- `se`, a SummarizedExperiment object containing the current dataset.

- `all_memory`, a named list containing
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) objects
  parameterizing the current state of the app.

- `all_contents`, a named list containing the contents of each panel.

Methods for this generic should generate appropriate files containing
the content of `x`. (For example, plots may create PDFs while tables may
create CSV files.) All files should be created in the working directory
at the time of the function call, possibly in further subdirectories.
Each file name should be prefixed with the
[`.getEncodedName`](https://isee.github.io/iSEE/reference/getEncodedName.md).
The method itself should return a character vector containing *relative*
paths to all newly created files.

To implement this method, we suggest simply passing all arguments onto
`.generateOutput` and then converting the output into an appropriate
file.

## Author

Aaron Lun
