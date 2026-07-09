# Generic for the panel observers

The workhorse generic for defining the Shiny observers for a given
panel, along with recommendations on its implementation.

## Creating parameter observers

In `.createObservers(x, se, input, session, pObjects, rObjects)`, the
required arguments are:

- `x`, an instance of a
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) class.

- `se`, a SummarizedExperiment object containing the current dataset.
  This can be assumed to have been produced by running
  [`.refineParameters`](https://isee.github.io/iSEE/reference/setup-generics.md)`(x, se)`.

- `input`, the Shiny input object from the server function.

- `session`, the Shiny session object from the server function.

- `pObjects`, an environment containing global parameters generated in
  the [`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md) app.

- `rObjects`, a reactive list of values generated in the
  [`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md) app.

Methods for this generic are expected to set up all observers required
to respond to changes in the interface elements set up by
[`.defineInterface`](https://isee.github.io/iSEE/reference/interface-generics.md).
Recall that each interface element has an ID of the form of
`PANEL_SLOT`, where `PANEL` is the panel name (from
[`.getEncodedName`](https://isee.github.io/iSEE/reference/getEncodedName.md))
and `SLOT` is the name of the slot modified by the interface element.
Thus, observers should respond to changes in those elements in `input`.
The return value of this generic is not used; only the side-effect of
observer set-up is relevant.

It is the developer's responsibility to call
[`callNextMethod`](https://rdrr.io/r/methods/NextMethod.html) to set up
the observers required by the parent class. This is best done by calling
[`callNextMethod`](https://rdrr.io/r/methods/NextMethod.html) at the top
of the method before defining up additional observers. Each parent class
should implement observers for its slots, so it is usually only
necessary to implement observers for any newly added slots in a
particular class.

## Modifying the memory

Consider an observer for an interface element that modifies a slot of
`x`. The code within this observer is expected to modify the “memory” of
the app state in `pObjects`, via:

    new_value <- input[[paste0(PANEL, "_", SLOT)]]
    pObjects$memory[[PANEL]][[SLOT]] <- new_value

This enables iSEE to keep a record of the current state of the
application. In fact, any changes must go through `pObjects$memory`
before they change the output in
[`.renderOutput`](https://isee.github.io/iSEE/reference/output-generics.md);
there is no direct interaction between `input` and `output` in this
framework.

We suggest using
[`.createProtectedParameterObservers`](https://isee.github.io/iSEE/reference/createProtectedParameterObservers.md)
and
[`.createUnprotectedParameterObservers`](https://isee.github.io/iSEE/reference/createProtectedParameterObservers.md),
which create simple observers that update the memory in response to
changes in the UI elements. For handling selectize elements filled with
server-side row/column names, we can use
[`.createCustomDimnamesModalObservers`](https://isee.github.io/iSEE/reference/createCustomDimnamesModalObservers.md).

Developers should not attempt to modify `x` in any observer expression.
This value does not have pass-by-reference semantics and any changes
will not propagate to other parts of the application. Rather,
modifications should occur to the version of `x` in `pObjects$memory`,
as described in the code chunk above.

## Triggering re-rendering

To trigger re-rendering of an output, observers should call
[`.requestUpdate`](https://isee.github.io/iSEE/reference/requestUpdate.md)`(PANEL, rObjects)`
where `PANEL` is the name of the current panel This will request a
re-rendering of the output with no additional side effects and is most
useful for responding to aesthetic parameters.

In some cases, changes to some parameters may invalidate existing
multiple selections, e.g., brushes and lassos are no longer valid if the
variable on the axes are altered. Observers responding to such changes
should instead call
[`.requestCleanUpdate`](https://isee.github.io/iSEE/reference/requestUpdate.md)`(PANEL, pObjects, rObjects)`,
which will destroy all existing selections in order to avoid misleading
conclusions.

## Author

Aaron Lun
