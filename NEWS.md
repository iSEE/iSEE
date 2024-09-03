# iSEE 2.17.4

* Add explicitly a "Stop app" button to close the application (should be a means to nicely behave in container-spawned instances). Addresses #630
* Fix text in 'configure' vignette. Closes #626
* Copy information from rowRanges to rowData Closes #637
* Require at least one valid value for atomic, groupable and numeric columns. Closes 660
* If heatmap matrix has no finite values, set the color range arbitrarily. Closes #610
* Limit the number of rows in heatmap annotation legends to 10. Closes #591
* Fix license. Closes #661.
* Rename some internal constants.
* Add checkbox to fix aspect ratio to 1. Closes #541.
* Add checkbox to hide violin boundaries. Closes #619.

# iSEE 2.17.3

* Preserve the existing order of rows when receiving a selection in `RowDataTable`, `ColumnDataTable` panels.

# iSEE 2.17.2

* Export function `.selectInputHidden()`.

# iSEE 2.17.1

* Fix typo in `setMethod(".getContinuousMetadataChoices", "RowDotPlot", ...)`.

# iSEE 2.15.1

* Add button 'Draft out a tour' to navigation bar.
* Add button 'About this data set' to navigation bar.

# iSEE 2.13.5

* Add generic `.isBrushable` to support panels that are not `DotPlot` extensions.

# iSEE 2.13.4

* Fix `COLORMAP` bug introduced in `2.13.3`.

# iSEE 2.13.3

* Let app maintainer define colormap in the landing page.

# iSEE 2.13.2

* Fix bug introduced in `2.11.2` (DataBoxOpen would apply also to Visual parameters box)

# iSEE 2.13.1

* Define missing methods for generics for custom tables (issues #608 and #612)

# iSEE 2.11.4

* Fix bug introduced in `2.11.2`.

# iSEE 2.11.3

* Use standard syntax to include empty icon in relevant places (issue #606).

# iSEE 2.11.2

* Make it possible to hide the "Visual Parameters" box (issue #611).

# iSEE 2.11.1

* Add `tooltip.signif` to `registerAppOptions()` to regulate the number of
  significant digits shown in the tooltip.

# iSEE 2.9.12

* Export constants used in `iSEEu`.

# iSEE 2.9.11

* Fix R CMD check warnings about missing documentation.

# iSEE 2.9.10

* Enable customisation of hovering tooltip in `DotPlot` panels
  including `colData` or `rowData` information.

# iSEE 2.9.9

* Allow screenshots in vignettes to use full width of pkgdown site.

# iSEE 2.9.8

* Enable autocompletion for feature names in the heatmap feature selection modal.

# iSEE 2.9.7

* Update FontAwesome icon `question-circle` to `circle-question` (v6).

# iSEE 2.9.6

* Bugfix related to <https://github.com/rstudio/shiny/issues/3125>,
  mainly applicable to custom landing pages that render a `DT::datatable()`
  prior to `selectInput()`.

# iSEE 2.9.5

* Bugfix reverting a change in `2.9.3` breaking re-rendering of reactivated panels.
* Complete bugfix to prevent unnecessary re-rendering of `ComplexHeatmapPlot` panel
  when dimension of an incoming multiple selection is dismissed by the options
  of the child panel.

# iSEE 2.9.4

* Bugfix setting the active multi-selection info of `Table` panels to a fixed
  message, as the panel is not re-rendered when search boxes are used.
* Bugfix re-rendering `ComplexHeatmapPlot` panels when displaying incoming
  column selection.

# iSEE 2.9.3

* Partial bugfix avoiding re-rendering of `ComplexHeatmapPlot` panel
  when an incoming row selection changes if custom rows are in use.
  The partial bugfix only applies if the `ComplexHeatmapPlot` also disables
  the restriction on any incoming column selection.

# iSEE 2.9.2

* Document the existing panel modification modes.

# iSEE 2.9.1

* Add spinner to `ComplexHeatmapPlot`.

# iSEE 2.5.3

* Replace icons with fontawesome 5 versions

# iSEE 2.5.2

* Bugfix for conversion of categorical columns with too many levels to numeric ones
* Bugfix for heatmap crashing if columns were ordered by a selection that was not shown

# iSEE 2.5.1

* Bugfix for removed panels showing up among the selectable ones.

# iSEE 2.3.14

* Allow modification of font sizes for row and column names in `ComplexHeatmapPlot`.
* Bugfix for assignment of annotation colors in `ComplexHeatmapPlot`.

# iSEE 2.3.13

* Avoid partial name matching in `.getCachedCommonInfo`.
* Deprecated `iSEEOptions` in favor of `panelDefaults` (for construction-time globals) and `registerAppOptions` (for runtime globals).

# iSEE 2.3.12

* Added an `.allowableColorByDataChoices` generic for downstream panels to control `ColorBy*Data` choices.

# iSEE 2.3.11

* Cleaned up tours for `Table`s and the `ComplexHeatmapPlot`.

# iSEE 2.3.10

* Document and export the `.getDotPlotColorHelp` utility.
* Bugfix for the `RowDotPlot` color tour.

# iSEE 2.3.9

* Add a distributed tour attached to each individual UI element.
* Bugfix for ordering of selected columns in `ComplexHeatmapPlot`.

# iSEE 2.3.8

* Use `shiny::MockShinySession$new()` to simulate Shiny session objects.

# iSEE 2.3.7

* Bugfix for missing import of geom_density_2d

# iSEE 2.3.6

* Bugfix for graceful deprecation of old parameters in various constructors.

# iSEE 2.3.5

* Added functionality to use multiple row/column selections as a factor on the axes, for faceting or for coloring.
* Moved selection transparency setter into the "Visual parameters" box.
* Deprecated `SelectionEffect="Color"` in favor of `ColorBy="Column selection"` and `ColorBy="Row selection"`.
* Deprecated `SelectionColor` as the coloring for selections is determined using `colDataColorMap()` instead.
* Deprecated `SelectionEffect="Restrict"` in favor of `ColumnSelectionRestrict` and `RowSelectionRestrict`.
* Deprecated `ColumnSelectionType` and `ColumnSelectionSaved` (ditto for rows) as all active/saved selections are now transmitted.

# iSEE 2.3.4

* Fix wiring of button observer to open vignette.

# iSEE 2.3.3

* Edge-case bugfix for correct cleaning of zero-row/column SummarizedExperiments.

# iSEE 2.3.2

* Added the `cleanDataset()` generic to ensure all names in the SummarizedExperiment are present and unique.

# iSEE 2.3.1

* Bugfix to the heatmap color selection for near-zero length ranges.

# iSEE 2.1.27

* Minor edits to the API documentation.

# iSEE 2.1.26

* Disable the import button on the dimnames modal when transmitter has not made a selection.
* Separate the maximum number of factor levels for colors from other applications.

# iSEE 2.1.25

* Support a named vector in the `SearchColumns` field of the `Table` subclasses.

# iSEE 2.1.24

* Enable custom saving of the application state via the new `saveState=` argument.
* Switch colormap getters to use an internal cache to avoid conflicts with user entries.

# iSEE 2.1.23

* `ExperimentColorMap` inherits from `Annotated`.

# iSEE 2.1.22

* Split and rename scripts for test setup.

# iSEE 2.1.21

* Minor fix to unit test.

# iSEE 2.1.20

* Export even more internal utilities for re-use in downstream packages.
* Minor fixes to the `ComplexHeatmapPlot` documentation.

# iSEE 2.1.19

* Export more internal utilities for re-use in downstream packages.
* Added a generic to define the selection effect UI.
* Fixes to the `ComplexHeatmapPlot` observers, most obviously for the assay choice.
* Fixes to the underlying reactive framework to avoid bugs due to unresponsiveness.

# iSEE 2.1.18

* Bugfix to properly support dynamic classes on landing page.

# iSEE 2.1.17

* `.refineParameters()` for `FeatureAssayPlot`, `SampleAssayPlot` protects the x-axis choice against absent metadata.
* CSS classes for each panel are now defined at app run-time, to make it easier to write landing pages without specifying `initial=` in `iSEE()`.

# iSEE 2.1.16

* Bugfix for initialization of the `ColorByFeatureDynamicSource` UI element in `ColumnDotPlot`s.

# iSEE 2.1.15

* Allow labelling of the medoid for each level of a discrete variable in a scatter-type `DotPlot`.
* Turned on validity checks during `[[<-` assignment into `Panel` classes.

# iSEE 2.1.14

* Right-aligned the help icon for individual panel tour.
* Added tooltip for mouseovers on `DotPlot` panels.
* Allowed custom annotation about selected table row to to be displayed in `Table` panels.

# iSEE 2.1.13

* Refactored the heatmap feature selection modal to be reusable for selecting rows or columns in other contexts.

# iSEE 2.1.12

* Added panel-specific tours via the `.definePanelTour()` generic.
* Generalized the `HiddenColumns` mechanism to all `Table` subclasses.

# iSEE 2.1.11

* Added `HiddenColumns` slot to hide columns in `ColumnDataTable`s and `RowDataTable`s.

# iSEE 2.1.10

* Avoided transmitting multiple selections for `Table`s when the number of columns change.
* Hid irrelevant UI elements for `Table` multiple selections.
* Generalized information about the number of selected rows/columns in each panel.
* Streamlined the `.defineOutput` signature.

# iSEE 2.1.9

* Improved documentation for generics and classes.
* Defined `.exportOutput` method for the `ComplexHeatmapPlot` class.
* Added missing documentation for slots and methods in the `ComplexHeatmapPlot` class.
* Enforced sensible defaults for the dynamic selection setting.
* Extract assays with dimnames for correct indexing.

# iSEE 2.1.8

* Fixed control of legend point size for continuous covariates.
* Extended control of legend point size for violin plots and Hinton plots.

# iSEE 2.1.7

* Added control of legend point size under the "Text" category of the "Visual parameters" box.

# iSEE 2.1.6

* Fixed bug for `sizeBy` observers.

# iSEE 2.1.5

* Fixed initialization of panel size to current value when the "Organize panels" window is closed and re-opened.
* Fixed removal of last panel from the interface.

# iSEE 2.1.4

* Added progress bar when exporting panel outputs. 
* Fixed missing section in `createLandingPage()` man page.

# iSEE 2.1.3

* Fixed handling of logical > 1 when processing the `CustomRowsText` slot in the `ComplexHeatmapPlot` constructor.

# iSEE 2.1.2

* Added a new vignette to describe panel links.
* Fixed documentation for `*DynamicSource` slots.
* Fixed reception of single selection from plot at initialization.
* Removed deprecated functionality.

# iSEE 2.1.1

* Added vignette documenting the use of out-of-memory matrices for big data.
* Added `TENxPBMCData` to `Suggests:`.

# iSEE 1.99.9

* Fixed slot names in the `ReducedDimensionPlot()` man page.

# iSEE 1.99.8

* Protected against transient invalid selected index; fixes #400.
* Forced `renderDT` to rerun expression upon panel reorg.

# iSEE 1.99.7

* Fixed out-of-date vignette content.

# iSEE 1.99.6

* Added static screenshots to vignettes.
* Added GitHub Actions for continuous integration and deployment.
* Updated Docker base image to `bioconductor/bioconductor_docker:devel`.

# iSEE 1.99.5

* Export utilities relevant to downstream panel development.

# iSEE 1.99.4

* Added options for dynamic choice of single/multiple selection sources.
* Fixed bug around `NA` groupings in `subsetPointsByGrid()`.
* Explicitly notify the user when removing invalid panels supplied by a landing page.

# iSEE 1.99.3

* Fixed occurrences of `rowData` in `RowDotPlot` panels.
* Refactored `.create_visual_box()`.
* Refactored visual parameter sections into generics.
* Apply global option `selected.color` to single selections.
* Added `panel.width`, `panel.height`, and `assay` to global options.
* Added `.checkboxInputHidden()`.

# iSEE 1.99.2

* Added extension points to the API.
* Added global settings using `iSEEOptions`.
* Added `.allowableYAxisChoices()` and `.allowableXAxisChoices()` methods to intercept choices of x/y-axis variables.

# iSEE 1.99.1

* Allowed customization of landing pages (from calling `iSEE()` in no-SE mode) for enterprise deployments.
* Allowed export of plot and table panel outputs as PDF and CSV files, respectively.
* Fixed handling of `se` objects missing `dimnames`.
* Added `createCustomPlot()` and `createCustomTable()` to provide on-ramp for making customized panels.
* Allowed global setting using `iSEEOptions`.
* Expanded class and slot names from "Feat" to "Feature", "Samp" to "Sample", "RedDimPlot" to "ReducedDimensionPlot".
* Renamed "StatTable" to "DataTable".

# iSEE 1.99.0

* Refactored panel implementation as S4 classes.
* Display a spinner while panels are rerendering.
* Refactored heatmap panel to use `ComplexHeatmap`.

# iSEE 1.7.2

* Added notification on birthday.
* Enabled hiding of the `*DataPlot` UI elements.

# iSEE 1.7.1

* Fix `rbind()` of `data.frame` and `DataFrame` objects.
* Fix error related to using `&&` with variable length greater than 1.
* Replace deprecated `scater` argument.
* Replace deprecated functions: `SingleCellExperiment::clearSpikes()`, `SingleCellExperiment::clearSizeFactors()`.

# iSEE 1.7.0

* Bioconductor release.

# iSEE 1.5.13

* Order features selected in heat map selectize from top to bottom.

# iSEE 1.5.12

* Support gene list input from `aceEditor()` and `fileInput()`.

# iSEE 1.5.11

* Renamed `isColorMapCompatible()` to `checkColormapCompatibility()`.
* Fixed graceful server side handling of `checkColormapCompatibility()`.
* Updated documentation about panel organisation in vignette.

# iSEE 1.5.10

* Fixed test to provide a non-empty selection to custom plot function.

# iSEE 1.5.9

* Introduced Bugs Easter egg.

# iSEE 1.5.8

* Substituted deprecated `scater::normalize()` by `scater::logNormCounts()`.

# iSEE 1.5.7

* Simplified protection of `redDimPlotDefaults()` against empty `reducedDims`.
* Fix to declare all panel types not available.

# iSEE 1.5.6

* Updates following deprecation of `isSpike()` and `sizeFactorNames()`.

# iSEE 1.5.5

* Added `modeEmpty()`.
* Support zero-row `initialPanels` argument.

# iSEE 1.5.4

* Added support for file upload with server re-initialization.
* Moved observers to separate file. Exclude from code coverage.
* Updated calls to `ReprocessedAllenData()` to load only `tophat_counts` assay.

# iSEE 1.5.3

* Use `ReprocessedAllenData()` following the deprecation of `data(allen)`.

# iSEE 1.5.2

* Minor doc fix.
* Do not allow duplicated values in `Name` field of `initialPanels`.
* Downsample points randomly.

# iSEE 1.5.1

* Fixed report of table links.

# iSEE 1.5.0

* Bioconductor release.

# iSEE 1.3.9

* Added ORCID identifiers.
* Fixed subscript error when tables receive a selection.
* Fixed panel names in panel organization selectize input.
* Control the application of panel organization updates using an action button.
* Fixed child replotting upon lasso close.
* Fixed code reporting for zero-length DataFrame.

# iSEE 1.3.8

* Added support for multiple selections.
* Avoid Javascript error with check group conditional.

# iSEE 1.3.7

* Added "ImmunoOncology" in `biocViews`.

# iSEE 1.3.6

* Control point size.

# iSEE 1.3.5

* Additional information during the default tour.

# iSEE 1.3.4

* Fixed panel organization selectize.

# iSEE 1.3.3

* Updated default tour steps to match updated user interface.
* Parsed quote symbols literally in default tour steps.
* Fixed name-to-index conversion of feature names for heat map panel.

# iSEE 1.3.2

* Moved panel organization to modal with selectize to control panel display and ordering, remove sidebar.
* Added control of width and height of new panels.
* Enabled voice control.
* Refactored internal functions.

# iSEE 1.3.1

* Fixed invalid row index sent from tables in RStudio browser.
* Fixed initialization of search fields for tables that are initialized with an incoming selection.
* Fixed constant field name.

# iSEE 1.3.0

* Bioconductor release.

# iSEE 1.1.14

* Fixed unit test.

# iSEE 1.1.13

* Added missing observer for assay type in row data plot panels.
* Added missing observer for colorpicker when colouring by feature name in row-based plots, or by sample name in column-based plots.
* Ignore `NA` values when computing the range of coloring scales.
* Added a size expansion factor (5x) to the selected point when colouring by feature name in row-based plots, or by sample name in column-based plots.
* Fixed redundant coloring of selected point when colouring by feature name in row-based plots, or by sample name in column-based plots.
* Updated basic vignette.

# iSEE 1.1.12

* Updated NEWS file.

# iSEE 1.1.11

* Exported list of panel names and codes.

# iSEE 1.1.10

* Fixed colour scale to be invariant when selecting on a different color.
* Protected heat map plot panels against restriction on zero samples.

# iSEE 1.1.9

* Fixed compatibility with `DelayedArray` assays.

# iSEE 1.1.8

* Extended unit test coverage.
* Moved generics to separate file.
* Minor fix to `annotateEnsembl()`.
* Updated list of functionalities in README.

# iSEE 1.1.7

* Resolved `BiocManager` message.

# iSEE 1.1.6

* Minor fix for Windows unit test.

# iSEE 1.1.5

* New panel colors.
* Added control for arguments to custom panels through action buttons.
* Distinguished visible from active arguments for custom panels.

# iSEE 1.1.4

* Split `?defaults` help page by panel type.
* Generalized support for custom data plots and statistics tables.

# iSEE 1.1.3

* Added new _Sample assay plot_ panel type.
* Extended documentation.
* Split vignette into three: basic, advanced, ExperimentColorMap.
* Fixed initialization of reduced dimensions with a single plot axis choice.
* Substituted discouraged use of `sapply()`.
* Moved roxygen `importFrom` instructions closer to the relevant code.
* Increased unit test coverage.
* Consistent use of "colormap" through the package.
* Updated installation instructions.
* Added CITATION file.
* Added Figure 1 of article in README.

# iSEE 1.1.2

* Enabled faceting by row and column, with appropriate updates to brush and lasso.
* Enabled shaping on data points.
* Minor fix of jitter for violin and square plots.
* INTERNAL: Enabled storage of additional `plot.data` beyond `X` and `Y` in `all.coordinates`.
    See constant `.allCoordinatesNames`.
    Necessary for correct behaviour of brushes on faceted plots.

# iSEE 1.1.0

* Bioconductor release.

# iSEE 1.0.1

* Renamed feature expression plots to feature assay plots, for generality.

# iSEE 0.99.3

* Custom tours can be restarted via the dropdown menu button, overwriting the default tour.
* Added functionality to provide a custom title to be displayed in the app.
* Preserved data points and width ratio upon zoom on discrete variables.

# iSEE 0.99.2

* Added functionality for providing additional custom tours, to be launched directly upon starting the app.

# iSEE 0.99.1

* Added grid-based visual point downsampling for faster plotting, including control of resolution.
* Added button "Clear features" for heat maps.
* Reorganized buttons in heat map panels.
* Transferred maintainer badge to Federico.

# iSEE 0.99.0

* Initial submission to _Bioconductor_.
