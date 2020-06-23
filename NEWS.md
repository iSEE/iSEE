# iSEE 2.1.8

* Fixed control of legend point size for continuous covariates.
* Extended control of legend point size for violin plots and Hinton plots.

# iSEE 2.1.7

* Added control of legend point size under the "Text" category of teh "Visual parameters" box.

# iSEE 2.1.6

* Fixed bug for `sizeBy` observers.

# iSEE 2.1.5

* Fixed initialization of panel size to current value when the "Organize panels" window is closed and re-opened.
* Fixed removal of last panel from the interface.

# iSEE 2.1.4

* Add progress bar when exporting panel outputs. 
* Fix missing section in `createLandingPage()` man page.

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

* Fixed occurences of `rowData` in `RowDotPlot` panels.
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
* Transfered maintainer badge to Federico.

# iSEE 0.99.0

* Initial submission to _Bioconductor_.
