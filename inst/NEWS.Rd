\name{NEWS}
\title{News for Package 'iSEE'}

\section{iSEE VERSION 1.7.2}{
\itemize{
    \item Added notification on birthday.
}
}

\section{iSEE VERSION 1.7.1}{
\itemize{
    \item Fixed \code{rbind} of \code{data.frame} and \code{DataFrame}.
    \item Fixed error related to using \code{&&} with variable length greater than 1.
    \item Replaced deprecated scater argument.
    \item Replaced deprecated functions: \code{clearSpikes}, \code{clearSizeFactors}.
}
}

\section{iSEE VERSION 1.7.0}{
\itemize{
    \item Bioconductor release.
}
}

\section{iSEE VERSION 1.5.13}{
\itemize{
    \item Order features selected in heat map selectize from top to bottom.
}
}

\section{iSEE VERSION 1.5.12}{
\itemize{
    \item Support gene list input from \code{aceEditor} and \code{fileInput}.
}
}

\section{iSEE VERSION 1.5.11}{
\itemize{
    \item Renamed \code{isColorMapCompatible} to \code{checkColormapCompatibility}.
    \item Fixed graceful server side handling of \code{checkColormapCompatibility}.
    \item Updated documentation about panel organisation in vignette.
}
}

\section{iSEE VERSION 1.5.10}{
\itemize{
    \item Fixed test to provide a non-empty selection to custom plot function.
}
}

\section{iSEE VERSION 1.5.9}{
\itemize{
    \item Introduced Bugs Easter egg.
}
}

\section{iSEE VERSION 1.5.8}{
\itemize{
    \item Substituted deprecated \code{scater::normalize} by \code{logNormCounts}.
}
}

\section{iSEE VERSION 1.5.7}{
\itemize{
    \item Simplified protection of \code{redDimPlotDefaults} against empty \code{reducedDims}.
    \item Fix to declare all panel types not available.
}
}

\section{iSEE VERSION 1.5.6}{
\itemize{
    \item Updates following deprecation of \code{isSpike} and \code{sizeFactorNames}.
}
}

\section{iSEE VERSION 1.5.5}{
\itemize{
    \item Added \code{modeEmpty()}.
    \item Support zero-row \code{initialPanels} argument.
}
}

\section{iSEE VERSION 1.5.4}{
\itemize{
    \item Added support for file upload with server re-initialization.
    \item Moved observers to separate file. Exclude from code coverage.
    \item Updated calls to \code{ReprocessedAllenData()} to load only \code{tophat_counts} assay.
}
}

\section{iSEE VERSION 1.5.3}{
\itemize{
    \item Use \code{ReprocessedAllenData()} following the deprecation of \code{data(allen)}.
}
}

\section{iSEE VERSION 1.5.2}{
\itemize{
    \item Minor doc fix..
    \item Do not allow duplicated values in \code{Name} field of \code{initialPanels}.
    \item Downsample points randomly.
}
}

\section{iSEE VERSION 1.5.1}{
\itemize{
    \item Fixed report of table links.
}
}

\section{iSEE VERSION 1.5.0}{
\itemize{
    \item Bioconductor release.
}
}

\section{iSEE VERSION 1.3.9}{
\itemize{
    \item Added ORCID identifiers.
    \item Fixed subscript error when tables receive a selection.
    \item Fixed panel names in panel organization selectize input.
    \item Control the application of panel organization updates using an action button.
    \item Fixed child replotting upon lasso close.
    \item Fixed code reporting for zero-length DataFrame
}
}

\section{iSEE VERSION 1.3.8}{
\itemize{
    \item Added support for multiple selections.
    \item Avoid Javascript error with check group conditional.
}
}

\section{iSEE VERSION 1.3.7}{
\itemize{
    \item Added ImmunoOncology in biocViews.
}
}

\section{iSEE VERSION 1.3.6}{
\itemize{
    \item Control point size.
}
}

\section{iSEE VERSION 1.3.5}{
\itemize{
    \item Additional information during the default tour.
}
}

\section{iSEE VERSION 1.3.4}{
\itemize{
    \item Fixed panel organization selectize.
}
}

\section{iSEE VERSION 1.3.3}{
\itemize{
    \item Updated default tour steps to match updated user interface.
    \item Parsed quote symbols literally in default tour steps.
    \item Fixed name-to-index conversion of feature names for heat map panel.
}
}

\section{iSEE VERSION 1.3.2}{
\itemize{
    \item Moved panel organization to modal with selectize to control panel display and ordering, remove sidebar.
    \item Added control of width and height of new panels.
    \item Enabled voice control.
    \item Refactored internal functions.
}
}

\section{iSEE VERSION 1.3.1}{
\itemize{
    \item Fixed invalid row index sent from tables in RStudio browser.
    \item Fixed initialization of search fields for tables that are initialized with an incoming selection.
    \item Fixed constant field name.
}
}

\section{iSEE VERSION 1.3.0}{
\itemize{
    \item Bioconductor release.
}
}

\section{iSEE VERSION 1.1.14}{
\itemize{
    \item Fixed unit test.
}
}

\section{iSEE VERSION 1.1.13}{
\itemize{
    \item Added missing observer for assay type in row data plot panels.
    \item Added missing observer for colorpicker
        when colouring by feature name in row-based plots, or by sample name in column-based plots.
    \item Ignore \code{NA} values when computing the range of coloring scales.
    \item Added a size expansion factor (5x) to the selected point
        when colouring by feature name in row-based plots, or by sample name in column-based plots.
    \item Fixed redundant coloring of selected point
        when colouring by feature name in row-based plots, or by sample name in column-based plots.
    \item Updated basic vignette.
}
}

\section{iSEE VERSION 1.1.12}{
\itemize{
    \item Updated NEWS file.
}
}

\section{iSEE VERSION 1.1.11}{
\itemize{
    \item Exported list of panel names and codes.
}
}

\section{iSEE VERSION 1.1.10}{
\itemize{
    \item Fixed colour scale to be invariant when selecting on a different color.
    \item Protected heat map plot panels against restriction on zero samples.
}
}

\section{iSEE VERSION 1.1.9}{
\itemize{
    \item Fixed compatibility with \code{DelayedArray} assays.
}
}

\section{iSEE VERSION 1.1.8}{
\itemize{
    \item Extended unit test coverage.
    \item Moved generics to separate file.
    \item Minor fix to \code{annotateEnsembl}.
    \item Updated list of functionalities in README.
}
}

\section{iSEE VERSION 1.1.7}{
\itemize{
    \item Resolved \code{BiocManager} message.
}
}

\section{iSEE VERSION 1.1.6}{
\itemize{
    \item Minor fix for Windows unit test.
}
}

\section{iSEE VERSION 1.1.5}{
\itemize{
    \item New panel colors.
    \item Added control for arguments to custom panels through action buttons.
    \item Distinguished visible from active arguments for custom panels.
}
}

\section{iSEE VERSION 1.1.4}{
\itemize{
    \item Split \code{?defaults} help page by panel type.
    \item Generalized support for custom data plots and statistics tables.
}
}

\section{iSEE VERSION 1.1.3}{
\itemize{
    \item Added new \emph{Sample assay plot} panel type.
    \item Extended documentation.
    \item Split vignette into three: basic, advanced, ExperimentColorMap.
    \item Fixed initialization of reduced dimensions with a single plot axis choice.
    \item Substituted discouraged use of \code{sapply}.
    \item Moved roxygen \code{importFrom} instructions closer to the relevant code.
    \item Increased unit test coverage.
    \item Consistent use of "colormap" through the package.
    \item Updated installation instructions.
    \item Added CITATION file.
    \item Added Figure 1 of article in README.
}
}

\section{iSEE VERSION 1.1.2}{
\itemize{
    \item Enabled faceting by row and column, with appropriate updates to brush and lasso.
    \item Enabled shaping on data points.
    \item Minor fix of jitter for violin and square plots.
    \item INTERNAL: Enabled storage of additional \code{plot.data} beyond \code{X} and \code{Y} in \code{all.coordinates}.
        See constant \code{.allCoordinatesNames}.
        Necessary for correct behaviour of brushes on faceted plots.
}
}

\section{iSEE VERSION 1.1.0}{
\itemize{
    \item Bioconductor release.
}
}

\section{iSEE VERSION 1.0.1}{
\itemize{
    \item Renamed feature expression plots to feature assay plots, for generality.
}
}

\section{iSEE VERSION 0.99.3}{
\itemize{
    \item Custom tours can be restarted via the dropdown menu button, overwriting the default tour.
    \item Added functionality to provide a custom title to be displayed in the app.
    \item Preserved data points and width ratio upon zoom on discrete variables.
}
}

\section{iSEE VERSION 0.99.2}{
\itemize{
    \item Added functionality for providing additional custom tours, to be launched directly upon starting the app.
}
}

\section{iSEE VERSION 0.99.1}{
\itemize{
    \item Added grid-based visual point downsampling for faster plotting, including control of resolution.
    \item Added button "Clear features" for heat maps.
    \item Reorganized buttons in heat map panels.
    \item Transfered maintainer badge to Federico.
}
}

\section{iSEE VERSION 0.99.0}{
\itemize{
    \item Initial submission to \emph{Bioconductor}.
}
}
