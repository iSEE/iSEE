\name{NEWS}
\title{News for Package 'iSEE'}

\section{iSEE VERSION 1.5.11}{
\itemize{
    \item Fix graceful handling of \code{isColorMapCompatible} on the server side using
        \code{shiny::showNotification} to display error to users.
}
}

\section{iSEE VERSION 1.5.10}{
\itemize{
    \item Fix test to provide a non-empty selection to custom plot function.
}
}

\section{iSEE VERSION 1.5.9}{
\itemize{
    \item Introduce Bugs Easter egg.
}
}

\section{iSEE VERSION 1.5.8}{
\itemize{
    \item Substitute deprecated \code{scater::normalize} by \code{logNormCounts}.
}
}

\section{iSEE VERSION 1.5.7}{
\itemize{
    \item Simplify protection of \code{redDimPlotDefaults} against empty \code{reducedDims}.
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
    \item Add \code{modeEmpty()}.
    \item Support zero-row \code{initialPanels} argument.
}
}

\section{iSEE VERSION 1.5.4}{
\itemize{
    \item Added support for file upload with server re-initialization.
    \item Moved observers to separate file. Exclude from code coverage.
    \item Updating calls to \code{ReprocessedAllenData()} to load only \code{tophat_counts} assay.
}
}

\section{iSEE VERSION 1.5.3}{
\itemize{
    \item Use \code{ReprocessedAllenData()} following the deprecation of
    \code{data(allen)}.
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
    \item Fix report of table links.
}
}

\section{iSEE VERSION 1.5.0}{
\itemize{
    \item Bioconductor release.
}
}

\section{iSEE VERSION 1.3.9}{
\itemize{
    \item Add ORCID identifiers.
    \item Fix subscript error when tables receive a selection.
    \item Fix panel names in panel organization selectize input.
    \item Control the application of panel organization updates using an action button.
    \item Fix child replotting upon lasso close.
    \item Fix code reporting for zero-length DataFrame
}
}

\section{iSEE VERSION 1.3.8}{
\itemize{
    \item Support multiple selections.
    \item Avoid Javascript error with check group conditional.
}
}

\section{iSEE VERSION 1.3.7}{
\itemize{
    \item Add ImmunoOncology in biocViews.
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
    \item Fix panel organization selectize.
}
}

\section{iSEE VERSION 1.3.3}{
\itemize{
    \item Update default tour steps to match updated user interface.
    \item Parse quote symbols literally in default tour steps.
    \item Fix name-to-index conversion of feature names for heat map panel.
}
}

\section{iSEE VERSION 1.3.2}{
\itemize{
    \item Move panel organization to modal with selectize to control panel display and ordering, remove sidebar.
    \item Add control of width and height of new panels.
    \item Enable voice control.
    \item Refactor internal functions.
}
}

\section{iSEE VERSION 1.3.1}{
\itemize{
    \item Fix invalid row index sent from tables in RStudio browser.
    \item Fix initialization of search fields for tables that are initialized with an incoming selection.
    \item Fix constant field name.
}
}

\section{iSEE VERSION 1.3.0}{
\itemize{
    \item Bioconductor release.
}
}

\section{iSEE VERSION 1.1.14}{
\itemize{
    \item Fix unit test.
}
}

\section{iSEE VERSION 1.1.13}{
\itemize{
    \item Add missing observer for assay type in row data plot panels.
    \item Add missing observer for colorpicker
        when colouring by feature name in row-based plots, or by sample name in column-based plots.
    \item Ignore \code{NA} values when computing the range of coloring scales.
    \item Add a size expansion factor (5x) to the selected point
        when colouring by feature name in row-based plots, or by sample name in column-based plots.
    \item Fix redundant coloring of selected point
        when colouring by feature name in row-based plots, or by sample name in column-based plots.
    \item Update basic vignette.
}
}

\section{iSEE VERSION 1.1.12}{
\itemize{
    \item Updated NEWS file.
}
}

\section{iSEE VERSION 1.1.11}{
\itemize{
    \item Export list of panel names and codes.
}
}

\section{iSEE VERSION 1.1.10}{
\itemize{
    \item Fix colour scale to be invariant when selecting on a different color.
    \item Protect heat map plot panels against restriction on zero samples.
}
}

\section{iSEE VERSION 1.1.9}{
\itemize{
    \item Fix compatibility with \code{DelayedArray} assays.
}
}

\section{iSEE VERSION 1.1.8}{
\itemize{
    \item Extend unit test coverage.
    \item Move generics to separate file.
    \item Minor fix to \code{annotateEnsembl}.
    \item Update list of functionalities in README.
}
}

\section{iSEE VERSION 1.1.7}{
\itemize{
    \item Resolved BiocManager message.
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
    \item Control arguments to custom panels through action buttons.
    \item Distinguish visible from active arguments for custom panels.
}
}

\section{iSEE VERSION 1.1.4}{
\itemize{
    \item Split \code{?defaults} help page by panel type.
    \item Generalized support for custome data plots and statistics tables.
}
}

\section{iSEE VERSION 1.1.3}{
\itemize{
    \item Add new \emph{Sample assay plot} panel type.
    \item Extend documentation.
    \item Split vignette into three: basic, advanced, ExperimentColorMap.
    \item Fix initialization of reduced dimensions with a single plot axis choice.
    \item Substitute discouraged use of \code{sapply}.
    \item Moved roxygen \code{importFrom} instructions closer to the relevant code.
    \item Increase unit test coverage.
    \item Consistent use of "colormap" through the package.
    \item Update installation instructions.
    \item Add CITATION file.
    \item Add Figure 1 of article in README.
}
}

\section{iSEE VERSION 1.1.2}{
\itemize{
    \item Enable faceting by row and column, with appropriate updates to brush and lasso.
    \item Enable shaping on data points.
    \item Minor fix of jitter for violin and square plots.
    \item INTERNAL: Enable storage of additional \code{plot.data} beyond \code{X} and \code{Y} in \code{all.coordinates}.
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
    \item Rename feature expression plots to feature assay plots, for generality.
}
}

\section{iSEE VERSION 0.99.3}{
\itemize{
    \item Custom tours can be restarted via the dropdown menu button, overwriting the default tour.
    \item Add functionality to provide a custom title to be displayed in the app.
    \item Preserve data points and width ratio upon zoom on discrete variables.
}
}

\section{iSEE VERSION 0.99.2}{
\itemize{
    \item Add functionality for providing additional custom tours, to be launched directly upon starting the app.
}
}

\section{iSEE VERSION 0.99.1}{
\itemize{
    \item Add grid-based visual point downsampling for faster plotting, including control of resolution.
    \item Add button "Clear features" for heat maps.
    \item Reorganize buttons in heat map panels.
    \item Maintainer badge transferred to Federico.
}
}

\section{iSEE VERSION 0.99.0}{
\itemize{
    \item Initial submission to \emph{Bioconductor}.
}
}
