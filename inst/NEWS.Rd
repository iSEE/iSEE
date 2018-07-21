\name{NEWS}
\title{News for Package 'iSEE'}

\section{iSEE VERSION 1.1.3}{
\itemize{
    \item Added new Sample assay plot panel type.
    \item Extended documentation.
    \item Split vignette into three: basic, advanced, ExperimentColorMap.
    \item Fixed initialization of reduced dimensions with a single plot axis choice.
    \item Substituted discouraged use of \code{sapply}.
    \item Moved roxygen \code{importFrom} instructions closer to the relevant code.
    \item Increased unit test coverage.
    \item Consistent use of "colormap" through the package.
    \item Substituted reference of deprecated \code{biocLite} to \code{BiocManager}.
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

\section{iSEE VERSION 1.0.1}{
\itemize{
    \item Renamed feature expression plots to feature assay plots, for generality.
}
}

\section{iSEE VERSION 0.99.3}{
\itemize{
    \item Custom tours can be restarted via the dropdown menu button, overwriting the default tour.
    \item Added functionality to provide a custom title to be displayed in the app.
    \item Preserve data points and width ratio upon zoom on discrete variables.
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
    \item Maintainer badge transferred to Federico
}
}


\section{iSEE VERSION 0.99.0}{
\itemize{
    \item Initial submission to \emph{Bioconductor}.
}
}
