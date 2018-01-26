
# Validation function ----

.valid.Colormap <- function(object){
  # To avoid later checking inside the app, this object should be stored in a
  # slot of its parent SummarizedExperiment that would check its validity
  # (e.g. all assay color maps should be present in the parent, etc.)
  # Note that default color maps will be provided for elements
  # of the parent that do not have a defined colormap.

  errors <- c()

  if (!all(sapply(object@assays, "typeof") == "character")){
    errors <- c(errors, "Non-character vectors present in assay colormaps.")
  }

  if (!all(sapply(object@colData, "typeof") == "character")){
    errors <- c(errors, "Non-character vectors present in assay colormaps.")
  }

  if (!all(sapply(object@rowData, "typeof") == "character")){
    errors <- c(errors, "Non-character vectors present in assay colormaps.")
  }

  if (length(errors > 0)){
    return(errors)
  }

  return(TRUE)
}

# ExperimentColorMap definition ----

setClass("ExperimentColorMap",
    contains="Vector",
    representation(
      assays="list", # continuous colormaps for assays
      colData="list", # continuous or discrete colormaps for sample metadata
      rowData="list" # continuous or discrete colormaps for gene metadata
    ),
    prototype(),
  validity = .valid.Colormap
)

# Constructor ----

#' \code{ExperimentColorMap} objects
#'
#' @name ExperimentColorMap
#'
#' @param assays List of color maps for \code{assays}.
#' @param colData List of color maps for \code{colData}.
#' @param rowData List of color maps for \code{rowData}.
#' @param ... additional arguments passed on to the \code{ExperimentColorMap}
#' constructor
#'
#' @section Accessors:
#'
#' In the following code snippets, \code{x} is an
#' \code{ExperimentColorMap} object.
#'
#' \code{assayColorMap(x, "counts")}: Get an assay colormap.
#'
#' @export ExperimentColorMap
#' @exportClass ExperimentColorMap
#' @export assayColorMap
#' @exportMethod assayColorMap
#' @export colDataColorMap
#' @export rowDataColorMap
#' @aliases class:ExperimentColorMap ExperimentColorMap-class
#' assayColorMap colDataColorMap rowDataColorMap
#' assayColorMap,ExperimentColorMap,character-method
#' assayColorMap,ExperimentColorMap,numeric-method
#' colDataColorMap,ExperimentColorMap,character-method
#' colDataColorMap,ExperimentColorMap,numeric-method
#' rowDataColorMap,ExperimentColorMap,character-method
#' rowDataColorMap,ExperimentColorMap,numeric-method
#'
#' @examples
#'
#' # Constructor ----
#'
#' count_colors <- viridis::viridis(10)
#' logcounts_colors <- viridis::magma(10)
#' fpkm_colors <- viridis::inferno(10)
#' tpm_colors <- viridis::plasma(10)
#'
#' qc_colors <- c("forestgreen", "firebrick1")
#' names(qc_colors) <- c("Y", "N")
#'
#' ecm <- new("ExperimentColorMap",
#'     assays = list(
#'         counts = count_colors,
#'         tophat_counts = count_colors,
#'         cufflinks_fpkm = logcounts_colors,
#'         cufflinks_fpkm = fpkm_colors,
#'         rsem_tpm = tpm_colors
#'     ),
#'     colData = list(
#'         passes_qc_checks_s = qc_colors
#'     )
#' )
#'
#' # Accessor ----
#'
#' assayColorMap(ecm, "logcounts") # viridis::magma(10)
#' assayColorMap(ecm, "undefined") # default: viridis::viridis(10)
#'
#' colDataColorMap(ecm, "passes_qc_checks_s")
#' colDataColorMap(ecm, "undefined")
#'
#' rowDataColorMap(ecm, "undefined")
#'
ExperimentColorMap <- function(
  assays = list(), colData = list(), rowData = list(), ...){
  new("ExperimentColorMap", assays=assays, colData=colData, rowData=rowData, ...)
}

# .defaultContinuousColorMap ----

# default continuous colormap
.defaultContinuousColorMap <- viridis::viridis(10)

# Accessors ----

# assayColorMap ----

setGeneric("assayColorMap", function(x, i, ...) standardGeneric("assayColorMap"))

setMethod("assayColorMap", c("ExperimentColorMap", "character"),
    function(x, i, ...)
{
    .assayColorMap(x, i, ...)
})

setMethod("assayColorMap", c("ExperimentColorMap", "numeric"),
    function(x, i, ...)
{
    .assayColorMap(x, i, ...)
})

.assayColorMap <- function(x, i, ...){
  res <- tryCatch({
        x@assays[[i]]
    }, error=function(err) {
        .defaultContinuousColorMap
    })
    if (is.null(res)){
      return(.defaultContinuousColorMap)
    }
    return(res)
}

# colDataColorMap ----

setGeneric("colDataColorMap", function(x, i, ...) standardGeneric("colDataColorMap"))

setMethod("colDataColorMap", c("ExperimentColorMap", "character"),
    function(x, i, ...)
{
      .nonAssayColorMap(x, "colData", i)
})

setMethod("colDataColorMap", c("ExperimentColorMap", "numeric"),
    function(x, i, ...)
{
      .nonAssayColorMap(x, "colData", i)
})

# rowDataColorMap ----

setGeneric("rowDataColorMap", function(x, i, ...) standardGeneric("rowDataColorMap"))

setMethod("rowDataColorMap", c("ExperimentColorMap", "character"),
    function(x, i, ...)
{
      .nonAssayColorMap(x, "rowData", i)
})

setMethod("rowDataColorMap", c("ExperimentColorMap", "numeric"),
    function(x, i, ...)
{
      .nonAssayColorMap(x, "rowData", i)
})

# As oppose to assays that are always numeric
# other colormaps may be either numeric or discrete
.nonAssayColorMap <- function(x, type, i){
  res <- tryCatch({
      slot(x, type)[[i]]
  }, error=function(err) {
      NULL
  })
  return(res)
}

# show ----

setMethod(
  "show", "ExperimentColorMap",
  function(object){
    scat <- function(fmt, vals=character(), exdent=2, ...)
    {
        vals <- ifelse(nzchar(vals), vals, "''")
        lbls <- paste(S4Vectors:::selectSome(vals), collapse=" ")
        txt <- sprintf(fmt, length(vals), lbls)
        cat(strwrap(txt, exdent=exdent, ...), sep="\n")
    }

    cat("Class: ExperimentColorMap\n")

    ## assays()
    scat("assays(%d): %s\n", names(object@assays))

    ## colData()
    scat("colData(%d): %s\n", names(object@colData))

    ## rowData()
    scat("rowData(%d): %s\n", names(object@rowData))

    return(NULL)
  }
)
