
# Validation function ----

.valid.Colormap <- function(object){
  # To avoid later checking inside the app, this object should be stored in a
  # slot of its parent SummarizedExperiment that would check its validity
  # (e.g. all assay color maps should be present in the parent, etc.)
  # Note that default color maps will be provided for elements
  # of the parent that do not have a defined colormap.

  errors <- c()

  if (length(errors > 0)){
    return(errors)
  }

  return(TRUE)
}

# ExperimentColorMap definition ----

setClassUnion("function_or_NULL", c("function","NULL"))

setClass("ExperimentColorMap",
    contains="Vector",
    representation(
      # each slot has a list of closures
      assays="list",
      colData="list",
      rowData="list",
      all="list",
      global="function_or_NULL"
    ),
    prototype(
      all=list(assays=NULL, colData=NULL, rowData=NULL),
      global=NULL
    ),
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
#' @param all List of color maps for \code{all}.
#' @param global List of color maps for \code{global}.
#' @param ... additional arguments passed on to the \code{ExperimentColorMap}
#' constructor
#' 
#' @details 
#' Color maps must all be functions that take at least one argument: the number
#' of (named) colours to return as a \code{character} vector.
#' This argument may be ignored in the body of the color map function
#' to produce constant color maps. 
#'
#' @section Accessors:
#'
#' In the following code snippets, \code{x} is an
#' \code{ExperimentColorMap} object.
#' 
#' \describe{
#'   \item{\code{assayColorMap(x, i, ...)}:}{Get an \code{assays} colormap.}
#'   
#'   \item{\code{colDataColorMap(x, i, ...)}:}{Get a \code{colData} colormap.}
#'   
#'   \item{\code{rowDataColorMap(x, i, ...)}:}{Get a \code{rowData} colormap.}
#' }
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
#' # Example color maps ----
#'
#' count_colors <- function(n){
#'   c("black","brown","red","orange","yellow")
#' }
#' fpkm_colors <- viridis::inferno
#' tpm_colors <- viridis::plasma
#'
#' qc_color_fun <- function(n){
#'   qc_colors <- c("forestgreen", "firebrick1")
#'   names(qc_colors) <- c("Y", "N")
#'   return(qc_colors)
#' }
#' 
#' # Constructor ----
#'
#' ecm <- new("ExperimentColorMap",
#'     assays = list(
#'         counts = count_colors,
#'         tophat_counts = count_colors,
#'         cufflinks_fpkm = fpkm_colors,
#'         cufflinks_fpkm = fpkm_colors,
#'         rsem_tpm = tpm_colors
#'     ),
#'     colData = list(
#'         passes_qc_checks_s = qc_color_fun
#'     )
#' )
#'
#' # Accessors ----
#'
#' assayColorMap(ecm, "undefined") # viridis::viridis(10) [default]
#' assayColorMap(ecm, "counts") # viridis::plasma(10)
#' assayColorMap(ecm, "cufflinks_fpkm") # viridis::inferno(10)
#'
#' colDataColorMap(ecm, "passes_qc_checks_s")
#' colDataColorMap(ecm, "undefined")
#'
#' rowDataColorMap(ecm, "undefined")
#'
ExperimentColorMap <- function(
  assays = list(), colData = list(), rowData = list(),
  all = list(assays=NULL, colData=NULL, rowData=NULL),
  global = NULL, ...){
  new(
    "ExperimentColorMap",
    assays=assays, colData=colData, rowData=rowData,
    all = all,
    global = global, ...)
}

# .default color maps ----

# default continuous colormap
.defaultContinuousColorMap <- viridis::viridis # function(n)
# default discrete colormap
.defaultDiscreteColorMap <- function(n) {
  # Credit: https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

.defaultColorMap <- function(discrete){
  if (discrete){
    .defaultDiscreteColorMap
  } else {
    .defaultContinuousColorMap
  }
}

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
  assay_map <- x@assays[[i]]
  if (is.null(assay_map)){
    return(.assayAllColorMap(x, ...))
  }
  return(assay_map)
}

.assayAllColorMap <- function(x, ...){
  all_assays_map <- x@all$assays
  if (is.null(all_assays_map)){
    return(.globalColorMap(x, ...))
  }
  return(all_assays_map)
}

# colDataColorMap ----

setGeneric("colDataColorMap", function(x, i, ...) standardGeneric("colDataColorMap"))

setMethod("colDataColorMap", c("ExperimentColorMap", "character"),
    function(x, i, ...)
{
      .colDataColorMap(x, i, ...)
})

setMethod("colDataColorMap", c("ExperimentColorMap", "numeric"),
    function(x, i, ...)
{
      .colDataColorMap(x, i, ...)
})

.colDataColorMap <- function(x, i, ...){
  coldata_map <- x@colData[[i]]
  if (is.null(coldata_map)){
    return(.colDataAllColorMap(x, ...))
  }
  return(coldata_map)
}

.colDataAllColorMap <- function(x, ...){
  all_coldata_map <- x@all$colData
  if (is.null(all_coldata_map)){
    return(.globalColorMap(x, ...))
  }
  return(all_coldata_map)
}

# rowDataColorMap ----

setGeneric("rowDataColorMap", function(x, i, ...) standardGeneric("rowDataColorMap"))

setMethod("rowDataColorMap", c("ExperimentColorMap", "character"),
    function(x, i, ...)
{
      .rowDataColorMap(x, "rowData", i)
})

setMethod("rowDataColorMap", c("ExperimentColorMap", "numeric"),
    function(x, i, ...)
{
      .rowDataColorMap(x, "rowData", i)
})

.rowDataColorMap <- function(x, i, ...){
  rowdata_map <- x@rowData[[i]]
  if (is.null(rowdata_map)){
    return(.rowDataAllColorMap(x, ...))
  }
  return(rowdata_map)
}

.rowDataAllColorMap <- function(x, ...){
  all_rowdata_map <- x@all$rowData
  if (is.null(all_rowdata_map)){
    return(.globalColorMap(x, ...))
  }
  return(all_rowdata_map)
}

# global color map ----

.globalColorMap <- function(x, ..., discrete = FALSE){
  global_map <- x@global
  if (is.null(global_map)){
    return(.defaultColorMap(discrete))
  }
  return(global_map)
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

    ## assays
    scat("assays(%d): %s\n", names(object@assays))

    ## colData
    scat("colData(%d): %s\n", names(object@colData))

    ## rowData
    scat("rowData(%d): %s\n", names(object@rowData))
    
    # all
    all_defined <- !sapply(object@all, is.null)
    scat("all(%d): %s\n", names(object@all)[all_defined])
    
    # global
    if (!is.null(object@global)){
      cat("global(1)\n")
    }

    return(NULL)
  }
)
