# Default colormaps ----

#' @importFrom viridisLite viridis
.defaultContinuousColorMap <- viridis # function(n)

#' @importFrom grDevices hcl
.defaultDiscreteColorMap <- function(n) {
    # Credit: https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
    hues=seq(15, 375, length=(n + 1))
    hcl(h=hues, l=65, c=100)[seq_len(n)]
}

.defaultColorMap <- function(discrete){
    if (discrete){
        .defaultDiscreteColorMap
    } else {
        .defaultContinuousColorMap
    }
}

# Constructor ----

#' ExperimentColorMap class
#'
#' @name ExperimentColorMap-class
#'
#' @param assays List of colormaps for \code{assays}.
#' @param colData List of colormaps for \code{colData}.
#' @param rowData List of colormaps for \code{rowData}.
#' @param all_discrete Colormaps applied to all undefined
#' categorical \code{assays}, \code{colData}, and \code{rowData}, respectively.
#' @param all_continuous Colormaps applied to all undefined
#' continuous \code{assays}, \code{colData}, and \code{rowData}, respectively.
#' @param global_discrete Colormap applied to all undefined
#' categorical covariates.
#' @param global_continuous Colormap applied to all undefined
#' continuous covariates.
#' @param ... additional arguments passed on to the \code{ExperimentColorMap}
#' constructor
#'
#' @details
#' Colormaps must all be functions that take at least one argument: the number
#' of (named) colours to return as a \code{character} vector.
#' This argument may be ignored in the body of the colormap function
#' to produce constant colormaps.
#'
#' @section Categorical colormaps:
#'
#' The default categorical colormap emulates the default ggplot2 categorical color palette
#' (Credit: \url{https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette}).
#' This palette returns a set of colors sampled in steps of equal size that correspond to approximately equal perceptual changes in color:
#'
#' \preformatted{
#' function(n) {
#'     hues=seq(15, 375, length=(n + 1))
#'     hcl(h=hues, l=65, c=100)[seq_len(n)]
#' }
#' }
#'
#' To change the palette for all categorical variables,
#' users must supply a colormap that returns a similar value;
#' namely, an unnamed character vector of length \code{n}.
#' For instance, using the base R palette \code{rainbow.colors}
#'
#' \preformatted{
#' function(n) {
#'     rainbow(n)
#' }
#' }
#'
#' @return An object of class \code{ExperimentColorMap}
#'
#' @section Accessors:
#'
#' In the following code snippets, \code{x} is an \code{ExperimentColorMap} object.
#' If the colormap can not immediately be found in the appropriate slot,
#' \code{discrete} is a \code{logical(1)} that indicates
#' whether the default colormap returned should be categorical \code{TRUE} or continuous (\code{FALSE}, default).
#'
#' \describe{
#'   \item{\code{assayColorMap(x, i, ..., discrete=FALSE)}:}{
#'   Get an \code{assays} colormap.}
#'
#'   \item{\code{colDataColorMap(x, i, ..., discrete=FALSE)}:}{
#'   Get a \code{colData} colormap.}
#'
#'   \item{\code{rowDataColorMap(x, i, ..., discrete=FALSE)}:}{
#'   Get a \code{rowData} colormap.}
#' }
#'
#' @section Setters:
#'
#' In the following code snippets, \code{x} is an \code{ExperimentColorMap} object,
#' and \code{i} is a character or numeric index.
#'
#' \describe{
#'   \item{\code{assayColorMap(x, i, ...) <- value}:}{
#'   Set an \code{assays} colormap.}
#'
#'   \item{\code{colDataColorMap(x, i, ...) <- value}:}{
#'   Set a \code{colData} colormap.}
#'
#'   \item{\code{rowDataColorMap(x, i, ...) <- value}:}{
#'   Set a \code{rowData} colormap.}
#'
#'   \item{\code{assay(x, i, ...) <- value}:}{
#'   Alias. Set an \code{assays} colormap.}
#' }
#'
#' @importMethodsFrom SummarizedExperiment assays assayNames
#' @importFrom methods new
#'
#' @export ExperimentColorMap
#' @exportClass ExperimentColorMap
#' @export assayColorMap
#' @export assayColorMap<-
#' @export colDataColorMap
#' @export colDataColorMap<-
#' @export rowDataColorMap
#' @export rowDataColorMap<-
#' @aliases class:ExperimentColorMap ExperimentColorMap-class
#' ExperimentColorMap
#' assayColorMap colDataColorMap rowDataColorMap
#' assayColorMap<- colDataColorMap<- rowDataColorMap<-
#' assays,ExperimentColorMap-method
#' assays<-,ExperimentColorMap,list-method
#' assayNames,ExperimentColorMap-method
#' assayNames<-,ExperimentColorMap,ANY-method
#' colData,ExperimentColorMap-method
#' colData<-,ExperimentColorMap,ANY-method
#' rowData,ExperimentColorMap-method
#' rowData<-,ExperimentColorMap,ANY-method
#' assayColorMap,ExperimentColorMap,character-method
#' assayColorMap,ExperimentColorMap,numeric-method
#' assay,ExperimentColorMap,character-method
#' assay,ExperimentColorMap,numeric-method
#' colDataColorMap,ExperimentColorMap,character-method
#' rowDataColorMap,ExperimentColorMap,character-method
#' assayColorMap<-,ExperimentColorMap,character-method
#' assayColorMap<-,ExperimentColorMap,numeric-method
#' colDataColorMap<-,ExperimentColorMap,character-method
#' rowDataColorMap<-,ExperimentColorMap,character-method
#'
#' @examples
#'
#' # Example colormaps ----
#'
#' count_colors <- function(n){
#'   c("black", "brown", "red", "orange", "yellow")
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
#' ecm <- ExperimentColorMap(
#'     assays=list(
#'         counts=count_colors,
#'         tophat_counts=count_colors,
#'         cufflinks_fpkm=fpkm_colors,
#'         rsem_tpm=tpm_colors
#'     ),
#'     colData=list(
#'         passes_qc_checks_s=qc_color_fun
#'     )
#' )
#'
#' # Accessors ----
#'
#' # assay colormaps
#' assayColorMap(ecm, "logcounts") # [undefined --> default]
#' assayColorMap(ecm, "counts")
#' assayColorMap(ecm, "cufflinks_fpkm")
#' assay(ecm, "cufflinks_fpkm") # alias
#'
#' # colData colormaps
#' colDataColorMap(ecm, "passes_qc_checks_s")
#' colDataColorMap(ecm, "undefined")
#'
#' # rowData colormaps
#' rowDataColorMap(ecm, "undefined")
#'
#' # generic accessors
#' assays(ecm)
#' assayNames(ecm)
#'
#' # Setters ----
#'
#' assayColorMap(ecm, "counts") <- function(n){c("blue", "white", "red")}
#' assay(ecm, 1) <- function(n){c("blue", "white", "red")}
#'
#' colDataColorMap(ecm, "passes_qc_checks_s") <- function(n){NULL}
#' rowDataColorMap(ecm, "undefined") <- function(n){NULL}
#'
#' # Categorical colormaps ----
#'
#' # Override all discrete colormaps using the base rainbow palette
#' ecm <- ExperimentColorMap(global_discrete = rainbow)
#' n <- 10
#' plot(1:n, col=assayColorMap(ecm, "undefined", discrete = TRUE)(n), pch=20, cex=3)
ExperimentColorMap <- function(
    assays=list(), colData=list(), rowData=list(),
    all_discrete=list(assays=NULL, colData=NULL, rowData=NULL),
    all_continuous=list(assays=NULL, colData=NULL, rowData=NULL),
    global_discrete=NULL, global_continuous=NULL,
    ...){

    if (is.null(names(assays))){
        names(assays) <- character(length(assays))
    }
    if (is.null(names(colData))){
        names(colData) <- character(length(colData))
    }
    if (is.null(names(rowData))){
        names(rowData) <- character(length(rowData))
    }

    all_discrete <- .sanitize_controlled_colormaps(usr=all_discrete)
    all_continuous <- .sanitize_controlled_colormaps(usr=all_continuous)

    return(new(
        "ExperimentColorMap",
        assays=assays, colData=colData, rowData=rowData,
        all_discrete=all_discrete, all_continuous=all_continuous,
        global_discrete=ifelse(is.null(global_discrete), .nullColorMap, global_discrete),
        global_continuous=ifelse(is.null(global_continuous), .nullColorMap, global_continuous),
        ...))
}


# replace NULL values by the .nullColorMap
.substituteNullColorMap <- function(x){ifelse(
    is.null(x),
    .nullColorMap,
    x
)}

# set missing names to .nullColorMap
.sanitize_controlled_colormaps <- function(
    usr,
    def=list(
        assays=.nullColorMap,
        colData=.nullColorMap,
        rowData=.nullColorMap)
){
    if (is.null(names(usr))){
        stop("User-defined colormap must be a named list")
    }
    usr_names <- names(usr)
    usr <- lapply(usr, .substituteNullColorMap)
    names(usr) <- usr_names
    # set the colormaps given by the user
    # note that invalid ones will be picked up by the class validity check later
    def[names(usr)] <- usr
    return(def)
}

# Accessors ----

# assays ----

setMethod("assays", c("ExperimentColorMap"),
    function(x)
    {
        x@assays
    })

setReplaceMethod(
    "assays", signature(x="ExperimentColorMap", value="list"),
    function(x, value) {
        x@assays <- value
        x
    })

# assayNames ----

setMethod("assayNames", c("ExperimentColorMap"),
    function(x)
    {
        names(x@assays)
    })

setReplaceMethod("assayNames", "ExperimentColorMap", function(x, value) {
    names(x@assays) <- value
    x
})

# colData ----

setMethod("colData", c("ExperimentColorMap"),
    function(x)
    {
        x@colData
    })

setReplaceMethod(
    "colData", signature(x="ExperimentColorMap", value="list"),
    function(x, value) {
        x@colData <- value
        x
    })

# rowData ----

setMethod("rowData", c("ExperimentColorMap"),
    function(x)
    {
        x@rowData
    })

setReplaceMethod(
    "rowData", signature(x="ExperimentColorMap", value="list"),
    function(x, value) {
        x@rowData <- value
        x
    })

# assay ----

setMethod("assay", c("ExperimentColorMap", "character"),
    function(x, i, ..., discrete=FALSE)
    {
        # Alias
        assayColorMap(x, i, ..., discrete=discrete)
    })

setMethod("assay", c("ExperimentColorMap", "numeric"),
    function(x, i, ..., discrete=FALSE)
    {
        # Alias
        assayColorMap(x, i, ..., discrete=discrete)
    })

setReplaceMethod(
    "assay", signature(x="ExperimentColorMap", i="character"),
    function(x, i, ..., value) {
        .replaceAssayColorMap(x, i, ..., value=value)
    })

setReplaceMethod(
    "assay", signature(x="ExperimentColorMap", i="numeric"),
    function(x, i, ..., value) {
        .replaceAssayColorMap(x, i, ..., value=value)
    })

# assayColorMap ----

setMethod("assayColorMap", c("ExperimentColorMap", "character"),
    function(x, i, ..., discrete=FALSE)
    {
        .assayColorMap(x, i, ..., discrete=discrete)
    })

setMethod("assayColorMap", c("ExperimentColorMap", "numeric"),
    function(x, i, ..., discrete=FALSE)
    {
        .assayColorMap(x, i, ..., discrete=discrete)
    })

.assayColorMap <- function(x, i, ..., discrete){
    assay_map <- tryCatch({
        x@assays[[i]]
    }, error=function(err) {
        # WARNG: out-of-bound index does not throw an error!
        # instead it returns the default colormap
        .nullColorMap
    })
    if (is.null(assay_map)){
        assay_map <- .nullColorMap
    }
    if (.activeColormap(assay_map)){
        return(assay_map)
    }
    return(.assayAllColorMap(x, discrete))
}

.assayAllColorMap <- function(x, discrete){
    if (discrete){
        all_assays_map <- x@all_discrete$assays
    } else {
        all_assays_map <- x@all_continuous$assays
    }
    if (.activeColormap(all_assays_map)){
        return(all_assays_map)
    }
    return(.globalColorMap(x, discrete))
}

setReplaceMethod(
    "assayColorMap", signature(x="ExperimentColorMap", i="character"),
    function(x, i, ..., value) {
        .replaceAssayColorMap(x, i, ..., value=value)
    })

setReplaceMethod(
    "assayColorMap", signature(x="ExperimentColorMap", i="numeric"),
    function(x, i, ..., value) {
        .replaceAssayColorMap(x, i, ..., value=value)
    })

.replaceAssayColorMap <- function(x, i, ..., value){
    new_assays <- assays(x)
    new_assays[[i]] <- value
    assays(x) <- new_assays
    x
}

# colDataColorMap ----

setMethod("colDataColorMap", c("ExperimentColorMap", "character"),
    function(x, i, ..., discrete=FALSE)
    {
        .colDataColorMap(x, i, ..., discrete=discrete)
    })

.colDataColorMap <- function(x, i, ..., discrete){
    coldata_map <- x@colData[[i]]
    if (is.null(coldata_map)){
        coldata_map <- .nullColorMap
    }
    if (.activeColormap(coldata_map)){
        return(coldata_map)
    }
    return(.colDataAllColorMap(x, discrete))
}

.colDataAllColorMap <- function(x, discrete){
    if (discrete){
        all_coldata_map <- x@all_discrete$colData
    } else {
        all_coldata_map <- x@all_continuous$colData
    }
    if (.activeColormap(all_coldata_map)){
        return(all_coldata_map)
    }
    return(.globalColorMap(x, discrete))
}

setReplaceMethod(
    "colDataColorMap", signature(x="ExperimentColorMap", i="character"),
    function(x, i, ..., value) {
        .replaceColDataColorMap(x, i, ..., value=value)
    })

.replaceColDataColorMap <- function(x, i, ..., value){
    new_coldata <- colData(x)
    new_coldata[[i]] <- value
    colData(x) <- new_coldata
    x
}

# rowDataColorMap ----

setMethod("rowDataColorMap", c("ExperimentColorMap", "character"),
    function(x, i, ..., discrete=FALSE)
    {
        .rowDataColorMap(x, i, ..., discrete=discrete)
    })

.rowDataColorMap <- function(x, i, ..., discrete){
    rowdata_map <- x@rowData[[i]]
    if (is.null(rowdata_map)){
        rowdata_map <- .nullColorMap
    }
    if (.activeColormap(rowdata_map)){
        return(rowdata_map)
    }
    return(.rowDataAllColorMap(x, discrete))
}

.rowDataAllColorMap <- function(x, discrete){
    if (discrete){
        all_rowdata_map <- x@all_discrete$rowData
    } else {
        all_rowdata_map <- x@all_continuous$rowData
    }
    if (.activeColormap(all_rowdata_map)){
        return(all_rowdata_map)
    }
    return(.globalColorMap(x, discrete))
}

setReplaceMethod(
    "rowDataColorMap", signature(x="ExperimentColorMap", i="character"),
    function(x, i, ..., value) {
        .replaceRowDataColorMap(x, i, ..., value=value)
    })

.replaceRowDataColorMap <- function(x, i, ..., value){
    new_rowdata <- rowData(x)
    new_rowdata[[i]] <- value
    rowData(x) <- new_rowdata
    x
}

# global colormap ----

.globalColorMap <- function(x, discrete){
    if (discrete){
        global_map <- x@global_discrete
    } else {
        global_map <- x@global_continuous
    }
    if (.activeColormap(global_map)){
        return(global_map)
    }
    return(.defaultColorMap(discrete))
}

# show ----

.activeColormap <- function(x){
    # Return TRUE if the colormap does not return NULL for an arbitrary
    # number of colors
    stopifnot(is.function(x))
    return(!is.null(x(21L)))
}

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

        ## all_discrete
        which_valid <- vapply(object@all_discrete, .activeColormap, logical(1))
        scat("all_discrete(%d): %s\n", names(object@all_discrete)[which_valid])

        ## all_continuous
        which_valid <- vapply(object@all_continuous, .activeColormap, logical(1))
        scat("all_continuous(%d): %s\n", names(object@all_continuous)[which_valid])

        ## global_discrete
        if (.activeColormap(object@global_discrete)){
            cat("global_discrete(1)\n")
        }

        ## global_continuous
        if (.activeColormap(object@global_continuous)){
            cat("global_continuous(1)\n")
        }

        return(NULL)
    }
)

# colorMap

# checkColormapCompatibility ----

#' Check compatibility between ExperimentColorMap and SummarizedExperiment
#' objects
#'
#' This function compares a pair of \linkS4class{ExperimentColorMap} and
#' \linkS4class{SingleCellExperiment} objects, and examines whether
#' all of the \code{assays}, \code{colData}, and \code{rowData} defined
#' in the ExperimentColorMap object exist in the SingleCellExperiment object.
#'
#' @param ecm An \linkS4class{ExperimentColorMap}.
#' @param se A \linkS4class{SingleCellExperiment}.
#'
#' @return A character vector of incompability error messages, if any.
#'
#' @export
#'
#' @author Kevin Rue-Albrecht
#' @examples
#'
#' # Example colormaps ----
#'
#' count_colors <- function(n){
#'   c("black","brown","red","orange","yellow")
#' }
#'
#' qc_color_fun <- function(n){
#'   qc_colors <- c("forestgreen", "firebrick1")
#'   names(qc_colors) <- c("Y", "N")
#'   return(qc_colors)
#' }
#'
#' ecm <- ExperimentColorMap(
#'     assays = list(
#'         tophat_counts = count_colors
#'     ),
#'     colData = list(
#'         passes_qc_checks_s = qc_color_fun
#'     )
#' )
#'
#' # Example SingleCellExperiment ----
#'
#' library(scRNAseq)
#' sce <- ReprocessedAllenData(assays="tophat_counts")
#'
#' # Test for compatibility ----
#'
#' checkColormapCompatibility(ecm, sce)
#'
checkColormapCompatibility <- function(ecm, se){

    errors <- c()

    # The count of colormaps cannot exceed the count of assays
    num_assay_maps <- length(ecm@assays)
    num_assay_se <- length(se@assays)
    if (num_assay_maps > num_assay_se){
        errors <- c(errors, paste0(
            "More assays in colormap (",
            num_assay_maps,
            ") than experiment (",
            num_assay_se,
            ")"))
    }

    # Named colormaps must map to existing data in the experiment
    names_assays_maps <- names(ecm@assays)
    names_coldata_maps <- names(ecm@colData)
    names_rowdata_maps <- names(ecm@rowData)

    names_assays_se <- assayNames(se)
    names_coldata_se <- names(colData(se))
    names_rowdata_se <- names(rowData(se))

    # process assays
    names_assays_maps <- names_assays_maps[names_assays_maps != ""]
    check_assay_names <- names_assays_maps %in% names_assays_se
    if (!all(check_assay_names)){
        errors <- c(errors, sprintf(
            "assay `%s` in colormap missing in experiment",
            names_assays_maps[!check_assay_names]
        ))
    }

    # process colData
    check_coldata_names <- names_coldata_maps %in% names_coldata_se
    if (!all(check_coldata_names)){
        errors <- c(errors, sprintf(
            "colData `%s` in colormap missing in experiment",
            names_coldata_maps[!check_coldata_names]
        ))
    }

    # process rowData
    check_rowdata_names <- names_rowdata_maps %in% names_rowdata_se
    if (!all(check_rowdata_names)){
        errors <- c(errors, sprintf(
            "rowData `%s` in colormap missing in experiment",
            names_rowdata_maps[!check_rowdata_names]
        ))
    }

    return(errors)
}

# synchronizeAssays ----

#' Synchronize assay colormaps to match those in a SummarizedExperiment
#'
#' This function returns an updated \linkS4class{ExperimentColorMap}
#' in which colormaps in the \code{assays} slot
#' are ordered to match the position of their corresponding
#' assay in the \linkS4class{SingleCellExperiment} object.
#' Assays in the SingleCellExperiment that do not have a match
#' in the ExperimentColorMap are assigned the appropriate default colormap.
#'
#' @details
#' It is highly recommended to name \emph{all} assays in both
#' ExperimentColorMap and SummarizedExperiment prior to calling this function,
#' as this will facilitate the identification of matching assays
#' between the two objects. In most cases, unnamed colormaps will be dropped
#' from the new ExperimentColorMap object.
#'
#' The function supports three main situations:
#'
#' \itemize{
#'
#' \item If \emph{all} assays in the SingleCellExperiment are named,
#' this function
#' will populate the \code{assays} slot of the new ExperimentColorMap
#' with the name-matched colormap from the input ExperimentColorMap,
#' if available.
#' Assays in the SingleCellExperiment that do not have a colormap defined
#' in the ExperimentColorMap are assigned the appropriate default colormap.
#'
#' \item If \emph{all} assays in the SingleCellExperiment are unnamed, this function
#' requires that the ExperimentColorMap supplies a number of assay colormaps
#' \emph{identical} to the number of assays in the SingleCellExperiment object.
#' In that case, the ExperimentColorMap object will be returned \emph{as is}.
#'
#' \item If only a subset of assays in the SingleCellExperiment are named,
#' this function will ignore unnamed colormaps in the ExperimentColorMap;
#' It will populate the \code{assays} slot of the new ExperimentColorMap
#' with the name-matched colormap from the input ExperimentColorMap,
#' if available.
#' Assays in the SingleCellExperiment that are unnamed, or that
#' do not have a colormap defined
#' in the ExperimentColorMap are assigned the appropriate default colormap.
#'
#'
#' }
#'
#' @param ecm An \linkS4class{ExperimentColorMap}.
#' @param se A \linkS4class{SingleCellExperiment}.
#'
#' @return An \linkS4class{ExperimentColorMap} with colormaps in the
#' \code{assay} slot synchronized to match the position of the corresponding
#' assay in the SingleCellExperiment.
#'
#' @export
#'
#' @author Kevin Rue-Albrecht
#' @examples
#'
#' # Example ExperimentColorMap ----
#'
#' count_colors <- function(n){
#'   c("black","brown","red","orange","yellow")
#' }
#' fpkm_colors <- viridis::inferno
#'
#' ecm <- ExperimentColorMap(
#'     assays = list(
#'         counts = count_colors,
#'         tophat_counts = count_colors,
#'         cufflinks_fpkm = fpkm_colors,
#'         rsem_counts = count_colors,
#'         orphan = count_colors,
#'         orphan2 = count_colors,
#'         count_colors,
#'         fpkm_colors
#'     )
#' )
#'
#' # Example SingleCellExperiment ----
#'
#' library(scRNAseq)
#' sce <- ReprocessedAllenData(assays="tophat_counts")
#' library(scater)
#' sce <- logNormCounts(sce, exprs_values="tophat_counts")
#' sce <- runPCA(sce)
#' sce <- runTSNE(sce)
#'
#' # Example ----
#'
#' ecm_sync <- synchronizeAssays(ecm, sce)
#'
synchronizeAssays <- function(ecm, se){
    stopifnot(is(ecm, "ExperimentColorMap"))
    stopifnot(inherits(se, "SummarizedExperiment"))

    se_assay_names <- assayNames(se)
    ecm_assay_names <- assayNames(ecm)

    # Prepare a warning message for unused colormaps
    unnamed_ecm <- which(ecm_assay_names == "")
    unnamed_warning <- ifelse(
        length(unnamed_ecm) == 0,
        "",
        sprintf("unnamed [%s]", paste(unnamed_ecm, collapse = ","))
    )

    orphan_ecm <- setdiff(ecm_assay_names, se_assay_names)
    orphan_ecm <- setdiff(orphan_ecm, "")
    orphan_warning <- ifelse(
        length(orphan_ecm) == 0,
        "",
        sprintf("named [%s]", paste(orphan_ecm, collapse = ","))
    )

    ecm_warning <- paste(unnamed_warning, orphan_warning, sep = ", ")

    if (all(se_assay_names != "")){
        # If all of the SCE assays are named

        # Drop assays from ECM that are absent in se
        if (length(orphan_ecm) + length(unnamed_ecm) > 0){
            warning(
                "Unused assays dropped from ecm: ",
                ecm_warning)
        }
        # Fetch named-matched assay colormaps
        new_ecm_assays <- lapply(se_assay_names, function(x){assayColorMap(ecm, x)})

    } else if (all(se_assay_names == "")){
        # If none of the SCE assays are named

        # Require that the number of assay colormaps in ECM is identical
        # if so, return the ECM as is
        if (length(ecm_assay_names) == length(se_assay_names)){
            new_ecm_assays <- assays(ecm)
            # NOTE: uncomment below to strip assayNames from the colormaps,
            # thereby matching the assayNames of the SCE
            # names(new_ecm_assays) <- rep("", length(new_ecm_assays))
        } else {
            stop(paste(
                "Cannot synchronize assays.",
                sprintf(
                    "Length of unnamed assays must match: se [%i], ecm [%i]",
                    length(se_assay_names),
                    length(ecm_assay_names)
                )
            ))
        }

    } else {
        # If a subset of the SCE assays are named

        if (length(orphan_ecm) + length(unnamed_ecm) > 0){
            warning(
                "Unused assays dropped from ecm: ",
                ecm_warning)
        }
        # Exclude unnamed assay colormaps in the ExperimentColorMap
        assays(ecm) <- assays(ecm)[assayNames(ecm) != ""]
        # Fetch named-matched assay colormaps
        new_ecm_assays <- lapply(se_assay_names, function(x){assayColorMap(ecm, x)})

    }

    # Apply assayNames from the SummarizedExperiment
    names(new_ecm_assays) <- se_assay_names

    assays(ecm) <- new_ecm_assays
    return(ecm)
}
