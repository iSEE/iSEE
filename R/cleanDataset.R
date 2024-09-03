#' Clean the dataset
#'
#' Clean the SummarizedExperiment by making sure that names of various fields are available and unique.
#' Also transfer any information stored in \code{rowRanges} into \code{rowData}.
#'
#' @param se A \linkS4class{SummarizedExperiment} object or one of its subclasses.
#'
#' @return 
#' A cleaned version of \code{se}.
#'
#' @details
#' Various \linkS4class{Panel}s assume that the row and column names of the input SummarizedExperiment are available and unique.
#' This function enforces that, adding consecutive integer names if not available and calling \code{\link{make.unique}} if they are duplicated.
#'
#' Various \linkS4class{Panel}s further assume that the \code{\link{assay}}, \code{\link{rowData}}, \code{\link{colData}} names are unique;
#' if this is not the case, \code{\link{selectInput}} behaves in unexpected (and incorrect) ways.
#' This function enforces that as well by running them through \code{\link{make.unique}}.
#' 
#' Finally, positional information in \code{rowRanges} is not accessible to \code{iSEE}.
#' This function moves this information into \code{rowData}, prefixing the column names with \code{rowRanges_}.
#'
#' For \linkS4class{SingleCellExperiment} object, we enforce uniqueness in the \code{\link{reducedDims}}.
#'
#' All changes result in warnings as a \dQuote{sensible} object is not expected to require any work.
#'
#' @author Aaron Lun, Charlotte Soneson
#'
#' @name cleanDataset
#'
#' @examples
#' # Creating a very naughty SE.
#' se <- SummarizedExperiment(list(cbind(1:10, 2:11), cbind(2:11, 3:12)),
#'    colData=DataFrame(A=1:2, A=3:4, check.names=FALSE), 
#'    rowData=DataFrame(B=1:10, B=1:10, check.names=FALSE))
#' se
#'
#' cleanDataset(se)
NULL

#' @export
#' @rdname cleanDataset
#' @importFrom SummarizedExperiment rowData colData rowData<- colData<- assayNames assayNames<- rowRanges
setMethod("cleanDataset", "SummarizedExperiment", function(se) {
    if (is.null(rownames(se))) {
        if (nrow(se) > 0) {
            warning("filling in the missing 'rownames(se)'")
        }
        rownames(se) <- seq_len(nrow(se))
    } else if (anyDuplicated(rownames(se))) {
        warning("duplicated 'rownames(se)' detected, making them unique")
        rownames(se) <- make.unique(rownames(se))
    }

    if (is.null(colnames(se))) {
        if (ncol(se) > 0) {
            warning("filling in the missing 'colnames(se)'")
        }
        colnames(se) <- seq_len(ncol(se))
    } else if (anyDuplicated(colnames(se))) {
        warning("duplicated 'colnames(se)' detected, making them unique")
        colnames(se) <- make.unique(colnames(se))
    }

    if (anyDuplicated(colnames(rowData(se)))) {
        warning("duplicated 'colnames(rowData(se))' detected, making them unique")
        colnames(rowData(se)) <- make.unique(colnames(rowData(se)))
    }

    if (anyDuplicated(colnames(colData(se)))) {
        warning("duplicated 'colnames(colData(se))' detected, making them unique")
        colnames(colData(se)) <- make.unique(colnames(colData(se)))
    }

    if (is.null(assayNames(se)) && length(assays(se)) > 0) {
        warning("empty 'assayNames(se)' detected, renaming to 'unnamed'")
        assayNames(se) <- rep("unnamed", length(assays(se)))
    } else if (any(empty <- assayNames(se) == "")) {
        warning("empty 'assayNames(se)' detected, renaming to 'unnamed'")
        assayNames(se)[empty] <- 'unnamed'
    }
    if (anyDuplicated(assayNames(se))) {
        warning("duplicated 'assayNames(se)' detected, making them unique")
        assayNames(se) <- make.unique(assayNames(se))
    }
    
    ## Only transfer data if rowRanges is a GRanges object - if it's a GRangesList, we can not ensure that the dimensions after unlisting would be compatible with the data set
    if (is(rowRanges(se), "GRanges") && length(rowRanges(se)) > 0 && !any(paste0("rowRanges_", c("start", "end", "seqnames", "strand")) %in% colnames(rowData(se)))) {
        warning("rowRanges (GRanges) detected - copying values to rowData")
        for (cn in c("start", "end")) {
            rowData(se)[[paste0("rowRanges_", cn)]] <- get(cn)(rowRanges(se))
        }
        for (cn in c("seqnames", "strand")) {
            rowData(se)[[paste0("rowRanges_", cn)]] <- factor(get(cn)(rowRanges(se)))
        }
    }
    
    se
})

#' @export
#' @rdname cleanDataset
#' @importFrom SingleCellExperiment reducedDimNames reducedDimNames<-
setMethod("cleanDataset", "SingleCellExperiment", function(se) {
    se <- callNextMethod()
    
    if (anyDuplicated(reducedDimNames(se))) {
        #nocov start
        warning("duplicated 'reducedDimNames(se)' detected, making them unique")
        reducedDimNames(se) <- make.unique(reducedDimNames(se))
        #nocov end
    }

    se
})
