#' Create custom panels
#'
#' Helper functions for quick-and-dirty creation of custom panels,
#' usually in the context of a one-off application.
#' This creates a new class with specialized methods for showing content based on a user-specified function.
#'
#' @param FUN A function that generates a data.frame or a \link{ggplot},
#' for \code{createCustomTable} and \code{createCustomPlot} respectively.
#' See Details for the expected arguments.
#' @param restrict Character vector of names of optional arguments in \code{FUN} to which the UI is restricted.
#' If specified, only the listed arguments receive UI elements in the interface.
#' @param className String containing the name of the new \linkS4class{Panel} class.
#' @param fullName String containing the full name of the new class.
#' @param FUN.selection.details Function generating a UI element that displays details about the current selection, if any.
#' @param where An environment indicating where the class and method definitions should be stored.
#'
#' @return
#' A new class and its methods are defined in the global environment.
#' A generator function for creating new instances of the class is returned.
#'
#' @details
#' \code{FUN} is expected to have the following first 3 arguments:
#' \itemize{
#' \item \code{se}, a \linkS4class{SummarizedExperiment} object for the current dataset of interest.
#' \item \code{rows}, a list of row selections received from the transmitting panel.
#' This contains one or more character vectors of row names in active and saved selections.
#' Alternatively, this may be \code{NULL} if no selection has been made in the transmitter.
#' \item \code{columns}, a list of column selections received from the transmitting panel.
#' This contains one or more character vectors of column names in active and saved selections.
#' Alternatively, this may be \code{NULL} if no selection has been made in the transmitter.
#' }
#'
#' Any number of additional named arguments may also be present in \code{FUN}.
#' All such arguments should have default values,
#' as these are used to automatically generate UI elements in the panel:
#' \itemize{
#' \item Character vectors will get a \code{\link{selectInput}}.
#' \item Strings will get a \code{\link{textInput}}.
#' \item Numeric scalars will get a \code{\link{numericInput}}.
#' \item Logical scalars will get a \code{\link{checkboxInput}}.
#' }
#' Arguments with other types of default values are ignored.
#' If \code{restrict} is specified, arguments will only have corresponding UI elements if they are listed in \code{restrict}.
#' All user interactions with these elements will automatically trigger regeneration of the panel contents.
#'
#' Classes created via these functions are extremely limited.
#' Only scalar inputs are supported via the UI and all panels cannot transmit to the rest of the app.
#' We recommend only using these functions for one-off applications to quickly prototype concepts;
#' serious \linkS4class{Panel} extensions should be done explicitly.
#'
#' @examples
#' library(scater)
#' CUSTOM_DIMRED <- function(se, rows, columns, ntop=500, scale=TRUE,
#'     mode=c("PCA", "TSNE", "UMAP"))
#' {
#'     if (is.null(columns)) {
#'         return(
#'             ggplot() + theme_void() + geom_text(
#'                 aes(x, y, label=label),
#'                 data.frame(x=0, y=0, label="No column data selected."),
#'                 size=5)
#'             )
#'     }
#'
#'     mode <- match.arg(mode)
#'     if (mode=="PCA") {
#'         calcFUN <- runPCA
#'     } else if (mode=="TSNE") {
#'         calcFUN <- runTSNE
#'     } else if (mode=="UMAP") {
#'         calcFUN <- runUMAP
#'     }
#'
#'     kept <- se[, unique(unlist(columns))]
#'     kept <- calcFUN(kept, ncomponents=2, ntop=ntop,
#'         scale=scale, subset_row=unique(unlist(rows)))
#'     plotReducedDim(kept, mode)
#' }
#'
#' GEN <- createCustomPlot(CUSTOM_DIMRED)
#' GEN()
#'
#' if (interactive()) {
#'     library(scRNAseq)
#'     sce <- ReprocessedAllenData("tophat_counts")
#'     library(scater)
#'     sce <- logNormCounts(sce, exprs_values="tophat_counts")
#'
#'     iSEE(sce, initial=list(
#'         ColumnDataPlot(PanelId=1L),
#'         GEN(ColumnSelectionSource="ColumnDataPlot1")
#'     ))
#' }
#'
#' @author Aaron Lun
#'
#' @export
#' @name createCustomPanels
createCustomTable <- function(FUN, restrict=NULL, className="CustomTable",
    fullName="Custom table", FUN.selection.details = NULL,
    where=topenv(parent.frame()))
{
    fn_args <- .grab_all_args(FUN, restrict)
    collated <- vapply(fn_args, class, "")
    generator <- setClass(className, contains="Table", slots=collated, where=where)

    .spawn_custom_methods(fn_args, className=className, fullName=fullName, where=where)

    fn_name <- deparse(substitute(FUN))
    setMethod(".generateTable", className, function(x, envir) {
        .execute_custom_function(x, FUN,
            fn_name=fn_name, assigned="tab", envir=envir,
            fn_args=names(fn_args))
    }, where=where)

    setMethod(".refineParameters", className, function(x, se) {
        x <- callNextMethod()
        if (is.null(x)) {
            return(NULL)
        }
        .replaceMissingWithFirst(x, .TableSelected, "")
    }, where=where)
    
    setMethod(".showSelectionDetails", className, function(x) {
        if (!is.null(FUN.selection.details)) {
            FUN.selection.details(slot(x, .TableSelected))
        }
    }, where = where)
    
    setMethod(".multiSelectionResponsive", className, function(x, dim) {
        TRUE
    }, where = where)

    generator
}

#' @export
#' @rdname createCustomPanels
createCustomPlot <- function(FUN, restrict=NULL, className="CustomPlot",
    fullName="Custom plot", where=topenv(parent.frame()))
{
    fn_args <- .grab_all_args(FUN, restrict)
    collated <- vapply(fn_args, class, "")
    generator <- setClass(className, contains="Panel", slots=collated, where=where)

    .spawn_custom_methods(fn_args, className=className, fullName=fullName, where=where)

    setMethod(".defineOutput", className, function(x) {
        plotOutput(.getEncodedName(x))
    }, where=where)

    fn_name <- deparse(substitute(FUN))
    setMethod(".generateOutput", className, function(x, se, all_memory, all_contents) {
        plot_env <- new.env()
        plot_env$se <- se

        selected <- .processMultiSelections(x, all_memory, all_contents, plot_env)
        commands <- .execute_custom_function(x, FUN,
            fn_name=fn_name, assigned="gg", envir=plot_env,
            fn_args=names(fn_args))

        commands <- sub("^gg <- ", "", commands) # to avoid an unnecessary variable.
        list(contents=plot_env$gg, commands=list(select=selected, plot=commands))
    }, where=where)

    setMethod(".renderOutput", className, function(x, se, output, pObjects, rObjects) {
        plot_name <- .getEncodedName(x)
        force(se) # defensive programming to avoid difficult bugs due to delayed evaluation.

        # nocov start
        output[[plot_name]] <- renderPlot({
            .retrieveOutput(plot_name, se, pObjects, rObjects)$contents
        })
        # nocov end

    }, where=where)

    generator
}

#' Internal custom panel methods
#'
#' @param defaults A named list of default arguments to the custom function.
#' @inheritParams createCustomPanels
#' @param x An instance of the custom \linkS4class{Panel} class.
#' @param fn_args Character vector of names of all arguments to pass from \code{x} to \code{FUN}.
#' @param fn_name String containing the name of the function, to show in the code tracker.
#' @param assigned String containing the name of the variable to assign the output of \code{FUN} during evaluation.
#' @param envir The evaluation environment.
#'
#' @return
#' \code{.spawn_custom_methods} will define methods for the custom class in \code{where}.
#'
#' \code{.grab_all_args} will return a named list of default values from arguments of \code{FUN},
#' which can be converted to supported UI elements.
#'
#' \code{.execute_custom_function} will execute \code{FUN} with the specified arguments from \code{x} and \code{fn_args},
#' returning a string of R commands to obtain call.
#'
#' @author Aaron Lun
#'
#' @rdname INTERNAL_custom_panel_methods
#' @importFrom shiny tagList textInput numericInput checkboxInput selectInput
.spawn_custom_methods <- function(defaults, className, fullName, where=topenv(parent.frame())) {
    force(defaults)
    force(fullName)

    setMethod("initialize", className, function(.Object, ...) {
        extra_args <- list(...)
        for (i in names(defaults)) {
            extra_args <- .emptyDefault(extra_args, i, defaults[[i]][1]) # select first element when multiple choice.
        }
        do.call(callNextMethod, c(list(.Object), extra_args))
    }, where=where)

    setMethod(".defineDataInterface", className, function(x, se, select_info) {
        tab_name <- .getEncodedName(x)
        collected <- list()

        for (i in names(defaults)) {
            options <- defaults[[i]]
            current <- slot(x, i)
            id <- paste0(tab_name, "_", i)

            collected[[i]] <- if (is.character(options) && length(options)==1L) {
                textInput(id, label=i, value=current)
            } else if (is.character(options) && length(options) >= 1L) {
                selectInput(id, label=i, choices=options, selected=current)
            } else if (is.numeric(options)) {
                numericInput(id, label=i, value=current)
            } else if (is.logical(options)) {
                checkboxInput(id, label=i, value=current)
            }
        }

        do.call(tagList, collected)
    }, where=where)

    setMethod(".createObservers", className, function(x, se, input, session, pObjects, rObjects) {
        callNextMethod()

        panel_name <- .getEncodedName(x)

        # Doesn't matter all that much whether they're protected or not,
        # given that custom panels cannot transmit.
        .createProtectedParameterObservers(panel_name, names(defaults),
            input=input, pObjects=pObjects, rObjects=rObjects)

        invisible(NULL)
    }, where=where)

    setMethod(".fullName", className, function(x) fullName, where=where)

    setMethod(".panelColor", className, function(x) "#4D4D4D", where=where)
}

#' @rdname INTERNAL_custom_panel_methods
.grab_all_args <- function(FUN, restrict=NULL) {
    default.args <- formals(FUN)
    default.args <- default.args[-seq_len(3)]

    # Prune out arguments that we can't support.
    keepers <- list()
    for (i in names(default.args)) {
        current <- eval(default.args[[i]])
        if ((is.character(current) && length(current)!=0L) ||
            (is.numeric(current) && length(current)==1L) ||
            (is.logical(current) && length(current)==1L))
        {
            keepers[[i]] <- current
        }
    }

    if (!is.null(restrict)) {
        keepers <- keepers[intersect(names(keepers), restrict)]
    }

    keepers
}

#' @rdname INTERNAL_custom_panel_methods
.execute_custom_function <- function(x, FUN, fn_name, fn_args, assigned, envir) {
    fn_call <- paste(assigned, "<- %s(se")

    if (exists("row_selected", envir, inherits=FALSE)) {
        fn_call <- paste(fn_call, ", row_selected")
    } else {
        fn_call <- paste(fn_call, ", NULL")
    }

    if (exists("col_selected", envir, inherits=FALSE)) {
        fn_call <- paste(fn_call, ", col_selected")
    } else {
        fn_call <- paste(fn_call, ", NULL")
    }

    extra_args <- list()
    for (i in fn_args) {
        extra_args[[i]] <- deparse(slot(x, i))
    }
    extra_args <- paste(sprintf("%s=%s", names(extra_args), unlist(extra_args)), collapse=", ")

    if (!identical(extra_args, "")) {
        fn_call <- paste(fn_call, extra_args, sep = ", ")
    }
    fn_call <- paste0(fn_call, ")")
    fn_call <- paste(strwrap(fn_call, exdent=4), collapse="\n")

    # Not using 'fn_name' to assign to 'envir', to avoid potentially
    # overwriting important variables like 'se' with arbitrary user names.
    envir$.customFUN <- FUN
    tmp_call <- sprintf(fn_call, ".customFUN")
    .textEval(tmp_call, envir)

    sprintf(fn_call, fn_name)
}
