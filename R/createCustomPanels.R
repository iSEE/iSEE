#' Create custom panels
#'
#' Helper functions for quick-and-dirty creation of custom panels,
#' usually in the context of a one-off application.
#' This creates a new class with specialized methods for showing content based on a user-specified function.
#'
#' @param FUN A function that generates a data.frame or a \link{ggplot},
#' for \code{createCustomTable} and \code{createCustomPlot} respectively.
#' See Details for the expected arguments.
#' @param argStrings Character vector of names of optional string arguments to \code{FUN}.
#' @param argNumbers Character vector of names of optional (scalar) numeric arguments to \code{FUN}.
#' @param argFlags Character vector of names of optional (scalar) logical arguments to \code{FUN}.
#' @param className String containing the name of the new \linkS4class{Panel} class.
#' @param fullName String containing the full name of the new class.
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
#' These will be passed from UI elements in the panel if the names of those arguments are listed in \code{argStrings}, etc.
#' All such additional arguments should have default values,
#' which are extracted for use as the default vfalues in the UI.
#'
#' Classes created via these functions are extremely limited.
#' Only scalar inputs are supported via the UI and all panels cannot transmit to the rest of the app.
#' We recommend only using these functions for one-off applications to quickly prototype concepts;
#' serious \linkS4class{Panel} extensions should be done explicitly.
#'
#' @examples
#' library(scater)
#' CUSTOM_PCA <- function(se, rows, columns, ntop=500, scale_features=TRUE) {
#'     if (!is.null(columns)) {
#'         kept <- se[, columns]
#'     } else {
#'         return(
#'             ggplot() + theme_void() + geom_text(
#'                 aes(x, y, label=label),
#'                 data.frame(x=0, y=0, label="No column data selected."),
#'                 size=5)
#'             )
#'     }
#' 
#'     scale_features <- as.logical(scale_features)
#'     kept <- runPCA(kept, ntop=ntop, scale_features=scale_features)
#'     plotPCA(kept, colour_by=colour_by)
#' }
#'
#' GEN <- createCustomPlot(CUSTOM_PCA, argNumbers="ntop", argFlags="scale_features")
#' GEN()
#'
#' \dontrun{
#' # Assume we have a SCE object lying around:
#' iSEE(sce, initial=list(
#'     RedDimPlot(PanelId=1L),
#'     GEN(SelectColSource="RedDimPlot1")
#' )
#' }
#' @author Aaron Lun
#' 
#' @export
#' @name createCustomPanels
createCustomTable <- function(FUN, 
    argStrings=character(0), argNumbers=character(0), argFlags=character(0),
    className="CustomTable", fullName="Custom table")
{
    collated <- character(0)
    collated[argStrings] <- "character"
    collated[argNumbers] <- "numeric"
    collated[argFlags] <- "logical"

    generator <- setClass(className, contains="Table", slots=collated, where=.GlobalEnv)

    .spawn_custom_methods(FUN, className=className, fullName=fullName,
        argStrings=argStrings, argNumbers=argNumbers, argFlags=argFlags)

    fn_name <- deparse(substitute(FUN))
    setMethod(".generateTable", className, function(x, envir) {
        .execute_custom_function(FUN, fn_name, assigned="tab", envir=envir)
    }, where=.GlobalEnv)

    generator
}

#' @export
#' @rdname createCustomPanels
createCustomPlot <- function(FUN,
    argStrings=character(0), argNumbers=character(0), argFlags=character(0),
    className="CustomPlot", fullName="Custom plot")
{
    collated <- character(0)
    collated[argStrings] <- "character"
    collated[argNumbers] <- "numeric"
    collated[argFlags] <- "logical"

    generator <- setClass(className, contains="Panel", slots=collated, where=.GlobalEnv)

    .spawn_custom_methods(FUN, className=className, fullName=fullName,
        argStrings=argStrings, argNumbers=argNumbers, argFlags=argFlags)

    fn_name <- deparse(substitute(FUN))
    setMethod(".generateOutput", className, function(x, se, all_memory, all_contents) {
        plot_env <- new.env()
        plot_env$se <- se

        selected <- .processMultiSelections(x, all_memory, all_contents, plot_env)
        commands <- .execute_custom_function(FUN, fn_name, assigned="gg", envir=plot_env)
        commands <- sub("^gg <- ", "", commands) # to avoid an unnecessary variable.
        
        list(contents=plot_env$gg, commands=list(select=selected, plot=commands))
    }, where=.GlobalEnv)

    setMethod(".renderOutput", className, function(x, se, output, pObjects, rObjects) {
        plot_name <- .getEncodedName(x)
        force(se) # defensive programming to avoid difficult bugs due to delayed evaluation.
        output[[plot_name]] <- renderPlot({
            p.out <- .retrieveOutput(plot_name, se, pObjects, rObjects)
            p.out$contents
        })
    }, where=.GlobalEnv)

    generator
}

#' @importFrom shiny tagList textInput numericInput checkboxInput
.spawn_custom_methods <- function(FUN, className, fullName,
    argStrings=character(0), argNumbers=character(0), argFlags=character(0))
{
    defaults <- formals(FUN)
    setMethod("initialize", className, function(.Object, ...) {
        args <- list(...)
        for (x in c(argStrings, argNumbers, argFlags)) {
            args <- .empty_default(args, x, defaults[[x]])
        }
        do.call(callNextMethod, c(list(.Object), args))
    }, where=.GlobalEnv)

    setMethod(".defineDataInterface", className, function(x, se, select_info) {
        tab_name <- .getEncodedName(x)

        string_ui <- lapply(argStrings, function(id) {
            textInput(paste0(tab_name, "_", id),  label=id, value=x[[id]])
        })

        number_ui <- lapply(argStrings, function(id) {
            numericInput(paste0(tab_name, "_", id), label=id, value=x[[id]])
        })

        flag_ui <- lapply(argFlags, function(id) {
            checkboxInput(paste0(tab_name, "_", id), label=id, value=x[[id]])
        })

        do.call(tagList, c(string_ui, number_ui, flag_ui))
    }, where=.GlobalEnv)

    setMethod(".fullName", className, function(x) fullName, where=.GlobalEnv)
}

.execute_custom_function <- function(FUN, fn_name, assigned, envir) {
    fn_args <- paste(assigned, "<- %s(se,") 

    if (exists("row_selected", envir, inherits=FALSE)) {
        fn_args <- paste(fn_args, "row_selected,")
    } else {
        fn_args <- paste(fn_args, "NULL,")
    }
    
    if (exists("col_selected", envir, inherits=FALSE)) {
        fn_args <- paste(fn_args, "col_selected,")
    } else {
        fn_args <- paste(fn_args, "NULL,")
    }

    extra_args <- list()
    for (i in c(argStrings, argNumbers, argFlags)) {
        extra_args[[i]] <- deparse(x[[i]])
    }
    extra_args <- paste(sprintf("%s=%s", names(fn_args), unlist(constructed)), collapse=", ")

    total_call <- paste(fn_args, extra_args)
    total_call <- paste0(total_call, ")")
    total_call <- paste(strwrap(total_call, exdent=4), collapse="\n")

    # Not using 'fn_name' to assign to 'envir', to avoid potentially
    # overwriting important variables like 'se' with arbitrary user names. 
    envir$.customFUN <- FUN
    tmp_call <- sprintf(total_call, ".customFUN")
    .text_eval(tmp_call, envir)

    sprintf(total_call, fn_name)
}
