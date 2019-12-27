#' Track code for the plots
#'
#' Fetches all the code that was used to generate the plots during the live iSEE session.
#' See section "Functions" for more details.
#'
#' @param active_panels A data.frame containing information about the currently active panels, of the same form as that produced by \code{\link{.setup_initial}}.
#' @param pObjects An environment containing \code{memory}, a list of DataFrames containing parameters for each panel of each type;
#' \code{selection_links}, a graph object containing point selection links between panels;
#' and \code{commands}, a list of commands required to generated each plot.
#' @param se_name String containing the name of the SummarizedExperiment or SingleCellExperiment object.
#' @param ecm_name String containing the name of the ExperimentColorMap in use.
#' @param select_only Logical scalar indicating whether only the commands to generate the selections should be returned.
#' Otherwise, all commands to generate the plots will also be returned.
#'
#' @return A character vector containing all lines of code (plus comments), to use in the \code{shinyAce} editor in \code{\link{iSEE}}.
#'
#' @author Federico Marini
#' @rdname INTERNAL_track_it_all
.track_it_all <- function(active_panels, pObjects, se_name, ecm_name) {
    tracked_code <- c(
        "## The following list of commands will generate the plots created in iSEE",
        "## Copy them into a script or an R session containing your SingleCellExperiment.",
        "## All commands below refer to your SingleCellExperiment object as `se`.",
        "",
        sprintf("se <- %s", se_name),
        sprintf("colormap <- %s", ecm_name),
        sprintf("colormap <- synchronizeAssays(colormap, se)"),
        "all_contents <- list()",

        "",

        .track_selection_code(active_panels),

        .track_plotting_code(active_panels, pObjects),

        strrep("#", 80),
        "## To guarantee the reproducibility of your code, you should also",
        "## record the output of sessionInfo()",
        "sessionInfo()")

    # Restoring indenting for multi-line ggplot code.
    ggplot_ix <- grep("\\+$", tracked_code) + 1L
    to_mod <- tracked_code[ggplot_ix]
    to_mod <- paste0("    ", to_mod)
    to_mod <- gsub("\n", "\n    ", to_mod)
    tracked_code[ggplot_ix] <- to_mod

    return(tracked_code)
}

#' @describeIn INTERNAL_track_it_all Returns a character vector of commands that create brushes, lassos, and selection history.
.track_selection_code  <- function(active_panels) {
    brush_code <- history_code <- list()
    for (panel_name in names(active_panels)) {
        instance <- active_panels[[panel_name]]
        if (!is(instance, "DotPlot")) {
            next
        }

        brush_struct <- active_panels[[panel_name]][[.brushData]]
        if (!is.null(brush_struct)) {
            brush_struct <- .deparse_for_viewing(brush_struct, indent=0) # deparsed list() auto-indents.
            brush_code[[panel_name]] <- sprintf("all_brushes[['%s']] <- %s", panel_name, brush_struct)
        }

        saved_struct <- active_panels[[panel_name]][[.multiSelectHistory]]
        if (length(saved_struct)) {
            saved_struct <- .deparse_for_viewing(saved_struct)
            history_code[[panel_name]] <- sprintf("all_select_histories[['%s']] <- %s", panel_name, saved_struct)
        }
    }

    tracked_code <- list(character(0), character(0))
    if (any(lengths(brush_code) > 0)) {
        tracked_code[[1]] <- c(strrep("#", 80),
            "# Defining brushes",
            strrep("#", 80), "",
            "all_brushes <- list()",
            unlist(brush_code), "")
    }

    if (any(lengths(history_code) > 0)) {
        tracked_code[[2]] <- c(strrep("#", 80),
            "# Defining selection histories",
            strrep("#", 80), "",
            "all_select_histories <- list()",
            unlist(history_code), "")
    }

    unlist(tracked_code)
}

#' @rdname INTERNAL_track_it_all
#' @importFrom igraph topo_sort
.track_plotting_code <- function(active_panels, pObjects, select_only = FALSE) {
    all_tracks <- list()
    ordering <- names(topo_sort(pObjects$selection_links, "out"))

    for (panel_name in ordering) {
        instance <- active_panels[[panel_name]]
        header_comments <- c(strrep("#", 80),
            paste("##", .getFullName(instance)),
            strrep("#", 80),
            "")

        # Adding the plotting commands.
        collated <- character(0)
        for (cmds in pObjects$commands[[panel_name]]) {
            collated <- c(collated, cmds, "")
        }

        # Saving data for transmission after selections have been processed;
        # this is the equivalent point in .create_plots() where coordinates are saved.
        varname <- pObjects$varname[[panel_name]]
        if (!is.null(varname)) {
            collated <- c(collated, "# Saving data for transmission",
                sprintf("all_contents[['%s']] <- %s", panel_name, varname),
                "")
        }

        all_tracks[[panel_name]] <- c(header_comments, collated)
    }

    unlist(all_tracks)
}

#' Deparse R expressions for viewing
#'
#' Generate the command required to produce an R object, in a multi-line string with appropriate indenting.
#'
#' @param exprs An R expression to be deparsed.
#' @param indent An integer scalar specifying the indent level.
#'
#' @return A string containing the R command to produce \code{exprs}.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_deparse_for_viewing
#' @seealso
#' \code{\link{.track_it_all}}
#' \code{\link{.report_memory}}
.deparse_for_viewing <- function(exprs, indent=1) {
    paste(deparse(exprs, width.cutoff=80),
        collapse=paste0("\n", strrep(" ", indent*4)))
}


#' Plot a snapshot of the links among panels
#'
#' Creates a graph of the current links among all currently active panels, and plots it.
#'
#' @param graph A \link{graph} containing panels as nodes and directional edges as transmissions of multiple selections.
#' @param panel_colors A named character vector of colors for each panel.
#'
#' @return Plots the graph representing the snapshot of links among plots/panels
#' @author Federico Marini
#' @rdname INTERNAL_snapshot_graph_linkedpanels
#' @importFrom graphics plot
.snapshot_graph_linkedpanels <- function(graph, panel_colors) {
    plot(graph,
        edge.arrow.size = .8,
        vertex.label.cex = 1.3,
        vertex.label.family = "Helvetica",
        vertex.label.color = "black",
        vertex.label.dist = 2.5,
        vertex.color = panel_colors[names(V(graph))])
}


#' Report current memory state
#'
#' Report the R commands necessary to reproduce the memory state of the interface.
#'
#' @param all_memory A list of \linkS4class{Panel} objects representing the current state of the application.
#'
#' @return A character vector of commands necessary to produce \code{all_memory}.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_report_memory
#' @seealso
#' \code{\link{iSEE}}
.report_memory <- function(all_memory) {
    collected <- list("initial <- list()")

    for (i in names(all_memory)) {
        panel <- all_memory[[i]]
        collected[[i]] <- list(
            "",
            strrep("#", 80),
            sprintf("# Settings for %s", .getFullName(panel)),
            strrep("#", 80), "",
            paste(
                sprintf("initial[[%s]] <-", deparse(i)),
                paste(deparse(panel), collapse="\n")
            )
        )
    }

    unlist(collected)
}