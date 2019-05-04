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
#' @param cdf_name String containing the expression used to create the list of functions for custom data plots.
#' @param csf_name String containing the expression used to create the list of functions for custom statistics tables.
#' @param sanitize_cmds Character vector containing the commands used to sanitizing the \code{se} object.
#' This is obtained by extracting \code{cmds} from the returned output of \code{.sanitize_SE_input}.
#' @param select_only Logical scalar indicating whether only the commands to generate the selections should be returned.
#' Otherwise, all commands to generate the plots will also be returned.
#'
#' @return A character vector containing all lines of code (plus comments), to use in the \code{shinyAce} editor in \code{\link{iSEE}}.
#'
#' @author Federico Marini
#' @rdname INTERNAL_track_it_all
.track_it_all <- function(active_panels, pObjects, se_name, ecm_name, cdf_name, csf_name, sanitize_cmds) {
    tracked_code <- c(
        "## The following list of commands will generate the plots created in iSEE",
        "## Copy them into a script or an R session containing your SingleCellExperiment.",
        "## All commands below refer to your SingleCellExperiment object as `se`.",
        "",
        sprintf("se <- %s", se_name),
        sanitize_cmds,
        sprintf("colormap <- %s", ecm_name),
        sprintf("colormap <- synchronizeAssays(colormap, se)"),
        "all_coordinates <- list()",
        sprintf("custom_data_fun <- %s", cdf_name),
        sprintf("custom_stat_fun <- %s", csf_name),

        "",

        .track_selection_code(active_panels, pObjects),

        .track_plotting_code(active_panels, pObjects),

        .track_custom_code(active_panels, pObjects),

        .track_heatmap_code(active_panels, pObjects),

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
.track_selection_code  <- function(active_panels, pObjects) {
    # Remove panels that don't have brushes or lassos.
    is_point_plot <- active_panels$Type %in% point_plot_types
    active_panels <- active_panels[is_point_plot,]
    brush_code <- lasso_code <- history_code <- vector("list", nrow(active_panels))

    for (i in seq_len(nrow(active_panels))) {
        panel_type <- active_panels$Type[i]
        panel_id <- active_panels$ID[i]
        panel_name <- paste0(panel_type, panel_id)

        brush_struct <- pObjects$memory[[panel_type]][,.brushData][[panel_id]]
        if (!is.null(brush_struct)) {
            brush_struct <- .deparse_for_viewing(brush_struct, indent=0) # deparsed list() auto-indents.
            brush_code[[i]] <- sprintf("all_brushes[['%s']] <- %s", panel_name, brush_struct)
        }

        lasso_struct <- pObjects$memory[[panel_type]][,.lassoData][[panel_id]]
        if (!is.null(lasso_struct)) {
            lasso_struct <- .deparse_for_viewing(lasso_struct)
            lasso_code[[i]] <- sprintf("all_lassos[['%s']] <- %s", panel_name, lasso_struct)
        }

        saved_struct <- pObjects$memory[[panel_type]][,.multiSelectHistory][[panel_id]]
        if (length(saved_struct)) {
            saved_struct <- .deparse_for_viewing(saved_struct)
            history_code[[i]] <- sprintf("all_select_histories[['%s']] <- %s", panel_name, saved_struct)
        }
    }

    tracked_code <- list(character(0), character(0), character(0))
    if (any(lengths(brush_code) > 0)) {
        tracked_code[[1]] <- c(strrep("#", 80),
                "# Defining brushes",
                strrep("#", 80), "",
                "all_brushes <- list()",
                unlist(brush_code), "")
    }

    if (any(lengths(lasso_code) > 0)) {
        tracked_code[[2]] <- c(strrep("#", 80),
                "# Defining lassos",
                strrep("#", 80), "",
                "all_lassos <- list()",
                unlist(lasso_code), "")
    }

    if (any(lengths(history_code) > 0)) {
        tracked_code[[2]] <- c(strrep("#", 80),
                "# Defining selection histories",
                strrep("#", 80), "",
                "all_select_histories <- list()",
                unlist(history_code), "")
    }

    return(unlist(tracked_code))
}

#' @rdname INTERNAL_track_it_all
.track_plotting_code <- function(active_panels, pObjects, select_only = FALSE) {
    # Ensure that plots are created in the order of their dependencies.
    active_panels <- active_panels[.get_reporting_order(active_panels, pObjects$selection_links),]
    is_point_plot <- active_panels$Type %in% point_plot_types
    active_panels <- active_panels[is_point_plot,]

    all_tracks <- vector("list", nrow(active_panels))
    for (i in seq_len(nrow(active_panels))) {
        panel_type <- active_panels$Type[i]
        panel_id <- active_panels$ID[i]
        panel_name <- paste0(panel_type, panel_id)

        header_comments <- c(strrep("#", 80),
                paste0("## ", .decode_panel_name(panel_type, panel_id)),
                strrep("#", 80),
                "")

        # Adding the plotting commands.
        cur_cmds <- pObjects$commands[[panel_name]]
        collated <- c(cur_cmds$data, "")
        if (length(cur_cmds$select)) {
            collated <- c(collated, "# Receiving point selection", cur_cmds$select, "")
        }

        # Saving data for transmission after selections have been processed;
        # this is the equivalent point in .create_plots() where coordinates are saved.
        collated <- c(collated, "# Saving data for transmission",
                      sprintf("all_coordinates[['%s']] <- plot.data", panel_name),
                      "")


        # Finishing off the rest of the commands.
        if (!select_only) {
            if (length(cur_cmds$setup)) {
                collated <- c(collated, "# Setting up plot coordinates", cur_cmds$setup, "")
            }
            if (length(cur_cmds$plot)) {
                collated <- c(collated, "# Creating the plot", cur_cmds$plot, "")
            }
        }

        all_tracks[[i]] <- c(header_comments, collated)
    }

    return(unlist(all_tracks))
}

#' @rdname INTERNAL_track_it_all
.track_custom_code <- function(active_panels, pObjects, select_only = FALSE) {
    active_panels <- active_panels[active_panels$Type=="customDataPlot",]

    all_custom <- vector("list", nrow(active_panels))
    for (i in seq_len(nrow(active_panels))) {
        panel_type <- active_panels$Type[i]
        panel_id <- active_panels$ID[i]
        panel_name <- paste0(panel_type, panel_id)

        header_comments <- c(strrep("#", 80),
                paste0("## ", .decode_panel_name(panel_type, panel_id)),
                strrep("#", 80),
                "")

        # Adding the selection commands.
        cur_cmds <- pObjects$commands[[panel_name]]
        collated <- c("# Receiving selection data", cur_cmds$select, "")

        # Adding the plotting commands.
        if (!select_only) {
            collated <- c(collated, "# Generating plot", cur_cmds$plot, "")
        }

        all_custom[[i]] <- c(header_comments, collated)
    }
    return(unlist(all_custom))
}

#' @rdname INTERNAL_track_it_all
.track_heatmap_code <- function(active_panels, pObjects,  select_only = FALSE) {
    hobjs <- active_panels[active_panels$Type=="heatMapPlot",]
    heat_names <- paste0(hobjs$Type, hobjs$ID)

    all_heat <- vector("list", nrow(hobjs))
    for (i in seq_along(heat_names)) {
        cur_cmds <- pObjects$commands[[heat_names[i]]]

        header_comments <- c(strrep("#", 80),
                paste0("## ", .decode_panel_name("heatMapPlot", hobjs$ID[i])),
                strrep("#", 80), "")

        # Adding the data setup.
        collated <- c(cur_cmds$data, "")

        # Finishing off the rest of the commands.
        if (length(cur_cmds$select)) {
            collated <- c(collated, "# Receiving selection data", cur_cmds$select, "")
        }

        if (!select_only) {
            if (length(cur_cmds$centerscale)) {
                collated <- c(collated, "# Centering and scaling", cur_cmds$centerscale, "")
            }
            if (length(cur_cmds$zoom)) {
                collated <- c(collated, "# Zooming in", cur_cmds$zoom, "")
            }
            if (length(cur_cmds$plot)) {
                collated <- c(collated, "# Creating the heat map", cur_cmds$plot, "")
            }
            if (length(cur_cmds$annot)) {
                collated <- c(collated, "# Adding annotations", cur_cmds$annot, "")
            }
            if (length(cur_cmds$grid)) {
                collated <- c(collated, "# Laying out the grid", cur_cmds$grid, "")
            }
        }

        all_heat[[i]] <- c(header_comments , collated)
    }

    return(unlist(all_heat))
}

#' Get the reporting order
#'
#' Reordering plots by whether or not they transmit point selection information.
#'
#' @param active_panels A data.frame containing information about the currently active panels, of the same form as that produced by \code{\link{.setup_initial}}.
#' @param select_chart A graph object containing information about how point selection information is transferred between panels.
#'
#' @details
#' This function is necessary to ensure that coordinates in transmitting plots are generated before point selection is performed in receiving plots.
#'
#' @return An integer vector specifying the order of the panels for reporting code.
#' @author Aaron Lun
#' @rdname INTERNAL_get_reporting_order
#' @seealso
#' \code{\link{.establish_eval_order}}
#' @importFrom igraph topo_sort
.get_reporting_order <- function(active_panels, select_chart)
{
    node_names <- sprintf("%s%i", active_panels$Type, active_panels$ID)
    ordering <- names(topo_sort(select_chart, "out"))
    ordering <- ordering[ordering %in% node_names]
    match(ordering, node_names)
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
#' @param active_panels A data.frame containing information about the currently active panels.
#' @param pObjects An environment containing \code{pObjects$selection_links}.
#'
#' @return Plots the graph representing the snapshot of links among plots/panels
#' @author Federico Marini
#' @rdname INTERNAL_snapshot_graph_linkedpanels
#' @importFrom igraph delete.vertices set_vertex_attr V add_edges
#' @importFrom graphics plot
.snapshot_graph_linkedpanels <- function(active_panels, pObjects) {
    cur_plots <- sprintf("%s%i", active_panels$Type, active_panels$ID)
    not_used <- setdiff(V(pObjects$selection_links)$name,cur_plots)
    curgraph <- delete.vertices(pObjects$selection_links, not_used)
    curgraph <- set_vertex_attr(curgraph,"plottype", value = sub("[0-9]+$","",V(curgraph)$name))

    # Incorporating table links as well.
    tbl_objs <- active_panels[active_panels$Type %in% linked_table_types,]
    cur_tables <- sprintf("%s%i",tbl_objs$Type,tbl_objs$ID)
    all_tlinks <- pObjects$table_links
    cur_tlinks <- all_tlinks[cur_tables]

    # instead of taking the links as such, check whether they are really used to link
    # following the implementation of .define_table_links...
    memory <- pObjects$memory

    for (i in seq_len(nrow(tbl_objs))) {
        table_used <- cur_tables[i]
        current <- cur_tlinks[[table_used]]
        for (trans in c("yaxis", "xaxis", "color")) {
            children <- current[[trans[[1]]]]
            child_enc <- .split_encoded(children)
            child_names <- .decode_panel_name(child_enc$Type, child_enc$ID)

            if (trans=="yaxis") {
                # Always adding links to Y-axis for sample/feature plots;
                # if it's linked, then there's no choice but to change.
                for (j in seq_along(child_names)) {
                    curgraph <- add_edges(curgraph, c(table_used, children[j]))
                }
            } else {
                if (trans=="xaxis") {
                    by_field <- c(.featAssayXAxis, .sampAssayXAxis)
                    ref_title <- c(.featAssayXAxisFeatNameTitle, .sampAssayXAxisSampNameTitle)
                } else {
                    by_field <- rep(.colorByField, 2)
                    ref_title <- c(.colorByFeatNameTitle, .colorBySampNameTitle)
                }

                # Only adding the edge if the plot actually receives the information via the
                # appropriate parameter choices.
                for (j in seq_along(child_names)) {
                    for (k in seq_along(by_field)) {
                        if (memory[[child_enc$Type[j]]][child_enc$ID[j], by_field[k]] == ref_title[k]) {
                            curgraph <- add_edges(curgraph, c(table_used, children[j]))
                        }
                    }
                }
            }
        }
    }

    # Creating the plot.
    plot(curgraph,
        edge.arrow.size = .8,
        vertex.label.cex = 1.3,
        vertex.label.family = "Helvetica",
        vertex.label.color = "black",
        vertex.label.dist = 2.5,
        vertex.color = panel_colors[V(curgraph)$plottype])
}


#' Report current memory state
#'
#' Report the R commands necessary to reproduce the memory state of the interface,
#' through parameters passed via the \code{*Args} arguments in \code{\link{iSEE}}.
#'
#' @param active_panels A data.frame containing information about the currently active panels.
#' @param memory A list of DataFrames, where each DataFrame corresponds to a panel type and contains the current settings for each individual panel of that type.
#'
#' @return A character vector of commands necessary to produce \code{memory} and \code{active_panels}.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_report_memory
#' @seealso
#' \code{\link{iSEE}}
#' @importClassesFrom S4Vectors DataFrame
#' @importFrom methods new
.report_memory <- function(active_panels, memory) {
    # First, reporting all of the individual panel types.
    collected <- list()
    for (mode in names(memory)) {
        current <- memory[[mode]]
        arg_obj <- paste0(mode, "Args")
        Npanels <- nrow(current)
        curcode <- list(strrep("#", 80),
                sprintf("# Settings for %ss", tolower(panelTypes[mode])),
                strrep("#", 80), "",
                sprintf("%s <- new('DataFrame', nrows=%iL, rownames=sprintf('%s%%i', seq_len(%i)))", arg_obj, Npanels, mode, Npanels))

        for (field in colnames(current)) {
            curvals <- current[[field]]
            if (!is.list(curvals)) {
                curcode[[field]] <- sprintf("%s[['%s']] <- %s", arg_obj, field,
                    .deparse_for_viewing(curvals))
            } else {
                list_init <- sprintf("tmp <- vector('list', %i)", Npanels)
                list_others <- vector("list", Npanels)
                for (id in seq_len(Npanels)) {
                    # Need !is.null here, otherwise the list will be shortened
                    # if the last element is NULL
                    if (!is.null(curvals[[id]])) {
                        list_others[[id]] <- sprintf("tmp[[%i]] <- %s", id,
                                                     .deparse_for_viewing(curvals[[id]]))
                    }
                }
                curcode[[field]] <- paste(c("", list_init, unlist(list_others),
                    sprintf("%s[['%s']] <- tmp", arg_obj, field)), collapse="\n")
            }
        }

        collected[[mode]] <- c(curcode, "")
    }

    # Secondly reporting on the active panels.
    initials <- list(strrep("#", 80),
        "# Initial panel settings",
        strrep("#", 80), "",
        sprintf("initialPanels <- DataFrame(
    Name=%s,
    Width=%s,
    Height=%s
)",
            .deparse_for_viewing(.decode_panel_name(active_panels$Type, active_panels$ID), indent=2),
            .deparse_for_viewing(active_panels$Width, indent=2),
            .deparse_for_viewing(active_panels$Height, indent=2)
        )
    )

    return(unlist(c(collected, "", initials)))
}
