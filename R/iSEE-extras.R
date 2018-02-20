#' Update list element
#'
#' Update an entry of a list field in a DataFrame with a new value.
#'
#' @param df A DataFrame, usually containing parameters for every panel of a given type.
#' @param i An integer row index specifying a panel of interest.
#' @param field String specifying the field of \code{df} that is a list.
#' @param value Value to replace \code{df[,field][[i]]}.
#'
#' @return A modified \code{df} where the relevant list entry has been replaced with \code{value}
#'
#' @details
#' It seems that DataFrame objects don't like direct assignment of list fields.
#' This convenience function just extracts the list, assigns into the list and then assigns the list back into \code{df}.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_update_list_element
#' @seealso
#' \code{\link{iSEE}}
.update_list_element <- function(df, i, field, value) {
    out <- df[[field]]
    out[i] <- list(value)
    df[[field]] <- out
    return(df)
}

#' Check panel feasibility
#'
#' Check which panel types are feasible and which ones cannot be generated with the information available in a SingleCellExperiment object.
#'
#' @param se A SingleCellExperiment object.
#'
#' @return A list of named logical scalars, where each entry corresponds to an encoded panel type (e.g., \code{"redDimPlot"})
#' and specifies whether that panel type can be generated.
#'
#' @details
#' While all panel types \emph{could} be generated, it doesn't make sense to do so in some cases:
#' \itemize{
#' \item Reduced dimension plots will not be generated if there are no results in \code{reducedDims(se)}, or no samples.
#' \item Column data plots will not be generated if there are no column metadata in \code{colData(se)}, or no samples.
#' \item Feature expression plots will not be generated if there are no samples, no features or no assays.
#' \item Row statistics tables will not be generated if there are no features.
#' \item Row data plots will not be generated if there are no row metdata in \code{rowData(se)} or no features.
#' \item Heatmaps will not be generated if there are no samples, no features or no assays.
#' }
#'
#' If a panel type is infeasible, users will not be able to initialize or add any panels of that type.
#' This is probably the safest course of action by avoiding the need to write error handling code in the panel functions themselves.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_check_plot_feasibility
#' @seealso
#' \code{\link{.panel_generation}},
#' \code{\link{iSEE}}
.check_plot_feasibility <- function(se) {
    return(list(redDimPlot=! (length(reducedDims(se))==0L || ncol(se)==0L),
                colDataPlot=! (ncol(colData(se))==0L || ncol(se)==0L),
                featExprPlot=! (nrow(se)==0L || ncol(se)==0L || length(assayNames(se))==0L),
                rowStatTable=! (nrow(se)==0L),
                rowDataPlot=! (ncol(rowData(se))==0L || nrow(se)==0L),
                heatMapPlot=! (nrow(se)==0L || ncol(se)==0L || length(assayNames(se))==0L)
    ))
}

#' Increment a counter
#'
#' Increments the counter for a reactive value to trigger downstream updates.
#'
#' @param counter An integer scalar, usually an isolated reactive variable.
#' @param max An integer scalar specifying the upper bound for the increment.
#'
#' @return Integer scalar of value equal to \code{counter+1L} if this is not greater than \code{max}; zero otherwise.
#'
#' @details
#' This function is primarily designed to increment reactive values to trigger downstream observers, conductors or UI endpoints.
#' The use of \code{max} avoids an integer overflow in (very!) long-running apps.
#'
#' Technically we could have flipped a logical flag instead.
#' The initial worry was that if one observer flips the flag and another observer flips it back, there wouldn't be any net change to trigger downstream events.
#' This is probably not the case, as Shiny links get invalidated upon any change to a reactive value, but nonetheless, here we are.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_increment_counter
#' @seealso
#' \code{\link{iSEE}}
.increment_counter <- function(counter, max=10000L) {
    counter <- counter + 1L
    if (counter >= max) {
        counter <- 0L
    }
    return(counter)
}

#' Set up session memory
#'
#' Set up the parameter memory for every individual panel of every panel type, for a particular session of the app.
#'
#' @param se A SingleCellExperiment object.
#' @param redDimArgs A DataFrame or data.frame of user-specified arguments for reduced dimension plots.
#' @param colDataArgs A DataFrame or data.frame of user-specified arguments for column data plots.
#' @param featExprArgs A DataFrame or data.frame of user-specified arguments for feature expression plots.
#' @param rowStatArgs A DataFrame or data.frame of user-specified arguments for row statistics tables.
#' @param rowDataArgs A DataFrame or data.frame of user-specified arguments for row data plots.
#' @param heatMapArgs A DataFrame or data.frame of user-specified arguments for heat maps.
#' @param redDimMax Integer scalar specifying the maximum number of reduced dimension plots.
#' @param colDataMax Integer scalar specifying the maximum number of column data plots.
#' @param featExprMax Integer scalar specifying the maximum number of feature expression plots.
#' @param rowStatMax Integer scalar specifying the maximum number of row statistics tables.
#' @param rowDataMax Integer scalar specifying the maximum number of row data plots.
#' @param heatMapMax Integer scalar specifying the maximum number of heat maps.
#'
#' @return
#' A list of DataFrames, where each DataFrame corresponds to a panel type and contains the initial settings for each individual panel of that type.
#' Each row of the DataFrame is also named according to the encoded name of the panel it represents (e.g., \code{"redDimPlot2"}).
#'
#' @details
#' The maximum number of panels for each panel type is defined as the maximum of \code{*Max} and \code{nrow(*Args)} for that type.
#' This is unless the panel type is not feasible (see \code{\link{.check_plot_feasibility}}) in which case the maximum number is set to zero.
#'
#' This function will generate a default set of parameters for each panel type with the appropriate \code{*Defaults} function.
#' It will then call \code{\link{.override_defaults}} to replace the defaults with user-specified values in \code{*Args}.
#' Not all arguments need to be specified of a given panel type; similarly, arguments do not need to be specified for all panels of a given type
#'
#' Some parameters can be specified as strings for convenience, while internally being represented as integer indices.
#' Any such string values are converted to integers using the \code{\link{.name2index}} function, for consistency in downstream processing.
#'
#' Each of the \code{*Args} can also be \code{NULL}, in which case they are ignored and only the defaults are used.
#' However, each of the \code{*Max} values must be specified.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_setup_memory
#' @seealso
#' \code{\link{iSEE}},
#' \code{\link{.name2index}},
#' \code{\link{.check_plot_feasibility}}
.setup_memory <- function(se, redDimArgs, colDataArgs, featExprArgs, rowStatArgs, rowDataArgs, heatMapArgs,
                          redDimMax, colDataMax, featExprMax, rowStatMax, rowDataMax, heatMapMax) {
    all_args <- list(redDimPlot=redDimArgs, colDataPlot=colDataArgs, featExprPlot=featExprArgs,
                     rowStatTable=rowStatArgs, rowDataPlot=rowDataArgs, heatMapPlot=heatMapArgs)
    all_maxes <- list(redDimPlot=redDimMax, colDataPlot=colDataMax, featExprPlot=featExprMax,
                      rowStatTable=rowStatMax, rowDataPlot=rowDataMax, heatMapPlot=heatMapMax)
    feasibility <- .check_plot_feasibility(se)

    for (x in names(all_args)) {
        if (!feasibility[[x]]) {
            all_args[x] <- list(NULL)
            all_maxes[[x]] <- 0L
        } else {
            all_maxes[[x]] <- max(all_maxes[[x]], nrow(all_args[[x]]))
        }
    }

    # Coercing string arguments that should be integers.
    all_args$redDimPlot <- .name2index(all_args$redDimPlot, .redDimType, reducedDimNames(se))

    all_args$featExprPlot <- .name2index(all_args$featExprPlot, c(.featExprXAxisFeatName, .featExprYAxisFeatName), rownames(se))
    all_args$featExprPlot <- .name2index(all_args$featExprPlot, .featExprAssay, assayNames(se))

    all_args$rowStatTable <- .name2index(all_args$rowStatTable, .rowStatSelected, rownames(se))

    all_args$heatMapPlot <- .name2index(all_args$heatMapPlot, .heatMapFeatName, rownames(se))
    all_args$heatMapPlot <- .name2index(all_args$heatMapPlot, .heatMapAssay, assayNames(se))

    for (mode in c("redDimPlot", "featExprPlot", "colDataPlot")) {
        all_args[[mode]] <- .name2index(all_args[[mode]], .colorByFeatName, rownames(se))
        all_args[[mode]] <- .name2index(all_args[[mode]], c(.colorByFeatNameAssay, .colorByRowTableAssay), assayNames(se))
    }
    all_args$rowDataPlot <- .name2index(all_args$rowDataPlot, .colorByFeatName, rownames(se))

    # Setting up parameters for each panel.
    memory <- list()
    for (mode in names(all_maxes)) {
        DEFFUN <- switch(mode,
                         redDimPlot=redDimPlotDefaults,
                         featExprPlot=featExprPlotDefaults,
                         colDataPlot=colDataPlotDefaults,
                         rowDataPlot=rowDataPlotDefaults,
                         rowStatTable=rowStatTableDefaults,
                         heatMapPlot=heatMapPlotDefaults)

        cur_max <- all_maxes[[mode]]
        cur_args <- all_args[[mode]]
        tmp <- DEFFUN(se, cur_max)
        if (!is.null(cur_args)) {
            tmp <- .override_defaults(tmp, cur_args)
        }
        rownames(tmp) <- sprintf("%s%i", mode, seq_len(cur_max))
        memory[[mode]] <- tmp
    }

    return(memory)
}

#' Convert string to index
#'
#' Convert the name of something (e.g., row, assay or reduced dimension result) to the corresponding integer index.
#'
#' @param df A DataFrame containing parameter values for a particular panel type.
#' @param fields A character vector specifying fields of \code{df} that may contain strings to be converted to integers.
#' @param choices A character vector containing the ordered strings, to use to convert values to integer indices.
#'
#' @return A DataFrame where all strings in the specified \code{fields} are converted to integer indices.
#'
#' @details
#' If the field contains strings, the function will \code{match} them to the choices in \code{choices} to identify their indices.
#' Any missing matches are set to indices of 1.
#' If the field already contains integer values, this function does nothing.
#'
#' The reason for this function is that users may find it easier to specify strings for particular parameter settings, e.g., gene or assay names.
#' However, integer indices are safer to work with and are expected in the internal \pkg{iSEE} functions.
#' Hence the need for this conversion.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_name2index
#' @seealso
#' \code{\link{.setup_memory}}
.name2index <- function(df, fields, choices) {
    for (f in intersect(fields, colnames(df))) {
        vals <- df[,f]
        if (is.character(vals)) {
            m <- match(vals, choices)
            m[is.na(m)] <- 1L
            df[,f] <- m
        } else if (!is.integer(vals)) {
            df[,f] <- as.integer(vals)
        }
    }
    return(df)
}

#' Set up the initial active panels
#'
#' Define which panels are initially active in the app, and their initial heights and widths.
#'
#' @param initialPanels A DataFrame or data.frame specifying the panels to be initially active, see \code{\link{iSEE}} for details.
#' @param memory A list of DataFrames containing parameters for each panel of each type.
#'
#' @return A data.frame with one row per initial panel, containing the fields:
#' \describe{
#' \item{\code{Type}:}{String, an encoded panel type.}
#' \item{\code{ID}:}{Integer, the identifier for the panel of the given type.}
#' \item{\code{Width}:}{Integer, width of the panel in a fluid layout.}
#' \item{code{Height}:}{Integer, height of the panel in pixels.}
#' }
#'
#' @details
#' This extracts the decoded panel name from \code{initialPanels} and converts it to the encoded panel type and identifier.
#' It also adds default width (of 4) and height (of 500) to each panel if these are not specified.
#' If they are specified, they are coerced in to the acceptable range (2-12 for width, 400-1000 for height).
#'
#' The difference between \code{initialPanels} and \code{memory} is that the latter defines the panels that \emph{could} exist in the app,
#' while the former defines the panels that \emph{actually} exist upon initialization of the app.
#' Panels are removed if they are illegal, i.e., do not exist in \code{memory}.
#'
#' If \code{initialPanels=NULL}, it will default to the first panel of each panel type, with default height and width values.
#' Some of these may be illegal and will be removed prior to returning.
#'
#' @author Aaron Lun and others
#' @rdname INTERNAL_setup_initial
#' @seealso
#' \code{\link{iSEE}}
.setup_initial <- function(initialPanels, memory) {
    if (is.null(initialPanels)) {
        initialPanels <- data.frame(Name=c("Reduced dimension plot 1", "Column data plot 1",
                                           "Feature expression plot 1", "Row statistics table 1",
                                           "Row data plot 1", "Heat map 1"),
                                    Width=4, Height=500L, stringsAsFactors=FALSE)
    }

    if (is.null(initialPanels$Name)) {
        stop("need 'Name' field in 'initialPanels'")
    }

    if (is.null(initialPanels$Width)) {
        initialPanels$Width <- 4L
    } else {
        initialPanels$Width <- pmax(width_limits[1], pmin(width_limits[2], as.integer(initialPanels$Width)))
    }

    if (is.null(initialPanels$Height)) {
        initialPanels$Height <- 500L
    } else {
        initialPanels$Height <- pmax(height_limits[1], pmin(height_limits[2], as.integer(initialPanels$Height)))
    }

    encoded <- .encode_panel_name(initialPanels$Name)
    max_each <- unlist(lapply(memory, nrow))
    illegal <- max_each[encoded$Type] < encoded$ID
    if (any(illegal)) {
        badpanel <- which(illegal)[1]
        message(sprintf("'%s' in 'initialPanels' is not available (maximum ID is %i)",
                        initialPanels$Name[badpanel], max_each[encoded$Type[badpanel]]))
    }

    data.frame(Type=encoded$Type, ID=encoded$ID,
               Width=initialPanels$Width,
               Height=initialPanels$Height,
               stringsAsFactors=FALSE)[!illegal,,drop=FALSE]
}

width_limits <- c(2L, 12L)
height_limits <- c(400L, 1000L)

#' Sanitize the memory
#' 
#' Make sure that the memory is sane by ensuring that links only occur between two active panels.
#'
#' @param active_panels A data.frame produced by \code{\link{.setup_initial}}.
#' @param memory A list of DataFrames containing parameters for each panel of each type.
#'
#' @return A modified \code{memory} where links to inactive panels (due to brushing or row tables) are removed.
#'
#' @details
#' This function ensures that the links in memory are valid with respect to the starting panels.
#' Active panels should not receive brushes from inactive panels or have links to inactive row statistics tables.
#' Similarly, inactive panels should not receive brushes from any panels (set to \code{"---"}) or link to any tables (set to an empty string).
#' 
#' This behaviour ensures that the graph in \code{\link{.spawn_brush_chart}} or the links in \code{\link{.spawn_table_links}} are valid.
#' Specifically, there are never any inactive entities in either of these two constructs.
#' This ensures that only active dependent panels are updated throughout the course of the app, avoiding unnecessary work and improving efficiency.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_sanitize_memory
#' @seealso
#' \code{\link{.spawn_brush_chart}},
#' \code{\link{.spawn_table_links}},
#' \code{\link{.setup_initial}}
.sanitize_memory <- function(active_panels, memory) {
    link_sources <- .define_link_sources(active_panels)
    active_tab <- link_sources$tab
    row_brushable <- link_sources$row
    col_brushable <-  link_sources$col
    all_active <- paste0(active_panels$Type, active_panels$ID)

    # Checking for brushing/linking of column-based plots.
    for (mode in c("redDimPlot", "colDataPlot", "featExprPlot")) {
        cur_memory <- memory[[mode]]
        self_active <- rownames(cur_memory)

        bb <- cur_memory[,.brushByPlot]
        bad <- !bb %in% col_brushable | !self_active %in% all_active
        if (any(bad)) {
            memory[[mode]][,.brushByPlot][bad] <- .noSelection
        }

        cb <- cur_memory[,.colorByRowTable]
        bad <- !cb %in% active_tab | !self_active %in% all_active
        if (any(bad)) {
            memory[[mode]][,.colorByRowTable][bad] <- ""
        }
    }

    # Checking for brushing/linking of row data plots.
    cur_memory <- memory$rowDataPlot
    self_active <- rownames(cur_memory)

    bb <- cur_memory[,.brushByPlot]
    bad <- !bb %in% row_brushable | !self_active %in% all_active
    if (any(bad)) {
        memory$rowDataPlot[,.brushByPlot][bad] <- .noSelection
    }

    cb <- cur_memory[,.colorByRowTable]
    bad <- !cb %in% active_tab | !self_active %in% all_active
    if (any(bad)) {
        memory$rowDataPlot[,.colorByRowTable][bad] <- ""
    }

    # Checking for linking of x/y-axes of feature expression plots.
    feat_active <- rownames(memory$featExprPlot)
    for (field in c(.featExprXAxisRowTable, .featExprYAxisRowTable)) {
        bb <- memory$featExprPlot[,field]

        bad <- !bb %in% active_tab | !feat_active %in% all_active
        if (any(bad)) {
            memory$featExprPlot[,field][bad] <- ""
        }
    }
    return(memory)
}

#' Trigger replotting
#'
#' Trigger regeneration of a particular plot, clearing all selections from Shiny brushes or lasso waypoints.
#' 
#' @param mode String specifying the (encoded) panel type of the panel to be replotted.
#' @param i Integer specifying the ID of the panel of the specified type.
#' @param pObjects An environment containing \code{memory}, a list of DataFrames containing parameters for each panel of each type.
#' It should also contain a \code{force_rerender} logical vector.
#' @param rObjects A reactive list containing incrementable counters for all panels,
#' @param input A Shiny list of inputs, generated by the server.
#' @param session A \code{session} object from a Shiny server.
#'
#' @return \code{NULL}, invisibly.
#'
#' @details
#' This function will trigger replotting of the current panel by updating the appropriate incrementable counter in \code{rObjects}.
#' It will also clear Shiny brushes (via \code{\link{session}$resetBrush}) and lasso way points.
#'
#' This function should be used when the entire plot needs to be re-rendered due to changes in the fundamental plot parameters.
#' Setting \code{force_rerender} ensures that complete re-rendering occurs, as the Shiny brush observer only re-renders the brushing box by default.
#'
#' Note that this function relies on the fact that \code{pObjects}, \code{rObjects} and \code{session} are passed by reference.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_regenerate_unselected_plot
#' @seealso 
#' \code{\link{iSEE}}
.regenerate_unselected_plot <- function(mode, i, pObjects, rObjects, input, session) {
    plot_name <- paste0(mode, i)
    brush_id <- paste0(plot_name, "_", .brushField)

    if (!is.null(isolate(input[[brush_id]]))) {
        # This will trigger replotting via the Shiny brush observer.
        # The observer will implicitly wipe the lasso data, so there's no need to do that manually.
        session$resetBrush(brush_id)
        pObjects$force_rerender[plot_name] <- TRUE
    } else {
        # Manually triggering replotting.
        rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))

        # Destroying any lasso waypoints as well.
        pObjects$memory[[mode]] <- .update_list_element(pObjects$memory[[mode]], i, .lassoData, NULL)
    }
    return(invisible(NULL))
}

#' Report plot links
#'
#' Report the links to/from other panels in the interface for the current plotting panel.
#'
#' @param panel String containing the encoded name for the current plotting panel.
#' @param memory A list of DataFrames containing parameters for each panel of each type.
#' @param graph A graph object produced by \code{\link{.spawn_brush_chart}}, specifying the brushing links between panels.
#'
#' @return A HTML object containing a description of the panel from which \code{panel} receives information,
#' and a description of all the other panels to which \code{panel} transmits information. 
#'
#' @details
#' Information reception includes the receipt of a brush from a transmitting plot,
#' or the receipt of a feature selection information from a row statistics table (for color or x/y-axis specification).
#' Information transmission should take the form of brush transmission to other panels.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_define_plot_links
#' @seealso
#' \code{\link{iSEE}}
#'
#' @importFrom shiny em strong br tagList 
#' @importFrom igraph adjacent_vertices
.define_plot_links <- function(panel, memory, graph)
{
    enc <- .split_encoded(panel)
    param_choices <- memory[[enc$Type]][enc$ID,]
    output <- list()

    # Checking brush status.
    brush_in <- param_choices[[.brushByPlot]]
    if (brush_in!=.noSelection) {
        output <- c(output, list("Receiving brush from", em(strong(brush_in)), br()))
    }

    # Checking colour status.
    if (param_choices[[.colorByField]]==.colorByRowTableTitle) {
        output <- c(output, list("Receiving color from", em(strong(param_choices[[.colorByRowTable]])), br()))
    }

    # Checking input/output for feature expression plots.
    if (enc$Type=="featExprPlot") {
        if (param_choices[[.featExprYAxis]]==.featExprYAxisRowTableTitle) {
            output <- c(output, list("Receiving y-axis from", em(strong(param_choices[[.featExprYAxisRowTable]])), br()))
        }
        if (param_choices[[.featExprXAxis]]==.featExprXAxisRowTableTitle) {
            output <- c(output, list("Receiving x-axis from", em(strong(param_choices[[.featExprXAxisRowTable]])), br()))
        }
    }

    # Defining immediate children.
    children <- names(adjacent_vertices(graph, panel)[[1]])
    child_enc <- .split_encoded(children)
    child_names <- .decode_panel_name(child_enc$Type, child_enc$ID)
    for (child in child_names) {
        output <- c(output, list("Transmitting brush to", em(strong(child)), br()))
    }

    do.call(tagList, output)
}

#' Report table links
#'
#' Report the links to/from other panels in the interface for the current row statistics table. 
#'
#' @param panel String containing the encoded name for the current row statistics table.
#' @param memory A list of DataFrames containing parameters for each panel of each type.
#' @param table_links A list of lists produced by \code{\link{.spawn_table_links}}, specifying the brushing links between tables and dependent plots. 
#'
#' @return A HTML object containing a description of the panel from which \code{panel} receives information,
#' and a description of all the other panels to which \code{panel} transmits information. 
#'
#' @details
#' Information reception includes the receipt of a brush from a transmitting plot.
#' Information transmission should take the form of selection of features for use in color or x/y-axis specification in other plots.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_define_plot_links
#' @seealso
#' \code{\link{iSEE}}
#'
#' @importFrom shiny em strong br tagList 
#' @importFrom igraph adjacent_vertices
.define_table_links <- function(panel, memory, table_links)
# This creates a description of all of the incoming/outgoing
# relationships between a table panel and the other plots/tables.
{
    enc <- .split_encoded(panel)
    param_choices <- memory[[enc$Type]][enc$ID,]
    output <- list()

    # Checking brush status.
    brush_in <- param_choices[[.brushByPlot]]
    if (brush_in!=.noSelection) {
        output <- c(output, list("Receiving brush from", em(strong(brush_in)), br()))
    }

    # Checking where it broadcasts to plots.
    current <- table_links[[panel]]
    for (trans in list(c("yaxis", "y-axis"),
                       c("xaxis", "x-axis"),
                       c("color", "color"))) {

        children <- current[[trans[1]]]
        child_enc <- .split_encoded(children)
        child_names <- .decode_panel_name(child_enc$Type, child_enc$ID)

        for (child in child_names) {
            output <- c(output, list(paste("Transmitting", trans[2], "to"), em(strong(child)), br()))
        }
    }

    do.call(tagList, output)
}

#' @importFrom shiny tagList HTML a br
iSEE_info <- tagList(
    HTML(sprintf("iSEE is a project developed by
Aaron Lun (%s),
Charlotte Soneson (%s),
Kevin Rue-Albrecht (%s),
and Federico Marini (%s).",
    a(href="http://www.cruk.cam.ac.uk/", "CRUK Cambridge Institute, University of Cambridge"),
    a(href="https://www.sib.swiss/", "University of Zurich and SIB Swiss Institute of Bioinformatics"),
    a(href="https://www.kennedy.ox.ac.uk", "Kennedy Institute of Rheumatology, University of Oxford"),
    a(href="http://www.unimedizin-mainz.de/imbei","Institute for Medical Biostatistics, Epidemiology and Informatics"))),
    br(), br(),
    HTML(sprintf("The iSEE package is being developed on %s under the %s license.",
    a(href="https://github.com/csoneson/iSEE", "GitHub"),
    a(href="https://opensource.org/licenses/MIT","MIT")))
)
