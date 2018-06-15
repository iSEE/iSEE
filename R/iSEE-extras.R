#' Update list element
#'
#' Update an entry of a list field in a DataFrame with a new value.
#'
#' @param df A DataFrame, usually containing parameters for every panel of a given type.
#' @param id An integer row index specifying the current panel of interest.
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
.update_list_element <- function(df, id, field, value) {
    out <- df[[field]]
    out[id] <- list(value)
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
#' \item Feature assay plots will not be generated if there are no samples, no features or no assays.
#' \item Row statistics tables will not be generated if there are no features.
#' \item Row data plots will not be generated if there are no row metdata in \code{rowData(se)} or no features.
#' \item Custom column data plots will not be generated if there are no samples or if no custom functions are available in the internal metadata of \code{se}.
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
                featAssayPlot=! (nrow(se)==0L || ncol(se)==0L || length(assayNames(se))==0L),
                rowStatTable=! (nrow(se)==0L),
                rowDataPlot=! (ncol(rowData(se))==0L || nrow(se)==0L),
                customColPlot=! (ncol(se)==0L || length(.get_internal_info(se, "custom_col_fun"))==0L),
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
#' @param featAssayArgs A DataFrame or data.frame of user-specified arguments for feature assay plots.
#' @param rowStatArgs A DataFrame or data.frame of user-specified arguments for row statistics tables.
#' @param rowDataArgs A DataFrame or data.frame of user-specified arguments for row data plots.
#' @param customColArgs A DataFrame or data.frame of user-specified arguments for custom column plots.
#' @param heatMapArgs A DataFrame or data.frame of user-specified arguments for heat maps.
#' @param redDimMax Integer scalar specifying the maximum number of reduced dimension plots.
#' @param colDataMax Integer scalar specifying the maximum number of column data plots.
#' @param featAssayMax Integer scalar specifying the maximum number of feature assay plots.
#' @param rowStatMax Integer scalar specifying the maximum number of row statistics tables.
#' @param rowDataMax Integer scalar specifying the maximum number of row data plots.
#' @param customColMax Integer scalar specifying the maximum number of custom column plots.
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
#' Each of the \code{*Args} can also be \code{NULL}, in which case they are ignored and only the defaults are used.
#' However, each of the \code{*Max} values must be specified.
#'
#' In each DataFrame, some parameters can be specified as strings for convenience, while internally being represented as integer indices.
#' Any such string values are converted to integers using the \code{\link{.name2index}} function, for consistency in downstream processing.
#' See \code{?"\link{defaults}"} for more details.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_setup_memory
#' @seealso
#' \code{\link{iSEE}},
#' \code{\link{.name2index}},
#' \code{\link{.check_plot_feasibility}}
.setup_memory <- function(se,
        redDimArgs, 
        colDataArgs, 
        featAssayArgs,
        rowStatArgs, 
        rowDataArgs, 
        customColArgs, 
        heatMapArgs, 
        redDimMax, 
        colDataMax, 
        featAssayMax, 
        rowStatMax, 
        rowDataMax, 
        customColMax,
        heatMapMax) {
    
    all_args <- list(redDimPlot=redDimArgs, 
                     colDataPlot=colDataArgs, 
                     featAssayPlot=featAssayArgs,
                     rowStatTable=rowStatArgs, 
                     rowDataPlot=rowDataArgs, 
                     customColPlot=customColArgs,
                     heatMapPlot=heatMapArgs)
    
    all_maxes <- list(redDimPlot=redDimMax, 
                      colDataPlot=colDataMax, 
                      featAssayPlot=featAssayMax,
                      rowStatTable=rowStatMax, 
                      rowDataPlot=rowDataMax, 
                      customColPlot=customColMax,
                      heatMapPlot=heatMapMax)

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

    all_args$featAssayPlot <- .name2index(all_args$featAssayPlot, c(.featAssayXAxisFeatName, .featAssayYAxisFeatName), rownames(se))
    all_args$featAssayPlot <- .name2index(all_args$featAssayPlot, .featAssayAssay, assayNames(se))

    all_args$rowStatTable <- .name2index(all_args$rowStatTable, .rowStatSelected, rownames(se))

    all_args$heatMapPlot <- .name2index(all_args$heatMapPlot, .heatMapFeatName, rownames(se))
    all_args$heatMapPlot <- .name2index(all_args$heatMapPlot, .heatMapAssay, assayNames(se))

    for (mode in c("redDimPlot", "featAssayPlot", "colDataPlot")) {
        all_args[[mode]] <- .name2index(all_args[[mode]], .colorByFeatName, rownames(se))
        all_args[[mode]] <- .name2index(all_args[[mode]], .colorByFeatNameAssay, assayNames(se))
    }
    all_args$rowDataPlot <- .name2index(all_args$rowDataPlot, .colorByFeatName, rownames(se))

    # Setting up parameters for each panel.
    memory <- list()
    for (mode in names(all_maxes)) {
        DEFFUN <- switch(mode,
                         redDimPlot=redDimPlotDefaults,
                         featAssayPlot=featAssayPlotDefaults,
                         colDataPlot=colDataPlotDefaults,
                         rowStatTable=rowStatTableDefaults,
                         rowDataPlot=rowDataPlotDefaults,
                         customColPlot=customColPlotDefaults,
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
#' This function was developed because users may find it easier to specify strings for particular parameter settings, e.g., gene or assay names.
#' However, integer indices are safer to work with (as duplicates cannot occur) and are expected in the internal \pkg{iSEE} functions.
#' Hence the need for this conversion.
#' 
#' If the field contains strings, the function will \code{match} them to the choices in \code{choices} to identify their indices.
#' Any missing matches are set to indices of 1.
#' If the field contains some other atomic value, the function will try to coerce it into an integer, setting 1 for any failures.
#' If the field contains a list, each element of the list will be coerced to an integer vector.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_name2index
#' @seealso
#' \code{\link{.setup_memory}}
#' @importFrom methods is
#' @importClassesFrom S4Vectors SimpleList
.name2index <- function(df, fields, choices) {
    # Defining an internal function to do the heavy lifting.
    internal_fun <- function(vals) {
        if (is.character(vals)) {
            vals <- match(vals, choices)
        } else {
            vals <- as.integer(vals)
        }
        vals[is.na(vals)] <- 1L
        return(vals)
    }

    # Looping over and handling lists with special behaviour.
    for (f in intersect(fields, colnames(df))) {
        vals <- df[,f]
        if (is.list(vals) || is(vals, "SimpleList")) {
            vals <- lapply(vals, internal_fun)
        } else {
            vals <- internal_fun(vals)
        }
        df[[f]] <- vals # need double brackets otherwise list assigment doesn't work.
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
        initialPanels <- data.frame(Name=paste(translation, 1),
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
#' @return A modified \code{memory} where links to inactive panels (due to point selection or feature selection via row tables) are removed.
#'
#' @details
#' This function ensures that the links in memory are valid with respect to the starting panels.
#' Active panels should not receive point selection information from inactive panels or have links to inactive row statistics tables.
#' Similarly, inactive panels should not receive point selection information from \emph{any} panels.
#' In both cases, transmitters in memory are set to \code{"---"}.
#' 
#' This behaviour ensures that the graph in \code{\link{.spawn_selection_chart}} or the links in \code{\link{.spawn_table_links}} are valid.
#' Specifically, there are never any inactive entities in either of these two constructs.
#' This ensures that only active dependent panels are updated throughout the course of the app, avoiding unnecessary work and improving efficiency.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_sanitize_memory
#' @seealso
#' \code{\link{.spawn_selection_chart}},
#' \code{\link{.spawn_table_links}},
#' \code{\link{.setup_initial}}
.sanitize_memory <- function(active_panels, memory) {
    link_sources <- .define_link_sources(active_panels)
    active_tab <- link_sources$tab
    row_selectable <- link_sources$row
    col_selectable <-  link_sources$col
    all_active <- paste0(active_panels$Type, active_panels$ID)

    # Checking for selecting/linking of column-based plots.
    for (mode in c("redDimPlot", "colDataPlot", "featAssayPlot", "customColPlot")) {
        cur_memory <- memory[[mode]]
        self_active <- rownames(cur_memory)

        bb <- cur_memory[,.selectByPlot]
        bad <- !bb %in% col_selectable | !self_active %in% all_active
        if (any(bad)) {
            memory[[mode]][,.selectByPlot][bad] <- .noSelection
        }

        cb <- cur_memory[,.colorByRowTable]
        bad <- !cb %in% active_tab | !self_active %in% all_active
        if (any(bad)) {
            memory[[mode]][,.colorByRowTable][bad] <- .noSelection
        }
    }

    # Checking for selecting/linking of row data plots.
    cur_memory <- memory$rowDataPlot
    self_active <- rownames(cur_memory)

    bb <- cur_memory[,.selectByPlot]
    bad <- !bb %in% row_selectable | !self_active %in% all_active
    if (any(bad)) {
        memory$rowDataPlot[,.selectByPlot][bad] <- .noSelection
    }

    cb <- cur_memory[,.colorByRowTable]
    bad <- !cb %in% active_tab | !self_active %in% all_active
    if (any(bad)) {
        memory$rowDataPlot[,.colorByRowTable][bad] <- .noSelection
    }

    # Checking for linking of x/y-axes of feature assay plots.
    feat_active <- rownames(memory$featAssayPlot)
    for (field in c(.featAssayXAxisRowTable, .featAssayYAxisRowTable)) {
        bb <- memory$featAssayPlot[,field]

        bad <- !bb %in% active_tab | !feat_active %in% all_active
        if (any(bad)) {
            memory$featAssayPlot[,field][bad] <- .noSelection
        }
    }
    return(memory)
}

#' Trigger replotting
#'
#' Trigger regeneration of a particular plot, clearing all selections from Shiny brushes or lasso waypoints.
#' 
#' @param mode String specifying the (encoded) panel type of the current panel to be replotted.
#' @param id Integer scalar specifying the ID of the current panel of the specified type.
#' @param pObjects An environment containing \code{memory}, a list of DataFrames containing parameters for each panel of each type.
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
#' Note that this function relies on the fact that \code{pObjects}, \code{rObjects} and \code{session} are passed by reference.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_regenerate_unselected_plot
#' @seealso 
#' \code{\link{iSEE}}
.regenerate_unselected_plot <- function(mode, id, pObjects, rObjects, input, session) {
    plot_name <- paste0(mode, id)
    brush_id <- paste0(plot_name, "_", .brushField)

    if (!is.null(isolate(input[[brush_id]]))) {
        # This will trigger replotting via the Shiny brush observer.
        # The observer will implicitly wipe the lasso data, so there's no need to do that manually.
        session$resetBrush(brush_id)
    } else {
        # Manually triggering replotting.
        rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))

        # Destroying any lasso waypoints as well.
        pObjects$memory[[mode]] <- .update_list_element(pObjects$memory[[mode]], id, .lassoData, NULL)
    }
    return(invisible(NULL))
}

#' Report plot links
#'
#' Report the links to/from other panels in the interface for the current plotting panel.
#'
#' @param panel String containing the encoded name for the current plotting panel.
#' @param memory A list of DataFrames containing parameters for each panel of each type.
#' @param graph A graph object produced by \code{\link{.spawn_selection_chart}}, specifying the point selection links between panels.
#'
#' @return A HTML object containing a description of the panel from which \code{panel} receives information,
#' and a description of all the other panels to which \code{panel} transmits information. 
#'
#' @details
#' Information reception includes the receipt of point selection information from a transmitting plot,
#' or the receipt of a feature selection information from a row statistics table (for color or x/y-axis specification).
#' Transmission should involve transferring point selection information from \code{panel} to other receiving panels.
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

    # Checking select status.
    select_in <- param_choices[[.selectByPlot]]
    if (select_in!=.noSelection) {
        output <- c(output, list("Receiving selection from", em(strong(select_in)), br()))
    }

    # Checking colour status.
    if (param_choices[[.colorByField]]==.colorByFeatNameTitle
        && param_choices[[.colorByRowTable]]!=.noSelection) {
        output <- c(output, list("Receiving color from", em(strong(param_choices[[.colorByRowTable]])), br()))
    }

    # Checking input/output for feature assay plots.
    if (enc$Type=="featAssayPlot") {
        if (param_choices[[.featAssayYAxisRowTable]]!=.noSelection) {
            output <- c(output, list("Receiving y-axis from", em(strong(param_choices[[.featAssayYAxisRowTable]])), br()))
        }
        if (param_choices[[.featAssayXAxis]]==.featAssayXAxisFeatNameTitle
            && param_choices[[.featAssayXAxisRowTable]]!=.noSelection) {
            output <- c(output, list("Receiving x-axis from", em(strong(param_choices[[.featAssayXAxisRowTable]])), br()))
        }
    }

    # Defining immediate children.
    children <- names(adjacent_vertices(graph, panel)[[1]])
    child_enc <- .split_encoded(children)
    child_names <- .decode_panel_name(child_enc$Type, child_enc$ID)
    for (child in child_names) {
        output <- c(output, list("Transmitting selection to", em(strong(child)), br()))
    }

    do.call(tagList, output)
}

#' Report table links
#'
#' Report the links to/from other panels in the interface for the current row statistics table. 
#'
#' @param panel String containing the encoded name for the current row statistics table.
#' @param memory A list of DataFrames containing parameters for each panel of each type.
#' @param table_links A list of lists produced by \code{\link{.spawn_table_links}}, specifying the links between tables and dependent plots. 
#'
#' @return A HTML object containing a description of the panel from which \code{panel} receives information,
#' and a description of all the other panels to which \code{panel} transmits information. 
#'
#' @details
#' Information transmission from a row statistics table involves selection of features for use in color or x/y-axis specification in other plots.
#' Information reception is less common and involves the receipt of point selections from a transmitting plot.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_define_table_links
#' @seealso
#' \code{\link{iSEE}}
#'
#' @importFrom shiny em strong br tagList 
#' @importFrom igraph adjacent_vertices
.define_table_links <- function(panel, memory, table_links) {
    enc <- .split_encoded(panel)
    param_choices <- memory[[enc$Type]][enc$ID,]
    output <- list()

    # Checking select status.
    select_in <- param_choices[[.selectByPlot]]
    if (select_in!=.noSelection) {
        output <- c(output, list("Receiving selection from", em(strong(select_in)), br()))
    }

    # Checking where it broadcasts to plots.
    current <- table_links[[panel]]
    for (trans in list(c("yaxis", "y-axis", NA, NA),
                       c("xaxis", "x-axis", .featAssayXAxis, .featAssayXAxisFeatNameTitle),
                       c("color", "color", .colorByField, .colorByFeatNameTitle))
        ) {

        children <- current[[trans[1]]]
        child_enc <- .split_encoded(children)
        child_names <- .decode_panel_name(child_enc$Type, child_enc$ID)

        out_str <- paste("Transmitting", trans[2], "to")
        by_field <- trans[3]
        ref_title <- trans[4]

        # Only writing a broadcast label if the plot actually receives the information via the
        # appropriate parameter choices. Y-axis for feature plots is NA, as there are no choices there.
        for (i in seq_along(child_names)) {
            if (is.na(by_field) || memory[[child_enc$Type[i]]][child_enc$ID[i], by_field]==ref_title) {
                output <- c(output, list(out_str, em(strong(child_names[i])), br()))
            }
        }
    }

    do.call(tagList, output)
}

#' Establish the evaluation order
#' 
#' Establish the order in which connected panels are to be evaluated during app initialization.
#' 
#' @param graph A graph object containing links between panels, produced by \code{\link{.spawn_selection_chart}}.
#' 
#' @details
#' This function identifies any initial connections between panels (e.g., specified in the panel arguments) for point selection.
#' It then orders the connected panels such that any transmitters are placed in front of their receivers.
#' 
#' The idea is to \dQuote{evaluate} the plots at the start of the app, to obtain the coordinates for transmitting to other panels.
#' Otherwise, errors will be encountered whereby a panel tries to select from a set of coordinates that do not yet exist.
#'
#' Unlike its relative \code{\link{.get_reporting_order}}, only transmitting panels are ever reported by this function.
#' It is not necessary to evaluate receiving-only panels, and in fact will result in errors for heatmaps and row statistics tables,
#' as these do not even have coordinates to save.
#' 
#' @return A character vector containing encoded names for transmitting panels in their evaluation order.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_establish_eval_order
#' @seealso
#' \code{\link{iSEE}},
#' \code{\link{.spawn_selection_chart}}
#' @importFrom igraph delete.vertices V topo_sort degree
.establish_eval_order <- function(graph) {
    iso <- V(graph)[degree(graph, mode="out")==0]
    graph <- delete.vertices(graph, iso)
    names(topo_sort(graph, mode="out"))
}

#' Define the selected points
#' 
#' Evaluate the point selection commands to obtain the selected set of points, usually features.
#'
#' @param names A character vector containing the names of all points.
#' @param transmitter String containing the decoded name of the transmitting panel.
#' @param all_memory A list of DataFrames containing parameters for each panel of each type.
#' @param all_coordinates A list of data.frames that contain the coordinates and covariates of data points visible in each of the plots.
#'
#' @return 
#' A logical vector of length equal to \code{names}, specifying which points were selected in the \code{transmitter}.
#'
#' @details
#' This function obtains the commands to select points from \code{\link{.process_selectby_choice}}, and evaluates them to identify the selected points.
#' Such a procedure is necessary in \code{\link{iSEE}} to obtain the actual feature names to show to the user in the interface.
#' 
#' Some work is required to trick \code{\link{.process_selectby_choice}} into thinking it is operating on the parameters for a point-based receiving panel.
#' We also set \code{self_source=FALSE} to ensure that the function uses the coordinates in \code{all_coordinates}, and does not try to self-brush from \code{plot.data}
#' (which would be meaningless here, given the lack of coordinates).
#'
#' @author Aaron Lun
#' @rdname INTERNAL_get_selected_points
#' @seealso 
#' \code{\link{.process_selectby_choice}},
#' \code{\link{iSEE}}
#'
#' @importFrom S4Vectors DataFrame 
.get_selected_points <- function(names, transmitter, all_memory, all_coordinates) {
    dummy <- DataFrame(transmitter, .selectColorTitle) 
    colnames(dummy) <- c(.selectByPlot, .selectEffect)
    selected <- .process_selectby_choice(dummy, all_memory, self_source=FALSE)

    if (!is.null(selected$cmd)) { 
        chosen.env <- new.env()
        chosen.env$plot.data <- data.frame(row.names=names)
        chosen.env$all_coordinates <- all_coordinates
        chosen.env$all_brushes <- selected$data
        chosen.env$all_lassos <- selected$data
        .text_eval(selected$cmd, envir=chosen.env)
        return(chosen.env$plot.data$SelectBy)
    } 
    return(NULL)
}

#' Sanitize a SummarizedExperiment 
#'
#' Coerce inputs to SummarizedExperiment, flatten nested DataFrames, add row and column names, and remove other non-atomic fields.
#' Also sanitize a SingleCellExperiment by moving internal fields into the column- or row-level metadata.
#'
#' @param se A SingleCellExperiment or anything that be coerced to a SummarizedExperiment.
#'
#' @return A list containing \code{cmds}, a character vector of commands required to obtain a sanitized SingleCellExperiment;
#' and \code{object}, a sanitized SingleCellExperiment object derived from \code{se}.
#' 
#' @details
#' Nested fields are renamed by using \code{:} as separators in the flattened DataFrame.
#' This is also the case for subtypes of \code{\link{sizeFactors}} or \code{\link{isSpike}}.
#' Name clashes are resolved by adding \code{_} to the start of the newer name.
#'
#' Note that non-atomic fields are removed from \code{object} only, to ensure that they don't show up in the UI options.
#' Removal is not captured in \code{cmds}, as this is not strictly necessary for code reproducibility (names are already unique anyway).
#'
#' Automated renaming requires users to carefully consider their choices when constructing ExperimentColorMap objects for \code{colData} or \code{rowData}.
#' Existing names are always prioritized where possible, so users can guarantee the use of particular colormaps by manually renaming them before \code{\link{iSEE}}.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_sanitize_SE_input
#' @seealso
#' \code{\link{iSEE}},
#' \code{\link{.safe_field_name}},
#' \code{\link{.extract_nested_DF}}
#'
#' @importFrom BiocGenerics sizeFactors
#' @importFrom SingleCellExperiment isSpike
#' @importFrom S4Vectors DataFrame
#' @importFrom methods is as
#' @importFrom SummarizedExperiment colData rowData
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
.sanitize_SE_input <- function(se) {
    done <- commands <- list()
    eval_env <- new.env()
    eval_env$se <- se
    all_cmds <- .initialize_cmd_store()

    # Coercing to a SingleCellExperiment object.
    if (!is(se, "SummarizedExperiment")) {
        all_cmds <- .add_command(all_cmds, 'se <- as(se, "SummarizedExperiment")')
    }
    if (!is(se, "SingleCellExperiment")) { 
        all_cmds <- .add_command(all_cmds, 'se <- as(se, "SingleCellExperiment")')
    }
    all_cmds <- .evaluate_commands(all_cmds, eval_env)

    # Adding row and column names if necessary.
    if (is.null(colnames(eval_env$se))) {
        all_cmds <- .add_command(all_cmds, 'colnames(se) <- sprintf("SAMPLE_%i", seq_len(ncol(se)))')
    }
    if (is.null(rownames(eval_env$se))) {
        all_cmds <- .add_command(all_cmds, 'rownames(se) <- sprintf("FEATURE_%i", seq_len(nrow(se)))')
    }
    all_cmds <- .evaluate_commands(all_cmds, eval_env)

    # Filling in with any sizeFactors.
    if (!is.null(sizeFactors(eval_env$se))) {
        new_name <- .safe_field_name("sizeFactors(se)", colnames(colData(eval_env$se)))
        all_cmds <- .add_command(all_cmds, sprintf('colData(se)[,%s] <- sizeFactors(se)', deparse(new_name)))
    }
    for (sf_name in sizeFactorNames(eval_env$se)) {
        get_cmd <- sprintf("sizeFactors(se, %s)", deparse(sf_name))
        new_name <- .safe_field_name(get_cmd, colnames(colData(eval_env$se)))
        all_cmds <- .add_command(all_cmds, sprintf('colData(se)[,%s] <- %s', deparse(new_name), get_cmd))
    }
    all_cmds <- .evaluate_commands(all_cmds, eval_env)
    
    # Filling in with spike-ins.
    if (!is.null(isSpike(eval_env$se))) {
        new_name <- .safe_field_name("isSpike(se)", colnames(rowData(eval_env$se)))
        all_cmds <- .add_command(all_cmds, sprintf('rowData(se)[,%s] <- isSpike(se)', deparse(new_name)))
    }
    for (s_name in spikeNames(eval_env$se)) {
        get_cmd <- sprintf("isSpike(se, %s)", deparse(s_name))
        new_name <- .safe_field_name(get_cmd, colnames(rowData(eval_env$se)))
        all_cmds <- .add_command(all_cmds, sprintf('rowData(se)[,%s] <- %s', deparse(new_name), get_cmd))
    }
    all_cmds <- .evaluate_commands(all_cmds, eval_env)

    # Decomposing nested DataFrames and discarding non-atomic types.
    new_rows <- .extract_nested_DF(rowData(eval_env$se))
    for (f in seq_along(new_rows$getter)) {
        new_name <- .safe_field_name(new_rows$setter[f], colnames(rowData(eval_env$se)))
        all_cmds <- .add_command(all_cmds, sprintf("rowData(se)[,%s] <- rowData(se)%s", deparse(new_name), new_rows$getter[f]))
    }
    new_cols <- .extract_nested_DF(colData(eval_env$se))
    for (f in seq_along(new_cols$getter)) {
        new_name <- .safe_field_name(new_cols$setter[f], colnames(colData(eval_env$se)))
        all_cmds <- .add_command(all_cmds, sprintf("colData(se)[,%s] <- colData(se)%s", deparse(new_name), new_cols$getter[f]))
    }
    all_cmds <- .evaluate_commands(all_cmds, eval_env)

    # Destroy all non-atomic fields (only internal, no need to hold commands).
    output_se <- eval_env$se
    for (f in colnames(rowData(output_se))) {
        cur_field <- rowData(output_se)[[f]]
        if (!is.numeric(cur_field) && !is.factor(cur_field) && !is.character(cur_field) && !is.logical(cur_field)) {
            rowData(output_se)[[f]] <- NULL
        }
    }
    for (f in colnames(colData(output_se))) {
        cur_field <- colData(output_se)[[f]]
        if (!is.numeric(cur_field) && !is.factor(cur_field) && !is.character(cur_field) && !is.logical(cur_field)) {
            colData(output_se)[[f]] <- NULL
        }
    }
    
    return(list(cmds=all_cmds$processed, object=output_se))
}

#' Make a safe field name
#'
#' Append underscores to the start of a name for a new field, until it does not clash with any existing field names.
#'
#' @param candidate String, specifying the name of the new field.
#' @param existing Character vector containing the names of existing fields.
#' 
#' @return A string with a suffix of \code{candidate} and varying numbers of underscores prefixed to the start.
#' This is guaranteed to not lie in \code{existing}.
#' 
#' @author Aaron Lun
#' @rdname INTERNAL_safe_field_name
#' @seealso
#' \code{\link{iSEE}},
#' \code{\link{.sanitize_SE_input}}
.safe_field_name <- function(candidate, existing) {
    while (candidate %in% existing) {
        candidate <- paste0("_", candidate)
    }
    return(candidate)
}

#' Extract nested DataFrames
#' 
#' Extract information from nested DataFrames, for use in creating a flattened DataFrame.
#' 
#' @param DF A DataFrame, possibly containing nested DataFrames.
#' @param top A logical scalar indicating whether \code{DF} is the top-level DataFrame, required for sensible behaviour during recursion.
#' 
#' @return
#' A list containing \code{getter}, a character vector of commands to be suffixed to \code{colData} or \code{rowData} calls to obtain nested fields;
#' and \code{setter}, a character vector of names for each field to be used in the flattened DataFrame.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_extract_nested_DF
#' @seealso
#' \code{\link{.sanitize_SE_input}}
#'
#' @importClassesFrom S4Vectors DataFrame
#' @importFrom methods is
.extract_nested_DF <- function(DF, top=TRUE) {
    collected <- renamed <- vector("list", ncol(DF))
    cnames <- colnames(DF)

    for (f in seq_along(collected)) {
        fdata <- DF[[f]]
        if (is(fdata, "DataFrame")) {
            nextlevel <- .extract_nested_DF(fdata, top=FALSE)
            collected[[f]] <- sprintf("[[%s]]%s", deparse(cnames[f]), nextlevel$getter)
            renamed[[f]] <- sprintf("%s:%s", cnames[f], nextlevel$setter)
        } else if (!top) { 
            collected[[f]] <- sprintf("[[%s]]", deparse(cnames[f]))
            renamed[[f]] <- cnames[f]
        }
    }
    return(list(getter=unlist(collected), setter=unlist(renamed)))
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
