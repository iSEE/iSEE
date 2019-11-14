############################################
# Aesthetics constants -----
############################################

.all_aes_names <- c("x", "y", "color", "shape", "size", "fill", "group")
.all_aes_values <- c("X", "Y", "ColorBy", "ShapeBy", "SizeBy", "FillBy", "GroupBy")
names(.all_aes_values) <- .all_aes_names

############################################
# Title and labels constants -----
############################################

.all_labs_names <- c(.all_aes_names, "title", "subtitle")

############################################
# Lasso constants -----
############################################

# Default behaviour
.lassoStartShape <- 22
.lassoWaypointShape <- 20

# If shape is being used for data aesthetics, fall back on size
.lassoStartSize <- 1.5
.lassoWaypointSize <- 0.25

############################################
# .make_redDimPlot  ----
############################################

#' Makes the dimension reduction plot
#'
#' Make a plot in reduced dimensions embeddings on X/Y axes.
#'
#' @param id Integer scalar specifying the index of the current reduced
#' dimension plot.
#' @param all_memory list of DataFrames, where each DataFrame corresponds to
#' a panel type and contains the settings for each individual panel of that type.
#' @param all_coordinates A list of data.frames that contain the coordinates
#' and covariates of data points visible in each of the plots.
#' @param se A SingleCellExperiment object.
#' @param colormap An ExperimentColorMap object that defines custom colormaps
#' for individual \code{assays}, \code{colData}, and \code{rowData} covariates.
#'
#' @return A list containing \code{cmd_list}, \code{xy} and \code{plot}; see \code{?\link{.plot_wrapper}} for more details.
#'
#' @details
#' This function extracts out the data from \code{se} required to produce a reduced dimension plot.
#' It then calls \code{\link{.plot_wrapper}} to complete the plotting data (\code{xy} in output) and to generate the ggplot object (\code{plot} in output).
#'
#' @author Kevin Rue-Albrecht, Aaron Lun, Charlotte Soneson
#' @rdname INTERNAL_make_redDimPlot
#' @seealso
#' \code{\link{.extract_plotting_data}},
#' \code{\link{.create_plot}}
#'
#' @importFrom SingleCellExperiment reducedDim
.make_redDimPlot <- function(id, all_memory, all_coordinates, se, colormap) {
    param_choices <- all_memory$redDimPlot[id,]
    data_cmds <- list()
    data_cmds[["reducedDim"]] <- sprintf(
        "red.dim <- reducedDim(se, %i);", param_choices[[.redDimType]])
    data_cmds[["xy"]] <- sprintf(
        "plot.data <- data.frame(X=red.dim[, %i], Y=red.dim[, %i], row.names=colnames(se));",
        param_choices[[.redDimXAxis]], param_choices[[.redDimYAxis]])

    reddim_names <- names(.get_internal_info(se, field="red_dim_names"))
    plot_title <- reddim_names[param_choices[[.redDimType]]]

    x_lab <- sprintf("Dimension %s", param_choices[[.redDimXAxis]])
    y_lab <- sprintf("Dimension %s", param_choices[[.redDimYAxis]])

    .plot_wrapper(data_cmds, param_choices=param_choices, all_memory=all_memory,
        all_coordinates=all_coordinates, se=se, by_row=FALSE,
        colormap=colormap, x_lab=x_lab, y_lab=y_lab, title=plot_title)
}

############################################
# .make_colDataPlot  ----
############################################

#' Makes a plot of column data variables
#'
#' Make a plot of sample metadata on Y axis (and optionally on the X axis).
#'
#' @param id Integer scalar specifying the index of the current column data
#' plot.
#' @param all_memory list of DataFrames, where each DataFrame corresponds to
#' a panel type and contains the settings for each individual panel of that type.
#' @param all_coordinates A list of data.frames that contain the coordinates
#' and covariates of data points visible in each of the plots.
#' @param se A SingleCellExperiment object.
#' @param colormap An ExperimentColorMap object that defines custom colormaps
#' for individual \code{assays}, \code{colData}, and \code{rowData} covariates.
#'
#' @return A list containing \code{cmd_list}, \code{xy} and \code{plot}; see \code{?\link{.plot_wrapper}} for more details.
#'
#' @details
#' This function extracts out the data from \code{se} required to produce a column data plot.
#' It then calls \code{\link{.plot_wrapper}} to complete the plotting data (\code{xy} in output) and to generate the ggplot object (\code{plot} in output).
#'
#' @author Kevin Rue-Albrecht, Aaron Lun, Charlotte Soneson
#' @rdname INTERNAL_make_colDataPlot
#' @seealso
#' \code{\link{.create_plot}},
#' \code{\link{.extract_plotting_data}}
#'
#' @importFrom SummarizedExperiment colData
.make_colDataPlot <- function(id, all_memory, all_coordinates, se, colormap)
{
    param_choices <- all_memory$colDataPlot[id,]
    data_cmds <- list()
    y_lab <- param_choices[[.colDataYAxis]]
    # NOTE: deparse() automatically adds quotes, AND protects against existing quotes/escapes.
    data_cmds[["y"]] <- sprintf(
        "plot.data <- data.frame(Y=colData(se)[, %s], row.names=colnames(se));",
        deparse(y_lab)
    )

    # Prepare X-axis data.
    if (param_choices[[.colDataXAxis]] == .colDataXAxisNothingTitle) {
        x_lab <- ''
        data_cmds[["x"]] <- "plot.data$X <- factor(character(ncol(se)))"
    } else {
        x_lab <- param_choices[[.colDataXAxisColData]]
        data_cmds[["x"]] <- sprintf(
            "plot.data$X <- colData(se)[, %s];",
            deparse(x_lab)
        )
    }

    x_title <- ifelse(x_lab == '', x_lab, sprintf("vs %s", x_lab))
    plot_title <- sprintf("%s %s", y_lab, x_title)

    .plot_wrapper(
        data_cmds, param_choices=param_choices, all_memory=all_memory,
        all_coordinates=all_coordinates, se=se, by_row=FALSE,
        colormap=colormap, x_lab=x_lab, y_lab=y_lab, title=plot_title)
}

############################################
# .make_featAssayPlot  ----
############################################

#' Makes a plot of feature-level assay values
#'
#' Make a plot of feature assay data on Y axis, with column data or other
#' feature assay on the X axis.
#'
#' @param id Integer scalar specifying the index of the current feature
#' expression plot.
#' @param all_memory list of DataFrames, where each DataFrame corresponds
#' to a panel type and contains the settings for each individual panel of that type.
#' @param all_coordinates A list of data.frames that contain the coordinates
#' and covariates of data points visible in each of the plots.
#' @param se A SingleCellExperiment object.
#' @param colormap An ExperimentColorMap object that defines custom colormaps
#' for individual \code{assays}, \code{colData}, and \code{rowData} covariates.
#'
#' @return A list containing \code{cmd_list}, \code{xy} and \code{plot}; see \code{?\link{.plot_wrapper}} for more details.
#'
#' @details
#' This function extracts out the data from \code{se} required to produce a feature assay plot.
#' It then calls \code{\link{.plot_wrapper}} to complete the plotting data (\code{xy} in output) and to generate the ggplot object (\code{plot} in output).
#'
#' @author Kevin Rue-Albrecht, Aaron Lun, Charlotte Soneson
#' @rdname INTERNAL_make_featAssayPlot
#' @seealso
#' \code{\link{.create_plot}},
#' \code{\link{.extract_plotting_data}}
#'
#' @importFrom shiny validate need
#' @importFrom SummarizedExperiment assay colData
.make_featAssayPlot <- function(id, all_memory, all_coordinates, se, colormap) {
    param_choices <- all_memory$featAssayPlot[id,]
    data_cmds <- list()

    ## Setting up the y-axis:
    gene_selected_y <- param_choices[[.featAssayYAxisFeatName]]
    assay_choice <- param_choices[[.featAssayAssay]]
    plot_title <- rownames(se)[gene_selected_y]
    y_lab <- .feature_axis_label(se, gene_selected_y, assay_choice, multiline=FALSE)
    data_cmds[["y"]] <- sprintf(
        "plot.data <- data.frame(Y=assay(se, %i, withDimnames=FALSE)[%i, ], row.names=colnames(se))",
        assay_choice, gene_selected_y
    )

    ## Checking X axis choice:
    x_choice <- param_choices[[.featAssayXAxis]]

    if (x_choice == .featAssayXAxisColDataTitle) { # colData column selected
        x_lab <- param_choices[[.featAssayXAxisColData]]
        plot_title <- paste(plot_title, "vs", x_lab)
        data_cmds[["x"]] <- sprintf("plot.data$X <- colData(se)[, %s];", deparse(x_lab))

    } else if (x_choice == .featAssayXAxisFeatNameTitle) { # gene selected
        gene_selected_x <- param_choices[[.featAssayXAxisFeatName]]
        plot_title <- paste(plot_title, "vs", rownames(se)[gene_selected_x])
        x_lab <- .feature_axis_label(se, gene_selected_x, assay_choice, multiline=FALSE)
        data_cmds[["x"]] <- sprintf(
            "plot.data$X <- assay(se, %i, withDimnames=FALSE)[%i, ];",
            assay_choice, gene_selected_x
        )

    } else { # no x axis variable specified: show single violin
        x_lab <- ''
        data_cmds[["x"]] <- "plot.data$X <- factor(character(ncol(se)))"
    }

    .plot_wrapper(
        data_cmds, param_choices=param_choices, all_memory=all_memory,
        all_coordinates=all_coordinates, se=se, by_row=FALSE,
        colormap=colormap, x_lab=x_lab, y_lab=y_lab, title=plot_title)
}

############################################
# .make_rowDataPlot  ----
############################################

#' Makes a plot of row data variables
#'
#' Make a plot of row metadata variables on the Y axis
#' (optionally on the X axis as well).
#'
#' @param id Integer scalar specifying the index of the current row data plot.
#' @param all_memory list of DataFrames, where each DataFrame corresponds to
#' a panel type and contains the settings for each individual panel of that
#' type.
#' @param all_coordinates A list of data.frames that contain the coordinates
#' and covariates of data points visible in each of the plots.
#' @param se A SingleCellExperiment object.
#' @param colormap An ExperimentColorMap object that defines custom colormaps
#' for individual \code{assays}, \code{colData}, and \code{rowData} covariates.
#'
#' @return A list containing \code{cmd_list}, \code{xy} and \code{plot}; see \code{?\link{.plot_wrapper}} for more details.
#'
#' @details
#' This function extracts the data from \code{se} required to produce a row data plot.
#' It then calls \code{\link{.plot_wrapper}} to complete the plotting data (\code{xy} in output) and to generate the ggplot object (\code{plot} in output).
#'
#' @author Kevin Rue-Albrecht, Aaron Lun, Charlotte Soneson
#' @rdname INTERNAL_make_rowDataPlot
#' @seealso
#' \code{\link{.create_plot}},
#' \code{\link{.extract_plotting_data}}
#'
#' @importFrom SummarizedExperiment rowData
.make_rowDataPlot <- function(id, all_memory, all_coordinates, se, colormap) {
    param_choices <- all_memory$rowDataPlot[id,]
    data_cmds <- list()
    y_lab <- param_choices[[.rowDataYAxis]]

    # NOTE: deparse() automatically adds quotes, AND protects against existing quotes/escapes.
    data_cmds[["y"]] <- sprintf(
        "plot.data <- data.frame(Y=rowData(se)[, %s], row.names=rownames(se));",
        deparse(y_lab)
    )

    # Prepare X-axis data.
    if (param_choices[[.rowDataXAxis]] == .rowDataXAxisNothingTitle) {
        x_lab <- ''
        data_cmds[["x"]] <- "plot.data$X <- factor(character(nrow(se)))"
    } else {
        x_lab <- param_choices[[.rowDataXAxisRowData]]
        data_cmds[["x"]] <- sprintf("plot.data$X <- rowData(se)[, %s];", deparse(x_lab))
    }

    x_title <- ifelse(x_lab == '', x_lab, sprintf("vs %s", x_lab))
    plot_title <- sprintf("%s %s", y_lab, x_title)

    .plot_wrapper(
        data_cmds, param_choices=param_choices, all_memory=all_memory,
        all_coordinates=all_coordinates, se=se, by_row=TRUE,
        colormap=colormap, x_lab=x_lab, y_lab=y_lab, title=plot_title)
}

############################################
# .make_sampleAssayPlot  ----
############################################

#' Makes a plot of sample-level assay values
#'
#' Make a plot of sample assay data on the Y axis (optionally on the X axis as well).
#'
#' @param id Integer scalar specifying the index of the current row data plot.
#' @param all_memory List of DataFrames, where each DataFrame corresponds to a panel type and contains the settings for each individual panel of that type.
#' @param all_coordinates A list of data.frames that contain the coordinates and covariates of data points visible in each of the plots.
#' @param se A SingleCellExperiment object.
#' @param colormap An ExperimentColorMap object that defines custom colormaps for individual \code{assays}, \code{colData}, and \code{rowData} covariates.
#'
#' @return A list containing \code{cmd_list}, \code{xy} and \code{plot}; see \code{?\link{.plot_wrapper}} for more details.
#'
#' @details
#' This function extracts the data from \code{se} required to produce a sample assay plot.
#' It then calls \code{\link{.plot_wrapper}} to complete the plotting data (\code{xy} in output) and to generate the ggplot object (\code{plot} in output).
#'
#' @author Aaron Lun, Charlotte Soneson
#' @rdname INTERNAL_make_sampleAssayPlot
#' @seealso
#' \code{\link{.create_plot}},
#' \code{\link{.extract_plotting_data}}
#'
#' @importFrom SummarizedExperiment rowData
.make_sampAssayPlot <- function(id, all_memory, all_coordinates, se, colormap) {
    param_choices <- all_memory$sampAssayPlot[id,]
    data_cmds <- list()

    samp_selected_y <- param_choices[[.sampAssayYAxisSampName]]
    assay_choice <- param_choices[[.sampAssayAssay]]

    plot_title <- colnames(se)[samp_selected_y]
    y_lab <- .sample_axis_label(se, samp_selected_y, assay_choice, multiline=FALSE)
    data_cmds[["y"]] <- sprintf(
        "plot.data <- data.frame(Y=assay(se, %i, withDimnames=FALSE)[,%i], row.names=rownames(se));",
        assay_choice, samp_selected_y
    )

    # Prepare X-axis data.
    x_choice <- param_choices[[.sampAssayXAxis]]

    if (x_choice == .sampAssayXAxisNothingTitle) {
        x_lab <- ''
        data_cmds[["x"]] <- "plot.data$X <- factor(character(nrow(se)));"

    } else if (x_choice == .sampAssayXAxisRowDataTitle) {
        x_lab <- param_choices[[.sampAssayXAxisRowData]]
        plot_title <- paste(plot_title, "vs", x_lab)
        data_cmds[["x"]] <- sprintf("plot.data$X <- rowData(se)[, %s];", deparse(x_lab))

    } else {
        samp_selected_x <- param_choices[[.sampAssayXAxisSampName]]
        plot_title <- paste(plot_title, "vs", colnames(se)[samp_selected_x])
        x_lab <- .sample_axis_label(se, samp_selected_x, assay_choice, multiline=FALSE)
        data_cmds[["x"]] <- sprintf(
            "plot.data$X <- assay(se, %i, withDimnames=FALSE)[, %i];",
            assay_choice, samp_selected_x
        )
    }

    .plot_wrapper(
        data_cmds, param_choices=param_choices, all_memory=all_memory,
        all_coordinates=all_coordinates, se=se, by_row=TRUE,
        colormap=colormap, x_lab=x_lab, y_lab=y_lab, title=plot_title)
}

############################################
# Internal functions: plot wrapper ----
############################################

#' Plot creation wrapper
#'
#' Wraps the commands to complete the data.frame containing the plot data,
#' and to create the ggplot object to return to the Shiny server function.
#'
#' @param data_cmds A list of character vectors containing commands to initialize the plotting data.frame.
#' @param param_choices A single-row DataFrame that contains all the input settings for the current panel.
#' @param all_memory A list of DataFrames, where each DataFrame corresponds to a panel type and contains the settings for each individual panel of that type.
#' @param all_coordinates A list of data.frames, where each data.frame contains the x/y coordinates of data points on a specific plot (named by the encoded panel name).
#' @param se A SingleCellExperiment object.
#' @param by_row A logical vector indicating whether this data.frame is for a row-based plot.
#' @param ... Further arguments to pass to \code{\link{.create_plot}}.
#'
#' @details
#' This function is a convenience wrapper for the combination of \code{\link{.extract_plotting_data}}, \code{\link{.downsample_points}} and \code{\link{.create_plot}}.
#' The first command adds information such as coloring and point selection;
#' the second incorporates density-dependent downsampling;
#' and the last will generate the ggplot object itself.
#' This wrapper is to be used across the individual top-level functions for each plot type.
#'
#' @return A list that includes the following elements:
#' \describe{
#'   \item{cmd_list}{A list of character vectors, where each vector contains commands to parse and evaluate to produce the final plot.
#'     Each list element groups functionally related commands - namely, \code{"data"}, \code{"select"}, \code{"setup"}, \code{"plot"}).
#'   }
#'   \item{xy}{A data.frame that includes coordinates and covariates of the plot.
#'     This is the result of running all commands in \code{cmd_list} prior to \code{"plot"}.
#'   }
#'   \item{plot}{A ggplot object that results from the evaluation of \code{"plot"} commands.}
#' }
#'
#' @author Aaron Lun
#' @rdname INTERNAL_plot_wrapper
#' @seealso
#' \code{\link{.extract_plotting_data}},
#' \code{\link{.downsample_points}},
#' \code{\link{.create_plot}}
.plot_wrapper <- function(data_cmds, param_choices, all_memory, all_coordinates, se, by_row, ...) {
    setup_out <- .extract_plotting_data(data_cmds, param_choices, all_memory, all_coordinates, se, by_row=by_row)

    xy <- setup_out$envir$plot.data # DO NOT MOVE below .downsample_points, as downsampling will alter the value in 'envir'.

    downsample_cmds <- .downsample_points(param_choices, setup_out$envir)

    plot_out <- .create_plot(setup_out$envir, param_choices, ..., color_lab=setup_out$color_lab, shape_lab=setup_out$shape_lab, size_lab=setup_out$size_lab, by_row=by_row)

    return(list(cmd_list=c(setup_out$cmd_list, list(plot=c(downsample_cmds, plot_out$cmds))), xy=xy, plot=plot_out$plot))
}

############################################
# Internal functions: data formatter ----
############################################

#' Complete the plotting data.frame
#'
#' Fully define all of the data to be stored in a data.frame for ggplot
#' construction.
#' This ensures that the data acquisition is separated from the plotting.
#'
#' @param data_cmds A list of character vectors containing commands to
#' initialize the plotting data.frame.
#' @param param_choices A single-row DataFrame that contains all the input
#' settings for the current panel.
#' @param all_memory A list of DataFrames, where each DataFrame corresponds
#' to a panel type and contains the settings for each individual panel of that
#' type.
#' @param all_coordinates A list of data.frames, where each data.frame contains
#' the x/y coordinates of data points on a specific plot
#' (named by the encoded panel name).
#' @param se A SingleCellExperiment object.
#' @param by_row A logical vector indicating whether this data.frame is for a
#' row-based plot.
#'
#' @return
#' A list containing \code{cmd_list}, itself a list containing:
#' \itemize{
#' \item \code{data}, a character vector containing commands to generate the plotting data.frame.
#' \item \code{select}, a character vector containing commands to add \code{SelectBy} information to the plotting data.frame.
#' \item \code{specific}, a character vector containing commands to generate plot-type-specific commands, e.g., scatter for violin plots.
#' }
#'
#' The top-level list also contains \code{envir}, an environment that is guaranteed to hold \code{plot.data} and \code{plot.type} entries.
#'
#' Finally, the top-level list will contain \code{color_lab}, a string specifying how to label the color scale in the ggplot.
#'
#' @details
#' The output \code{envir} is guaranteed to contain a \code{plot.data} data.frame.
#' This is the fully constructed data.frame from evaluation of \code{cmds}, containing all plotting information required to generate a ggplot object.
#' The fields present in \code{plot.data} are:
#' \itemize{
#' \item \code{X} and \code{Y}, specifying the x- and y-coordinates, respectively.
#' All categorical variables are guaranteed to be factors, and everything else must be numeric.
#' \item \code{ColorBy}, specifying how to colour each point from the \code{.define_color_for_*_plots} functions.
#' As above, this is either a factor or numeric.
#' \item \code{SelectBy}, a logical field specifying whether the points were selected by a point selection in a transmitting plot.
#' If selecting points to restrict, any subsetting will already have been applied.
#' }
#'
#' The nature of the plot to be generated will determine whether any additional fields are present in \code{plot.data}.
#' Violin plots will contain \code{GroupBy} fields as well as \code{jitteredX}.
#' In horizontal violin plots, the values of \code{X} and \code{Y} will be swapped in preparation for \code{\link{coord_flip}} in \code{\link{.create_plot}}.
#' Square plots will have \code{jitteredX} and \code{jitteredY}.
#'
#' The \code{envir$plot.type} variable will specify the type of plot for correct dispatch in \code{\link{.create_plot}}.
#' The environment may also contain \code{plot.data.all}, see \code{\link{.process_selectby_choice}} for details.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_extract_plotting_data
#' @seealso
#' \code{\link{.define_colorby_for_column_plot}},
#' \code{\link{.define_colorby_for_row_plot}},
#' \code{\link{.process_selectby_choice}}
.extract_plotting_data <- function(data_cmds, param_choices, all_memory, all_coordinates, se, by_row=FALSE) {
    eval_env <- new.env()
    eval_env$se <- se
    eval_env$all_coordinates <- all_coordinates

    more_data_cmds <- .initialize_cmd_store()
    more_data_cmds <- .add_command(more_data_cmds, data_cmds)
    more_data_cmds <- .evaluate_commands(more_data_cmds, eval_env)

    # It is important that non-numeric X/Y become explicit factors here, which simplifies downstream processing
    # (e.g., coercion to integer, no lost levels upon subsetting).
    xvals <- eval_env$plot.data$X
    group_X <- .is_groupable(xvals)
    more_data_cmds <- .add_command(more_data_cmds, .coerce_type(xvals, "X", as_numeric=!group_X), name='more_X')

    yvals <- eval_env$plot.data$Y
    group_Y <- .is_groupable(yvals)
    more_data_cmds <- .add_command(more_data_cmds, .coerce_type(yvals, "Y", as_numeric=!group_Y), name='more_Y')

    # Adding coloring and faceting information as well.
    if (by_row) {
        color_out <- .define_colorby_for_row_plot(param_choices, se)
        shape_out <- .define_shapeby_for_row_plot(param_choices, se)
        size_out <- .define_sizeby_for_row_plot(param_choices, se)
        facet_out <- .define_facetby_for_row_plot(param_choices, se)
    } else {
        color_out <- .define_colorby_for_column_plot(param_choices, se)
        shape_out <- .define_shapeby_for_column_plot(param_choices, se)
        size_out <- .define_sizeby_for_column_plot(param_choices, se)
        facet_out <- .define_facetby_for_column_plot(param_choices, se)
    }

    more_data_cmds <- .add_command(more_data_cmds, color_out$cmds, name='color')
    color_lab <- color_out$label
    more_data_cmds <- .add_command(more_data_cmds, shape_out$cmds, name='shape')
    shape_lab <- shape_out$label
    more_data_cmds <- .add_command(more_data_cmds, size_out$cmds, name='size')
    size_lab <- size_out$label
    more_data_cmds <- .add_command(more_data_cmds, facet_out)

    # Ensuring that colors are either factor or numeric.
    more_data_cmds <- .evaluate_commands(more_data_cmds, eval_env)

    coloring <- eval_env$plot.data$ColorBy
    if (!is.null(coloring)) {
        more_data_cmds <- .add_command(more_data_cmds, .coerce_type(coloring, "ColorBy", as_numeric=!.is_groupable(coloring)), name='more_color')
    }

    # Removing NAs as they mess up .process_selectby_choice.
    clean_fields <- c("X", "Y", names(facet_out))
    clean_expression <- paste(sprintf("!is.na(%s)", clean_fields), collapse=" & ")
    more_data_cmds <- .add_command(more_data_cmds, sprintf("plot.data <- subset(plot.data, %s);", clean_expression), name='na.rm')

    more_data_cmds <- .evaluate_commands(more_data_cmds, eval_env)

    # Creating the command to define SelectBy.
    select_out <- .process_selectby_choice(param_choices, all_memory)
    select_cmds <- select_out$cmds
    if (!is.null(select_cmds)) {
        .populate_selection_environment(all_memory[[select_out$transmitter$Type]][select_out$transmitter$ID,], eval_env)
        .text_eval(select_cmds, eval_env)
    }

    # Adding more plot-specific information, depending on the type of plot to be created.
    specific <- .choose_plot_type(group_X, group_Y, eval_env)

    return(list(
        cmd_list=list(data=more_data_cmds$processed, select=select_cmds, setup=specific),
        envir=eval_env,
        color_lab=color_lab,
        shape_lab=shape_lab,
        size_lab=size_lab))
}

#' Choose the plot type
#'
#' Define and execute commands to choose the type of plot based on whether X and/or Y are categorical or continuous.
#'
#' @param group_X Logical scalar specifying if X is cateogrical.
#' @param group_Y Logical scalar specifying if Y is cateogrical.
#' @param envir Environment containing a \code{plot.data} data.frame with \code{X} and \code{Y} fields.
#'
#' @return
#' A character vector is returned containing commands to perform calculations for each plot type.
#' All commands are evaluated within \code{envir}.
#'
#' @details
#' \code{envir} is effectively passed by reference, as the setup commands are executed in the environment by this function.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_choose_plot_type
#' @seealso
#' \code{\link{.violin_setup}},
#' \code{\link{.square_setup}},
#' \code{\link{.extract_plotting_data}}
.choose_plot_type <- function(group_X, group_Y, envir) {
    if (!group_Y && !group_X) {
        mode <- "scatter"
        specific <- character()
    } else if (!group_Y) {
        mode <- "violin"
        specific <- .violin_setup(envir$plot.data, horizontal=FALSE)
    } else if (!group_X) {
        mode <- "violin_horizontal"
        specific <- .violin_setup(envir$plot.data, horizontal=TRUE)

        if (exists("plot.data.all", envir)) { # flipping plot.data.all as well, otherwise it becomes chaotic in .violin_plot().
            specific <- c(specific,
                "tmp <- plot.data.all$X;
                plot.data.all$X <- plot.data.all$Y;
                plot.data.all$Y <- tmp;")
        }
    } else {
        mode <- "square"
        specific <- .square_setup(envir$plot.data)
    }

    .text_eval(specific, envir)
    envir$plot.type <- mode
    return(specific)
}

############################################
# Internal functions: downsampler ----
############################################

#' Downsampling commands
#'
#' Define and execute commands to downsample points for speed.
#'
#' @param param_choices A single-row DataFrame that contains all the input settings for the current panel.
#' @param envir Environment containing a \code{plot.data} data.frame with \code{X} and \code{Y} fields.
#'
#' @details
#' Density-dependent downsampling for speed is performed in this function, based on \code{\link{subsetPointsByGrid}}.
#' \code{envir} is effectively passed by reference, as the setup commands are executed in the environment by this function.
#' A \code{plot.data.pre} data.frame is also added to \code{envir} to keep the pre-subsetted information, e.g., for use in \code{\link{.violin_plot}}.
#'
#' @return
#' A character vector is returned containing commands to perform downsampling.
#' All commands are evaluated within \code{envir}.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_downsample_points
#' @seealso
#' \code{\link{subsetPointsByGrid}},
#' \code{\link{.plot_wrapper}}
.downsample_points <- function(param_choices, envir) {
    if (param_choices[[.plotPointDownsample]]) {
        xtype <- "X"
        ytype <- "Y"

        plot_type <- envir$plot.type
        if (plot_type == "square") {
            xtype <- "jitteredX"
            ytype <- "jitteredY"
        } else if (plot_type == "violin" || plot_type == "violin_horizontal") {
            xtype <- "jitteredX"
        }

        ## If we color by sample name in a column-based plot, or by feature name
        ## in a row-based plot, we make sure to keep the selected column/row in
        ## the downsampling
        enc <- .split_encoded(rownames(param_choices))
        is_color_by_sample_name <- param_choices[[.colorByField]] == .colorBySampNameTitle && enc$Type %in% col_point_plot_types
        is_color_by_feature_name <- param_choices[[.colorByField]] == .colorByFeatNameTitle && enc$Type %in% row_point_plot_types
        downsample_cmds <- c(
            "plot.data.pre <- plot.data;",
            "# Randomize data points to avoid a data set bias during the downsampling",
            "set.seed(100);",
            "plot.data <- plot.data[sample(nrow(plot.data)), , drop=FALSE];",
            sprintf(
                "plot.data <- subset(plot.data, subsetPointsByGrid(%s, %s, resolution=%i)%s);",
                xtype, ytype, param_choices[[.plotPointSampleRes]],
                ifelse(is_color_by_sample_name || is_color_by_feature_name, " | as.logical(plot.data$ColorBy)", "")
            ),
            "")

        .text_eval(downsample_cmds, envir)
        return(downsample_cmds)
    } else {
        return(NULL)
    }
}

############################################
# Internal functions: central plotter ----
############################################

#' Central plotting function
#'
#' Create and evaluate plotting commands to generate a ggplot for each
#' combination of X/Y covariate types.
#'
#' @param envir An environment produced by \code{\link{.extract_plotting_data}}.
#' @param param_choices A single-row DataFrame that contains all the input
#' settings for the current panel.
#' @param colormap An ExperimentColorMap object that defines custom colormaps
#' to apply to individual \code{assays}, \code{colData}, and \code{rowData}
#' covariates.
#' @param ... Further arguments passed to the specific plotting functions.
#'
#' @return A list that includes the following elements:
#' \describe{
#'   \item{cmds}{A character vector containing commands to parse and evaluate
#'     to produce the final plot.}
#'   \item{plot}{A ggplot object.}
#' }
#'
#' @details
#' This function should \emph{only} add commands related to the generation of the ggplot object.
#' Any commands involving persistent manipulation of \code{plot.data} should be placed in \code{\link{.extract_plotting_data}} instead.
#'
#' This function will generate a scatter plot if both X and Y are numeric; a violin plot if only Y is numeric;
#' a horizontal violin plot if only X is numeric; and a square plot, if neither are numeric.
#' Refer to the documentation of the individual plotting functions for more details.
#'
#' Plotting commands will be executed in the evaluation environment (i.e., \code{envir}) to produce the output \code{plot} object.
#' This function will also add a box representing the Shiny brush coordinates, if one is available - see \code{?\link{.self_brush_box}}.
#' Alternatively, if lasso waypoints are present, it will add them to the plot - see \code{?\link{.self_lasso_path}}.
#'
#' @author Kevin Rue-Albrecht, Aaron Lun, Charlotte Soneson
#' @rdname INTERNAL_create_plot
#' @seealso
#' \code{\link{.extract_plotting_data}},
#' \code{\link{.scatter_plot}},
#' \code{\link{.violin_plot}},
#' \code{\link{.square_plot}}
.create_plot <- function(envir, param_choices, colormap, ...) {
    envir$colormap <- colormap
    plot_data <- envir$plot.data
    plot_type <- envir$plot.type
    extra_cmds <- character(0)

    # Figuring out whether or not we need certain fields.
    is_subsetted <- exists("plot.data.all", envir=envir)
    is_downsampled <- exists("plot.data.pre", envir=envir)

    # Dispatch to different plotting commands, depending on X/Y being groupable
    extra_cmds <- c(extra_cmds, switch(plot_type,
        square=.square_plot(plot_data=plot_data, param_choices=param_choices,
            is_subsetted=is_subsetted, ...),
        violin=.violin_plot(plot_data=plot_data, param_choices=param_choices,
            is_subsetted=is_subsetted, is_downsampled=is_downsampled, ...),
        violin_horizontal=.violin_plot(plot_data=plot_data, param_choices=param_choices,
            is_subsetted=is_subsetted, is_downsampled=is_downsampled, ..., horizontal=TRUE),
        scatter=.scatter_plot(plot_data=plot_data, param_choices=param_choices,
            is_subsetted=is_subsetted, is_downsampled=is_downsampled, ...)
    ))

    # Adding a faceting command, if applicable
    facet_cmd <- .add_facets(param_choices)

    if (length(facet_cmd)) {
        N <- length(extra_cmds)
        extra_cmds[[N]] <- paste(extra_cmds[[N]], "+")
        extra_cmds <- c(extra_cmds, facet_cmd)
    }

    # Adding self-brushing boxes, if they exist.
    to_flip <- plot_type == "violin_horizontal"
    brush_out <- .self_brush_box(param_choices, flip=to_flip)
    lasso_out <- .self_lasso_path(param_choices, flip=to_flip)
    select_cmds <- c(brush_out, lasso_out)

    if (length(select_cmds)) {
        N <- length(extra_cmds)
        extra_cmds[[N]] <- paste(extra_cmds[[N]], "+")

        intermediate <- seq_len(length(select_cmds)-1L)
        select_cmds[intermediate] <- paste(select_cmds[intermediate], "+")
        extra_cmds <- c(extra_cmds, select_cmds)

        # We overwrite any existing 'all_brushes' or 'all_lassos',
        # as they have already served their purpose in defining plot_data
        # in .extract_plotting_data().
        .populate_selection_environment(param_choices, envir)
    }

    # Evaluating the plotting commands.
    plot_out <- .text_eval(extra_cmds, envir)
    return(list(cmds=extra_cmds, plot=plot_out))
}

############################################
# Internal functions: scatter plotter ----
############################################

#' Produce a scatter plot
#'
#' Generate (but not evaluate) commands to create a scatter plot of numeric X/Y.
#'
#' @param plot_data A data.frame containing all of the plotting information,
#' returned by \code{\link{.extract_plotting_data}} in \code{envir$plot.data}.
#' @param param_choices A single-row DataFrame that contains all the' input settings for the current panel.
#' @param x_lab A character label for the X axis.
#' Set to \code{NULL} to have no x-axis label.
#' @param y_lab A character label for the Y axis.
#' Set to \code{NULL} to have no y-axis label.
#' @param color_lab A character label for the color scale.
#' Set to \code{NULL} to have no color label.
#' @param shape_lab A character label for the shape scale.
#' Set to \code{NULL} to have no shape label.
#' @param size_lab A character label for the size scale.
#' Set to \code{NULL} to have no size label.
#' @param title A character title for the plot.
#' Set to \code{NULL} to have no title.
#' @param by_row A logical scalar specifying whether the plot deals with row-level metadata.
#' @param is_subsetted A logical scalar specifying whether \code{plot_data} was subsetted during \code{\link{.process_selectby_choice}}.
#' @param is_downsampled A logical scalar specifying whether \code{plot_data} was downsampled in \code{\link{.plot_wrapper}}.
#'
#' @return A character vector of commands to be parsed and evaluated by \code{\link{.create_plot}} to produce the scatter plot.
#'
#' @details
#' As described in \code{?\link{.create_plot}}, the \code{\link{.scatter_plot}} function should only contain commands to generate the final ggplot object.
#'
#' \code{plot.data.all} will be used to define the plot boundaries when selecting points to restrict (see \code{?\link{.process_selectby_choice}}).
#' If there is no restriction and we are downsampling for speed (see \code{?\link{.plot_wrapper}}), \code{plot.data.pre} will be used to define the boundaries.
#'
#' @author Kevin Rue-Albrecht, Aaron Lun.
#' @rdname INTERNAL_scatter_plot
#' @seealso
#' \code{\link{.create_plot}}
#'
#' @importFrom ggplot2 ggplot coord_cartesian theme_bw theme element_text
.scatter_plot <- function(plot_data, param_choices, x_lab, y_lab, color_lab, shape_lab, size_lab, title, by_row=FALSE, is_subsetted=FALSE, is_downsampled=FALSE) {
    plot_cmds <- list()
    plot_cmds[["ggplot"]] <- "ggplot() +"

    # Adding points to the plot.
    color_set <- !is.null(plot_data$ColorBy)
    shape_set <- param_choices[[.shapeByField]] != .shapeByNothingTitle
    size_set <- param_choices[[.sizeByField]] != .sizeByNothingTitle
    new_aes <- .build_aes(color=color_set, shape=shape_set, size=size_set)
    plot_cmds[["points"]] <- .create_points(param_choices, !is.null(plot_data$SelectBy), new_aes, color_set, size_set)

    # Defining the color commands.
    if (by_row) {
        color_scale_cmd <- .add_color_to_row_plot(plot_data$ColorBy, param_choices)
    } else {
        color_scale_cmd <- .add_color_to_column_plot(plot_data$ColorBy, param_choices)
    }

    # Adding axes labels.
    plot_cmds[["labs"]] <- .build_labs(x=x_lab, y=y_lab, color=color_lab, shape=shape_lab, size=size_lab, title=title)

    # Defining boundaries if zoomed.
    bounds <- param_choices[[.zoomData]][[1]]
    if (!is.null(bounds)) {
        plot_cmds[["coord"]] <- sprintf(
            "coord_cartesian(xlim=c(%s, %s), ylim=c(%s, %s), expand=FALSE) +", # FALSE, to get a literal zoom.
            deparse(bounds["xmin"]), deparse(bounds["xmax"]),
            deparse(bounds["ymin"]),  deparse(bounds["ymax"])
        )
    } else {
        full_data <- ifelse(is_subsetted, "plot.data.all", ifelse(is_downsampled, "plot.data.pre", "plot.data"))
        plot_cmds[["coord"]] <- sprintf("coord_cartesian(xlim=range(%s$X, na.rm=TRUE),
    ylim=range(%s$Y, na.rm=TRUE), expand=TRUE) +", full_data, full_data)
    }

    if (param_choices[[.contourAddTitle]]) {
        plot_cmds[["contours"]] <- sprintf("geom_density_2d(aes(x=X, y=Y), plot.data, colour='%s') +", param_choices[[.contourColor]])
    }

    # Retain axes when no points are present.
    if (nrow(plot_data) == 0 && is_subsetted) {
        plot_cmds[["select_blank"]] <- "geom_blank(data=plot.data.all, inherit.aes=FALSE, aes(x=X, y=Y)) +"
    }

    # Adding further aesthetic elements.
    plot_cmds[["scale_color"]] <- color_scale_cmd
    plot_cmds[["theme_base"]] <- "theme_bw() +"
    plot_cmds[["theme_custom"]] <- sprintf(
        "theme(legend.position='%s', legend.box='vertical', legend.text=element_text(size=%s), legend.title=element_text(size=%s),
        axis.text=element_text(size=%s), axis.title=element_text(size=%s), title=element_text(size=%s))",
        tolower(param_choices[[.plotLegendPosition]]),
        param_choices[[.plotFontSize]]*.plotFontSizeLegendTextDefault,
        param_choices[[.plotFontSize]]*.plotFontSizeLegendTitleDefault,
        param_choices[[.plotFontSize]]*.plotFontSizeAxisTextDefault,
        param_choices[[.plotFontSize]]*.plotFontSizeAxisTitleDefault,
        param_choices[[.plotFontSize]]*.plotFontSizeTitleDefault)

    return(unlist(plot_cmds))
}

############################################
# Internal functions: violin plotter ----
############################################

#' Produce a violin plot
#'
#' Generate (but not evaluate) the commands required to produce a vertical or
#' horizontal violin plot.
#'
#' @param plot_data A data.frame containing all of the plotting information, returned by \code{\link{.extract_plotting_data}} in \code{envir$plot.data}.
#' @param param_choices A single-row DataFrame that contains all the input settings for the current panel.
#' @param x_lab A character label for the X axis.
#' Set to \code{NULL} to have no x-axis label.
#' @param y_lab A character label for the Y axis.
#' Set to \code{NULL} to have no y-axis label.
#' @param color_lab A character label for the color scale.
#' Set to \code{NULL} to have no color label.
#' @param shape_lab A character label for the shape scale.
#' Set to \code{NULL} to have no shape label.
#' @param size_lab A character label for the size scale.
#' Set to \code{NULL} to have no size label.
#' @param title A character title for the plot.
#' Set to \code{NULL} to have no title.
#' @param horizontal A logical value that indicates whether violins should be drawn horizontally
#' (i.e., Y axis categorical and X axis continuous).
#' @param by_row A logical scalar specifying whether the plot deals with row-level metadata.
#' @param is_subsetted A logical scalar specifying whether \code{plot_data} was subsetted during \code{\link{.process_selectby_choice}}.
#' @param is_downsampled A logical scalar specifying whether \code{plot_data} was downsampled in \code{\link{.plot_wrapper}}.
#'
#' @return
#' For \code{\link{.violin_setup}}, a character vector of commands to be parsed
#' and evaluated by \code{\link{.extract_plotting_data}} to set up the
#' required fields.
#'
#' For \code{\link{.violin_plot}}, a character vector of commands to be parsed
#' and evaluated by \code{\link{.create_plot}} to produce the violin plot.
#'
#' @details
#' Any commands to modify \code{plot.data} in preparation for creating a violin plot should be placed in \code{\link{.violin_setup}},
#' to be called by \code{\link{.extract_plotting_data}}.
#' This includes swapping of X and Y variables when \code{horizontal=TRUE}, and adding of horizontal/vertical jitter to points.
#'
#' As described in \code{?\link{.create_plot}}, the \code{\link{.violin_plot}} function should only contain commands to generate the final ggplot object.
#'
#' \code{plot.data.all} will be used to define the y-axis boundaries (or x-axis boundaries when \code{horizontal=TRUE}).
#' This ensures consistent plot boundaries when selecting points to restrict (see \code{?\link{.process_selectby_choice}}),
#' or when downsampling for speed (see \code{?\link{.create_plot}}.
#'
#' Similarly, \code{envir$plot.data.pre} will be used to create the violins (see \code{\link{.create_plot}}).
#' This ensures consistent violins when downsampling for speed - otherwise the violins will be computed from the downsampled set of points.
#'
#' @author Kevin Rue-Albrecht, Aaron Lun, Charlotte Soneson.
#' @rdname INTERNAL_violin_plot
#' @seealso
#' \code{\link{.extract_plotting_data}},
#' \code{\link{.create_plot}}
#'
#' @importFrom ggplot2 ggplot geom_violin coord_cartesian theme_bw theme
#' coord_flip scale_x_discrete scale_y_discrete
.violin_plot <- function(
    plot_data, param_choices, x_lab, y_lab, color_lab, shape_lab, size_lab, title,
    horizontal=FALSE, by_row=FALSE, is_subsetted=FALSE, is_downsampled=FALSE) {

    plot_cmds <- list()
    plot_cmds[["ggplot"]] <- "ggplot() +" # do NOT put aes here, it does not play nice with shiny brushes.
    plot_cmds[["violin"]] <- sprintf(
        "geom_violin(%s, alpha=0.2, data=%s, scale='width', width=0.8) +",
        .build_aes(color=FALSE, group=TRUE),
        ifelse(is_downsampled, "plot.data.pre", "plot.data")
    )

    # Adding the points to the plot (with/without point selection).
    color_set <- !is.null(plot_data$ColorBy)
    shape_set <- param_choices[[.shapeByField]] != .shapeByNothingTitle
    size_set <- param_choices[[.sizeByField]] != .sizeByNothingTitle
    new_aes <- .build_aes(color=color_set, shape=shape_set, size=size_set, alt=c(x="jitteredX"))
    plot_cmds[["points"]] <- .create_points(param_choices, !is.null(plot_data$SelectBy), new_aes, color_set, size_set)

    # Defining the color commands.
    if (by_row) {
        color_scale_cmd <- .add_color_to_row_plot(plot_data$ColorBy, param_choices, x_aes="jitteredX")
    } else {
        color_scale_cmd <- .add_color_to_column_plot(plot_data$ColorBy, param_choices, x_aes="jitteredX")
    }

    # Adding axis labels.
    if (horizontal) {
        tmp <- y_lab
        y_lab <- x_lab
        x_lab <- tmp
    }

    plot_cmds[["labs"]] <- .build_labs(x=x_lab, y=y_lab, color=color_lab, shape=shape_lab, size=size_lab, title=title)

    # Defining boundaries if zoomed. This requires some finesse to deal with horizontal plots,
    # where the point selection is computed on the flipped coordinates.
    bounds <- param_choices[[.zoomData]][[1]]
    if (horizontal) {
        coord_cmd <- "coord_flip"
        if (!is.null(bounds)) {
            names(bounds) <- c(xmin="ymin", xmax="ymax", ymin="xmin", ymax="xmax")[names(bounds)]
        }
    } else {
        coord_cmd <- "coord_cartesian"
    }

    if (!is.null(bounds)) {
        # Ensure zoom preserves the data points and width ratio of visible groups
        bounds["xmin"] <- ceiling(bounds["xmin"]) - 0.5
        bounds["xmax"] <- floor(bounds["xmax"]) + 0.5

        plot_cmds[["coord"]] <- sprintf(
            "%s(xlim=c(%s, %s), ylim=c(%s, %s), expand=FALSE) +", # FALSE, to get a literal zoom.
            coord_cmd, deparse(bounds["xmin"]), deparse(bounds["xmax"]),
            deparse(bounds["ymin"]), deparse(bounds["ymax"])
        )
    } else {
        plot_cmds[["coord"]] <- sprintf("%s(ylim=range(%s$Y, na.rm=TRUE), expand=TRUE) +",
            coord_cmd, ifelse(is_subsetted, "plot.data.all", ifelse(is_downsampled, "plot.data.pre", "plot.data"))
        )
    }

    plot_cmds[["scale_color"]] <- color_scale_cmd

    # Retain axes when no points are generated.
    if (nrow(plot_data) == 0 && is_subsetted) {
        plot_cmds[["select_blank"]] <- "geom_blank(data=plot.data.all, inherit.aes=FALSE, aes(x=X, y=Y)) +"
    }

    # Preserving the x-axis range when no zoom is applied.
    # This applies even for horizontal violin plots, as this command is executed internally before coord_flip().
    scale_x_cmd <- "scale_x_discrete(drop=FALSE%s) +"
    if (is.null(bounds)) {
        scale_x_extra <- ""
    } else {
        # Restrict axis ticks to visible levels
        scale_x_extra <- sprintf(
            ", breaks=levels(plot.data$X)[%i:%i]",
            ceiling(bounds["xmin"]), floor(bounds["xmax"]))
    }
     plot_cmds[["scale_x"]] <- sprintf(scale_x_cmd, scale_x_extra)

    plot_cmds[["theme_base"]] <- "theme_bw() +"
    plot_cmds[["theme_custom"]] <- sprintf(
        "theme(legend.position='%s', legend.text=element_text(size=%s),
        legend.title=element_text(size=%s), legend.box='vertical',
        axis.text.x=element_text(angle=90, size=%s, hjust=1, vjust=0.5),
        axis.text.y=element_text(size=%s),
        axis.title=element_text(size=%s), title=element_text(size=%s))",
        tolower(param_choices[[.plotLegendPosition]]),
        param_choices[[.plotFontSize]]*.plotFontSizeLegendTextDefault,
        param_choices[[.plotFontSize]]*.plotFontSizeLegendTitleDefault,
        param_choices[[.plotFontSize]]*.plotFontSizeAxisTextDefault,
        param_choices[[.plotFontSize]]*.plotFontSizeAxisTextDefault,
        param_choices[[.plotFontSize]]*.plotFontSizeAxisTitleDefault,
        param_choices[[.plotFontSize]]*.plotFontSizeTitleDefault)

    return(unlist(plot_cmds))
}

#' @rdname INTERNAL_violin_plot
.violin_setup <- function(plot_data, horizontal=FALSE) {
    setup_cmds <- list()

    # Switching X and Y axes if we want a horizontal violin plot.
    if (horizontal) {
        setup_cmds[["swap"]] <- c("tmp <- plot.data$X;
plot.data$X <- plot.data$Y;
plot.data$Y <- tmp;")
    }
    setup_cmds[["group"]] <- "plot.data$GroupBy <- plot.data$X;"

    # Handling the specification of the jitter-by-group argument.
    groupvar <- ""
    if (!is.null(plot_data$FacetRow) || !is.null(plot_data$FacetColumn)) {
        groupvar <- character(0)
        if (!is.null(plot_data$FacetRow)) {
            groupvar <- c(groupvar, "FacetRow=plot.data$FacetRow")
        }
        if (!is.null(plot_data$FacetColumn)) {
            groupvar <- c(groupvar, "FacetColumn=plot.data$FacetColumn")
        }
        groupvar <- paste0("\n    list(", paste(groupvar, collapse=", "), "),")
    }

    # Figuring out the jitter. This is done ahead of time to guarantee the
    # same results regardless of the subset used for point selection. Note adjust=1
    # for consistency with geom_violin (differs from geom_quasirandom default).
    setup_cmds[["seed"]] <- "set.seed(100);"
    setup_cmds[["calcX"]] <- sprintf(
"plot.data$jitteredX <- iSEE::jitterViolinPoints(plot.data$X, plot.data$Y, %s
    width=0.4, varwidth=FALSE, adjust=1,
    method='quasirandom', nbins=NULL);", groupvar)

    return(unlist(setup_cmds))
}

############################################
# Internal functions: rectangle plotter ----
############################################

#' Produce a square plot
#'
#' Generate (but not evaluate) the commands required to produce a square plot.
#'
#' @param plot_data A data.frame containing all of the plotting information,
#' returned by \code{\link{.extract_plotting_data}} in \code{envir$plot.data}.
#' @param param_choices A single-row DataFrame that contains all the
#' input settings for the current panel.
#' @param se A SingleCellExperiment object.
#' @param x_lab A character label for the X axis.
#' Set to \code{NULL} to have no x-axis label.
#' @param y_lab A character label for the Y axis.
#' Set to \code{NULL} to have no y-axis label.
#' @param color_lab A character label for the color scale.
#' Set to \code{NULL} to have no color label.
#' @param title A character title for the plot.
#' Set to \code{NULL} to have no title.
#' @param by_row A logical scalar specifying whether the plot deals with row-level metadata.
#' @param is_subsetted A logical scalar specifying whether \code{plot_data} was subsetted during \code{\link{.process_selectby_choice}}.
#' @param shape_lab A character label for the shape scale.
#' Set to \code{NULL} to have no shape label.
#' @param size_lab A character label for the size scale.
#' Set to \code{NULL} to have no size label.
#'
#' @return
#' For \code{\link{.square_setup}}, a character vector of commands to be parsed and evaluated by \code{\link{.extract_plotting_data}} to set up the required fields.
#'
#' For \code{\link{.square_plot}}, a character vector of commands to be parsed and evaluated by \code{\link{.create_plot}} to produce the square plot.
#'
#' @details
#' Any commands to modify \code{plot.data} in preparation for creating a square plot should be placed in \code{\link{.square_setup}}.
#' This function will subsequently be called by \code{\link{.extract_plotting_data}}.
#'
#' The square plot is set up so that the widths on the x-axis are constant when there is only one y-axis level.
#' This means that the dimensions of the squares on the y-axis are directly comparable, without any need to compare areas.
#' Similarly, the widths on the y-axis default are constant when there is only one x-axis level.
#'
#' As described in \code{?\link{.create_plot}}, the \code{\link{.square_plot}} function should only contain commands to generate the final ggplot object.
#'
#' @author Kevin Rue-Albrecht, Aaron Lun, Charlotte Soneson.
#' @rdname INTERNAL_square_plot
#' @seealso
#' \code{\link{.extract_plotting_data}},
#' \code{\link{.create_plot}}
#'
#' @importFrom ggplot2 ggplot geom_tile coord_cartesian theme_bw theme
#' scale_x_discrete scale_y_discrete guides
.square_plot <- function(plot_data, param_choices, se, x_lab, y_lab, color_lab, shape_lab, size_lab, title, by_row=FALSE, is_subsetted=FALSE) {
    plot_cmds <- list()
    plot_cmds[["ggplot"]] <- "ggplot(plot.data) +"
    plot_cmds[["tile"]] <-
"geom_tile(aes(x=X, y=Y, height=2*YWidth, width=2*XWidth, group=interaction(X, Y)),
    summary.data, color='black', alpha=0, size=0.5) +"

    # Adding the points to the plot (with/without point selection).
    color_set <- !is.null(plot_data$ColorBy)
    shape_set <- param_choices[[.shapeByField]] != .shapeByNothingTitle
    size_set <- param_choices[[.sizeByField]] != .sizeByNothingTitle
    new_aes <- .build_aes(color=color_set, shape=shape_set, size=size_set, alt=c(x="jitteredX", y="jitteredY"))
    plot_cmds[["points"]] <- .create_points(param_choices, !is.null(plot_data$SelectBy), new_aes, color_set, size_set)

    # Defining the color commands.
    if (by_row) {
        color_scale_cmd <- .add_color_to_row_plot(plot_data$ColorBy, param_choices, x_aes="jitteredX", y_aes="jitteredY")
    } else {
        color_scale_cmd <- .add_color_to_column_plot(plot_data$ColorBy, param_choices, x_aes="jitteredX", y_aes="jitteredY")
    }

    # Adding the commands to color the points and the point selection area
    # (NULL if undefined).
    plot_cmds[["scale_color"]] <- color_scale_cmd

    # Creating labels.
    plot_cmds[["labs"]] <- .build_labs(x=x_lab, y=y_lab, color=color_lab, shape=shape_lab, size=size_lab, title=title)

    # Defining boundaries if zoomed.
    bounds <- param_choices[[.zoomData]][[1]]
    if (!is.null(bounds)) {

        # Ensure zoom preserves the data points and width ratio of visible groups
        bounds["xmin"] <- ceiling(bounds["xmin"]) - 0.5
        bounds["xmax"] <- floor(bounds["xmax"]) + 0.5
        bounds["ymin"] <- ceiling(bounds["ymin"]) - 0.5
        bounds["ymax"] <- floor(bounds["ymax"]) + 0.5

        plot_cmds[["coord"]] <- sprintf(
            "coord_cartesian(xlim=c(%s, %s), ylim=c(%s, %s), expand=FALSE) +",
            deparse(bounds["xmin"]), deparse(bounds["xmax"]),
            deparse(bounds["ymin"]), deparse(bounds["ymax"])
        )
    }

    scale_x_cmd <- "scale_x_discrete(drop=FALSE%s) +"
    scale_y_cmd <- "scale_y_discrete(drop=FALSE%s) +"
    if (is.null(bounds)) {
        scale_x_extra <- ""
        scale_y_extra <- ""
    } else {
        # Restrict axis ticks to visible levels
        scale_x_extra <- sprintf(
            ", breaks=levels(plot.data$X)[%i:%i]",
            ceiling(bounds["xmin"]), floor(bounds["xmax"]))
        scale_y_extra <- sprintf(
            ", breaks=levels(plot.data$Y)[%i:%i]",
            ceiling(bounds["ymin"]), floor(bounds["ymax"]))
    }
     plot_cmds[["scale_x"]] <- sprintf(scale_x_cmd, scale_x_extra)
     plot_cmds[["scale_y"]] <- sprintf(scale_y_cmd, scale_y_extra)

    # Retain axes when no points are present.
    if (nrow(plot_data) == 0 && is_subsetted) {
        plot_cmds[["select_blank"]] <- "geom_blank(data=plot.data.all, inherit.aes=FALSE, aes(x=X, y=Y)) +"
    }

    # Do not display the size legend (saves plot space, as well)
    plot_cmds[["theme_base"]] <- "theme_bw() +"
    plot_cmds[["theme_custom"]] <- sprintf("theme(legend.position='%s', legend.text=element_text(size=%s),
    legend.title=element_text(size=%s), legend.box='vertical',
    axis.text.x=element_text(angle=90, size=%s, hjust=1, vjust=0.5),
    axis.text.y=element_text(size=%s),
    axis.title=element_text(size=%s), title=element_text(size=%s))",
        tolower(param_choices[[.plotLegendPosition]]),
        param_choices[[.plotFontSize]]*.plotFontSizeLegendTextDefault,
        param_choices[[.plotFontSize]]*.plotFontSizeLegendTitleDefault,
        param_choices[[.plotFontSize]]*.plotFontSizeAxisTextDefault,
        param_choices[[.plotFontSize]]*.plotFontSizeAxisTextDefault,
        param_choices[[.plotFontSize]]*.plotFontSizeAxisTitleDefault,
        param_choices[[.plotFontSize]]*.plotFontSizeTitleDefault)
    return(unlist(plot_cmds))
}

#' @rdname INTERNAL_square_plot
#' @importFrom stats runif
.square_setup <- function(plot_data) {
    setup_cmds  <- list()

    # Handling the specification of the jitter-by-group argument.
    groupvar <- ""
    if (!is.null(plot_data$FacetRow) || !is.null(plot_data$FacetColumn)) {
        groupvar <- character(0)
        if (!is.null(plot_data$FacetRow)) {
            groupvar <- c(groupvar, "FacetRow=plot.data$FacetRow")
        }
        if (!is.null(plot_data$FacetColumn)) {
            groupvar <- c(groupvar, "FacetColumn=plot.data$FacetColumn")
        }
        groupvar <- paste0(",\n    list(", paste(groupvar, collapse=", "), ")")
    }

    # Setting the seed to ensure reproducible results.
    setup_cmds[["jitter"]] <- sprintf("set.seed(100);
j.out <- iSEE:::jitterSquarePoints(plot.data$X, plot.data$Y%s);
summary.data <- j.out$summary;
plot.data$jitteredX <- j.out$X;
plot.data$jitteredY <- j.out$Y;", groupvar)
    return(unlist(setup_cmds))
}

############################################
# Internal functions: coloring ----
############################################

#' Define coloring variables
#'
#' Generates the commands necessary to define the variables to color by in the data.frame to be supplied to ggplot.
#'
#' @param param_choices A single-row DataFrame that contains all the input settings for the current panel.
#' @param se A SingleCellExperiment object.
#'
#' @details
#' These functions generate commands to extract the variable to use for coloring individual points in row- or column-based plots,
#' i.e., where each point is a feature or sample, respectively.
#' The commands should be evaluated in the evaluation environment generated in \code{\link{.extract_plotting_data}}.
#'
#' In these commands, the coloring variable is added to the \code{plot.data} data.frame in the \code{ColorBy} field.
#' This is distinct from \code{.add_color_to_*_plot}, which generates the commands for coloring a ggplot by the values in \code{ColourBy}.
#'
#' @return
#' A list containing \code{label}, a string to use for labelling the color scale;
#' and \code{cmds}, a character vector of commands to add a \code{ColorBy} field to \code{plot.data}.
#' Alternatively \code{NULL}, if no variable to color by is specified.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_define_color_variables
#' @seealso
#' \code{\link{.extract_plotting_data}},
#' \code{\link{.add_color_to_row_plot}},
#' \code{\link{.add_color_to_column_plot}}
#'
#' @importFrom SummarizedExperiment assay
.define_colorby_for_column_plot <- function(param_choices, se) {
    color_choice <- param_choices[[.colorByField]]

    if (color_choice == .colorByColDataTitle) {
        covariate_name <- param_choices[[.colorByColData]]
        return(list(label=covariate_name,
            cmds=sprintf("plot.data$ColorBy <- colData(se)[, %s];", deparse(covariate_name))))

    } else if (color_choice == .colorByFeatNameTitle) {
        # Set the color to the selected gene
        chosen_gene <- param_choices[[.colorByFeatName]]
        assay_choice <- param_choices[[.colorByFeatNameAssay]]
        return(list(
            label=.feature_axis_label(se, chosen_gene, assay_choice, multiline=TRUE),
            cmds=sprintf("plot.data$ColorBy <- assay(se, %i, withDimnames=FALSE)[%i, ];", assay_choice, chosen_gene)))

    } else if (color_choice == .colorBySampNameTitle) {
        chosen_sample <- param_choices[[.colorBySampName]]
        return(list(
            label=.sample_axis_label(se, chosen_sample, assay_id=NULL),
            cmds=sprintf("plot.data$ColorBy <- logical(nrow(plot.data));
plot.data[%s, 'ColorBy'] <- TRUE;", deparse(chosen_sample))))

    } else {
        return(NULL)
    }
}

#' @rdname INTERNAL_define_color_variables
.define_colorby_for_row_plot <- function(param_choices, se) {
    color_choice <- param_choices[[.colorByField]]

    if (color_choice == .colorByRowDataTitle) {
        covariate_name <- param_choices[[.colorByRowData]]
        return(list(
            label=covariate_name,
            cmds=sprintf("plot.data$ColorBy <- rowData(se)[, %s];", deparse(covariate_name))))

    } else if (color_choice == .colorByFeatNameTitle) {
        chosen_gene <- param_choices[[.colorByFeatName]]
        return(list(
            label=.feature_axis_label(se, chosen_gene, assay_id=NULL),
            cmds=sprintf("plot.data$ColorBy <- logical(nrow(plot.data));
plot.data[%s, 'ColorBy'] <- TRUE;", deparse(chosen_gene))))

    } else if (color_choice == .colorBySampNameTitle) {
        chosen_sample <- param_choices[[.colorBySampName]]
        assay_choice <- param_choices[[.colorBySampNameAssay]]
        return(list(
            label=.sample_axis_label(se, chosen_sample, assay_choice, multiline=TRUE),
            cmds=sprintf("plot.data$ColorBy <- assay(se, %i, withDimnames=FALSE)[, %i];", assay_choice, chosen_sample)))

    } else {
        return(NULL)
    }
}

#' Add color scale
#'
#' Generates commands to add a color scale to the ggplot object, based on the
#' specification in the ExperimentColorMap.
#'
#' @param colorby A vector of values to color points by, taken from
#' \code{plot.data$ColorBy} in upstream functions.
#' @param param_choices A single-row DataFrame that contains all the
#' input settings for the current panel.
#' @param x_aes Name of the column in \code{plot.data} to use for the x-axis.
#' @param y_aes Name of the column in \code{plot.data} to use for the y-axis.
#'
#' @return
#' A character vector containing commands to add a color scale to an existing ggplot object, or \code{NULL} if no color scale needs to be added.
#'
#' @details
#' These functions generate commands to add a color scale for individual points in row- or column-based plots,' i.e., where each point is a feature or sample, respectively.
#'
#' These commands assume that an ExperimentColorMap object named  \code{colormap} exists in the evaluation environment.
#' The availability of \code{colorby} allows the function to determine whether  discrete or continuous color scales need to be used,
#' and if discrete, how many levels (i.e., colors) should be requested from \code{colormap}.
#'
#' \code{x_aes} and \code{y_aes} are necessary to ensure that jittering is respected when adding a layer to highlight a specific point.
#'
#' @author Kevin Rue-Albrecht, Aaron Lun.
#' @rdname INTERNAL_add_color_scale
#' @seealso
#' \code{\link{.scatter_plot}},
#' \code{\link{.violin_plot}},
#' \code{\link{.square_plot}},
#' \code{\link{.define_colorby_for_row_plot}},
#' \code{\link{.define_colorby_for_column_plot}}
.add_color_to_column_plot <- function(colorby, param_choices, x_aes="X", y_aes="Y") {
    if (is.null(colorby)) {
        return(NULL)
    }

    cmds <- NULL
    color_choice <- param_choices[[.colorByField]]

    # This slightly duplicates the work in .define_colorby_for_row_plot(),
    # but this is necessary to separate the function of data acquisition and plot generation.
    if (color_choice == .colorByColDataTitle) {
        covariate_name <- param_choices[[.colorByColData]]
        cmds <- .create_color_scale("colDataColorMap", deparse(covariate_name), colorby)

    } else if (color_choice == .colorByFeatNameTitle) {
        assay_choice <- param_choices[[.colorByFeatNameAssay]]
        cmds <- .create_color_scale("assayColorMap", deparse(assay_choice), colorby)

    } else if (color_choice == .colorBySampNameTitle) {
        col_choice <- param_choices[[.colorBySampNameColor]]
        cmds <- c(
            sprintf(
                "scale_color_manual(values=c(`FALSE`='black', `TRUE`=%s), drop=FALSE) +",
                deparse(col_choice)),
            sprintf(
                "geom_point(aes(x=%s, y=%s), data=subset(plot.data, ColorBy == 'TRUE'), col=%s, alpha=1%s) +",
                x_aes, y_aes, deparse(col_choice),
                ifelse(param_choices[[.sizeByField]] == .sizeByNothingTitle,
                       paste0(", size=5*", param_choices[[.plotPointSize]]),
                       ""))
        )
    }

    return(cmds)
}

#' @rdname INTERNAL_add_color_scale
#' @importFrom ggplot2 scale_color_manual geom_point
.add_color_to_row_plot <- function(colorby, param_choices, x_aes="X", y_aes="Y") {
    if (is.null(colorby)) {
        return(NULL)
    }

    cmds <- NULL
    color_choice <- param_choices[[.colorByField]]

    # This slightly duplicates the work in .define_colorby_for_row_plot(),
    # but this is necessary to separate the function of data acquisition and plot generation.
    if (color_choice == .colorByRowDataTitle) {
        covariate_name <- param_choices[[.colorByRowData]]
        cmds <- .create_color_scale("rowDataColorMap", deparse(covariate_name), colorby)

    } else if (color_choice == .colorByFeatNameTitle) {
        col_choice <- param_choices[[.colorByFeatNameColor]]
        cmds <- c(
            sprintf(
                "scale_color_manual(values=c(`FALSE`='black', `TRUE`=%s), drop=FALSE) +",
                deparse(col_choice)),
            sprintf(
                "geom_point(aes(x=%s, y=%s), data=subset(plot.data, ColorBy == 'TRUE'), col=%s, alpha=1%s) +",
                x_aes, y_aes, deparse(col_choice),
                ifelse(param_choices[[.sizeByField]] == .sizeByNothingTitle,
                       paste0(", size=5*", param_choices[[.plotPointSize]]),
                       ""))
        )

    } else if (color_choice == .colorBySampNameTitle) {
        assay_choice <- param_choices[[.colorBySampNameAssay]]
        cmds <- .create_color_scale("assayColorMap", deparse(assay_choice), colorby)
    }
    return(cmds)
}

#' Choose between discrete and continuous color scales
#'
#' Generates a ggplot \code{color_scale} command depending on the number of
#' levels in the coloring variable.
#'
#' @param command A string containing an ExperimentColorMap accessor.
#' @param choice An argument to pass to the accessor in \code{command} to
#' specify the colormap to use.
#' @param colorby A vector of values to color points by, taken from
#' \code{plot.data$ColorBy} in upstream functions.
#'
#' @return A string containing an appropriate ggplot \code{color_scale}
#' command.
#'
#' @details
#' The appropriate ggplot coloring command will depend on whether
#' \code{colorby} is categorical or not.
#' If it is, \code{\link{scale_color_manual}} is used with the appropriate
#' number of levels.
#' Otherwise, \code{\link{scale_color_gradientn}} is used.
#' The \code{discrete=} argument of the accessor in \code{command} will also
#' be set appropriately.
#'
#' @author Kevin Rue-Albrecht, Aaron Lun, Charlotte Soneson.
#' @rdname INTERNAL_create_color_scale
#' @seealso
#' \code{\link{.add_color_to_row_plot}},
#' \code{\link{.add_color_to_column_plot}}
#'
#' @importFrom ggplot2 scale_color_manual scale_fill_manual
#' scale_color_gradientn scale_fill_gradientn
.create_color_scale <- function(command, choice, colorby) {
    discrete_color <- is.factor(colorby)
    if (discrete_color) {
        ncolors <- nlevels(colorby)
    } else {
        ncolors <- 21L
    }

    cm_cmd <- sprintf(
        "%s(colormap, %s, discrete=%s)(%i)",
        command, choice, discrete_color, ncolors)

    if (discrete_color){
        return(c(
            sprintf(
                "scale_color_manual(values=%s, na.value='grey50', drop=FALSE) +",
                cm_cmd),
            sprintf(
                "scale_fill_manual(values=%s, na.value='grey50', drop=FALSE) +",
                cm_cmd)))
    } else {
        return(c(
            sprintf(
                "scale_color_gradientn(colors=%s, na.value='grey50', limits=range(plot.data$ColorBy, na.rm=TRUE)) +",
                cm_cmd)#,
            # sprintf(
            #     "scale_fill_gradientn(colors=%s, na.value='grey50') +",
            #     cm_cmd)
        ))
    }
}

############################################
# Internal functions: shaping ----
############################################

#' Define shaping variables
#'
#' Generates the commands necessary to define the variables to shape by in the data.frame to be supplied to ggplot.
#'
#' @param param_choices A single-row DataFrame that contains all the input settings for the current panel.
#' @param se A SingleCellExperiment object.
#'
#' @details
#' These functions generate commands to extract the variable to use for shaping individual points in row- or column-based plots,
#' i.e., where each point is a feature or sample, respectively.
#' The commands should be evaluated in the evaluation environment generated in \code{\link{.extract_plotting_data}}.
#'
#' In these commands, the shaping variable is added to the \code{plot.data} data.frame in the \code{ShapeBy} field.
#' This is distinct from \code{.add_shape_to_*_plot}, which generates the commands for shaping a ggplot by the values in \code{ColourBy}.
#'
#' @return
#' A list containing \code{label}, a string to use for labelling the shape scale;
#' and \code{cmds}, a character vector of commands to add a \code{ColorBy} field to \code{plot.data}.
#' Alternatively \code{NULL}, if no variable to shape by is specified.
#'
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_define_shape_variables
#' @seealso
#' \code{\link{.extract_plotting_data}}
#'
#' @importFrom SummarizedExperiment assay
.define_shapeby_for_column_plot <- function(param_choices, se) {
    shape_choice <- param_choices[[.shapeByField]]

    if (shape_choice == .shapeByColDataTitle) {
        covariate_name <- param_choices[[.shapeByColData]]
        return(list(label=covariate_name,
            cmds=sprintf("plot.data$ShapeBy <- colData(se)[, %s];", deparse(covariate_name))))

    } else {
        return(NULL)
    }
}

#' @rdname INTERNAL_define_shape_variables
.define_shapeby_for_row_plot <- function(param_choices, se) {
    shape_choice <- param_choices[[.shapeByField]]

    if (shape_choice == .shapeByRowDataTitle) {
        covariate_name <- param_choices[[.shapeByRowData]]
        return(list(label=covariate_name,
            cmds=sprintf("plot.data$ShapeBy <- rowData(se)[, %s];", deparse(covariate_name))))

    } else {
        return(NULL)
    }
}

############################################
# Internal functions: sizing ----
############################################

#' Define sizing variables
#'
#' Generates the commands necessary to define the variables to size by in the data.frame to be supplied to ggplot.
#'
#' @param param_choices A single-row DataFrame that contains all the input settings for the current panel.
#' @param se A SingleCellExperiment object.
#'
#' @details
#' These functions generate commands to extract the variable to use for sizing individual points in row- or column-based plots,
#' i.e., where each point is a feature or sample, respectively.
#' The commands should be evaluated in the evaluation environment generated in \code{\link{.extract_plotting_data}}.
#'
#' In these commands, the sizing variable is added to the \code{plot.data} data.frame in the \code{SizeBy} field.
#' This is distinct from \code{.add_size_to_*_plot}, which generates the commands for sizing a ggplot by the values in \code{SizeBy}.
#'
#' @return
#' A list containing \code{label}, a string to use for labelling the size scale;
#' and \code{cmds}, a character vector of commands to add a \code{SizeBy} field to \code{plot.data}.
#' Alternatively \code{NULL}, if no variable to size by is specified.
#'
#' @author Charlotte Soneson
#' @rdname INTERNAL_define_size_variables
#' @seealso
#' \code{\link{.extract_plotting_data}}
#'
#' @importFrom SummarizedExperiment assay
.define_sizeby_for_column_plot <- function(param_choices, se) {
    size_choice <- param_choices[[.sizeByField]]

    if (size_choice == .sizeByColDataTitle) {
        covariate_name <- param_choices[[.sizeByColData]]
        return(list(label=covariate_name,
                    cmds=sprintf("plot.data$SizeBy <- colData(se)[, %s];", deparse(covariate_name))))

    } else {
        return(NULL)
    }
}

#' @rdname INTERNAL_define_size_variables
.define_sizeby_for_row_plot <- function(param_choices, se) {
    size_choice <- param_choices[[.sizeByField]]

    if (size_choice == .sizeByRowDataTitle) {
        covariate_name <- param_choices[[.sizeByRowData]]
        return(list(label=covariate_name,
                    cmds=sprintf("plot.data$SizeBy <- rowData(se)[, %s];", deparse(covariate_name))))

    } else {
        return(NULL)
    }
}

############################################
# Internal functions: Point selection ----
############################################

#' Process point selection choice
#'
#' @param param_choices A single-row DataFrame that contains all the input settings for the current panel.
#' @param all_memory A list of DataFrames, where each DataFrame corresponds to a panel type and contains the settings for each individual panel of that type.
#' @param self_source A logical scalar indicating whether it is allowable to select points based on coordinates in \code{plot.data}.
#'
#' @return A list containing:
#' \itemize{
#' \item \code{cmds}, a character vector of commands that results in the addition of a \code{SelectBy} covariate column in the \code{plot.data} data.frame.
#' If no selection should be applied, this is set to \code{NULL}.
#' \item \code{transmitter}, a list containing the encoded plot name and ID of the transmitting plot for the current panel (see \code{\link{.encode_panel_name}}).
#' If no selection should be applied, this is set to \code{NULL}.
#' }
#'
#' @details
#' For the current panel, this function identifies the transmitting panel, i.e., from which the current panel is receiving a selection of data points.
#' It then generates the commands necessary to identify the points selected in the transmitter, to add as \code{SelectBy} in the current panel.
#' This requires extraction of the Shiny brush or lasso waypoints in the transmitter, which are used during evaluation to define the selection.
#'
#' If selecting to restrict, an extra \code{plot.data.all} variable will be generated in the evaluation environment.
#' This will be used in \code{\link{.scatter_plot}} and \code{\link{.violin_plot}} to define the boundaries of the plot based on the full data.
#' In this manner, the boundaries of the plot are kept consistent even when only a subset of the data are used to generate the ggplot object.
#'
#' Setting \code{self_source=FALSE} is used in \code{\link{.get_selected_points}} to force it to use existing coordinates to define \code{SelectBy}.
#'
#' Evaluation of the output commands require:
#' \itemize{
#' \item a list object called \code{all_brushes} where each entry is named by the plot name.
#' The entry corresponding to the transmitting plot should contain the contents of \code{.brushData} in its parameter set in \code{all_memory}.
#' \item a list object called \code{all_lassos} where each entry is named by the plot name.
#' The entry corresponding to the transmitting plot should contain the contents of \code{.lassoData} in its parameter set in \code{all_memory}.
#' \item a list object called \code{all_select_histories} where each entry is named by the plot name.
#' The entry corresponding to the transmitting plot should contain the contents of \code{.multiSelectHistory} in its parameter set in \code{all_memory}.
#' }
#' All of these objects should exist in the environment in which the commands are evaluated.
#'
#' @author Kevin Rue-Albrecht, Aaron Lun.
#' @rdname INTERNAL_process_selectby_choice
#' @seealso
#' \code{\link{.extract_plotting_data}},
#' \code{\link{brushedPoints}},
#' \code{\link{in.out}}
#'
#' @importFrom mgcv in.out
#' @importFrom shiny brushedPoints
.process_selectby_choice <- function(param_choices, all_memory, self_source=TRUE) {
    select_in <- param_choices[[.selectByPlot]]
    cmds <- list()
    select_by <- NULL

    if (!identical(select_in, .noSelection)) {

        select_by <- .encode_panel_name(select_in)
        transmitter <- paste0(select_by$Type, select_by$ID)
        if (self_source && identical(rownames(param_choices), transmitter)) {
            source_data <- 'plot.data'
        } else {
            source_data <- sprintf("all_coordinates[['%s']]", transmitter)
        }

        transmit_type_param <- all_memory[[select_by$Type]]

        cur_choice <- param_choices[, .selectMultiType]
        if (cur_choice == .selectMultiUnionTitle) {
            select_sources <- c(NA_integer_, seq_along(transmit_type_param[, .multiSelectHistory][[1]]))
        } else if (cur_choice == .selectMultiActiveTitle) {
            select_sources <- NA_integer_
        } else {
            select_sources <- param_choices[, .selectMultiSaved]
            if (select_sources == 0L) {
                # '0' selection in memory means no selection.
                select_sources <- integer(0)
            }
        }

        starting <- TRUE
        for (i in select_sources) {
            if (is.na(i)) {
                brush_val <- transmit_type_param[, .brushData][[select_by$ID]]
                lasso_val <- transmit_type_param[, .lassoData][[select_by$ID]]
                brush_src <- sprintf("all_brushes[['%s']]", transmitter)
                lasso_src <- sprintf("all_lassos[['%s']]", transmitter)
                use_brush <- !is.null(brush_val)
                use_lasso <- !is.null(lasso_val) && lasso_val$closed
            } else {
                all_histories <- transmit_type_param[, .multiSelectHistory][[select_by$ID]]
                brush_val <- lasso_val <- all_histories[[i]]
                brush_src <- lasso_src <- sprintf("all_select_histories[['%s']][[%i]]", transmitter, i)

                # TODO: get rid of some of these checks once UI uses selectInput for .selectMultiSaved.
                use_brush <- !is.null(brush_val) && !any(is.na(brush_val)) && !.is_lasso(brush_val)
                use_lasso <- !is.null(lasso_val) && !any(is.na(lasso_val)) && .is_lasso(lasso_val)
            }

            if (use_brush || use_lasso) {
                if (starting) {
                    starting <- FALSE
                    LEFT <- RIGHT <- ""
                } else {
                    LEFT <- "union(selected_pts, "
                    RIGHT <- ")"
                }
            }

            if (use_brush) {
                cmds[[paste0("brush", i)]] <- sprintf("selected_pts <- %srownames(shiny::brushedPoints(%s, %s))%s;", LEFT, source_data, brush_src, RIGHT)
            } else if (use_lasso) {
                cmds[[paste0("lasso", i)]] <- sprintf("selected_pts <- %srownames(lassoPoints(%s, %s))%s;", LEFT, source_data, lasso_src, RIGHT)
            }
        }

        if (length(cmds)) {
            cmds[["select"]] <- "plot.data$SelectBy <- rownames(plot.data) %in% selected_pts;"

            if (param_choices[[.selectEffect]] == .selectRestrictTitle) {
                cmds[["saved"]] <- "plot.data.all <- plot.data;"
                cmds[["subset"]] <- "plot.data <- subset(plot.data, SelectBy);"
            }
        }
    }

    list(cmds=unlist(cmds), transmitter=select_by)
}

#' Populate selection structures
#'
#' Populate the environment with data structures required for selection.
#'
#' @param param_choices A single-row DataFrame that contains all the input settings for the current panel.
#' @param envir An environment produced by \code{\link{.extract_plotting_data}}.
#'
#' @details
#' This function provides a convenient wrapper to add Shiny brush and lasso objects from any panel to the evaluation environment.
#' These are needed for certain commands to be executed (see the \dQuote{See also} section below).
#'
#' @return \code{all_brushes}, \code{all_lassos} and \code{all_select_histories} are added to \code{envir} using pass-by-reference behaviour of environments.
#' A\code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun.
#' @rdname INTERNAL_populate_selection_environment
#' @seealso
#' \code{\link{.process_selectby_choice}},
#' \code{\link{.self_brush_box}},
#' \code{\link{.self_lasso_path}}
.populate_selection_environment <- function(param_choices, envir) {
    envir$all_brushes <- list(param_choices[, .brushData][[1]])
    envir$all_lassos <- list(param_choices[, .lassoData][[1]])
    envir$all_select_histories <- list(param_choices[, .multiSelectHistory][[1]])
    names(envir$all_brushes) <- names(envir$all_lassos) <- names(envir$all_select_histories) <- rownames(param_choices)
    invisible(NULL)
}

#' Add points to plot
#'
#' Generate ggplot commands to control the appearance of data points while
#' accounting for a point selection effect, if active.
#'
#' @param param_choices A single-row DataFrame that contains all the
#' input settings for the current panel.
#' @param selected A logical scalar indicating whether any points were
#' selected on the transmitting plot, via a Shiny brush or lasso path.
#' @param aes A string containing the ggplot aesthetic instructions.
#' @param color A logical scalar indicating whether coloring information is
#'   already included in the \code{aes}.
#' @param size A logical scaler indicating whether sizing information is already
#'   included in the \code{aes}.
#'
#' @return A character vector containing ggplot commands to add points
#' to the plot.
#'
#' @details
#' Addition of point commands is done via \code{geom_point} on the
#' X/Y coordinates (in the \code{plot.data} of the evaluation environment).
#' This involves some work to highlight selected data points.
#' Any color specifications are passed in via \code{aes}.
#'
#' When selecting points to restrict,
#' this function relies on the availability of a
#' \code{plot.data.all} variable in the evaluation environment.
#' See \code{?\link{.process_selectby_choice}} for more details.
#'
#' A separate \code{selected} argument is necessary here, despite the fact
#' that most point selection information can be retrieved from
#' \code{param_choices},
#' This is because \code{param_choices} does not contain any information on
#' whether the transmitter actually contains a selection of points.
#' If no Shiny select or closed lasso path is defined in the transmitter,
#' \code{selected=FALSE} and the default appearance of the points is used.
#'
#' @author Kevin Rue-Albrecht, Aaron Lun.
#' @rdname INTERNAL_create_points
#' @seealso
#' \code{\link{.scatter_plot}},
#' \code{\link{.violin_plot}},
#' \code{\link{.square_plot}}
#'
#' @importFrom ggplot2 geom_point geom_blank
.create_points <- function(param_choices, selected, aes, color, size) {
    plot_cmds <- list()

    # If there is already coloring information available in the aes, don't add an
    # additional color= statement to the geom_point() command, since this will
    # overrule the one given in aes().
    if (color) {
        default_color <- ""
    } else {
        default_color <- sprintf(", color='%s'", param_choices[[.colorByDefaultColor]])
    }

    ## If there is already size information available in the aes, don't add an
    ## additional size=statement to the geom_point() command.
    if (size) {
        common_size <- ""
    } else {
        common_size <- sprintf(", size=%s", param_choices[[.plotPointSize]])
    }

    if (selected) {
        select_effect <- param_choices[[.selectEffect]]
        if (select_effect == .selectColorTitle) {
            plot_cmds[["select_other"]] <- sprintf(
                "geom_point(%s, alpha=%s, data=subset(plot.data, !SelectBy)%s%s) +",
                aes, param_choices[[.plotPointAlpha]], default_color,
                common_size
                # param_choices[[.plotPointSize]]
            )
            plot_cmds[["select_color"]] <- sprintf(
                "geom_point(%s, alpha=%s, data=subset(plot.data, SelectBy), color=%s%s) +",
                aes, param_choices[[.plotPointAlpha]],
                deparse(param_choices[[.selectColor]]), common_size
                # param_choices[[.plotPointSize]]
            )
        }
        if (select_effect == .selectTransTitle) {
            plot_cmds[["select_other"]] <- sprintf(
                "geom_point(%s, subset(plot.data, !SelectBy), alpha=%.2f%s%s) +",
                aes, param_choices[[.selectTransAlpha]], default_color,
                common_size
                # param_choices[[.plotPointSize]]
            )
            plot_cmds[["select_alpha"]] <- sprintf(
                "geom_point(%s, subset(plot.data, SelectBy)%s%s) +",
                aes, default_color, common_size
                # param_choices[[.plotPointSize]]
            )
        }
        if (select_effect == .selectRestrictTitle) {
            plot_cmds[["select_restrict"]] <- sprintf(
                "geom_point(%s, alpha=%s, plot.data%s%s) +",
                aes, param_choices[[.plotPointAlpha]], default_color,
                common_size
                # param_choices[[.plotPointSize]]
            )
        }
    } else {
        plot_cmds[["point"]] <- sprintf(
            "geom_point(%s, alpha=%s, plot.data%s%s) +",
            aes, param_choices[[.plotPointAlpha]], default_color,
            common_size
            # param_choices[[.plotPointSize]]
        )
    }

    return(unlist(plot_cmds))
}

############################################
# Internal functions: aesthetics ----
############################################

#' @title Assay axis labels
#'
#' @description
#' Generate an axis label when assay data is used for colouring row- or column-based plots.
#'
#' @param se A \linkS4class{SingleCellExperiment} object.
#' @param feature_id A integer index of the feature in \code{rownames(se)}.
#' @param sample_id A integer index of the sample in \code{colnames(se)}.
#' @param name A string containing the feature or sample name.
#' @param assay_id The integer index of an assay in \code{assayNames(se)}.
#' If \code{NULL}, only the feature name is reported.
#' @param multiline A logical value that indicates whether feature/sample and assay names should appear on separate lines.
#'
#' @return A character value to use as axis label.
#'
#' @author Kevin Rue-Albrecht, Aaron Lun.
#' @rdname INTERNAL_assay_axis_label
#' @importFrom BiocGenerics rownames
#' @seealso
#' \code{\link{.make_featAssayPlot}},
#' \code{\link{.make_sampAssayPlot}},
#' \code{\link{.add_color_to_column_plot}},
#' \code{\link{.add_color_to_row_plot}}
.feature_axis_label <- function(se, feature_id, assay_id, multiline=FALSE){
    .assay_axis_label(se, rownames(se)[feature_id], assay_id, multiline=multiline)
}

#' @rdname INTERNAL_assay_axis_label
#' @importFrom BiocGenerics colnames
.sample_axis_label <- function(se, sample_id, assay_id, multiline=FALSE){
    .assay_axis_label(se, colnames(se)[sample_id], assay_id, multiline=multiline)
}

#' @rdname INTERNAL_assay_axis_label
#' @importFrom SummarizedExperiment assayNames
.assay_axis_label <- function(se, name, assay_id, multiline=FALSE) {
    if (is.null(assay_id)) {
        return(name)
    }

    assay_name <- assayNames(se)[assay_id]
    if (is.null(assay_name) || identical(assay_name, "")) {
        assay_name <- paste("assay", assay_id)
    }

    sep <- ifelse(multiline, "\n", " ")
    sprintf("%s%s(%s)", name, sep, assay_name)
}

#' Generate ggplot aesthetic instructions
#'
#' @param x A \code{logical} that indicates whether to enable \code{x} in the
#' aesthetic instructions (default: \code{TRUE}).
#' @param y A \code{logical} that indicates whether to enable \code{y} in the
#' aesthetic instructions (default: \code{TRUE}).
#' @param color A \code{logical} that indicates whether to enable
#' \code{color} in the aesthetic instructions (default: \code{FALSE}).
#' @param shape A \code{logical} that indicates whether to enable
#' \code{shape} in the aesthetic instructions (default: \code{FALSE}).
#' @param size A \code{logical} that indicates whether to enable
#' \code{size} in the aesthetic instructions (default: \code{FALSE}).
#' @param fill A \code{logical} that indicates whether to enable
#' \code{fill} in the aesthetic instructions (default: \code{FALSE}).
#' @param group A \code{logical} that indicates whether to enable
#' \code{group} in the aesthetic instructions (default: \code{FALSE}).
#' @param alt Alternative aesthetics, supplied as a named character vector.
#'
#' @return Aesthetic instructions for \code{\link{ggplot}} as a character
#' value.
#'
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_build_aes
#' @seealso
#' \code{\link{.scatter_plot}},
#' \code{\link{.violin_plot}},
#' \code{\link{.square_plot}}
#'
#' @importFrom ggplot2 aes
.build_aes <- function(
    x=TRUE, y=TRUE, color=FALSE, shape=FALSE, size=FALSE, fill=FALSE,
    group=FALSE, alt=NULL) {
    active_aes <- .all_aes_values[c(x, y, color, shape, size, fill, group)]
    if (!is.null(alt)) {
        active_aes <- c(active_aes, alt)
        active_aes <- active_aes[!duplicated(names(active_aes), fromLast=TRUE)]
    }
    aes_specs <- mapply(
        FUN=.make_single_aes, names(active_aes), active_aes, USE.NAMES=FALSE)
    aes_specs <- paste(aes_specs, collapse=", ")
    return(sprintf("aes(%s)", aes_specs))
}

#' Generate a single aesthetic instruction for ggplot
#'
#' @param name The name of a ggplot aesthetic.
#' @param value The name of a column in the plot data that will be mapped to
#' the aesthetic declared in \code{name}.
#'
#' @return A character value of the form \code{name=value}.
#'
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_make_single_aes
#' @seealso
#' \code{\link{.build_aes}}.
.make_single_aes <- function(name, value){
    sprintf("%s=%s", name, value)
}

#' Generate ggplot title and label instructions
#'
#' @param x The character label for the horizontal axis.
#' @param y x The character label for the vertical axis.
#' @param color The character title for the color scale legend.
#' @param shape The character title for the point shape legend.
#' @param size The character title for the point size legend.
#' @param fill The character title for the color fill legend.
#' @param group The character title for the group legend.
#' @param title The character title for the plot title.
#' @param subtitle The character title for the plot subtitle
#'
#' @details
#' If any argument is \code{NULL}, the corresponding label is not set.
#'
#' @return Title and label instructions for \code{\link{ggplot}} as a character value.
#'
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_build_labs
#' @seealso
#' \code{\link{.scatter_plot}},
#' \code{\link{.violin_plot}},
#' \code{\link{.square_plot}}
#'
#' @importFrom ggplot2 labs
.build_labs <- function(x=NULL, y=NULL, color=NULL, shape=NULL, size=NULL, fill=NULL, group=NULL, title=NULL, subtitle=NULL){
    labs_specs <- list(x, y, color, shape, size, fill, group, title, subtitle)
    names(labs_specs) <- .all_labs_names
    labs_specs <- labs_specs[lengths(labs_specs)>0L]
    if (identical(length(labs_specs), 0L)){
        return(NULL)
    }
    labs_specs <- mapply(FUN=.make_single_lab, names(labs_specs), labs_specs, USE.NAMES=FALSE)
    labs_specs <- paste(labs_specs, collapse=", ")
    return(sprintf("labs(%s) +", labs_specs))
}

#' Generate a single title or label instruction for ggplot
#'
#' @param name The name of a ggplot label.
#' @param value A character value for the title or label declared in
#' \code{name}.
#'
#' @return A character value of the form \code{name=value}.
#'
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_make_single_lab
#' @seealso
#' \code{\link{.build_labs}}.
.make_single_lab <- function(name, value){
    sprintf("%s=%s", name, deparse(value))
}

############################################
# Internal functions: grouping ----
############################################

#' Coerce data to a specific type
#'
#' This function ensures that a specific column of the \code{plot.data} data.frame is either a numeric or factor.
#' If that is not the case, it returns a command (as a string) that coerces the column into the desired type.
#'
#' @param values Input vector that must be coerced to \code{numeric}.
#' @param field Column name in the \code{plot.data} data.frame that contains \code{values}.
#' @param as_numeric A logical scalar indicating whether the column should be coerced to numeric (if \code{TRUE}) or factor (otherwise).
#'
#' @return A command that coerces the plot data.frame column to the specified type, or \code{NULL} if no coercion is required.
#'
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_coerce_type
#' @seealso
#' \code{\link{.create_plot}}.
.coerce_type <- function(values, field, as_numeric=TRUE) {
    if (as_numeric) {
        if (!is.numeric(values)) {
            warning("coloring covariate has too many unique values, coercing to numeric")
            col_var <- sprintf("plot.data$%s", field)
            if (!is.factor(values)) {
                col_var <- sprintf("as.factor(%s)", col_var)
            }
            return(sprintf("plot.data$%s <- as.numeric(%s);", field, col_var))
        }
    } else {
        if (!is.factor(values)) {
            return(sprintf("plot.data$%s <- factor(plot.data$%s);", field, field))
        }
    }
    return(NULL)
}

############################################
# Internal functions: faceting ----
############################################

#' Define faceting variables
#'
#' Generates the commands necessary to define the variables to facet by in the data.frame to be supplied to ggplot.
#'
#' @param param_choices A single-row DataFrame that contains all the input settings for the current panel.
#' @param se A SingleCellExperiment object.
#'
#' @return
#' A character vector of commands to add \code{FacetRow} and/or \code{FacetColumn} field to \code{plot.data}, as required.
#'
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_define_facet_variables
#' @seealso
#' \code{\link{.extract_plotting_data}},
#' \code{\link{.add_color_to_column_plot}}
#'
#' @importFrom SummarizedExperiment colData
.define_facetby_for_column_plot <- function(param_choices, se) {
    facet_cmds <- c()

    facet_row <- param_choices[[.facetRowsByColData]]
    if (param_choices[[.facetByRow]]) {
        facet_cmds["FacetRow"] <- sprintf(
            "plot.data$FacetRow <- colData(se)[, %s];", deparse(facet_row))
    }

    facet_column <- param_choices[[.facetColumnsByColData]]
    if (param_choices[[.facetByColumn]]) {
        facet_cmds["FacetColumn"] <- sprintf(
            "plot.data$FacetColumn <- colData(se)[, %s];", deparse(facet_column))
    }

    return(facet_cmds)
}

#' @rdname INTERNAL_define_facet_variables
.define_facetby_for_row_plot <- function(param_choices, se) {
    facet_cmds <- c()

    facet_row <- param_choices[[.facetRowsByRowData]]
    if (param_choices[[.facetByRow]]) {
        facet_cmds["FacetRow"] <- sprintf(
            "plot.data$FacetRow <- rowData(se)[, %s];", deparse(facet_row))
    }

    facet_column <- param_choices[[.facetColumnsByRowData]]
    if (param_choices[[.facetByColumn]]) {
        facet_cmds["FacetColumn"] <- sprintf(
            "plot.data$FacetColumn <- rowData(se)[, %s];", deparse(facet_column))
    }

    return(facet_cmds)
}


#' Process faceting choices
#'
#' Generate ggplot instructions to facet a plot by row and/or column
#'
#' @param param_choices A single-row DataFrame that contains all the
#' input settings for the current panel.
#'
#' @return A string containing a command to define the row and column faceting
#' covariates.
#' @author Kevin Rue-Albrecht.
#' @rdname INTERNAL_add_facets
#' @seealso
#' \code{\link{.define_facetby_for_column_plot}}
#'
#' @importFrom ggplot2 facet_grid
.add_facets <- function(param_choices){

    if (!param_choices[[.facetByRow]] && !param_choices[[.facetByColumn]]) {
        return(NULL)
    }

    facet_x <- ifelse(param_choices[[.facetByRow]], "FacetRow", ".")
    facet_y <- ifelse(param_choices[[.facetByColumn]], "FacetColumn", ".")

    facet_cmd <- sprintf("facet_grid(%s ~ %s)", facet_x, facet_y)

    return(facet_cmd)
}

############################################
# Plot update functions ----
############################################

#' Draw Shiny brushes
#'
#' Generate ggplot instructions to draw a rectangular box corresponding to Shiny brush coordinates (both active and saved) in the current plot.
#'
#' @param param_choices A single-row DataFrame that contains all the input settings for the current panel.
#' @param flip A \code{logical} value that indicates whether \code{\link{coord_flip}} was applied to the plot.
#'
#' @return A character vector containing a command to overlay one or more rectangles on the plot, indicating the position of the active and saved Shiny brushes.
#'
#' @details
#' Evaluation of the output commands require:
#' \itemize{
#' \item a list object called \code{all_brushes} where each entry is named by the plot name.
#' The entry corresponding to the current plot should contain the contents of \code{.brushData} in \code{param_choices}.
#' \item a list object called \code{all_select_histories} where each entry is named by the plot name.
#' The entry corresponding to the current plot should contain the contents of \code{.multiSelectHistory} in \code{param_choices}.
#' }
#' Both of these objects should exist in the environment in which the commands are evaluated.
#'
#' @author Kevin Rue-Albrecht, Aaron Lun.
#' @rdname INTERNAL_self_brush_box
#' @seealso
#' \code{\link{.create_plot}}
#'
#' @importFrom ggplot2 geom_rect geom_text
.self_brush_box <- function(param_choices, flip=FALSE) {
    active <- param_choices[, .brushData][[1]]
    saved <- param_choices[, .multiSelectHistory][[1]]

    keep <- which(!vapply(saved, .is_lasso, FUN.VALUE=TRUE))
    total <- as.integer(!is.null(active)) + length(keep)
    if (total == 0L) {
        return(NULL)
    }

    if (flip) {
        xmin <- 'ymin'
        xmax <- 'ymax'
        ymin <- 'xmin'
        ymax <- 'xmax'
    } else {
        xmin <- 'xmin'
        xmax <- 'xmax'
        ymin <- 'ymin'
        ymax <- 'ymax'
    }

    # Note: Faceting simultaneously on row and column produces a 'flip' effect on the brush data
    if (param_choices[[.facetByRow]] && param_choices[[.facetByColumn]]) {
        facetrow <- 'panelvar2'
        facetcolumn <- 'panelvar1'
    } else {
        facetrow <- facetcolumn <- 'panelvar1'
    }

    plot_name <- rownames(param_choices)
    enc <- .split_encoded(plot_name)
    stroke_color <- panel_colors[enc$Type]
    fill_color <- brush_fill_color[enc$Type]

    # Build up the aesthetics call
    aes_call <- sprintf("xmin=%s, xmax=%s, ymin=%s, ymax=%s", xmin, xmax, ymin, ymax)

    cmds <- character(0)
    for (i in seq_len(total) - !is.null(active)) {
        if (i == 0L) {
            brush_src <- sprintf("all_brushes[['%s']]", plot_name)
        } else {
            brush_src <- sprintf("all_select_histories[['%s']][[%i]]", plot_name, keep[i])
        }

        # Initialize the minimal brush information
        brush_data <- sprintf("%s[c('xmin', 'xmax', 'ymin', 'ymax')]", brush_src)

        # Collect additional panel information for the brush
        addPanels <- c()
        if (param_choices[[.facetByRow]]) {
            addPanels["FacetRow"] <- sprintf("FacetRow=%s[['%s']]", brush_src, facetrow)
        }
        if (param_choices[[.facetByColumn]]) {
            addPanels["FacetColumn"] <- sprintf("FacetColumn=%s[['%s']]", brush_src, facetcolumn)
        }

        # If any facting (row, column) is active, add the relevant data fields
        if (length(addPanels)) {
            panel_list <- sprintf("list(%s)", paste(addPanels, collapse=", "))
            brush_data <- sprintf("append(%s, %s)", brush_data, panel_list)
        }

        # Build up the command that draws the brush
        brush_draw_cmd <- sprintf(
"geom_rect(aes(%s), color='%s', alpha=%s, fill='%s',
    data=do.call(data.frame, %s),
    inherit.aes=FALSE)",
            aes_call, stroke_color, .brushFillOpacity, fill_color, brush_data)

        # Put a number for saved brushes.
        if (i!=0L) {
            text_data <- c(sprintf("x=mean(unlist(%s[c('%s', '%s')]))", brush_src, xmin, xmax),
				sprintf("y=mean(unlist(%s[c('%s', '%s')]))", brush_src, ymin, ymax),
				addPanels)

            text_cmd <- sprintf(
"geom_text(aes(x=x, y=y), inherit.aes=FALSE,
	data=data.frame(
		%s),
    label=%i, size=%s, colour='%s')",
				paste(text_data, collapse=",\n        "),
                keep[i], param_choices[[.plotFontSize]] * .plotFontSizeLegendTextDefault, stroke_color)
            brush_draw_cmd <- c(brush_draw_cmd, text_cmd)
        }

        cmds <- c(cmds, brush_draw_cmd)
    }

    cmds
}

#' Generate ggplot instructions to draw a lasso selection path
#'
#' @param param_choices A single-row DataFrame that contains all the input settings for the current panel.
#' @param flip A \code{logical} value that indicates whether \code{\link{coord_flip}} was applied to the plot.
#'
#' @return A character vector containing commands to overlay a point, path or polygon, indicating the position of any active or saved lassos.
#'
#' @details
#' This function will generate commands to add a point to the plot, if there is only one lasso waypoint defined;
#' a path, if multiple waypoints are defined but the lasso is not yet closed;
#' or a polygon, if multiple waypoints are defined for a closed lasso.
#'
#' The starting point of open lassos is distinguished from the waypoints using a shape aesthetic;
#' with one exception, if the shape aesthetic is already being mapped to a covariate for data points,
#' then lasso points switch to the size aesthetic.
#'
#' Evaluation of the output commands require:
#' \itemize{
#' \item a list object called \code{all_lassos} where each entry is named by the plot name.
#' The entry corresponding to the current plot should contain the contents of \code{.lassoData} in \code{param_choices}.
#' \item a list object called \code{all_select_histories} where each entry is named by the plot name.
#' The entry corresponding to the current plot should contain the contents of \code{.multiSelectHistory} in \code{param_choices}.
#' }
#' Both of these objects should exist in the environment in which the commands are evaluated.
#'
#' @author Kevin Rue-Albrecht, Aaron Lun.
#' @rdname INTERNAL_self_lasso_path
#' @seealso
#' \code{\link{.create_plot}}
#'
#' @importFrom ggplot2 geom_point geom_polygon geom_path scale_shape_manual
#' scale_fill_manual guides
.self_lasso_path <- function(param_choices, flip=FALSE) {
    active <- param_choices[, .lassoData][[1]]
    saved <- param_choices[, .multiSelectHistory][[1]]

    keep <- which(vapply(saved, .is_lasso, FUN.VALUE=TRUE))
    total <- as.integer(!is.null(active)) + length(keep)
    if (total == 0L) {
        return(NULL)
    }

    plot_name <- rownames(param_choices)
    enc <- .split_encoded(plot_name)
    stroke_color <- panel_colors[enc$Type]
    fill_color <- brush_fill_color[enc$Type]

    # Note: Faceting simultaneously on row and column produces a 'flip' effect on the lasso data
    if (param_choices[[.facetByRow]] && param_choices[[.facetByColumn]]) {
        facetrow <- 'panelvar2'
        facetcolumn <- 'panelvar1'
    } else {
        facetrow <- facetcolumn <- 'panelvar1'
    }

    cmds <- character(0)
    firstClosed <- TRUE
    for (i in seq_len(total) - !is.null(active)) {
        if (i == 0L) {
            lasso_src <- sprintf("all_lassos[['%s']]", plot_name)
            current <- active
        } else {
            chosen <- keep[i]
            lasso_src <- sprintf("all_select_histories[['%s']][[%i]]", plot_name, chosen)
            current <- saved[[chosen]]
        }

        # Initialize the minimal lasso information
        lasso_data <- sprintf("X=%s$coord[, 1], Y=%s$coord[, 2]", lasso_src, lasso_src)

        # Collect additional panel information for the lasso.
        addPanels <- c()
        if (param_choices[[.facetByRow]]) {
            addPanels["FacetRow"] <- sprintf("FacetRow=%s[['%s']]", lasso_src, facetrow)
        }
        if (param_choices[[.facetByColumn]]) {
            addPanels["FacetColumn"] <- sprintf("FacetColumn=%s[['%s']]", lasso_src, facetcolumn)
        }

        # If any facting (row, column) is active, add the relevant data fields
        if (length(addPanels)) {
            panel_data <- paste(unlist(addPanels), collapse=", ")
            lasso_data <- paste(lasso_data, panel_data, sep=", ")
        }

        if (identical(nrow(current$coord), 1L)) { # lasso has only a start point
            point_cmd <- sprintf(
"geom_point(aes(x=%s, y=%s),
    data=data.frame(%s),
    inherit.aes=FALSE, alpha=1, stroke=1, color='%s', shape=%s)",
                current$mapping$x, current$mapping$y, lasso_data, stroke_color, .lassoStartShape)
            full_cmd_list <- point_cmd

        } else if (current$closed){ # lasso is closed
            polygon_cmd <- sprintf(
    "geom_polygon(aes(x=%s, y=%s), alpha=%s, color='%s',
        data=data.frame(%s),
        inherit.aes=FALSE, fill='%s')",
                current$mapping$x, current$mapping$y,
                .brushFillOpacity, stroke_color,
                lasso_data, fill_color)

            # Put a number for saved lassos.
            if (i!=0L) {
                text_data <- c(sprintf("X=mean(%s$coord[, 1])", lasso_src),
    				sprintf("Y=mean(%s$coord[, 2])", lasso_src),
    				addPanels)

                text_cmd <- sprintf(
"geom_text(aes(x=%s, y=%s), inherit.aes=FALSE,
    data=data.frame(
        %s),
    label=%i, size=%s, colour='%s')",
	                current$mapping$x, current$mapping$y,
    				paste(text_data, collapse=",\n        "),
                    chosen, param_choices[[.plotFontSize]] * .plotFontSizeLegendTextDefault, stroke_color)
                polygon_cmd <- c(polygon_cmd, text_cmd)
            }

            scale_fill_cmd <- NULL
            guides_cmd <- NULL

            if (firstClosed) {
                # Commands to put only once
                scale_fill_cmd <- sprintf(
                    "scale_fill_manual(values=c('TRUE'='%s', 'FALSE'='%s'), labels=NULL)",
                    stroke_color, fill_color)

                if (param_choices[[.shapeByField]] == .shapeByNothingTitle) {
                    guides_cmd <- "guides(shape='none')"
                }
                firstClosed <- FALSE
            }

            full_cmd_list <- c(polygon_cmd, scale_fill_cmd, guides_cmd)

        } else { # lasso is still open
            path_cmd <- sprintf(
"geom_path(aes(x=%s, y=%s),
    data=data.frame(%s),
    inherit.aes=FALSE, alpha=1, color='%s', linetype='longdash')",
                current$mapping$x, current$mapping$y, lasso_data, stroke_color)

            # Do not control the shape of waypoints if shape is already being mapped to a covariate
            if (param_choices[[.shapeByField]] == .shapeByNothingTitle) {
                point_cmd <- sprintf(
"geom_point(aes(x=%s, y=%s, shape=First),
    data=data.frame(%s,
        First=seq_len(nrow(%s$coord)) == 1L),
    inherit.aes=FALSE, alpha=1, stroke=1, color='%s')",
                    current$mapping$x, current$mapping$y,
                    lasso_data, lasso_src, stroke_color)

                scale_shape_cmd <- sprintf(
                    "scale_shape_manual(values=c('TRUE'=%s, 'FALSE'=%s))",
                    .lassoStartShape, .lassoWaypointShape
                )

                guides_cmd <- "guides(shape='none')"
            } else {
                point_cmd <- sprintf(
"geom_point(aes(x=%s, y=%s, size=First),
    data=data.frame(%s,
        First=seq_len(nrow(%s$coord)) == 1L),
    inherit.aes=FALSE, alpha=1, stroke=1, shape=%s, color='%s')",
                    current$mapping$x, current$mapping$y,
                    lasso_data, lasso_src, .lassoStartShape, stroke_color)

                scale_shape_cmd <- sprintf(
                    "scale_size_manual(values=c('TRUE'=%s, 'FALSE'=%s))",
                    .lassoStartSize, .lassoWaypointSize
                )

                guides_cmd <- "guides(size='none')"
            }

            full_cmd_list <- c(path_cmd, point_cmd, scale_shape_cmd, guides_cmd)
        }

        cmds <- c(cmds, full_cmd_list)
    }

    cmds
}
