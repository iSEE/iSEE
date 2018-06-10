############################################
# Aesthetics constants -----
############################################

.all_aes_names <- c("x", "y", "color", "shape", "fill", "group")
.all_aes_values <-
  c("X", "Y", "ColorBy", "ShapeBy", "FillBy", "GroupBy")
names(.all_aes_values) <- .all_aes_names

############################################
# Title and labels constants -----
############################################

.all_labs_names <- c(.all_aes_names, "title", "subtitle")

############################################
# Lasso constants -----
############################################

.lassoStartShape <- 22
.lassoWaypointShape <- 20

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
#' @param colormap An ExperimentColorMap object that defines custom color maps
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
      "plot.data <- data.frame(X = red.dim[, %i], Y = red.dim[, %i], row.names=colnames(se));",
      param_choices[[.redDimXAxis]], param_choices[[.redDimYAxis]])
  
    reddim_names <- names(.sanitize_names(reducedDimNames(se)))
    plot_title <- reddim_names[param_choices[[.redDimType]]]

    x_lab <- sprintf("Dimension %s", param_choices[[.redDimXAxis]])
    y_lab <- sprintf("Dimension %s", param_choices[[.redDimYAxis]])

    .plot_wrapper(data_cmds, param_choices = param_choices, all_memory = all_memory, 
                  all_coordinates = all_coordinates, se = se, by_row = FALSE, 
                  colormap = colormap, x_lab = x_lab, y_lab = y_lab, title = plot_title)
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
#' @param colormap An ExperimentColorMap object that defines custom color maps
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
        "plot.data <- data.frame(Y = colData(se)[,%s], row.names=colnames(se));", 
        deparse(y_lab) 
    )
    
    # Prepare X-axis data.
    if (param_choices[[.colDataXAxis]]==.colDataXAxisNothingTitle) {
        x_lab <- ''
        data_cmds[["x"]] <- "plot.data$X <- factor(character(ncol(se)))"
    } else {
        x_lab <- param_choices[[.colDataXAxisColData]]
        data_cmds[["x"]] <- sprintf(
            "plot.data$X <- colData(se)[,%s];",
            deparse(x_lab)
        )
    }
  
    x_title <- ifelse(x_lab == '', x_lab, sprintf("vs %s", x_lab))
    plot_title <- sprintf("%s %s", y_lab, x_title)

    .plot_wrapper(data_cmds, param_choices = param_choices, all_memory = all_memory, 
                  all_coordinates = all_coordinates, se = se, by_row = FALSE, 
                  colormap = colormap, x_lab = x_lab, y_lab = y_lab, title = plot_title)
}

############################################
# .make_featAssayPlot  ----
############################################

#' Makes a gene expression plot
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
#' @param colormap An ExperimentColorMap object that defines custom color maps
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
    validate(need( 
        length(gene_selected_y)==1L,
        "Invalid y-axis input"
    ))
  
    assay_choice <- param_choices[[.featAssayAssay]]
    y_title <- rownames(se)[gene_selected_y]
    y_lab <-
      .gene_axis_label(se, gene_selected_y, assay_choice, multiline = FALSE)
    # NOTE: deparse() also handles integer selections correctly.
    data_cmds[["y"]] <- sprintf(
        "plot.data <- data.frame(Y=assay(se, %i)[%s,], row.names = colnames(se))",
        assay_choice, deparse(gene_selected_y)
    )
  
    ## Checking X axis choice:
    x_choice <- param_choices[[.featAssayXAxis]]
  
    if (x_choice==.featAssayXAxisColDataTitle) { # colData column selected
        x_lab <- x_title <- param_choices[[.featAssayXAxisColData]]
        data_cmds[["x"]] <-
          sprintf("plot.data$X <- colData(se)[,%s];", deparse(x_lab))
  
    } else if (x_choice==.featAssayXAxisFeatNameTitle) { # gene selected
        gene_selected_x <- param_choices[[.featAssayXAxisFeatName]]
        validate(need(
            length(gene_selected_x)==1L,
            sprintf("Invalid '%s' > '%s' input", .featAssayXAxis, x_choice)
        ))
      
        x_title <- rownames(se)[gene_selected_x]
        x_lab <- .gene_axis_label(
          se, gene_selected_x, assay_choice, multiline = FALSE)
        data_cmds[["x"]] <- sprintf(
          "plot.data$X <- assay(se, %i)[%s,];",
          assay_choice, deparse(gene_selected_x)
        )
  
    } else { # no x axis variable specified: show single violin
          x_lab <- x_title <- ''
          data_cmds[["x"]] <- "plot.data$X <- factor(character(ncol(se)))"
    }
    
    x_title <- ifelse(x_title == '', x_title, sprintf("vs %s", x_title))
    plot_title <- sprintf("%s %s", y_title, x_title)

    .plot_wrapper(data_cmds, param_choices = param_choices, all_memory = all_memory, 
                  all_coordinates = all_coordinates, se = se, by_row = FALSE, 
                  colormap = colormap, x_lab = x_lab, y_lab = y_lab, title = plot_title)
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
#' @param colormap An ExperimentColorMap object that defines custom color maps
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
    # NOTE: # deparse() automatically adds quotes, AND protects against existing quotes/escapes.
    data_cmds[["y"]] <- sprintf(
        "plot.data <- data.frame(Y = rowData(se)[,%s], row.names=rownames(se));", 
        deparse(y_lab) 
    )
    
    # Prepare X-axis data.
    if (param_choices[[.rowDataXAxis]]==.rowDataXAxisNothingTitle) {
        x_lab <- ''
        data_cmds[["x"]] <- "plot.data$X <- factor(character(nrow(se)))"
    } else {
        x_lab <- param_choices[[.rowDataXAxisRowData]]
        data_cmds[["x"]] <-
          sprintf("plot.data$X <- rowData(se)[,%s];", deparse(x_lab))
    }
    
    x_title <- ifelse(x_lab == '', x_lab, sprintf("vs %s", x_lab))
    plot_title <- sprintf("%s %s", y_lab, x_title)

    .plot_wrapper(data_cmds, param_choices = param_choices, all_memory = all_memory, 
                  all_coordinates = all_coordinates, se = se, by_row = TRUE, 
                  colormap = colormap, x_lab = x_lab, y_lab = y_lab, title = plot_title)
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
    setup_out <- .extract_plotting_data(data_cmds, param_choices, all_memory, all_coordinates, se, by_row = by_row)

    xy <- setup_out$envir$plot.data # DO NOT MOVE below .downsample_points, as downsampling will alter the value in 'envir'.

    downsample_cmds <- .downsample_points(param_choices, setup_out$envir)

    plot_out <- .create_plot(setup_out$envir, param_choices, ..., color_lab = setup_out$color_lab, by_row = by_row)

    return(list(cmd_list = c(setup_out$cmd_list, list(plot=c(downsample_cmds, plot_out$cmds))), xy = xy, plot = plot_out$plot)) 
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
    # Evaluating to check the grouping status of various fields.
    # It is important that non-numeric X/Y become explicit factors here, which simplifies downstream processing 
    # (e.g., coercion to integer, no lost levels upon subsetting).
    eval_env <- new.env()
    eval_env$se <- se
    eval_env$all_coordinates <- all_coordinates
    eval(parse(text=unlist(data_cmds)), envir=eval_env)
    more_data_cmds <- list() 
  
    xvals <- eval_env$plot.data$X
    group_X <- .is_groupable(xvals)
    more_data_cmds[["more_X"]] <- .coerce_type(xvals, "X", as_numeric=!group_X)
    
    yvals <- eval_env$plot.data$Y
    group_Y <- .is_groupable(yvals)
    more_data_cmds[["more_Y"]] <- .coerce_type(yvals, "Y", as_numeric=!group_Y)

    # Adding coloring and faceting information as well.    
    if (by_row) {
        color_out <- .define_colorby_for_row_plot(param_choices, se)
        shape_out <- .define_shapeby_for_row_plot(param_choices, se)
        facet_out <- .define_facetby_for_row_plot(param_choices, se)
    } else {
        color_out <- .define_colorby_for_column_plot(param_choices, se)
        shape_out <- .define_shapeby_for_column_plot(param_choices, se)
        facet_out <- .define_facetby_for_column_plot(param_choices, se)
    }
    more_data_cmds[["color"]] <- color_out$cmds
    color_lab <- color_out$label
    more_data_cmds[["shape"]] <- shape_out$cmds
    shape_lab <- shape_out$label

    # Add X and Y faceting variables
    for (facet_axis in names(facet_out)){
        more_data_cmds[[facet_axis]] <- facet_out[facet_axis]
    }

    # Evaluate the latest set of commands, and move them to the evaluated set. 
    # This must be done to determine if we need to coerce the ColorBy choice.
    if (length(more_data_cmds)) { 
        eval(parse(text=unlist(more_data_cmds)), envir=eval_env)
        data_cmds <- c(data_cmds, more_data_cmds)
        more_data_cmds <- list() 
    }

    # Ensuring that colors are either factor or numeric. 
    coloring <- eval_env$plot.data$ColorBy
    if (!is.null(coloring)) {
        more_data_cmds[["more_color"]] <- .coerce_type(coloring, "ColorBy", as_numeric=!.is_groupable(coloring))
    }

    # Removing NAs as they mess up .process_selectby_choice.
    cleanFields <- c("X", "Y", names(facet_out))
    cleanExpression <- paste(sprintf("!is.na(%s)", cleanFields), collapse = " & ")
    more_data_cmds[["na.rm"]] <- sprintf("plot.data <- subset(plot.data, %s);", cleanExpression)

    # Evaluating and clearing the commands.
    eval(parse(text=unlist(more_data_cmds)), envir=eval_env)
    data_cmds <- c(data_cmds, more_data_cmds)
  
    # Creating the command to define SelectBy.
    # Note that 'all_brushes' or 'all_lassos' is needed for the eval() to obtain SelectBy.
    # This approach relatively easy to deparse() in the code tracker, rather than having to construct the Shiny select object or lasso waypoints manually.
    select_out <- .process_selectby_choice(param_choices, all_memory)
    select_cmds <- select_out$cmds
    if (length(select_cmds)) { 
        eval_env$all_brushes <- select_out$data
        eval_env$all_lassos <- select_out$data
        eval(parse(text=select_cmds), envir=eval_env)
    }
    
    # Adding more plot-specific information, depending on the type of plot to be created.
    specific <- .choose_plot_type(group_X, group_Y, eval_env)

    return(list(cmd_list=list(data=unlist(data_cmds), select=select_cmds, setup=specific), 
                envir=eval_env, color_lab=color_lab))
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
        specific <- .violin_setup(FALSE) 
    } else if (!group_X) {
        mode <- "violin_horizontal"
        specific <- .violin_setup(TRUE)

        if (exists("plot.data.all", envir)) { # flipping plot.data.all as well, otherwise it becomes choatic in .violin_plot().
            specific <- c(specific, 
                "tmp <- plot.data.all$X;
                plot.data.all$X <- plot.data.all$Y;
                plot.data.all$Y <- tmp;")
        }
    } else {
        mode <- "square"
        specific <- .square_setup(envir$plot.data)
    }
    if (length(specific)) { 
        eval(parse(text=unlist(specific)), envir=envir)
    }
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
        if (plot_type=="square") {
            xtype <- "jitteredX"
            ytype <- "jitteredY"
        } else if (plot_type=="violin" || plot_type=="violin_horizontal") {
            xtype <- "jitteredX"
        }

        downsample_cmds <- c("plot.data.pre <- plot.data;", 
            sprintf("plot.data <- subset(plot.data, subsetPointsByGrid(%s, %s, resolution=%i));",
                xtype, ytype, param_choices[[.plotPointSampleRes]]), "")

        eval(parse(text=downsample_cmds), envir=envir)
    } else {
        downsample_cmds <- NULL
    }
    return(downsample_cmds)
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
#' @param colormap An ExperimentColorMap object that defines custom color maps
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
        square = .square_plot(plot_data=plot_data, param_choices=param_choices, 
            is_subsetted=is_subsetted, ...), 
        violin = .violin_plot(plot_data=plot_data, param_choices=param_choices, 
            is_subsetted=is_subsetted, is_downsampled=is_downsampled, ...),
        violin_horizontal = .violin_plot(plot_data=plot_data, param_choices=param_choices, 
            is_subsetted=is_subsetted, is_downsampled=is_downsampled, ..., horizontal=TRUE),
        scatter = .scatter_plot(plot_data=plot_data, param_choices=param_choices,
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
    select_cmds <- c(brush_out$cmds, lasso_out$cmds)

    if (length(select_cmds)) {
        N <- length(extra_cmds)
        extra_cmds[[N]] <- paste(extra_cmds[[N]], "+")

        intermediate <- seq_len(length(select_cmds)-1L)
        select_cmds[intermediate] <- paste(select_cmds[intermediate], "+")
        extra_cmds <- c(extra_cmds, select_cmds)

        # We overwrite any existing 'all_brushes' or 'all_lassos', 
        # as they have served their purpose in defining plot_data.
        envir$all_brushes <- brush_out$data
        envir$all_lassos <- lasso_out$data
    }
    
    # Evaluating the plotting commands.
    plot_out <- eval(parse(text=extra_cmds), envir=envir)
    return(list(cmds = extra_cmds, plot = plot_out))
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
.scatter_plot <- function(plot_data, param_choices, x_lab, y_lab, color_lab, title, by_row = FALSE, is_subsetted = FALSE, is_downsampled = FALSE) {
    plot_cmds <- list()
    plot_cmds[["ggplot"]] <- "ggplot() +"

    # Adding points to the plot.
    color_set <- !is.null(plot_data$ColorBy)
    shape_set <- param_choices[[.shapeByField]] != .shapeByNothingTitle
    new_aes <- .build_aes(color = color_set, shape = shape_set)
    plot_cmds[["points"]] <- .create_points(param_choices, !is.null(plot_data$SelectBy), new_aes, color_set)

    # Defining the color commands.
    if (by_row) { 
        color_scale_cmd <- .add_color_to_row_plot(plot_data$ColorBy, param_choices)
    } else {
        color_scale_cmd <- .add_color_to_column_plot(plot_data$ColorBy, param_choices)
    }

    # Adding axes labels.
    plot_cmds[["labs"]] <- .build_labs(x = x_lab, y = y_lab, color = color_lab, title = title)

    # Defining boundaries if zoomed.
    bounds <- param_choices[[.zoomData]][[1]]
    if (!is.null(bounds)) {
        plot_cmds[["coord"]] <- sprintf(
            "coord_cartesian(xlim = c(%s, %s), ylim = c(%s, %s), expand = FALSE) +", # FALSE, to get a literal zoom.
            deparse(bounds["xmin"]), deparse(bounds["xmax"]),
            deparse(bounds["ymin"]),  deparse(bounds["ymax"])
        )
    } else {
        full_data <- ifelse(is_subsetted, "plot.data.all", ifelse(is_downsampled, "plot.data.pre", "plot.data"))
        plot_cmds[["coord"]] <- sprintf("coord_cartesian(xlim = range(%s$X, na.rm = TRUE),
    ylim = range(%s$Y, na.rm = TRUE), expand = TRUE) +", full_data, full_data)
    }

    # Retain axes when no points are present.
    if (nrow(plot_data)==0 && is_subsetted) {
        plot_cmds[["select_blank"]] <- "geom_blank(data = plot.data.all, inherit.aes = FALSE, aes(x = X, y = Y)) +"
    }

    # Adding further aesthetic elements.
    plot_cmds[["scale_color"]] <- color_scale_cmd
    plot_cmds[["theme_base"]] <- "theme_bw() +"
    plot_cmds[["theme_custom"]] <- sprintf(
        "theme(legend.position = '%s', legend.box = 'vertical', legend.text=element_text(size=%s), legend.title=element_text(size=%s),
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
.violin_plot <- function(plot_data, param_choices, x_lab, y_lab, color_lab, title, 
        horizontal = FALSE, by_row = FALSE, is_subsetted = FALSE, is_downsampled = FALSE) {

    plot_cmds <- list()
    plot_cmds[["ggplot"]] <- "ggplot() +" # do NOT put aes here, it does not play nice with shiny brushes.
    plot_cmds[["violin"]] <- sprintf(
        "geom_violin(%s, alpha = 0.2, data=%s, scale = 'width', width = 0.8) +", 
        .build_aes(color = FALSE, group = TRUE),
        ifelse(is_downsampled, "plot.data.pre", "plot.data")
    )

    # Adding the points to the plot (with/without point selection).
    color_set <- !is.null(plot_data$ColorBy)
    shape_set <- param_choices[[.shapeByField]] != .shapeByNothingTitle
    new_aes <- .build_aes(color = color_set, shape = shape_set, alt=c(x="jitteredX"))
    plot_cmds[["points"]] <- .create_points(param_choices, !is.null(plot_data$SelectBy), new_aes, color_set)

    # Defining the color commands.
    if (by_row) { 
        color_scale_cmd <- .add_color_to_row_plot(plot_data$ColorBy, param_choices)
    } else {
        color_scale_cmd <- .add_color_to_column_plot(plot_data$ColorBy, param_choices)
    }

    # Adding axis labels. 
    if (horizontal) {
        tmp <- y_lab
        y_lab <- x_lab
        x_lab <- tmp
    }
   
    plot_cmds[["labs"]] <- .build_labs(x = x_lab, y = y_lab, color = color_lab, title = title)

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
            "%s(xlim = c(%s, %s), ylim = c(%s, %s), expand = FALSE) +", # FALSE, to get a literal zoom.
            coord_cmd, deparse(bounds["xmin"]), deparse(bounds["xmax"]), 
            deparse(bounds["ymin"]), deparse(bounds["ymax"])
        )
    } else {
        plot_cmds[["coord"]] <- sprintf("%s(ylim = range(%s$Y, na.rm=TRUE), expand = TRUE) +", 
            coord_cmd, ifelse(is_subsetted, "plot.data.all", ifelse(is_downsampled, "plot.data.pre", "plot.data"))
        )
    }
  
    plot_cmds[["scale_color"]] <- color_scale_cmd
    
    # Retain axes when no points are generated.
    if (nrow(plot_data)==0 && is_subsetted) {
        plot_cmds[["select_blank"]] <- "geom_blank(data = plot.data.all, inherit.aes = FALSE, aes(x = X, y = Y)) +"
    }

    # Preserving the x-axis range. This applies even for horizontal violin plots,
    # as this command is executed internally before coord_flip().
    plot_cmds[["scale_x"]] <- "scale_x_discrete(drop = FALSE) +" 

    plot_cmds[["theme_base"]] <- "theme_bw() +"
    plot_cmds[["theme_custom"]] <- sprintf(
        "theme(legend.position = '%s', legend.text=element_text(size=%s), 
        legend.title=element_text(size=%s), legend.box = 'vertical',
        axis.text.x = element_text(angle=90, size=%s, hjust=1, vjust=0.5), 
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
#' @importFrom vipor offsetX
.violin_setup <- function(horizontal=FALSE) { 
    setup_cmds <- list()

    # Switching X and Y axes if we want a horizontal violin plot.
    if (horizontal) {
        setup_cmds[["swap"]] <- c("tmp <- plot.data$X;
plot.data$X <- plot.data$Y;
plot.data$Y <- tmp;")
    }
    setup_cmds[["group"]] <- "plot.data$GroupBy <- plot.data$X;"

    # Figuring out the scatter. This is done ahead of time to guarantee the
    # same results regardless of the subset used for point selection. Note adjust=1
    # for consistency with geom_violin (differs from geom_quasirandom default).
    setup_cmds[["seed"]] <- "set.seed(100);"
    setup_cmds[["calcX"]] <-
"plot.data$jitteredX <- vipor::offsetX(plot.data$Y,
    x=plot.data$X, width=0.4, varwidth=FALSE, adjust=1,
    method='quasirandom', nbins=NULL) + as.integer(plot.data$X);"

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
.square_plot <- function(plot_data, param_choices, se, x_lab, y_lab, color_lab, title, by_row = FALSE, is_subsetted = FALSE) {
    plot_cmds <- list()
    plot_cmds[["ggplot"]] <- "ggplot(plot.data) +"
    plot_cmds[["tile"]] <-
"geom_tile(aes(x = X, y = Y, height = 2*YWidth, width = 2*XWidth, group = interaction(X, Y)),
    summary.data, color = 'black', alpha = 0, size = 0.5) +"

    # Adding the points to the plot (with/without point selection).
    color_set <- !is.null(plot_data$ColorBy)
    shape_set <- param_choices[[.shapeByField]] != .shapeByNothingTitle
    new_aes <- .build_aes(color = color_set, shape = shape_set, alt=c(x="jitteredX", y="jitteredY"))
    plot_cmds[["points"]] <- .create_points(param_choices, !is.null(plot_data$SelectBy), new_aes, color_set)

    # Defining the color commands.
    if (by_row) { 
        color_scale_cmd <- .add_color_to_row_plot(plot_data$ColorBy, param_choices)
    } else {
        color_scale_cmd <- .add_color_to_column_plot(plot_data$ColorBy, param_choices)
    }

    # Adding the commands to color the points and the point selection area
    # (NULL if undefined).
    plot_cmds[["scale_color"]] <- color_scale_cmd

    # Creating labels.
    plot_cmds[["labs"]] <- .build_labs(x = x_lab, y = y_lab, color = color_lab, title = title)
    
    # Defining boundaries if zoomed.
    bounds <- param_choices[[.zoomData]][[1]]
    if (!is.null(bounds)) {
        
        # Ensure zoom preserves the data points and width ratio of visible groups
        bounds["xmin"] <- ceiling(bounds["xmin"]) - 0.5
        bounds["xmax"] <- floor(bounds["xmax"]) + 0.5
        bounds["ymin"] <- ceiling(bounds["ymin"]) - 0.5
        bounds["ymax"] <- floor(bounds["ymax"]) + 0.5
        
        plot_cmds[["coord"]] <- sprintf(
          "coord_cartesian(xlim = c(%s, %s), ylim = c(%s, %s), expand = FALSE) +",
          deparse(bounds["xmin"]), deparse(bounds["xmax"]),
          deparse(bounds["ymin"]), deparse(bounds["ymax"])
        )
    }
    
    plot_cmds[["scale_x"]] <- "scale_x_discrete(drop = FALSE) +"
    plot_cmds[["scale_y"]] <- "scale_y_discrete(drop = FALSE) +"

    # Retain axes when no points are present.
    if (nrow(plot_data)==0 && is_subsetted) {
        plot_cmds[["select_blank"]] <- "geom_blank(data = plot.data.all, inherit.aes = FALSE, aes(x = X, y = Y)) +"
    }
  
    # Do not display the size legend (saves plot space, as well)
    plot_cmds[["theme_base"]] <- "theme_bw() +"
    plot_cmds[["theme_custom"]] <- sprintf("theme(legend.position = '%s', legend.text=element_text(size=%s),
    legend.title=element_text(size=%s), legend.box = 'vertical', 
    axis.text.x = element_text(angle=90, size=%s, hjust=1, vjust=0.5), 
    axis.text.y = element_text(size=%s), 
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
    setup_cmds[["table"]] <- "summary.data <- as.data.frame(with(plot.data, table(X, Y)));"

    norm_freq <- "with(summary.data, Freq / max(Freq))"
    if (nlevels(plot_data$Y)==1L && nlevels(plot_data$X)!=1L) {
        width_cmd <- sprintf("summary.data$XWidth <- 0.4;
summary.data$YWidth <- 0.49 * %s;", norm_freq)
    } else if (nlevels(plot_data$Y)!=1L && nlevels(plot_data$X)==1L) {
        width_cmd <- sprintf("summary.data$XWidth <- 0.49 * %s;
summary.data$YWidth <- 0.4;", norm_freq)
    } else {
        width_cmd <- sprintf("summary.data$XWidth <- summary.data$YWidth <- 0.49 * sqrt(%s);", norm_freq)
    }
    setup_cmds[["radius"]] <- width_cmd

    setup_cmds[["merged"]] <- "plot.data$Marker <- seq_len(nrow(plot.data));
combined <- merge(plot.data, summary.data, by=c('X', 'Y'), all.x=TRUE);
o <- order(combined$Marker)
width.x <- combined$XWidth[o];
width.y <- combined$YWidth[o];
plot.data$Marker <- NULL;"
    setup_cmds[["jitter"]] <- "set.seed(100);
plot.data$jitteredX <- as.integer(plot.data$X) + width.x*runif(nrow(plot.data), -1, 1);
plot.data$jitteredY <- as.integer(plot.data$Y) + width.y*runif(nrow(plot.data), -1, 1);"
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

    if (color_choice==.colorByColDataTitle) {
        covariate_name <- param_choices[[.colorByColData]]
        return(list(label=covariate_name,
                    cmds=sprintf("plot.data$ColorBy <- colData(se)[,%s];", deparse(covariate_name))))
  
    } else if (color_choice==.colorByFeatNameTitle) {
        # Set the color to the selected gene
        chosen_gene <- param_choices[[.colorByFeatName]]
        assay_choice <- param_choices[[.colorByFeatNameAssay]]
        validate(need(
            length(chosen_gene)==1L,
            sprintf("Invalid '%s' > '%s' input", .colorByField, color_choice)
        ))
        return(list(label=.gene_axis_label(se, chosen_gene, assay_choice, multiline = TRUE),
                    cmds=sprintf("plot.data$ColorBy <- assay(se, %i)[%i,];", assay_choice, chosen_gene)))

    } else {
        return(NULL)
    }
}

#' @rdname INTERNAL_define_color_variables
.define_colorby_for_row_plot <- function(param_choices, se) {
    color_choice <- param_choices[[.colorByField]]

    if (color_choice==.colorByRowDataTitle) {
        covariate_name <- param_choices[[.colorByRowData]]
        return(list(label=covariate_name, 
                    cmds=sprintf("plot.data$ColorBy <- rowData(se)[,%s];", deparse(covariate_name))))
  
    } else if (color_choice==.colorByFeatNameTitle) {
        chosen_gene <- param_choices[[.colorByFeatName]]
        validate(need(
            length(chosen_gene)==1L,
            sprintf("Invalid '%s' > '%s' input", .colorByField, color_choice)
        ))
        return(list(label=.gene_axis_label(se, chosen_gene, assay_id=NULL),
                    cmds=sprintf("plot.data$ColorBy <- FALSE;
plot.data[%s, 'ColorBy'] <- TRUE;", deparse(chosen_gene))))

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
#'
#' @return 
#' A character vector containing commands to add a color scale to an existing ggplot object, or \code{NULL} if no color scale needs to be added.
#'
#' @details
#' These functions generate commands to add a color scale for individual
#' points in row- or column-based plots,
#' i.e., where each point is a feature or sample, respectively.
#'
#' These commands assume that an ExperimentColorMap object named
#'  \code{colormap} exists in the evaluation environment.
#' The availability of \code{colorby} allows the function to determine whether
#'  discrete or continuous color scales need to be used,
#' and if discrete, how many levels (i.e., colors) should be requested from
#'  \code{colormap}.
#'
#' @author Kevin Rue-Albrecht, Aaron Lun.
#' @rdname INTERNAL_add_color_scale
#' @seealso
#' \code{\link{.scatter_plot}},
#' \code{\link{.violin_plot}},
#' \code{\link{.square_plot}},
#' \code{\link{.define_colorby_for_row_plot}},
#' \code{\link{.define_colorby_for_column_plot}}
.add_color_to_column_plot <- function(colorby, param_choices) {
    if (is.null(colorby)) { 
        return(NULL)
    }
    
    cmds <- NULL
    color_choice <- param_choices[[.colorByField]]

    # This slightly duplicates the work in .define_colorby_for_row_plot(),
    # but this is necessary to separate the function of data acquisition and plot generation.
    if (color_choice==.colorByColDataTitle) {
        covariate_name <- param_choices[[.colorByColData]]
        cmds <- .create_color_scale("colDataColorMap", deparse(covariate_name), colorby)
  
    } else if (color_choice==.colorByFeatNameTitle) {
        assay_choice <- param_choices[[.colorByFeatNameAssay]]
        cmds <- .create_color_scale("assayColorMap", deparse(assay_choice), colorby)
    }
    
    return(cmds)
}

#' @rdname INTERNAL_add_color_scale
#' @importFrom ggplot2 scale_color_manual geom_point
.add_color_to_row_plot <- function(colorby, param_choices) {
    if (is.null(colorby)) { 
        return(NULL)
    }

    cmds <- NULL
    color_choice <- param_choices[[.colorByField]]

    # This slightly duplicates the work in .define_colorby_for_row_plot(),
    # but this is necessary to separate the function of data acquisition and plot generation.
    if (color_choice==.colorByRowDataTitle) {
        covariate_name <- param_choices[[.colorByRowData]]
        cmds <- .create_color_scale("rowDataColorMap", deparse(covariate_name), colorby)

    } else if (color_choice==.colorByFeatNameTitle) {
        col_choice <- param_choices[[.colorByFeatNameColor]]
        cmds <- c(
          sprintf(
            "scale_color_manual(values=c(`FALSE`='black', `TRUE`=%s), drop=FALSE) +",
            deparse(col_choice)),
          sprintf(
            "geom_point(aes(x=X, y=Y), data=subset(plot.data, ColorBy=='TRUE'), col=%s, size=%s, alpha=1) +",
            deparse(col_choice), param_choices[[.plotPointSize]])
          )
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
            "scale_color_gradientn(colors=%s, na.value='grey50') +",
            cm_cmd),
          sprintf(
            "scale_fill_gradientn(colors=%s, na.value='grey50') +",
            cm_cmd)
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
#' \code{\link{.extract_plotting_data}},
#' \code{\link{.add_shape_to_row_plot}},
#' \code{\link{.add_shape_to_column_plot}}
#' 
#' @importFrom SummarizedExperiment assay
.define_shapeby_for_column_plot <- function(param_choices, se) {
    shape_choice <- param_choices[[.shapeByField]]

    if (shape_choice==.shapeByColDataTitle) {
        covariate_name <- param_choices[[.shapeByColData]]
        return(list(label=covariate_name,
                    cmds=sprintf("plot.data$ShapeBy <- colData(se)[,%s];", deparse(covariate_name))))
  
    } else {
        return(NULL)
    }
}

#' @rdname INTERNAL_define_shape_variables
.define_shapeby_for_row_plot <- function(param_choices, se) {
    shape_choice <- param_choices[[.shapeByField]]

    if (shape_choice==.shapeByRowDataTitle) {
        covariate_name <- param_choices[[.shapeByRowData]]
        return(list(label=covariate_name,
                    cmds=sprintf("plot.data$ShapeBy <- rowData(se)[,%s];", deparse(covariate_name))))
  
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
#'
#' @return A list that includes the following elements:
#' \describe{
#' \item{cmds}{A character vector of commands that results in the addition of a \code{SelectBy} covariate column in the \code{plot.data} data.frame.
#' This will be \code{NULL} if no selection should be applied.
#' }
#' \item{data}{A list containing a Shiny brush object or a matrix of closed lasso waypoint coordinates.
#' This is named with the encoded panel name of the transmitting plot.
#' }
#' }
#'
#' @details
#' For the current panel, this function identifies the transmitting panel, i.e., from which the current panel is receiving a selection of data points.
#' It then generates the commands necessary to identify the points selected in the transmitter, to add as \code{SelectBy} in the current panel.
#' This requires extraction of the Shiny brush or lasso waypoints in the transmitter, which are used during evaluation to define the selection.
#'
#' Note that if selecting to restrict, an extra \code{plot.data.all} variable will be generated in the evaluation environment.
#' This will be used in \code{\link{.scatter_plot}} and \code{\link{.violin_plot}} to define the boundaries of the plot based on the full data.
#' In this manner, the boundaries of the plot are kept consistent even when only a subset of the data are used to generate the ggplot object.
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
.process_selectby_choice <- function(param_choices, all_memory) {
    select_in <- param_choices[[.selectByPlot]]
    select_obj <- list()
    cmds <- list()

    # Checking which points are selected in the transmitting plot.
    if (!identical(select_in, .noSelection)) {

        select_by <- .encode_panel_name(select_in)
        transmitter <- paste0(select_by$Type, select_by$ID)
        if (identical(rownames(param_choices), transmitter)) {
            source_data <- 'plot.data'
        } else {
            source_data <- sprintf("all_coordinates[['%s']]", transmitter)
        }
        
        brush_val <- all_memory[[select_by$Type]][,.brushData][[select_by$ID]]
        if (!is.null(brush_val)) {
            select_obj[[transmitter]] <- brush_val
            cmds[["brush"]] <- sprintf(
                "selected_pts <- shiny::brushedPoints(%s, all_brushes[['%s']])",
                source_data, transmitter)
            cmds[["select"]] <-
              "plot.data$SelectBy <- rownames(plot.data) %in% rownames(selected_pts);"
    
        } else {
            lasso_val <- all_memory[[select_by$Type]][,.lassoData][[select_by$ID]]
            closed <- attr(lasso_val, "closed")
        
            if (!is.null(lasso_val) && closed) { 
                flipped <- attr(lasso_val, "flipped")
                if (flipped) {
                    v1 <- "Y"
                    v2 <- "X"
                } else {
                    v1 <- "X"
                    v2 <- "Y"
                }
                
                select_obj[[transmitter]] <- lasso_val
                cmds[["lasso"]] <- sprintf("selected_pts <- mgcv::in.out(all_lassos[['%s']], cbind(as.numeric(%s$%s), as.numeric(%s$%s)))",
                                           transmitter, source_data, v1, source_data, v2)
                cmds[["select"]] <- sprintf("plot.data$SelectBy <- rownames(plot.data) %%in%% rownames(%s)[selected_pts]", source_data)
            }
        }

        if (length(select_obj) && param_choices[[.selectEffect]]==.selectRestrictTitle) {
          # Duplicate plot.data before selecting points, to make sure that axes are retained
          # even in case of an empty selected subset.
          cmds[["saved"]] <- "plot.data.all <- plot.data;"
          cmds[["subset"]] <- "plot.data <- subset(plot.data, SelectBy);"
        }
    }

    return(list(cmds=unlist(cmds), data=select_obj))
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
.create_points <- function(param_choices, selected, aes, color) {
  plot_cmds <- list()

  # If there is already coloring information available in the aes, don't add an
  # additional color= statement to the geom_point() command, since this will
  # overrule the one given in aes().
  if (color) {
      default_color <- ""
  } else {
      default_color <- sprintf("color='%s', ", param_choices[[.colorByDefaultColor]])
  }
  
  if (selected) {
    select_effect <- param_choices[[.selectEffect]]
    if (select_effect==.selectColorTitle) {
      plot_cmds[["select_other"]] <- sprintf(
        "geom_point(%s, alpha=%s, data=subset(plot.data, !SelectBy), %ssize=%s) +", 
        aes, param_choices[[.plotPointAlpha]], default_color,
        param_choices[[.plotPointSize]]
      )
      plot_cmds[["select_color"]] <- sprintf(
        "geom_point(%s, alpha=%s, data=subset(plot.data, SelectBy), color=%s, size=%s) +",
        aes, param_choices[[.plotPointAlpha]], 
        deparse(param_choices[[.selectColor]]), param_choices[[.plotPointSize]]
      )
    }
    if (select_effect==.selectTransTitle) {
      plot_cmds[["select_other"]] <- sprintf(
        "geom_point(%s, subset(plot.data, !SelectBy), alpha = %.2f, %ssize=%s) +",
        aes, param_choices[[.selectTransAlpha]], default_color, 
        param_choices[[.plotPointSize]]
      )
      plot_cmds[["select_alpha"]] <- sprintf(
        "geom_point(%s, subset(plot.data, SelectBy), %ssize=%s) +", 
        aes, default_color, param_choices[[.plotPointSize]]
      )
    }
    if (select_effect==.selectRestrictTitle) {
      plot_cmds[["select_restrict"]] <- sprintf(
        "geom_point(%s, alpha = %s, plot.data, %ssize=%s) +",
        aes, param_choices[[.plotPointAlpha]], default_color, 
        param_choices[[.plotPointSize]])
    }
  } else {
    plot_cmds[["point"]] <- sprintf(
      "geom_point(%s, alpha = %s, plot.data, %ssize=%s) +",
      aes, param_choices[[.plotPointAlpha]], default_color,
      param_choices[[.plotPointSize]]
    )
  }
  
  return(unlist(plot_cmds))
}

############################################
# Internal functions: aesthetics ----
############################################

#' Generate an axis label for gene expression data
#'
#' @param se A \linkS4class{SingleCellExperiment} object.
#' @param gene_id A integer index of the feature in \code{rownames(se)}.
#' @param assay_id The integer index of an assay in \code{assayNames(se)}.
#' If \code{NULL}, only the feature name is reported.
#' @param multiline A logical value that indicates whether feature and
#' assay names should appear on separate lines.
#'
#' @return A character value to use as axis label.
#'
#' @author Kevin Rue-Albrecht, Aaron Lun.
#' @rdname INTERNAL_gene_axis_label
#' @seealso 
#' \code{\link{.make_featAssayPlot}},
#' \code{\link{.add_color_to_column_plot}},
#' \code{\link{.add_color_to_row_plot}}
.gene_axis_label <- function(se, gene_id, assay_id, multiline=FALSE){
  gene_id <- rownames(se)[gene_id]
  if (is.null(assay_id)) { 
    return(gene_id)
  }

  assay_name <- assayNames(se)[assay_id]
  if (is.null(assay_name) || identical(assay_name, "")) {
    assay_name <- paste("assay", assay_id)
  }

  sep <- ifelse(multiline, "\n", " ")
  sprintf("%s%s(%s)", gene_id, sep, assay_name)
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
  x = TRUE, y = TRUE, color = FALSE, shape = FALSE, fill = FALSE,
  group = FALSE, alt=NULL) {
    active_aes <- .all_aes_values[c(x, y, color, shape, fill, group)]
    if (!is.null(alt)) {
        active_aes <- c(active_aes, alt)
        active_aes <- active_aes[!duplicated(names(active_aes), fromLast=TRUE)]
    }
    aes_specs <- mapply(
      FUN = .make_single_aes, names(active_aes), active_aes, USE.NAMES = FALSE)
    aes_specs <- paste(aes_specs, collapse = ", ")
    return(sprintf("aes(%s)", aes_specs))
}

#' Generate a single aesthetic instruction for ggplot
#'
#' @param name The name of a ggplot aesthetic.
#' @param value The name of a column in the plot data that will be mapped to
#' the aesthetic declared in \code{name}.
#'
#' @return A character value of the form \code{name = value}.
#'
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_make_single_aes
#' @seealso 
#' \code{\link{.build_aes}}.
.make_single_aes <- function(name, value){
    sprintf("%s = %s", name, value)
}

#' Generate ggplot title and label instructions
#'
#' @param x The character label for the horizontal axis.
#' @param y x The character label for the vertical axis.
#' @param color The character title for the color scale legend.
#' @param shape The character title for the point shape legend.
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
.build_labs <- function(x = NULL, y = NULL, color = NULL, shape = NULL, fill = NULL, group = NULL, title = NULL, subtitle = NULL){
    labs_specs <- list(x, y, color, shape, fill, group, title, subtitle)
    names(labs_specs) <- .all_labs_names
    labs_specs <- labs_specs[lengths(labs_specs)>0L]
    if (identical(length(labs_specs), 0L)){
      return(NULL)
    }
    labs_specs <- mapply(FUN = .make_single_lab, names(labs_specs), labs_specs, USE.NAMES = FALSE)
    labs_specs <- paste(labs_specs, collapse = ", ")
    return(sprintf("labs(%s) +", labs_specs))
}

#' Generate a single title or label instruction for ggplot
#'
#' @param name The name of a ggplot label.
#' @param value A character value for the title or label declared in
#' \code{name}.
#'
#' @return A character value of the form \code{name = value}.
#'
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_make_single_lab
#' @seealso 
#' \code{\link{.build_labs}}.
.make_single_lab <- function(name, value){
    sprintf("%s = %s", name, deparse(value))
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
    
    facet_row <- param_choices[[.facetByRowColData]]
    if (param_choices[[.facetByRow]]) {
        facet_cmds["FacetRow"] <- sprintf(
            "plot.data$FacetRow <- colData(se)[,%s];", deparse(facet_row))
    }
    
    facet_column <- param_choices[[.facetByColumnColData]]
    if (param_choices[[.facetByColumn]]) {
        facet_cmds["FacetColumn"] <- sprintf(
            "plot.data$FacetColumn <- colData(se)[,%s];", deparse(facet_column))
    }
    
    return(facet_cmds)
}

#' @rdname INTERNAL_define_facet_variables
.define_facetby_for_row_plot <- function(param_choices, se) {
    facet_cmds <- c()
    
    facet_row <- param_choices[[.facetByRowRowData]]
    if (param_choices[[.facetByRow]]) {
        facet_cmds["FacetRow"] <- sprintf(
            "plot.data$FacetRow <- rowData(se)[,%s];", deparse(facet_row))
    }
    
    facet_column <- param_choices[[.facetByColumnRowData]]
    if (param_choices[[.facetByColumn]]) {
        facet_cmds["FacetColumn"] <- sprintf(
            "plot.data$FacetColumn <- rowData(se)[,%s];", deparse(facet_column))
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

#' Draw a Shiny brush
#'
#' Generate ggplot instructions to draw a rectangular box corresponding
#' to the Shiny brush coordinates.
#'
#' @param param_choices A single-row DataFrame that contains all the
#' input settings for the current panel.
#' @param flip A \code{logical} value that indicates whether
#' \code{\link{coord_flip}} was applied to the plot.
#'
#' @return A list that includes the following elements:
#' \describe{
#'   \item{cmds}{A string containing a command to overlay a rectangle
#'     on the plot, indicating the position of an active Shiny brush.}
#'   \item{data}{A list containing the Shiny brush structure, named after
#'     the encoded panel name of the current panel.}
#' }
#'
#' @details
#' Returning \code{data} is necessary for evaluation of \code{cmd}
#' in the evaluation environment.
#' In particular, the command expects that \code{data} is assigned to a
#' variable named \code{all_brushes} in the evaluation environment.
#'
#' @author Kevin Rue-Albrecht, Aaron Lun.
#' @rdname INTERNAL_self_brush_box
#' @seealso 
#' \code{\link{.create_plot}}
#'
#' @importFrom ggplot2 geom_rect
.self_brush_box <- function(param_choices, flip=FALSE) { 
    current <- param_choices[,.brushData][[1]]
    if (is.null(current)) {
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
    if (param_choices[[.facetByRow]] && param_choices[[.facetByRow]]) {
        facetrow <- 'panelvar2'
        facetcolumn <- 'panelvar1'
    } else {
        facetrow <- 'panelvar1'
        facetcolumn <- 'panelvar2'
    }
    
    plot_name <- rownames(param_choices)
    enc <- .split_encoded(plot_name)
    
    # Build up the aesthetics call
    aes_call <- sprintf("xmin = %s, xmax = %s, ymin = %s, ymax = %s", xmin, xmax, ymin, ymax)
    
    # Initialize the minimal brush information
    brush_data <- sprintf("all_brushes[['%s']][c('xmin', 'xmax', 'ymin', 'ymax')]", plot_name)
    
    # Collect additional panel information for the brush
    addPanels <- c()
    if (param_choices[[.facetByRow]]) {
        addPanels["FacetRow"] <- sprintf("FacetRow = all_brushes[['%s']][['%s']]", plot_name, facetrow)
    }
    if (param_choices[[.facetByColumn]]) {
        addPanels["FacetColumn"] <- sprintf("FacetColumn = all_brushes[['%s']][['%s']]", plot_name, facetcolumn)
    }
    
    # If any facting (row, column) is active, add the relevant data fields
    if (length(addPanels)) {
        panel_list <- sprintf("list(%s)", paste(addPanels, collapse = ", "))
        brush_data <- sprintf("
            append(
                %s,
                %s)",
            brush_data, panel_list)
    }
    
    # Build up the command that draws the brush
    cmd <- sprintf(
"geom_rect(aes(%s), color='%s', alpha=0, 
    data=do.call(data.frame, %s),
    inherit.aes=FALSE)",
      aes_call, panel_colors[enc$Type], brush_data)
    
    data <- list()
    data[[plot_name]] <- current
    return(list(cmds=cmd, data=data))
}

#' Generate ggplot instruction to draw a lasso selection path
#'
#' @param param_choices A single-row DataFrame that contains all the
#' input settings for the current panel.
#' @param flip A \code{logical} value that indicates whether
#' \code{\link{coord_flip}} was applied to the plot.
#'
#' @return A list that includes the following elements:
#' \describe{
#'   \item{cmds}{A character vector containing commands to overlay a point,
#'   path or polygon, indicating the position of an active lasso.}
#'   \item{data}{A list containing a matrix of lasso waypoint coordinates,
#'   named after the encoded panel name of the current panel.}
#' }
#'
#' @details
#' This function will generate commands to add a point to the plot,
#' if there is only one lasso waypoint defined;
#' a path, if multiple waypoints are defined but the lasso is not yet closed;
#' or a polygon, if multiple waypoints are defined for a closed lasso.
#'
#' Returning \code{data} is necessary for evaluation of \code{cmd} in the
#' evaluation environment.
#' In particular, the command expects that \code{data} is assigned to a
#' variable named \code{all_lassos} in the evaluation environment.
#'
#' @author Kevin Rue-Albrecht, Aaron Lun.
#' @rdname INTERNAL_self_lasso_path
#' @seealso 
#' \code{\link{.create_plot}}
#'
#' @importFrom ggplot2 geom_point geom_polygon geom_path scale_shape_manual
#' scale_fill_manual guides
.self_lasso_path <- function(param_choices, flip=FALSE) { 
    current <- param_choices[,.lassoData][[1]]
    if (is.null(current) || !is.null(param_choices[,.brushData][[1]])) {
        return(NULL)
    }
    is_closed <- attr(current, "closed")
  
    if (flip) {
        x <- "y"
        y <- "x"
    } else {
        x <- "x"
        y <- "y"
    }
  
    plot_name <- rownames(param_choices)
    enc <- .split_encoded(plot_name)
    stroke_color <- panel_colors[enc$Type]
    fill_color <- brush_fill_color[enc$Type]
 
    if (identical(nrow(current), 1L)) { # lasso has only a start point
      point_cmd <- sprintf("geom_point(aes(x = %s, y = %s), 
    data=data.frame(x = all_lassos[['%s']][,1], y = all_lassos[['%s']][,2]),
    inherit.aes=FALSE, alpha=1, stroke = 1, color = '%s', shape = %s)",
        x, y, plot_name, plot_name, stroke_color,
        .lassoStartShape)
      full_cmd_list <- list(point_cmd)
      
    } else if (is_closed){ # lasso is closed
      polygon_cmd <- sprintf(
"geom_polygon(aes(x = %s, y = %s), alpha=%s, color='%s', 
    data=data.frame(x = all_lassos[['%s']][,1], y = all_lassos[['%s']][,2]), 
    inherit.aes=FALSE, fill = '%s')", 
          x, y ,
          .brushFillOpacity, stroke_color,
          plot_name, plot_name, fill_color)
    
        scale_fill_cmd <- sprintf(
          "scale_fill_manual(values = c('TRUE' = '%s', 'FALSE' = '%s'))",
          stroke_color, fill_color)

        guides_cmd <- "guides(shape = 'none')"
        full_cmd_list <- list(polygon_cmd, scale_fill_cmd, guides_cmd)

    } else { # lasso is still open
      path_cmd <- sprintf("geom_path(aes(x = %s, y = %s), 
    data=data.frame(x = all_lassos[['%s']][,1], y = all_lassos[['%s']][,2]),
    inherit.aes=FALSE, alpha=1, color='%s', linetype = 'longdash')", 
        x, y, plot_name, plot_name, stroke_color)
    
        point_cmd <- sprintf("geom_point(aes(x = %s, y = %s, shape = First), 
    data=data.frame(x = all_lassos[['%s']][,1], y = all_lassos[['%s']][,2], 
                    First = seq_len(nrow(all_lassos[['%s']]))==1L),
    inherit.aes=FALSE, alpha=1, stroke = 1, color = '%s')",
        x, y, plot_name, plot_name, plot_name, stroke_color)

        scale_shape_cmd <- sprintf(
          "scale_shape_manual(values = c('TRUE' = %s, 'FALSE' = %s))",
          .lassoStartShape, .lassoWaypointShape
        )
        
        guides_cmd <- "guides(shape = 'none')"
        full_cmd_list <- list(
          path_cmd, point_cmd, scale_shape_cmd, guides_cmd)
        
    }
   
    data <- list()
    data[[plot_name]] <- current
    return(list(cmds=unlist(full_cmd_list), data=data))
}
