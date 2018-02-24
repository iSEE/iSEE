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
#' @param id Integer scalar specifying the index of the current
#' reduced dimension plot
#' @param all_memory list of DataFrames, where each DataFrame corresponds to a
#' panel type and contains the settings for each individual panel of that
#' type.
#' @param all_coordinates A list of data.frames that contain
#' the coordinates and covariates of data points visible in each of the plots;
#' in particular data points excluded by "restrict" brushes are not included in
#' the corresponding data.frame.
#' @param se A \code{\linkS4class{SingleCellExperiment}} object.
#' @param colormap An \code{\linkS4class{ExperimentColorMap}} object that
#' defines custom color maps to apply to individual
#' \code{assays}, \code{colData}, and \code{rowData} covariates.
#'
#' @return A list that includes the following elements:
#' \describe{
#'   \item{cmd}{A list of list of commands
#'   as character vectors to parse and evaluate to produce the final plot.
#'   Each list element groups functionally related commands with a common
#'   purpose that should be evaluated together, yet separately from other
#'   groups for certains tasks (\emph{e.g.}, \code{"data"}, \code{"lim"},
#'   \code{"brush"},\code{"setup"}, \code{"plot"}).
#'   }
#'   \item{xy}{A data.frame that includes coordinates and covariates of
#'   the plot.}
#'   \item{plot}{A \code{\link{ggplot}} object that results of the evaluation
#'   of \code{cmd} items \code{c("data", "setup", "plot")}
#'   }
#' }
#' 
#' @author Kevin Rue-Albrecht, Aaron Lun, Charlotte Soneson
#' @rdname INTERNAL_make_redDimPlot
#' @seealso
#' \code{\link{.create_plot}},
#' \code{\link{.scatter_plot}}
.make_redDimPlot <- function(id, all_memory, all_coordinates, se, colormap) {
    param_choices <- all_memory$redDimPlot[id,]
    data_cmds <- list()
    data_cmds[["reducedDim"]] <- sprintf("red.dim <- reducedDim(se, %i);", param_choices[[.redDimType]])
    data_cmds[["xy"]] <- sprintf("plot.data <- data.frame(X = red.dim[, %i], Y = red.dim[, %i], row.names=colnames(se));",
                                 param_choices[[.redDimXAxis]], param_choices[[.redDimYAxis]])
  
    reddim_names <- names(.sanitize_names(reducedDimNames(se)))
    plot_title <- reddim_names[param_choices[[.redDimType]]]

    x_lab <- sprintf("Dimension %s", param_choices[[.redDimXAxis]])
    y_lab <- sprintf("Dimension %s", param_choices[[.redDimYAxis]])

    # Generating a function that generates the plot.
    setup_out <- .complete_plotting_data(data_cmds, param_choices, all_memory, se, all_coordinates, by_row = FALSE)
    plot_out <- .create_plot(envir = setup_out$envir, param_choices = param_choices, colormap = colormap,
                             x_lab = x_lab, y_lab = y_lab, title = plot_title, by_row = FALSE)
    return(list(cmds = c(setup_out$cmds, list(plot=plot_out$cmds)), xy = setup_out$envir$plot.data, plot = plot_out$plot)) 
}

############################################
# .make_colDataPlot  ----
############################################

#' Makes a plot of column data variables
#' 
#' Make a plot of sample metadata on Y axis.
#'
#' @param id Integer scalar specifying the index of the current
#' column data plot
#' @param all_memory A list of DataFrames, where
#' each DataFrame corresponds to a panel type and
#' contains the settings for each individual panel of that type.
#' @param all_coordinates A list of data.frames that contain
#' the coordinates and covariates of data points visible in each of the plots;
#' in particular data points excluded by "restrict" brushes are not included in
#' the corresponding data.frame.
#' @param se A \code{\linkS4class{SingleCellExperiment}} object.
#' @param colormap An \code{\linkS4class{ExperimentColorMap}} object that
#' defines custom color maps to apply to individual
#' \code{assays}, \code{colData}, and \code{rowData} covariates.
#'
#' @return A list that includes the following elements:
#' \describe{
#'   \item{cmd}{A list of list of commands
#'   as character vectors to parse and evaluate to produce the final plot.
#'   Each list element groups functionally related commands with a common
#'   purpose that should be evaluated together, yet separately from other
#'   groups for certains tasks (\emph{e.g.}, \code{"data"}, \code{"lim"},
#'   \code{"brush"},\code{"setup"}, \code{"plot"}).
#'   }
#'   \item{xy}{A data.frame that includes coordinates and covariates of
#'   the plot.}
#'   \item{plot}{A \code{\link{ggplot}} object that results of the evaluation
#'   of \code{cmd} items \code{c("data", "setup", "plot")}
#'   }
#' }
#' 
#' @author Kevin Rue-Albrecht, Aaron Lun, Charlotte Soneson
#' @rdname INTERNAL_make_colDataPlot
#' @seealso
#' \code{\link{.create_plot}},
#' \code{\link{.scatter_plot}},
#' \code{\link{.violin_plot}},
#' \code{\link{.griddotplot}}
.make_colDataPlot <- function(id, all_memory, all_coordinates, se, colormap)
{
    param_choices <- all_memory$colDataPlot[id,]
    data_cmds <- list()
    y_lab <- param_choices[[.colDataYAxis]]
    data_cmds[["y"]] <- sprintf(
        "plot.data <- data.frame(Y = colData(se)[,%s], row.names=colnames(se));", 
        deparse(y_lab) # deparse() automatically adds quotes, AND protects against existing quotes/escapes.
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

    # Generating a function that generates the plot.
    setup_out <- .complete_plotting_data(data_cmds, param_choices, all_memory, se, all_coordinates, by_row = FALSE)
    plot_out <- .create_plot(envir = setup_out$envir, param_choices = param_choices, colormap = colormap,
                             x_lab = x_lab, y_lab = y_lab, title = plot_title, by_row = FALSE)
    return(list(cmds = c(setup_out$cmds, list(plot=plot_out$cmds)), xy = setup_out$envir$plot.data, plot = plot_out$plot)) 
}

############################################
# .make_featExprPlot  ----
############################################

#' Makes a gene expression plot
#' 
#' Make a plot of feature expression data on Y axis.
#'
#' @param id Integer scalar specifying the index of the current
#' feature expression plot
#' @param all_memory A list of DataFrames, where
#' each DataFrame corresponds to a panel type and contains
#' the settings for each individual panel of that type.
#' @param all_coordinates A list of data.frames that contain
#' the coordinates and covariates of data points visible in each of the plots;
#' in particular data points excluded by "restrict" brushes are not included in
#' the corresponding data.frame.
#' @param se A \code{\linkS4class{SingleCellExperiment}} object.
#' @param colormap An \code{\linkS4class{ExperimentColorMap}} object that
#' defines custom color maps to apply to individual
#' \code{assays}, \code{colData}, and \code{rowData} covariates.
#'
#' @return A list that includes the following elements:
#' \describe{
#'   \item{cmd}{A list of list of commands
#'   as character vectors to parse and evaluate to produce the final
#'   plot.
#'   Each list element groups functionally related commands with a common
#'   purpose that should be evaluated together, yet separately from other
#'   groups for certains tasks (\emph{e.g.}, \code{"data"}, \code{"lim"},
#'   \code{"brush"},\code{"setup"}, \code{"plot"}).
#'   }
#'   \item{xy}{A data.frame that includes coordinates and covariates of
#'   the plot.}
#'   \item{plot}{A \code{\link{ggplot}} object that results of the evaluation
#'   of \code{cmd} items \code{c("data", "setup", "plot")}
#'   }
#' }
#' 
#' @author Kevin Rue-Albrecht, Aaron Lun, Charlotte Soneson
#' @rdname INTERNAL_make_featExprPlot
#' @seealso
#' \code{\link{.create_plot}},
#' \code{\link{.scatter_plot}},
#' \code{\link{.violin_plot}}.
.make_featExprPlot <- function(id, all_memory, all_coordinates, se, colormap) {
    param_choices <- all_memory$featExprPlot[id,]
    data_cmds <- list()
  
    ## Setting up the y-axis:
    y_choice <- param_choices[[.featExprYAxis]]
    if (y_choice==.featExprYAxisRowTableTitle) {
        chosen_tab <- .decoded2encoded(param_choices[[.featExprYAxisRowTable]])
        gene_selected_y <- all_memory$rowStatTable[chosen_tab, .rowStatSelected]
    } else if (y_choice==.featExprYAxisFeatNameTitle) {
        gene_selected_y <- param_choices[[.featExprYAxisFeatName]]
    }
  
    validate(need( 
        length(gene_selected_y)==1L,
        sprintf("Invalid '%s' > '%s' input", .featExprYAxis, y_choice)
    ))
  
    assay_choice <- param_choices[[.featExprAssay]]
    y_title <- rownames(se)[gene_selected_y]
    y_lab <- .gene_axis_label(se, gene_selected_y, assay_choice, multiline = FALSE)
    data_cmds[["y"]] <- sprintf(
        "plot.data <- data.frame(Y=assay(se, %i)[%s,], row.names = colnames(se))",
        assay_choice, deparse(gene_selected_y) # deparse() also handles integer selections correctly.
    )
  
    ## Checking X axis choice:
    x_choice <- param_choices[[.featExprXAxis]]
  
    if (x_choice==.featExprXAxisColDataTitle) { # colData column selected
        x_lab <- x_title <- param_choices[[.featExprXAxisColData]]
        data_cmds[["x"]] <- sprintf("plot.data$X <- colData(se)[,%s];", deparse(x_lab))
  
    } else if (x_choice==.featExprXAxisRowTableTitle || x_choice==.featExprXAxisFeatNameTitle) { # gene selected
        if (x_choice==.featExprXAxisRowTableTitle) {
            chosen_tab <- .decoded2encoded(param_choices[[.featExprXAxisRowTable]])
            gene_selected_x <- all_memory$rowStatTable[chosen_tab, .rowStatSelected]
        } else if (x_choice==.featExprXAxisFeatNameTitle) {
            gene_selected_x <- param_choices[[.featExprXAxisFeatName]]
        }
      
        validate(need(
            length(gene_selected_x)==1L,
            sprintf("Invalid '%s' > '%s' input", .featExprXAxis, x_choice)
        ))
      
        x_title <- rownames(se)[gene_selected_x]
        x_lab <-
          .gene_axis_label(se, gene_selected_x, assay_choice, multiline = FALSE)
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
  
    # Generating a function that generates the plot.
    setup_out <- .complete_plotting_data(data_cmds, param_choices, all_memory, se, all_coordinates, by_row = FALSE)
    plot_out <- .create_plot(envir = setup_out$envir, param_choices = param_choices, colormap = colormap,
                             x_lab = x_lab, y_lab = y_lab, title = plot_title, by_row = FALSE)
    return(list(cmds = c(setup_out$cmds, list(plot=plot_out$cmds)), xy = setup_out$envir$plot.data, plot = plot_out$plot)) 
}

############################################
# .make_rowDataPlot  ----
############################################

#' Makes a plot of column data variables
#' 
#' Make a plot of feature expression data on Y axis.
#'
#' @param id Integer scalar specifying the index of the current
#' row data plot
#' @param all_memory A list of DataFrames, where each
#' DataFrame corresponds to a panel type and contains the
#' settings for each individual panel of that type.
#' @param all_coordinates A list of data.frames that contain
#' the coordinates and covariates of data points visible in each of the plots;
#' in particular data points excluded by "restrict" brushes are not included in
#' the corresponding data.frame.
#' @param se A \code{\linkS4class{SingleCellExperiment}} object.
#' @param colormap An \code{\linkS4class{ExperimentColorMap}} object that
#' defines custom color maps to apply to individual
#' \code{assays}, \code{colData}, and \code{rowData} covariates.
#'
#' @return A list that includes the following elements:
#' \describe{
#'   \item{cmd}{A list of list of commands as character vectors
#'   to parse and evaluate to produce the final plot.
#'   Each list element groups functionally related commands with a common
#'   purpose that should be evaluated together, yet separately from other
#'   groups for certains tasks (\emph{e.g.}, \code{"data"}, \code{"lim"},
#'   \code{"brush"},\code{"setup"}, \code{"plot"}).
#'   }
#'   \item{xy}{A data.frame that includes coordinates and covariates for
#'   the plot.}
#'   \item{plot}{A \code{\link{ggplot}} object that results of the evaluation
#'   of \code{cmd} items \code{c("data", "setup", "plot")}
#'   }
#' }
#' 
#' @author Kevin Rue-Albrecht, Aaron Lun, Charlotte Soneson
#' @rdname INTERNAL_make_rowDataPlot
#' @seealso
#' \code{\link{.create_plot}},
#' \code{\link{.scatter_plot}},
#' \code{\link{.violin_plot}},
#' \code{\link{.griddotplot}}
.make_rowDataPlot <- function(id, all_memory, all_coordinates, se, colormap) {
    param_choices <- all_memory$rowDataPlot[id,]
    data_cmds <- list()
    y_lab <- param_choices[[.rowDataYAxis]]
    data_cmds[["y"]] <- sprintf(
        "plot.data <- data.frame(Y = rowData(se)[,%s], row.names=rownames(se));", 
        deparse(y_lab) # deparse() automatically adds quotes, AND protects against existing quotes/escapes.
    )
    
    # Prepare X-axis data.
    if (param_choices[[.rowDataXAxis]]==.rowDataXAxisNothingTitle) {
        x_lab <- ''
        data_cmds[["x"]] <- "plot.data$X <- factor(character(nrow(se)))"
    } else {
        x_lab <- param_choices[[.rowDataXAxisRowData]]
        data_cmds[["x"]] <- sprintf("plot.data$X <- rowData(se)[,%s];", deparse(x_lab))
    }
    
    x_title <- ifelse(x_lab == '', x_lab, sprintf("vs %s", x_lab))
    plot_title <- sprintf("%s %s", y_lab, x_title)
  
    # Generating a function that generates the plot.
    setup_out <- .complete_plotting_data(data_cmds, param_choices, all_memory, se, all_coordinates, by_row = TRUE)
    plot_out <- .create_plot(envir = setup_out$envir, param_choices = param_choices, colormap = colormap,
                             x_lab = x_lab, y_lab = y_lab, title = plot_title, by_row = TRUE)
    return(list(cmds = c(setup_out$cmds, list(plot=plot_out$cmds)), xy = setup_out$envir$plot.data, plot = plot_out$plot)) 
}

############################################
# Internal functions: data formatter ----
############################################

#' Complete the plotting data.frame
#'
#' Fully define all of the data to be stored in a data.frame for ggplot construction.
#' This ensures that the data acquisition is separated from the plotting. 
#'
#' @param data_cmds A list of character vectors containing commands to initialize the plotting data.frame.
#' @param param_choices A single-row DataFrame that contains all the input settings for the current panel.
#' @param all_memory A list of DataFrames, where each DataFrame corresponds to a panel type and contains the settings for each individual panel of that type.
#' @param se A \code{\linkS4class{SingleCellExperiment}} object.
#' @param all_coordinates A list of data.frames, where each data.frame contains the x/y coordinates of data points on a specific plot (named by the encoded panel name).
#' @param by_row A logical vector indicating whether this data.frame is for a row-based plot.
#'
#' @return
#' A list containing \code{cmds}, itself a list containing:
#' \itemize{
#' \item \code{data}, a list of strings containing commands to generate the plotting data.frame.
#' \item \code{brush}, a list of strings containing commands to add \code{BrushBy} information to the plotting data.frame.
#' \item \code{specific}, a list of strings containing commands to generate plot-type-specific commands, e.g., scatter for violin plots.
#' }
#' 
#' The top-level list also contains \code{envir}, an environment that is guaranteed to hold:
#' \itemize{
#' \item \code{plot.data}, the fully constructed data.frame containing all plotting information required to generate the plots.
#' If brushing to restrict, any subsetting will already have been applied.
#' \item \code{plot.data.all}, a data.frame equivalent to \code{plot.data} but without any \code{BrushBy} information or subsetting by \code{BrushBy}. 
#' This is useful for determining the plotting boundaries of the entire data set, even after subsetting of \code{plot.data}.
#' }
#' All and only the commands in \code{cmds} are necessary to generate \code{plot.data} and \code{plot.data.all}.
#' 
#' @details
#' The output \code{envir$plot.data} is guaranteed to contain \code{X} and \code{Y} for the x- and y-coordinates, respectively.
#' It may also contain \code{ColorBy}, a field specifying how to colour each point.
#' All categorical variables in the output \code{data} are guaranteed to be factors, and everything else must be numeric.
#'
#' The nature of the plot to be generated will determine whether any additional fields are present in \code{plot.data}.
#' Violin plots will contain \code{GroupBy} fields as well as \code{jitteredX}.
#' In horizontal violin plots, \code{X} and \code{Y} will be swapped.
#' Square plots will have \code{jitteredX} and \code{jitteredY}.
#'
#' The \code{plot.data} may also contain \code{BrushBy}, a logical field specifying whether the points were selected by a brush in a transmitting plot.
#' 
#' @author Aaron Lun
#' @rdname INTERNAL_complete_plotting_data
#' @seealso
#' \code{\link{.create_plot}}
#' \code{\link{.make_redDimPlot}},
#' \code{\link{.make_rowDataPlot}},
#' \code{\link{.make_colDataPlot}},
#' \code{\link{.make_featExprPlot}}
#'
.complete_plotting_data <- function(data_cmds, param_choices, all_memory, se, all_coordinates, by_row=FALSE) {
    # Evaluating to check the grouping status of various fields. It is important that 
    # non-numeric X/Y become explicit factors here, which simplifies downstream 
    # processing (e.g., coercion to integer, no lost levels upon subsetting).
    eval_env <- new.env()
    eval_env$se <- se
    eval_env$all_coordinates <- all_coordinates
    eval(parse(text=unlist(data_cmds)), envir=eval_env)
    more_data_cmds <- list() 
  
    xvals <- eval_env$plot.data$X
    group_X <- .is_groupable(xvals)
    if (!group_X) {
        more_data_cmds[["more_X"]] <- .coerce_to_numeric(xvals, "X")
    } else {
        more_data_cmds[["more_X"]] <- "plot.data$X <- as.factor(plot.data$X);"
    }
    
    yvals <- eval_env$plot.data$Y
    group_Y <- .is_groupable(yvals)
    if (!group_Y) { 
        more_data_cmds[["more_Y"]] <- .coerce_to_numeric(yvals, "Y")
    } else {
        more_data_cmds[["more_Y"]] <- "plot.data$Y <- as.factor(plot.data$Y);"
    }

    # Adding coloring information as well.    
    if (by_row) {
        color_out <- .define_colorby_for_row_plot(param_choices, all_memory)
    } else {
        color_out <- .define_colorby_for_column_plot(param_choices, all_memory)
    }
    more_data_cmds[["color"]] <- color_out

    # Evaluating the latest set of commands, and moving commands to the evaluated set. 
    # This needs to be done to determine whether we need to coerce the ColorBy choice.
    if (length(more_data_cmds)) { 
        eval(parse(text=unlist(more_data_cmds)), envir=eval_env)
        data_cmds <- c(data_cmds, more_data_cmds)
        more_data_cmds <- list() 
    }

    # Choosing a color scale based on the nature of ColorBy.
    coloring <- eval_env$plot.data$ColorBy
    if (!is.null(coloring)) {
        if (!.is_groupable(coloring)) {
            more_data_cmds[["more_color"]] <- .coerce_to_numeric(coloring, "ColorBy")
        } else {
            more_data_cmds[["more_color"]] <- "plot.data$ColorBy <- as.factor(plot.data$ColorBy);"
        }
    }

    if (length(more_data_cmds)) { 
        eval(parse(text=unlist(more_data_cmds)), envir=eval_env)
        data_cmds <- c(data_cmds, more_data_cmds)
        more_data_cmds <- list() 
    }
  
    # Creating the command to define BrushBy.
    # Note that 'all_brushes' or 'all_lassos' is needed for the eval() to obtain BrushBy.
    # This approach relatively easy to deparse() in the code tracker, rather than
    # having to construct the brush object or lasso waypoints manually.
    brush_out <- .process_brushby_choice(param_choices, all_memory)
    brush_cmds <- brush_out$cmds
    if (length(brush_cmds)) { 
        eval_env$all_brushes <- brush_out$data
        eval_env$all_lassos <- brush_out$data
        eval(parse(text=unlist(brush_cmds)), envir=eval_env)
    }
    
    # Adding more plot-specific information, depending on the type of plot to be created.
    if (!group_Y && !group_X) {
        specific <- list()
    } else if (!group_Y) {
        specific <- .violin_setup(FALSE) 
    } else if (!group_X) {
        specific <- .violin_setup(TRUE) 
    } else {
        specific <- .square_setup()
    }
    if (length(specific)) { 
        eval(parse(text=unlist(specific)), envir=eval_env)
    }

    return(list(cmds=list(data=data_cmds, brush=brush_cmds, setup=specific), envir=eval_env))
}

############################################
# Internal functions: central plotter ----
############################################

#' Central plotting function
#' 
#' This function will generate plotting commands appropriate to
#' each combination of X/Y covariate types.
#' It does so by evaluating \code{data_cmds}
#' to determine the nature of X/Y, and then choose the matching plot type.
#' 
#' @details
#' Note that we need \code{all_coordinates} to be passed as an argument
#' for the evaluations to execute in this environment. All evaluations
#' are to take place in this function, \emph{not} in the calling environment
#' or in child environments. This constrains the scope of \code{eval} calls.
#'
#' @param data_cmds A list of data munging commands specific to the
#' calling (\emph{i.e.}, upstream) specialised plotting function.
#' @param param_choices A single-row DataFrame that
#' contains all the input settings for the current panel.
#' @param all_memory A list of DataFrames, where each
#' DataFrame corresponds to a panel type and contains the
#' settings for each individual panel of that type.
#' @param all_coordinates A list of data.frames that contain
#' the coordinates and covariates of data points visible in each of the plots;
#' in particular data points excluded by "restrict" brushes are not included in
#' the corresponding data.frame.
#' @param se A \code{\linkS4class{SingleCellExperiment}} object.
#' @param colormap An \code{\linkS4class{ExperimentColorMap}} object that
#' defines custom color maps to apply to individual
#' \code{assays}, \code{colData}, and \code{rowData} covariates.
#' @param ... Further arguments passed to or from other methods.
#' @param by_row logical value that declares whether the plot deals with
#' \code{\link{rowData}} (\code{TRUE}) or \code{\link{colData}}
#' (\code{TRUE}, the default).
#'
#' @return A list that includes the following elements:
#' \describe{
#'   \item{cmd}{A list of list of commands as character vectors
#'   to parse and evaluate to produce the final plot.
#'   Each list element groups functionally related commands with a common purpose
#'   that should be evaluated together, yet separately from other groups
#'   for certains tasks (\emph{e.g.}, \code{"data"}, \code{"lim"},
#'   \code{"brush"},\code{"setup"}, \code{"plot"}).
#'   }
#'   \item{xy}{A data.frame that includes coordinates and covariates of
#'   the plot.}
#'   \item{plot}{A \code{\link{ggplot}} object that results of the evaluation
#'   of \code{cmd} items \code{c("data", "setup", "plot")}
#'   }
#' }
#'
#' @author Kevin Rue-Albrecht, Aaron Lun, Charlotte Soneson
#' @rdname INTERNAL_create_plot
#' @seealso
#' \code{\link{.make_redDimPlot}},
#' \code{\link{.make_colDataPlot}},
#' \code{\link{.make_featExprPlot}},
#' \code{\link{.make_rowDataPlot}},
#' \code{\link{.scatter_plot}},
#' \code{\link{.violin_plot}},
#' \code{\link{.griddotplot}}.
.create_plot <- function(envir, param_choices, colormap, ..., by_row=FALSE) {
    envir$colormap <- colormap
    plot_data <- envir$plot.data
    
    # Dispatch to different plotting commands, depending on whether X/Y are groupable.
    group_X <- is.factor(plot_data$X)
    group_Y <- is.factor(plot_data$Y)
    if (group_X && group_Y) {
        extra_cmds <- .square_plot(plot_data=plot_data, param_choices=param_choices, ..., by_row=by_row) 
        
    } else if (group_X && !group_Y) {
        extra_cmds <- .violin_plot(plot_data=plot_data, param_choices=param_choices, ..., by_row=by_row)

    } else if (!group_X && group_Y) {
        extra_cmds <- .violin_plot(plot_data=plot_data, param_choices=param_choices, ..., by_row=by_row,
                                   horizontal=TRUE)
  
    } else {
        extra_cmds <- .scatter_plot(plot_data=plot_data, param_choices=param_choices, ..., by_row=by_row)
    }

    # Adding self-brushing boxes, if they exist.
    select_cmds <- list()
    to_flip <- !group_X && group_Y
    brush_out <- .self_brush_box(param_choices, flip=to_flip) # Adding a brush.
    select_cmds[["brush_box"]] <- brush_out$cmd
    lasso_out <- .self_lasso_path(param_choices, flip=to_flip) # Adding the lasso path.
    select_cmds[["lasso_path"]] <- lasso_out$cmd
    select_cmds <- unlist(select_cmds)

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
    plot_out <- eval(parse(text=unlist(extra_cmds)), envir=envir)
    return(list(cmds = extra_cmds, plot = plot_out))
}

############################################
# Internal functions: scatter plotter ----
############################################

#' Generate ggplot commands to produce a scatter plot
#' 
#' Creates a scatter plot of numeric X/Y. This function should purely
#' generate the plotting commands, with no modification of \code{cmds}.
#'
#' @param param_choices A single-row DataFrame that
#' contains all the input settings for the current panel.
#' @param x_lab A character label for the X axis.
#' Set to \code{NA_character_} to produce a \code{NULL} element
#' @param y_lab A character label for the Y axis.
#' Set to \code{NA_character_} to produce a \code{NULL} element.
#' @param color_label A character title for the color scale legend.
#' Set to \code{NA_character_} to produce a \code{NULL} element.
#' @param color_scale_cmd Color scale command for \code{ggplot}
#' (generated by \code{\link{.create_plot}}).
#' @param brush_cmd A character vector of commands that control the
#' subsetting or appearance of brushed data points.
#' @param title A character title for the plot.
#' Set to \code{NA_character_} to produce a \code{NULL} element.
#'
#' @return A list of list of commands as character vectors to parse and
#'   evaluate to produce the final plot.
#'   Each list element groups functionally related commands with a common
#'   purpose that should be evaluated together, yet separately from other
#'   groups for certains tasks (\emph{e.g.}, \code{"data"}, \code{"lim"},
#'   \code{"brush"},\code{"setup"}, \code{"plot"}).
#'
#' @author Kevin Rue-Albrecht, Aaron Lun.
#' @rdname INTERNAL_scatter_plot
#' @seealso
#' \code{\link{.make_redDimPlot}},
#' \code{\link{.make_colDataPlot}},
#' \code{\link{.make_featExprPlot}},
#' \code{\link{.make_rowDataPlot}}
.scatter_plot <- function(plot_data, param_choices, se, x_lab, y_lab, title, by_row = FALSE) {
    plot_cmds <- list()
    plot_cmds[["ggplot"]] <- "ggplot() +"

    # Adding points to the plot.
    new_aes <- .build_aes(color = !is.null(plot_data$ColorBy))
    plot_cmds[["points"]] <- unlist(.create_points(param_choices, !is.null(plot_data$BrushBy), new_aes))

    # Defining the color commands.
    if (by_row) { 
        color_out <- .add_color_to_row_plot(param_choices, se, plot_data$ColorBy)
    } else {
        color_out <- .add_color_to_column_plot(param_choices, se, plot_data$ColorBy)
    }
    color_label <- color_out$label
    color_scale_cmd <- unlist(color_out$cmd)

    # Adding axes labels.
    plot_cmds[["labs"]] <- .build_labs(x = x_lab, y = y_lab, color = color_label, title = title)

    # Defining boundaries if zoomed.
    bounds <- param_choices[[.zoomData]][[1]]
    if (!is.null(bounds)) {
        plot_cmds[["coord"]] <- sprintf(
            "coord_cartesian(xlim = c(%s, %s), ylim = c(%s, %s), expand = FALSE) +", # FALSE, to get a literal zoom.
            deparse(bounds["xmin"]), deparse(bounds["xmax"]), deparse(bounds["ymin"]),  deparse(bounds["ymax"])
        )
    } else {
        plot_cmds[["coord"]] <- "coord_cartesian(xlim = range(plot.data.all$X, na.rm = TRUE),
    ylim = range(plot.data.all$Y, na.rm = TRUE), expand = TRUE) +"
    }

    # Adding further aesthetic elements.
    plot_cmds[["scale_color"]] <- color_scale_cmd
    plot_cmds[["theme_base"]] <- "theme_bw() +"
    plot_cmds[["theme_custom"]] <- "theme(legend.position = 'bottom')"
    return(plot_cmds)
}

############################################
# Internal functions: violin plotter ----
############################################

#' Generate ggplot commands to produce a violin plot
#' 
#' Generates a \emph{vertical} violin plot. This function should purely
#' generate the plotting commands, with no modification of input commands.
#'
#' @param param_choices A single-row DataFrame that
#' contains all the input settings for the current panel.
#' @param x_lab A character label for the X axis.
#' Set to \code{NA_character_} to produce a \code{NULL} element
#' @param y_lab A character label for the Y axis.
#' Set to \code{NA_character_} to produce a \code{NULL} element.
#' @param color_label A character title for the color scale legend.
#' Set to \code{NA_character_} to produce a \code{NULL} element.
#' @param color_scale_cmd Color scale command for \code{ggplot}
#' (generated by \code{\link{.create_plot}}).
#' @param brush_cmd A character vector of commands that control the
#' subsetting or appearance of brushed data points.
#' @param horizontal A logical value that indicates whether violins
#' should be drawn horizontally.
#' (\emph{i.e.}, Y axis categorical and X axis continuous)
#' @param title A character title for the plot.
#' Set to \code{NA_character_} to produce a \code{NULL} element.
#'
#' @return A list of list of commands
#'   as character vectors to parse and evaluate to produce the final plot.
#'   Each list element groups functionally related commands with a common purpose
#'   that should be evaluated together, yet separately from other groups
#'   for certains tasks (\emph{e.g.}, \code{"data"}, \code{"lim"},
#'   \code{"brush"},\code{"setup"}, \code{"plot"}).
#'
#' @author Kevin Rue-Albrecht, Aaron Lun, Charlotte Soneson.
#' @rdname INTERNAL_violin_plot
#' @seealso
#' \code{\link{.make_redDimPlot}},
#' \code{\link{.make_colDataPlot}},
#' \code{\link{.make_featExprPlot}},
#' \code{\link{.make_rowDataPlot}}
#'
#' @importFrom vipor offsetX
.violin_plot <- function(plot_data, param_choices, se, x_lab, y_lab, title, horizontal = FALSE, by_row = FALSE) {
    plot_cmds <- list()
    plot_cmds[["ggplot"]] <- "ggplot() +" # do NOT put aes here, it does not play nice with shiny brushes.
    plot_cmds[["violin"]] <- sprintf("geom_violin(%s, alpha = 0.2, data=plot.data, scale = 'width', width = 0.9) +", 
                                     .build_aes(color = FALSE, group = TRUE))

    # Adding the points to the plot (with/without brushing).
    new_aes <- .build_aes(color = !is.null(plot_data$ColorBy), alt=c(x="jitteredX"))
    point_out <- .create_points(param_choices, !is.null(plot_data$BrushBy), new_aes)
    plot_cmds[["points"]] <- unlist(point_out)

    # Defining the color commands.
    if (by_row) { 
        color_out <- .add_color_to_row_plot(param_choices, se, plot_data$ColorBy)
    } else {
        color_out <- .add_color_to_column_plot(param_choices, se, plot_data$ColorBy)
    }
    color_label <- color_out$label
    color_scale_cmd <- unlist(color_out$cmd)

    # Adding axis labels. 
    if (horizontal) {
        tmp <- y_lab
        y_lab <- x_lab
        x_lab <- tmp
    }
   
    plot_cmds[["labs"]] <- .build_labs(
        x = x_lab,
        y = y_lab,
        color = color_label,
        title = title
    )

    # Defining boundaries if zoomed. This requires some finesse to deal
    # with horizontal plots, where the brush is computed on the flipped coordinates.
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
        plot_cmds[["coord"]] <- sprintf(
            "%s(xlim = c(%.5g, %.5g), ylim = c(%.5g, %.5g), expand = FALSE) +", # FALSE, to get a literal zoom.
            coord_cmd, deparse(bounds["xmin"]), deparse(bounds["xmax"]), 
            deparse(bounds["ymin"]), deparse(bounds["ymax"])
        )
    } else {
        plot_cmds[["coord"]] <- sprintf("%s(xlim = NULL, ylim = range(plot.data.all$Y, na.rm=TRUE), expand = TRUE) +", coord_cmd)
    }
  
    plot_cmds[["scale_color"]] <- color_scale_cmd
    plot_cmds[["scale_x"]] <-
      "scale_x_discrete(drop = FALSE) +" # preserving the x-axis range.
    plot_cmds[["theme_base"]] <- "theme_bw() +"
    plot_cmds[["theme_custom"]] <- "theme(legend.position = 'bottom', legend.box = 'vertical',
    axis.text.x = element_text(angle = 90))"

    return(plot_cmds)
}

#' @rdname INTERNAL_violin_plot
.violin_setup <- function(horizontal=FALSE) { 
    setup_cmds <- list()

    # Switching X and Y axes if we want a horizontal violin plot.
    # This is done in lim_cmds to guarantee sensible limits, though
    # it would technically be more appropriate to put in setup_cmds.
    if (horizontal) {
        setup_cmds[["swap"]] <- c("tmp <- plot.data$X;
plot.data$X <- plot.data$Y;
plot.data$Y <- tmp;")
    }
    setup_cmds[["na.rm"]] <- "plot.data <- subset(plot.data, !is.na(X) & !is.na(Y));"
    setup_cmds[["group"]] <- "plot.data$GroupBy <- plot.data$X;"

    # Figuring out the scatter. This is done ahead of time to guarantee the
    # same results regardless of the subset used for brushing. Note adjust=1
    # for consistency with geom_violin (differs from geom_quasirandom default).
    setup_cmds[["seed"]] <- "set.seed(100);"
    setup_cmds[["calcX"]] <- "plot.data$jitteredX <- vipor::offsetX(plot.data$Y,
    x=plot.data$X, width=0.4, varwidth=FALSE, adjust=1,
    method='quasirandom', nbins=NULL) + as.integer(plot.data$X);"

    return(setup_cmds)
}

############################################
# Internal functions: rectangle plotter ----
############################################

#' Generate ggplot commands to produce jittered dots in a discrete grid layout
#' 
#' Generates a grid dot plot. This function should purely
#' generate the plotting commands, with no modification of input commands.
#'
#' @param param_choices A single-row DataFrame that
#' contains all the input settings for the current panel.
#' @param x_lab A character label for the X axis.
#' Set to \code{NA_character_} to produce a \code{NULL} element
#' @param y_lab A character label for the Y axis.
#' Set to \code{NA_character_} to produce a \code{NULL} element.
#' @param color_label A character title for the color scale legend.
#' Set to \code{NA_character_} to produce a \code{NULL} element.
#' @param color_scale_cmd Color scale instruction for \code{ggplot}
#' (generated by \code{\link{.create_plot}}).
#' @param brush_cmd A character vector of commands that control the
#' subsetting or appearance of brushed data points.
#' @param title A character title for the plot.
#' Set to \code{NA_character_} to produce a \code{NULL} element.
#'
#' @return A list of list of commands
#'   as character vectors to parse and evaluate to produce the final plot.
#'   Each list element groups functionally related commands with a common purpose
#'   that should be evaluated together, yet separately from other groups
#'   for certains tasks (\emph{e.g.}, \code{"data"}, \code{"lim"},
#'   \code{"brush"},\code{"setup"}, \code{"plot"}).
#'
#' @author Kevin Rue-Albrecht, Aaron Lun, Charlotte Soneson.
#' @rdname INTERNAL_griddotplot
#' @seealso
#' \code{\link{.make_redDimPlot}},
#' \code{\link{.make_colDataPlot}},
#' \code{\link{.make_featExprPlot}},
#' \code{\link{.make_rowDataPlot}}.
.square_plot <- function(plot_data, param_choices, se, x_lab, y_lab, title, by_row = FALSE) {
    plot_cmds <- list()
    plot_cmds[["ggplot"]] <- "ggplot(plot.data) +"
    plot_cmds[["tile"]] <- "geom_tile(aes(x = X, y = Y, height = 2*Radius, width = 2*Radius),
    summary.data, color = 'black', alpha = 0, size = 0.5) +"

    # Adding the points to the plot (with/without brushing).
    new_aes <- .build_aes(color = !is.null(plot_data$ColorBy), alt=c(x="jitteredX", y="jitteredY"))
    point_out <- .create_points(param_choices, !is.null(plot_data$BrushBy), new_aes)
    plot_cmds[["points"]] <- unlist(point_out)
    plot_cmds[["scale"]] <- "scale_size_area(limits = c(0, 1), max_size = 30) +"

    # Defining the color commands.
    if (by_row) { 
        color_out <- .add_color_to_row_plot(param_choices, se, plot_data$ColorBy)
    } else {
        color_out <- .add_color_to_column_plot(param_choices, se, plot_data$ColorBy)
    }
    color_label <- color_out$label
    color_scale_cmd <- unlist(color_out$cmd)

    # Adding the commands to color the points and the brushing box (NULL if undefined).
    plot_cmds[["scale_color"]] <- color_scale_cmd

    # Creating labels.
    plot_cmds[["labs"]] <- .build_labs(x = x_lab, y = y_lab, color = color_label, title = title)
    
    # Defining boundaries if zoomed.
    bounds <- param_choices[[.zoomData]][[1]]
    if (!is.null(bounds)) {
        plot_cmds[["coord"]] <- sprintf(
            "coord_cartesian(xlim = c(%s, %s), ylim = c(%s, %s), expand = FALSE) +",
            deparse(bounds["xmin"]), deparse(bounds["xmax"]), deparse(bounds["ymin"]), deparse(bounds["ymax"])
        )
    }
    
    plot_cmds[["scale_x"]] <- "scale_x_discrete(drop = FALSE) +"
    plot_cmds[["scale_y"]] <- "scale_y_discrete(drop = FALSE) +"
  
    plot_cmds[["guides"]] <- "guides(size = 'none') +"
    plot_cmds[["theme_base"]] <- "theme_bw() +"
    plot_cmds[["theme_custom"]] <- "theme(legend.position = 'bottom', legend.box = 'vertical', axis.text.x = element_text(angle = 90))"
    return(plot_cmds)
}

.square_setup <- function() {
    setup_cmds  <- list()
    setup_cmds[["table"]] <- "summary.data <- as.data.frame(with(plot.data, table(X, Y)));"
    setup_cmds[["proportion"]] <- "summary.data$Proportion <- with(summary.data, Freq / sum(Freq));"
    setup_cmds[["radius"]] <- "summary.data$Radius <- 0.49*with(summary.data, sqrt(Proportion/max(Proportion)));"
    setup_cmds[["merged"]] <- "plot.data$Marker <- seq_len(nrow(plot.data));
combined <- merge(plot.data, summary.data, by=c('X', 'Y'), all.x=TRUE);
point.radius <- combined$Radius[order(combined$Marker)];
plot.data$Marker <- NULL;"
    setup_cmds[["jitter"]] <- "set.seed(100);
plot.data$jitteredX <- as.integer(plot.data$X) + point.radius*runif(nrow(plot.data), -1, 1);
plot.data$jitteredY <- as.integer(plot.data$Y) + point.radius*runif(nrow(plot.data), -1, 1);"
    return(setup_cmds)
}

############################################
# Internal functions: coloring ----
############################################

#' Define coloring variables 
#'
#' Generates the commands necessary to define the variables to color by in the data.frame to be supplied to ggplot.
#'
#' @param param_choices A single-row DataFrame that contains all the input settings for the current panel.
#' @param all_memory A list of DataFrames, where each DataFrame corresponds to a panel type and contains the settings for each individual panel of that type.
#' 
#' @details
#' These functions generate commands to extract the variable to use for coloring individual points in row- or column-based plots.
#' In these commands, the coloring variable is added to the \code{plot.data} data.frame in the \code{ColourBy} field.
#' This is distinct from \code{.add_color_to_*_plots}, which generates the commands for coloring a ggplot by the values in \code{ColourBy}.
#' 
#' @return
#' A string containing the command to add a \code{ColourBy} field to a \code{plot.data} data.frame.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_define_color_variables
#' @seealso
#' \code{\link{.add_color_to_row_plots}},
#' \code{\link{.add_color_to_column_plots}}
.define_colorby_for_column_plot <- function(param_choices, all_memory) {
    color_choice <- param_choices[[.colorByField]]
    if (color_choice==.colorByColDataTitle) {
        covariate_name <- param_choices[[.colorByColData]]
        return(sprintf("plot.data$ColorBy <- colData(se)[,%s];", deparse(covariate_name)))
  
    } else if (color_choice==.colorByRowTableTitle || color_choice==.colorByFeatNameTitle) {
        # Set the color to the selected gene
        if (color_choice==.colorByRowTableTitle) {
            chosen_tab <- .decoded2encoded(param_choices[[.colorByRowTable]])
            chosen_gene <- all_memory$rowStatTable[chosen_tab, .rowStatSelected]
            assay_choice <- param_choices[[.colorByRowTableAssay]]
        } else {
            chosen_gene <- param_choices[[.colorByFeatName]]
            assay_choice <- param_choices[[.colorByFeatNameAssay]]
        }
        
        validate(need(
            length(chosen_gene)==1L,
            sprintf("Invalid '%s' > '%s' input", .colorByField, color_choice)
        ))

        return(sprintf("plot.data$ColorBy <- assay(se, %i)[%s,];"))
    } else {
        return(NULL)
    }
}

#' @rdname INTERNAL_define_colorby_for_column_plot
.define_colorby_for_row_plot <- function(param_choices, all_memory) {
    color_choice <- param_choices[[.colorByField]]
    if (color_choice==.colorByRowDataTitle) {
        covariate_name <- param_choices[[.colorByRowData]]
        return(sprintf("plot.data$ColorBy <- rowData(se)[,%s];", deparse(covariate_name)))
  
    } else if (color_choice==.colorByRowTableTitle || color_choice==.colorByFeatNameTitle) {
        # Set the color to the selected gene
        if (color_choice==.colorByRowTableTitle) {
            chosen_tab <- .decoded2encoded(param_choices[[.colorByRowTable]])
            chosen_gene <- all_memory$rowStatTable[chosen_tab, .rowStatSelected]
        } else {
            chosen_gene <- param_choices[[.colorByFeatName]]
        }

        validate(need(
            length(chosen_gene)==1L,
            sprintf("Invalid '%s' > '%s' input", .colorByField, color_choice)
        ))

        return(sprintf("plot.data$ColorBy <- FALSE;
plot.data[%s, 'ColorBy'] <- TRUE;", deparse(chosen_gene)))
    } else {
        return(NULL)
    }
}

#' Process coloring choice
#' 
#' @description 
#' 
#' \code{.add_color_to_column_plot} defines the coloring choice
#' for \code{colData}-based plots
#' (\emph{i.e.}, where each point represents a sample).
#'     
#' \code{.add_color_to_row_plot} defines the coloring choice
#' for \code{rowData}-based plots
#' (\emph{i.e.}, where each point represents a feature.).
#' 
#' @details 
#' Note that the returned \code{FUN} may be a \code{function} that:
#' \itemize{
#'   \item{
#'     Ignore its argument and returns a fixed set of colors
#'     (practical for continuous scales).
#'   }
#'   \item{
#'     Returns a named, fixed, character vector of colors
#'     (practical for assigning colors to specific levels in discrete scales).
#'   }
#' }
#'
#' @param param_choices A single-row DataFrame that' contains all the input settings for the current panel
#' @param se A \code{\linkS4class{SingleCellExperiment}} object.
#' @param colormap An \code{\linkS4class{ExperimentColorMap}} object that defines custom color maps for individual \code{assays}, \code{colData}, and \code{rowData} covariates.
#'
#' @return A list that contains the following elements:
#' \describe{
#'   \item{label}{A character title for the color scale legend.
#'   Set to \code{NA_character_} to produce a \code{NULL} element.
#'   }
#'   \item{FUN}{A color scale generator function that takes a single
#'   \code{numeric} argument and returns a character vector of colors.
#'   }
#' }
#'
#' @author Kevin Rue-Albrecht, Aaron Lun.
#' @rdname INTERNAL_add_color_to_column_plot
#' @name .process_colorby_choice
#' @aliases .add_color_to_column_plot
#' @aliases .add_color_to_row_plot
#' @seealso
#' \code{\link{.create_color_function_chooser}},
.add_color_to_column_plot <- function(param_choices, se, colorby) {
    output <- list(label=NA_character_, cmd=NULL)
    if (is.null(colorby)) { 
        return(output)
    }
    color_choice <- param_choices[[.colorByField]]

    # Figuring out whether 'colorby' is categorical or not.
    discrete_color <- is.factor(colorby)
    if (discrete_color) {
        ncolors <- nlevels(colorby)
    } else {
        ncolors <- 21L
    }

    if (color_choice==.colorByColDataTitle) {
        covariate_name <- param_choices[[.colorByColData]]
        covariate_as_string <- deparse(covariate_name)
        output$label <- covariate_name
        output$cmd <- sprintf("colDataColorMap(colormap, %s, discrete=%s)(%i)", 
                              covariate_as_string, discrete_color, ncolors)
  
    } else if (color_choice==.colorByRowTableTitle || color_choice==.colorByFeatNameTitle) {
        if (color_choice==.colorByRowTableTitle) {
            assay_choice <- param_choices[[.colorByRowTableAssay]]
        } else {
            assay_choice <- param_choices[[.colorByFeatNameAssay]]
        }
    
        assay_name <- assayNames(se)[assay_choice]
        assay_access <- ifelse(assay_name=="", assay_choice, sprintf("'%s'",assay_name))
    
        output$label <- .gene_axis_label(se, chosen_gene, assay_choice, multiline = TRUE)
        output$cmd <- sprintf("assayColorMap(colormap, %s, discrete=%s)(%i)", 
                              assay_access, discrete_color, ncolors)
    }
    
    return(output)
}

#' @rdname INTERNAL_add_color_to_column_plot
.add_color_to_row_plot <- function(param_choices, se, colorby) {
    output <- list(label=NA_character_, cmd=NULL)
    if (is.null(colorby)) { 
        return(output)
    }
    color_choice <- param_choices[[.colorByField]]

    # Figuring out whether 'colorby' is categorical or not.
    discrete_color <- is.factor(colorby)
    if (discrete_color) {
        ncolors <- nlevels(colorby)
    } else {
        ncolors <- 21L
    }

    if (color_choice==.colorByRowDataTitle) {
        covariate_name <- param_choices[[.colorByRowData]]
        covariate_as_string <- deparse(covariate_name)
        output$label <- covariate_name
        output$cmd <- sprintf("rowDataColorMap(colormap, %s, discrete=%s)(%i)", 
                              covariate_as_string, discrete_color, ncolors)

    } else if (color_choice==.colorByRowTableTitle || color_choice==.colorByFeatNameTitle) {
        # Set the colour to the selected gene. This slightly duplicates the work 
        # in .define_colorby_for_row_plot(), but this is necessary to separate
        # the function of data acquisition and plot generation.
        if (color_choice==.colorByRowTableTitle) {
            chosen_tab <- .decoded2encoded(param_choices[[.colorByRowTable]])
            chosen_gene <- all_memory$rowStatTable[chosen_tab, .rowStatSelected]
            col_choice <- param_choices[[.colorByRowTableColor]]
        } else {
            chosen_gene <- param_choices[[.colorByFeatName]]
            col_choice <- param_choices[[.colorByFeatNameColor]]
        }
        output$label <- .gene_axis_label(se, chosen_gene, assay_id=NULL)
        output$cmd <- sprintf("scale_color_manual(values=c(`FALSE`='black', `TRUE`=%s), drop=FALSE) +", 
                              deparse(col_choice))
    }
    return(output)
}

############################################
# Internal functions: brushing ----
############################################

#' Process brushing choice
#'
#' @param param_choices A single-row DataFrame that
#' contains all the input settings for the current panel
#' @param all_memory A list of DataFrames, where each
#' DataFrame corresponds to a panel type and contains the
#' settings for each individual panel of that type.
#'
#' @return A list that includes the following elements:
#' \describe{
#'   \item{cmd}{A character vector of commands that results in the
#'   addition of a \code{BrushBy} covariate column in the data.frame underlying
#'   the plot, or \code{NULL} if no brush should be applied.
#'   }
#'   \item{data}{A list that describes the brush applied
#'   (either \code{input$plot_brush} for the standard Shiny brush,
#'   or a matrix of waypoint coordinates for our custom lasso method.).
#'   }
#' }
#'
#' @author Kevin Rue-Albrecht, Aaron Lun.
#' @rdname INTERNAL_process_brushby_choice
#' @seealso
#' \code{\link{brushedPoints}},
#' \code{\link{in.out}}.
#' See also \code{\link{.spawn_brush_chart.}} for further details on brushing
#' relationships between panels.
#'
#' @importFrom mgcv in.out
#' @importFrom shiny brushedPoints
.process_brushby_choice <- function(param_choices, all_memory) {
    brush_in <- param_choices[[.brushByPlot]]
    brush_obj <- list()
    cmds <- list()

    # Duplicate plot.data before brushing, to make sure that axes are retained
    # even in case of an empty brushed subset. 
    cmds[["full"]] <- "plot.data.all <- plot.data;"

    # Checking what points are brushed from the transmitting plot.
    if (!identical(brush_in, .noSelection)) {

        brush_by <- .encode_panel_name(brush_in)
        transmitter <- paste0(brush_by$Type, brush_by$ID)
        if (identical(rownames(param_choices), transmitter)) {
            source_data <- 'plot.data'
        } else {
            source_data <- sprintf("all_coordinates[['%s']]", transmitter)
        }
        
        brush_val <- all_memory[[brush_by$Type]][,.brushData][[brush_by$ID]]
        if (!is.null(brush_val)) {
            brush_obj[[transmitter]] <- brush_val
            cmd[["brush"]] <- sprintf(
                "brushed_pts <- shiny::brushedPoints(%s, all_brushes[['%s']])",
                source_data, transmitter)
            cmd[["select"]] <- "plot.data$BrushBy <- rownames(plot.data) %in% rownames(brushed_pts);"
    
        } else {
            lasso_val <- all_memory[[brush_by$Type]][,.lassoData][[brush_by$ID]]
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
                
                brush_obj[[transmitter]] <- lasso_val
                cmd[["na.rm"]] <- sprintf("to_check <- subset(%s, !is.na(X) & !is.na(Y))", source_data)
                cmd[["lasso"]] <- sprintf(
                    "brushed_pts <- mgcv::in.out(all_lassos[['%s']], cbind(as.numeric(to_check$%s), as.numeric(to_check$%s)))",
                    transmitter, v1, v2)
                cmd[["select"]] <- "plot.data$BrushBy <- rownames(plot.data) %in% rownames(to_check)[brushed_pts]"
            }
        }

        if (param_choices[[.brushEffect]]==.brushRestrictTitle) {
            cmds[["subset"]] <- "plot.data <- subset(plot.data, BrushBy);"
        }
    }
    return(list(cmds=cmds, data=brush_obj))
}

#' Implementing the brushing effect on data points
#' 
#' Generate ggplot commands to control the appearance of data points while
#' accounting for a brushing effect, if active.
#'
#' @param param_choices A single-row DataFrame that
#' contains all the input settings for the current panel.
#' @param brushed A logical scalar indicating whether any points were selected on the transmitting plot, via a Shiny brush or lasso.
#' @param aes The ggplot aesthetic instructions as a character vector.
#'
#' @return A list that includes the following elements:
#' \describe{
#'   \item{brush}{A list of commands as character vectors to
#'   produce the 'restrict' brushing effect on the final plot.
#'   }
#'   \item{plot}{A list of commands as character vectors to
#'   produce the color or transprency brushing effect on the final plot.
#'   }
#' }
#'
#' @details
#' While most brushing details can be retrieved from \code{param_choices}, a separate \code{brushed} parameter is necessary.
#' This is because \code{param_choices} does not contain any information on whether the transmitter actually contains a selection of points.
#' If no Shiny brush or lasso is defined in the transmitter, the default appearance of the points is used.
#'
#' @author Kevin Rue-Albrecht, Aaron Lun.
#' @rdname INTERNAL_create_points
#' @seealso 
#' \code{\link{.scatter_plot}},
#' \code{\link{.violin_plot}},
#' \code{\link{.griddotplot}}.
.create_points <- function(param_choices, brushed, aes) {
  plot_cmds <- list()

  if (brushed) {
    brush_effect <- param_choices[[.brushEffect]]
    if (brush_effect==.brushColorTitle) {
      plot_cmds[["brush_other"]] <- sprintf(
        "geom_point(%s, alpha = 0.6, data = subset(plot.data, !BrushBy)) +", 
        aes
      )
      plot_cmds[["brush_color"]] <- sprintf(
        "geom_point(%s, alpha = 0.6, data = subset(plot.data, BrushBy), color = %s) +",
        aes, deparse(param_choices[[.brushColor]])
      )
    }
    if (brush_effect==.brushTransTitle) {
      plot_cmds[["brush_other"]] <- sprintf(
        "geom_point(%s, subset(plot.data, !BrushBy), alpha = %.2f) +",
        aes, param_choices[[.brushTransAlpha]]
      )
      plot_cmds[["brush_alpha"]] <- sprintf(
        "geom_point(%s, subset(plot.data, BrushBy)) +", 
        aes
      )
    }
    if (brush_effect==.brushRestrictTitle) {
      plot_cmds[["brush_blank"]] <- "geom_blank(data = plot.data.all, inherit.aes = FALSE, aes(x = X, y = Y)) +"
      plot_cmds[["brush_restrict"]] <- sprintf("geom_point(%s, alpha = 0.6, plot.data) +", aes)
    }
  } else {
    plot_cmds[["point"]] <- sprintf(
      "geom_point(%s, alpha = 0.6, plot.data) +", 
      aes
    )
  }
  
  return(plot_cmds)
}

############################################
# Internal functions: aesthetics ----
############################################

#' Generate an axis label for gene expression data
#'
#' @param se A \code{\linkS4class{SingleCellExperiment}} object.
#' @param gene_id A feature name or integer index
#' of the feature in \code{rownames(se)}.
#' @param assay_id The integer index of an assay in
#' \code{assayNames(se)}.
#' @param multiline A logical value that indicates whether feature and
#' assay names should appear on separate lines.
#'
#' @return A character value to use as axis label.
#'
#' @author Kevin Rue-Albrecht, Aaron Lun.
#' @rdname INTERNAL_gene_axis_label
#' @seealso 
#' \code{\link{.make_featExprPlot}},
#' \code{\link{.process_colorby_choice}}
.gene_axis_label <- function(se, gene_id, assay_id, multiline=FALSE){
  if (is.integer(gene_id)) {
    if (is.null(rownames(se))) { 
      gene_id <- paste("Feature", gene_id)
    } else {
      gene_id <- rownames(se)[gene_id]
    }
  }

  if (is.null(assay_id)) { 
    return(gene_id)
  }

  assay_name <- assayNames(se)[assay_id]
  if (is.null(assay_name) | identical(assay_name, "")) {
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
#' \code{\link{.griddotplot}}
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
#' Set to \code{NA_character_} to produce a \code{NULL} element
#' @param y x The character label for the vertical axis.
#' Set to \code{NA_character_} to produce a \code{NULL} element
#' @param color The character title for the color scale legend.
#' Set to \code{NA_character_} to produce a \code{NULL} element
#' @param shape The character title for the point shape legend.
#' Set to \code{NA_character_} to produce a \code{NULL} element
#' @param fill The character title for the color fill legend.
#' Set to \code{NA_character_} to produce a \code{NULL} element
#' @param group The character title for the group legend.
#' Set to \code{NA_character_} to produce a \code{NULL} element
#' @param title The character title for the plot title.
#' Set to \code{NA_character_} to produce a \code{NULL} element
#' @param subtitle The character title for the plot subtitle
#' Set to \code{NA_character_} to produce a \code{NULL} element
#'
#' @return Title and label instructions for \code{\link{ggplot}} as a character
#' value.
#'
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_build_labs
#' @seealso 
#' \code{\link{.scatter_plot}},
#' \code{\link{.violin_plot}},
#' \code{\link{.griddotplot}}
.build_labs <- function(
  x = NA_character_, y = NA_character_,
  color = NA_character_, shape = NA_character_,
  fill = NA_character_, group = NA_character_,
  title = NA_character_, subtitle = NA_character_
){
    labs_specs <- c(x, y, color, shape, fill, group, title, subtitle)
    names(labs_specs) <- .all_labs_names
    labs_specs <- labs_specs[!is.na(labs_specs)]
    if (identical(length(labs_specs), 0L)){
      return(NULL)
    }
    labs_specs <- mapply(
      FUN = .make_single_lab, names(labs_specs), labs_specs, USE.NAMES = FALSE)
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

#' Number of levels for any data type
#'
#' @param x A \code{vector} of any R internal type.
#'
#' @return The length of \code{levels(x)}, or the count of unique values in
#' \code{x}.
#'
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_nlevels
#' @seealso 
#' \code{\link{nlevels}},
#' \code{\link{unique}}.
.nlevels <- function(x){
  # numeric covariates are defined to have infinite levels
  if (is.numeric(x)){
    return(Inf)
  }
  # default answer for factors
  if (is.factor(x)){
    return(nlevels(x))
  }
  # default answer for character would be NULL
  return(length(unique(x)))
}

#' Determine whether a vector is discrete
#' 
#' This function requires a threshold on the count of unique values
#' beyond which the vector is declared as continuous and coerced to numeric
#' values.
#'
#' @param x A \code{vector} of any R internal type.
#' @param max_levels Maximul count of levels or unique values beyond which
#' \code{x} is declared to be continuous (\emph{i.e.}, not groupable).
#'
#' @return A \code{logical} that indicates whether \code{x} has fewer than
#' \code{max_levels} levels or unique values.
#'
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_is_groupable
#' @seealso 
#' \code{\link{.nlevels}}.
.is_groupable <- function(x, max_levels = 24){
  return(.nlevels(x) <= max_levels)
}

#' Coerce numerous levels to numeric type
#'
#' This function ensures that a specific column of the data.frame
#' underlying a plot is of \code{numeric} type.
#' If that is not the case,
#' it returns a command (as a character value) that coerces
#' the column from any type (typically a factor with numerous levels
#' or a character vector with numerous unique values) to the
#' \code{numeric} R internal type, to avoid grouping by \code{\link{ggplot}},
#' for efficient plotting.
#'
#' @param values Input vector that must be coerced to \code{numeric}.
#' @param field Column name in the plot data.frame that contain
#' \code{values}.
#' @param warn A \code{logical} that indicates whether a warning should be
#' raised if \code{values} is not already of type \code{numeric}.
#'
#' @return A command that coerces the plot data.frame column to
#' \code{numeric} type, or \code{NULL} if no coercion is required.
#'
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_coerce_to_numeric
#' @seealso 
#' \code{\link{.create_plot}}.
.coerce_to_numeric <- function(values, field, warn=TRUE) {
  if (!is.numeric(values)) {
    if (warn) {
      warning("coloring covariate has too many unique values, coercing to numeric")
    }
    if (is.factor(values)) {
      extra_cmd <- sprintf(
        "plot.data$%s <- as.numeric(plot.data$%s)",
        field, field)
    } else {
      extra_cmd <- sprintf(
        "plot.data$%s <- as.numeric(as.factor(plot.data$%s))",
        field, field
        )
    }
    return(extra_cmd)
  }
  return(NULL)
}

############################################
# Plot update functions ----
############################################

#' Generate ggplot instruction to draw a rectangle brush
#'
#' @param mode String specifying the encoded panel type of the receiving plot.
#' @param id Integer scalar specifying the index of a panel of the specified type,
#' for the receiving plot.
#' @param memory A list of DataFrames containing parameters for each panel of each type.
#' @param flip A \code{logical} value that indicates whether \code{\link{coord_flip}}
#' was applied to the plot.
#'
#' @return A list that includes the following elements:
#' \describe{
#'   \item{cmd}{A command, as character value, that overlay a rectangle
#'   on the plot to indicate the position of an active Shiny brush.
#'   }
#'   \item{data}{The data describing the Shiny brush}
#'  }
#' @author Kevin Rue-Albrecht, Aaron Lun.
#' @rdname INTERNAL_self_brush_box
#' @seealso 
#' \code{\link{iSEE}}.
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
    
    plot_name <- rownames(param_choices)
    enc <- .split_encoded(plot_name)
    cmd <- sprintf(
"geom_rect(aes(xmin = %s, xmax = %s, ymin = %s, ymax = %s), color='%s', alpha=0, 
    data=do.call(data.frame, all_brushes[['%s']][c('xmin', 'xmax', 'ymin', 'ymax')]), inherit.aes=FALSE)",
      xmin, xmax, ymin, ymax, panel_colors[enc$Type], plot_name)
    
    data <- list()
    data[[plot_name]] <- current
    return(list(cmd=cmd, data=data))
}

#' Generate ggplot instruction to draw a lasso brush
#'
#' @param mode String specifying the encoded panel type of the receiving plot.
#' @param id Integer scalar specifying the index of a panel of the specified type,
#' for the receiving plot.
#' @param memory A list of DataFrames containing parameters for each panel of each type.
#' @param flip A \code{logical} value that indicates whether \code{\link{coord_flip}}
#' was applied to the plot.
#'
#' @return A list that includes the following elements:
#' \describe{
#'   \item{cmd}{A command, as character value, that overlay a rectangle
#'   on the plot to indicate the position of an active lasso brush.
#'   }
#'   \item{data}{The data from a lasso brush, as a two-column \code{matrix}
#'   that stores X and Y coordinats of each lasso waypoint.}
#' }
#' @author Kevin Rue-Albrecht, Aaron Lun.
#' @rdname INTERNAL_self_brush_box
#' @seealso 
#' \code{\link{iSEE}}.
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
      polygon_cmd <- sprintf("geom_polygon(aes(x = %s, y = %s), alpha=%s, color='%s', 
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
    return(list(cmd=full_cmd_list, data=data))
}
