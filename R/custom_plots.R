#' Make a custom column plot
#'
#' Make a custom plot of column (i.e., sample) data, using a user-specified function to generate coordinates.
#'
#' @param id Integer scalar specifying the index of the current custom column plot.
#' @param all_memory list of DataFrames, where each DataFrame corresponds to a panel type and contains the settings for each individual panel of that type.
#' @param all_coordinates A list of data.frames that contain the coordinates and covariates of data points visible in each of the plots.
#' @param se A SingleCellExperiment object.
#' @param colormap An ExperimentColorMap object that defines custom color maps for individual \code{assays}, \code{colData}, and \code{rowData} covariates.
#' @param cached A data.frame of cached results from previous runs.
#'
#' @return A list containing \code{cmd_list}, \code{xy} and \code{plot}, equivalent to the output of \code{\link{.plot_wrapper}}.
#' A \code{cached} field is also returned containing the result of evaluating the user-specified function.
#' 
#' @details
#' This function will try to use the input \code{cached} coordinates if possible.
#' In particular, it will only regenerate the coordinates if a user transmits a restricting selection to the custom column plot,
#' and the selection differs from what is in the input \code{cached} (based on differences in the names of the selected samples).
#'
#' For the output \code{cached}, the idea is that it will be stored in the main \code{\link{iSEE}} function.
#' It can then be passed to this function upon future re-plotting of the custom column plot.
#'
#' If the selected custom column function is \code{"---"}, this function will return \code{NULL} in \code{cmd_list};
#' a data.frame with no rows in \code{xy}; an empty ggplot in \code{plot}; and \code{NULL} in \code{cached}.
#' 
#' @author Aaron Lun
#' @rdname INTERNAL_make_customColumnPlot
#' @seealso
#' \code{\link{.process_selectby_choice}},
#' \code{\link{.choose_plot_type}},
#' \code{\link{.downsample_points}}
#' 
.make_customColPlot <- function(id, all_memory, all_coordinates, se, colormap, cached) {
    param_choices <- all_memory$customColPlot[id,]
    eval_env <- new.env()
    eval_env$se <- se
    eval_env$all_coordinates <- all_coordinates
    eval_env$custom_col_fun <- .get_internal_info(se, "custom_col_fun")

    # Mocking up a plot.data.
    data_cmds <- .initialize_cmd_store()
    data_cmds <- .add_command(data_cmds, "plot.data <- data.frame(row.names=colnames(se));")

    # Adding coloring information.
    {
        color_out <- .define_colorby_for_column_plot(param_choices, se)
        data_cmds <- .add_command(data_cmds, color_out$cmds)
        color_lab <- color_out$label

        # Ensuring that colors are either factor or numeric. 
        data_cmds <- .evaluate_commands(data_cmds, eval_env)

        coloring <- eval_env$plot.data$ColorBy
        if (!is.null(coloring)) {
            color_coerce_cmd <- .coerce_type(coloring, "ColorBy", as_numeric=!.is_groupable(coloring))
            if (!is.null(color_coerce_cmd)) {
                data_cmds <- .add_command(data_cmds, color_coerce_cmd)
                data_cmds <- .evaluate_commands(data_cmds, eval_env)
            }
        }
    }

    # Adding shape information.
    {
        shape_out <- .define_shapeby_for_column_plot(param_choices, se)
        data_cmds <- .add_command(data_cmds, shape_out$cmds)
        shape_lab <- shape_out$label

        # Ensuring that shapes are either factor or numeric. 
        data_cmds <- .evaluate_commands(data_cmds, eval_env)
        
        shaped <- eval_env$plot.data$ShapeBy
        if (!is.null(shaped)) {
            shape_coerce_cmd <- .coerce_type(shaped, "ShapeBy", as_numeric=!.is_groupable(shaped))
            if (!is.null(shape_coerce_cmd)) { 
                data_cmds <- .add_command(data_cmds, shape_coerce_cmd)
                data_cmds <- .evaluate_commands(data_cmds, eval_env)
            }
        }
    }

    # Adding faceting information.
    facet_out <- .define_facetby_for_column_plot(param_choices, se)
    data_cmds <- .add_command(data_cmds, facet_out)
    data_cmds <- .evaluate_commands(data_cmds, eval_env)

    # Implementing the selection, and _eliminating_ plot.data.all, which is not necessary when plotting only the restricted subset.
    select_out <- .process_selectby_choice(param_choices, all_memory)
    select_cmds <- select_out$cmds
    select_cmds <- select_cmds[!grepl("^plot.data.all", select_cmds)]
    if (length(select_cmds)) { 
        eval_env$all_brushes <- select_out$data
        eval_env$all_lassos <- select_out$data
        .text_eval(select_cmds, eval_env)
    }
    
    # Constructing the evaluation command to get the points.
    fun_name <- param_choices[[.customColFun]] 
    if (fun_name==.noSelection) {
        return(list(cmd_list=NULL, xy=data.frame(X=numeric(0), Y=numeric(0)), plot=ggplot(), cached=NULL))
    }
    custom_cmds <- .initialize_cmd_store()
    custom_cmds <- .add_command(custom_cmds, sprintf("custom.data <- custom_col_fun[[%s]](se, rownames(plot.data));", deparse(fun_name)))

    # Checking whether the selected points are the same as before.
    # Otherwise we 'fill in' the cached results.
    if (!identical(rownames(eval_env$plot.data), rownames(cached$coordinates))) {
        custom_cmds <- .evaluate_commands(custom_cmds, eval_env)
        cached <- eval_env$custom.data 
    } else {
        custom_cmds <- .evaluate_commands(custom_cmds, NULL)
        eval_env$custom.data <- cached
    }

    custom_cmds <- .add_command(custom_cmds, c("plot.data$X <- custom.data$coordinates$X;", "plot.data$Y <- custom.data$coordinates$Y;"))
    custom_cmds <- .evaluate_commands(custom_cmds, eval_env)

    # Coercing type of the X and Y values.
    xvals <- eval_env$plot.data$X
    group_X <- .is_groupable(xvals)
    custom_cmds <- .add_command(custom_cmds, .coerce_type(xvals, "X", as_numeric=!group_X))
    
    yvals <- eval_env$plot.data$Y
    group_Y <- .is_groupable(yvals)
    custom_cmds <- .add_command(custom_cmds, .coerce_type(yvals, "Y", as_numeric=!group_Y))

    custom_cmds <- .evaluate_commands(custom_cmds, eval_env)

    # Adding more plot-specific information, depending on the type of plot to be created.
    specific <- .choose_plot_type(group_X, group_Y, eval_env)
    if (length(specific)) {
        specific <- c("", specific)
    }
    
    cmd_list <- list(data=data_cmds$processed, select=select_cmds, setup=c(custom_cmds$processed, specific)) 

    # Downsampling and creating the plot object.
    # DO NOT MOVE the 'xy' below .downsample_points(), as downsampling will alter the value in 'envir'.
    xy <- eval_env$plot.data 
    downsample_cmds <- .downsample_points(param_choices, eval_env)

    plot_out <- .create_plot(eval_env, param_choices, colormap=colormap, 
        x_lab=cached$xlab, y_lab=cached$ylab, title=cached$title, 
        color_lab = color_lab, shape_lab = shape_lab, 
        by_row = FALSE)

    return(list(cmd_list = c(cmd_list, list(plot=c(downsample_cmds, plot_out$cmds))), xy = xy, plot = plot_out$plot, cached = cached))
}

#' @name Custom iSEE plots
#' @title Creating custom plots
#' @description Instructions for creating custom plots for use in \code{\link{iSEE}}.
#'
#' @section Functions for custom column plots:
#' Any function should accept the following arguments:
#' \itemize{
#' \item \code{se}, a SingleCellExperiment object.
#' \item \code{columns}, a character vector of column names, corresponding to a transmitted selection of points.
#' This will be different from \code{colnames(se)} if the current custom column plot is set to \code{"Restrict"}.
#' }
#' In return, it should produce a list containing the following elements:
#' \describe{
#' \item{\code{coordinates}:}{A data.frame with one row per entry of \code{columns}.
#' It should contain the fields \code{X} and \code{Y}, for the x- and y-axis values per row.
#' Row names must be set to \code{columns}.}
#' \item{\code{xlab}:}{A string specifying the x-axis label.}
#' \item{\code{ylab}:}{A string specifying the y-axis label.}
#' \item{\code{title}:}{A string specifying the plot title.}
#' }
#'
#' @author Aaron Lun
#' @rdname custom_plots
#' @examples
#' CUSTOM <- function(se, columns) {
#'     kept <- logcounts(se)[,columns]
#'     out <- prcomp(t(kept), rank.=2)$x
#'     coords <- data.frame(X=out[,1], Y=out[,2], row.names=columns)
#'     return(list(coordinates=coords, xlab="PC1", ylab="PC2", 
#'            title="PCA on selected points"))           
#' }
#'
#' library(scRNAseq)
#' data(allen)
#' class(allen)
#'
#' library(scater)
#' sce <- as(allen, "SingleCellExperiment")
#' counts(sce) <- assay(sce, "tophat_counts")
#' sce <- normalize(sce)
#' sce <- runPCA(sce)
#'
#' rdp <- redDimPlotDefaults(sce, 1)
#' ccp <- customColPlotDefaults(sce, 1)
#' ccp$Function <- "PCA2"
#' ccp$SelectByPlot <- "Reduced dimension plot 1"
#' ccp$SelectEffect <- "Restrict"
#' 
#' app <- iSEE(sce, redDimArgs=rdp, customColArgs=ccp, 
#'    initialPanels=DataFrame(Name=c("Reduced dimension plot 1", 
#'        "Custom column plot 1")),
#'    customColFun=list(PCA2=CUSTOM))
NULL
