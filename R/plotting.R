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
.choose_plot_type <- function(envir) {
    group_X <- .is_groupable(envir$plot.data$X)
    group_Y <- .is_groupable(envir$plot.data$Y)
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
    bounds <- param_choices[[.zoomData]]
    if (length(bounds)) {
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

    if (param_choices[[.contourAdd]]) {
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
    by_row=FALSE, is_subsetted=FALSE, is_downsampled=FALSE, horizontal=FALSE) {

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
    bounds <- param_choices[[.zoomData]]
    if (horizontal) {
        coord_cmd <- "coord_flip"
        if (length(bounds)) {
            names(bounds) <- c(xmin="ymin", xmax="ymax", ymin="xmin", ymax="xmax")[names(bounds)]
        }
    } else {
        coord_cmd <- "coord_cartesian"
    }

    if (length(bounds)) {
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
    if (!length(bounds)) {
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
.square_plot <- function(plot_data, param_choices, x_lab, y_lab, color_lab, shape_lab, size_lab, title, by_row=FALSE, is_subsetted=FALSE) {
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
    bounds <- param_choices[[.zoomData]]
    if (length(bounds)) {

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
    if (!length(bounds)) {
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
.process_selectby_choice <- function(param_choices, by_field, type_field, saved_field, all_memory, var_name="selected_pts") {
    transmitter <- param_choices[[by_field]]
    cmds <- list()

    if (!identical(transmitter, .noSelection)) {
        source_data <- sprintf("all_coordinates[['%s']]", transmitter)
        init_cmd <- paste("transmitter <-", source_data)

        transmit_param <- all_memory[[transmitter]]
        cur_choice <- param_choices[[type_field]]
        if (cur_choice == .selectMultiUnionTitle) {
            select_sources <- c(NA_integer_, seq_along(transmit_param[[.multiSelectHistory]]))
        } else if (cur_choice == .selectMultiActiveTitle) {
            select_sources <- NA_integer_
        } else {
            select_sources <- param_choices[[saved_field]]
            if (select_sources == 0L) {
                # '0' selection in memory means no selection.
                select_sources <- integer(0)
            }
        }

        for (i in select_sources) {
            cur_cmds <- .processTransmission(transmit_param, i)
            if (is.null(cur_cmds)) {
                next
            }
            outname <- if (is.na(i)) "active" else paste0("saved", i)
            cmds[[outname]] <- c(cur_cmds, sprintf("%s[[%s]] <- selected;", var_name, deparse(outname)))
        }

        if (length(cmds)) {
            cmds <- c(init=c(init_cmd, paste(var_name, "<- list();")), cmds)
        }
    }
    unlist(cmds)
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
.populate_selection_environment <- function(x, envir) {
    if (is(x, "DotPlot")) {
        envir$all_brushes <- list(x[[.brushData]])
        envir$all_select_histories <- list(x[[.multiSelectHistory]])

        mode <- .getEncodedName(x)
        id <- x[[.organizationId]]
        names(envir$all_brushes) <- names(envir$all_select_histories) <- paste0(mode, id)
    }
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

# TODO: document
.add_commands_coerce <- function(plot_env, data_cmds_store, fields) {

    for (field0 in fields) {
        values0 <- plot_env$plot.data[[field0]]
        groupable0 <- .is_groupable(values0)
        data_cmds_store <- .add_command(data_cmds_store, .coerce_type(values0, field0, as_numeric=!groupable0), name=sprintf("coerce_%s", field0))
    }

    return(data_cmds_store)
}

############################################
# Internal functions: faceting ----
############################################

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
.self_select_boxes <- function(param_choices, flip=FALSE) {
    active <- param_choices[[.brushData]]
    saved <- param_choices[[.multiSelectHistory]]

    has_active <- as.integer(length(active) > 0)
    total <- has_active + length(saved)
    if (total == 0L) {
        return(NULL)
    }

    # Note: Faceting simultaneously on row and column produces a 'flip' effect on the brush data
    if (param_choices[[.facetByRow]] && param_choices[[.facetByColumn]]) {
        facetrow <- 'panelvar2'
        facetcolumn <- 'panelvar1'
    } else {
        facetrow <- facetcolumn <- 'panelvar1'
    }

    mode <- .getEncodedName(param_choices)
    id <- param_choices[[.organizationId]]
    stroke_color <- panel_colors[mode]
    fill_color <- brush_fill_color[mode]
    plot_name <- paste0(mode, id)

    # TODO: workaround to make brushes visible on non-builtin panels
    stroke_color <- ifelse(is.na(stroke_color), "gray10", stroke_color)
    fill_color <- ifelse(is.na(fill_color), "gray90", fill_color)

    cmds <- character(0)
    firstClosed <- FALSE
    for (i in seq_len(total) - has_active) {
        if (i==0L) {
            chosen <- active
        } else {
            chosen <- saved[[i]]
        }

        if (.is_brush(chosen)) {
            draw_cmd <- .draw_brush(plot_name, param_choices, index=i,
                flip=flip, facetrow=facetrow, facetcolumn=facetcolumn,
                stroke_color=stroke_color, fill_color=fill_color)
        } else {
            cmd.out <- .draw_lasso(plot_name, param_choices, index=i,
                facetrow=facetrow, facetcolumn=facetcolumn,
                stroke_color=stroke_color, fill_color=fill_color,
                firstClosed=firstClosed)
            firstClosed <- cmd.out$firstClosed
            draw_cmd <- cmd.out$cmds
        }

        cmds <- c(cmds, draw_cmd)
    }

    cmds
}

.draw_brush <- function(plot_name, param_choices, index, flip,
    facetrow, facetcolumn, stroke_color, fill_color)
{
    if (index == 0L) {
        brush_src <- sprintf("all_brushes[['%s']]", plot_name)
    } else {
        brush_src <- sprintf("all_select_histories[['%s']][[%i]]", plot_name, index)
    }

    # Build up the aes call, to account for flipped behavior.
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
    aes_call <- sprintf("xmin=%s, xmax=%s, ymin=%s, ymax=%s", xmin, xmax, ymin, ymax)

    # Initialize the minimal brush information
    brush_data <- sprintf("%s[c('xmin', 'xmax', 'ymin', 'ymax')]", brush_src)

    # Collect additional panel information for the brush
    addPanels <- character(0)
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
    if (index!=0L) {
        text_data <- c(sprintf("x=mean(unlist(%s[c('%s', '%s')]))", brush_src, xmin, xmax),
            sprintf("y=mean(unlist(%s[c('%s', '%s')]))", brush_src, ymin, ymax),
            addPanels)

        text_cmd <- sprintf(
"geom_text(aes(x=x, y=y), inherit.aes=FALSE,
data=data.frame(
    %s),
label=%i, size=%s, colour='%s')",
            paste(text_data, collapse=",\n        "),
            index, param_choices[[.plotFontSize]] * .plotFontSizeLegendTextDefault, stroke_color)
        brush_draw_cmd <- c(brush_draw_cmd, text_cmd)
    }

    brush_draw_cmd
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
.draw_lasso <- function(plot_name, param_choices, index,
    facetrow, facetcolumn, stroke_color, fill_color, firstClosed)
{
    if (index == 0L) {
        lasso_src <- sprintf("all_brushes[['%s']]", plot_name)
        current <- param_choices[[.brushData]]
    } else {
        lasso_src <- sprintf("all_select_histories[['%s']][[%i]]", plot_name, index)
        current <- param_choices[[.multiSelectHistory]][[index]]
    }

    # Initialize the minimal lasso information
    lasso_data <- sprintf("X=%s$coord[, 1], Y=%s$coord[, 2]", lasso_src, lasso_src)

    # Collect additional panel information for the lasso.
    addPanels <- character(0)
    if (param_choices[[.facetByRow]]) {
        addPanels["FacetRow"] <- sprintf("FacetRow=%s[['%s']]", lasso_src, facetrow)
    }
    if (param_choices[[.facetByColumn]]) {
        addPanels["FacetColumn"] <- sprintf("FacetColumn=%s[['%s']]", lasso_src, facetcolumn)
    }
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
        if (index!=0L) {
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
                index, param_choices[[.plotFontSize]] * .plotFontSizeLegendTextDefault, stroke_color)
            polygon_cmd <- c(polygon_cmd, text_cmd)
        }

        full_cmd_list <- polygon_cmd

        if (firstClosed) { # Commands to add only once for both saved/active lassos.
            scale_fill_cmd <- sprintf(
                "scale_fill_manual(values=c('TRUE'='%s', 'FALSE'='%s'), labels=NULL)",
                stroke_color, fill_color)
            full_cmd_list <- c(polygon_cmd, scale_fill_cmd)

            if (param_choices[[.shapeByField]] == .shapeByNothingTitle) {
                guides_cmd <- "guides(shape='none')"
                full_cmd_list <- c(full_cmd_list, guides_cmd)
            }
            firstClosed <- FALSE
        }

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

    list(cmds=full_cmd_list, firstClosed=firstClosed)
}
