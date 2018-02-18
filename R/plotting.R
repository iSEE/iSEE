############################################
# Plotting constants -----
############################################

.all_aes_names <- c("x", "y", "color", "shape", "fill", "group")
.all_aes_values <-
  c("X", "Y", "ColorBy", "ShapeBy", "FillBy", "GroupBy")
.all_labs_names <- c(.all_aes_names, "title")

names(.all_aes_values) <- .all_aes_names

############################################
# .make_redDimPlot  ----
############################################

.make_redDimPlot <- function(id, all_memory, all_coordinates, se, colormap)
# Makes the dimension reduction plot.
{
  param_choices <- all_memory$redDimPlot[id,]
  data_cmds <- list()
  data_cmds[["reducedDim"]] <- sprintf(
    "red.dim <- reducedDim(se, %i);", param_choices[[.redDimType]])
  data_cmds[["xy"]] <- sprintf(
    "plot.data <- data.frame(X = red.dim[, %i], Y = red.dim[, %i], row.names=colnames(se));",
    param_choices[[.redDimXAxis]],
    param_choices[[.redDimYAxis]]
  )
  
  reddim_names <- names(.sanitize_red_dim_names(reducedDimNames(se)))
  plot_title <- reddim_names[param_choices[[.redDimType]]]

  # Generating the plotting commands.
  .create_plot(
    data_cmds, param_choices, all_memory, all_coordinates, se, colormap,
    x_lab = sprintf(
      "Dimension %s",
      param_choices[[.redDimXAxis]]),
    y_lab = sprintf(
      "Dimension %s",
      param_choices[[.redDimYAxis]]),
    title = plot_title
  )
}

############################################
# .make_colDataPlot  ----
############################################

.make_colDataPlot <- function(id, all_memory, all_coordinates, se, colormap)
# Makes a plot of column data variables.
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
    data_cmds[["x"]] <- sprintf("plot.data$X <- colData(se)[,%s];", deparse(x_lab))
  }
  
  x_title <- ifelse(x_lab == '', x_lab, sprintf("vs %s", x_lab))
  plot_title <- sprintf("%s %s", y_lab, x_title)

  # Generating the plot.
  .create_plot(
    data_cmds, param_choices, all_memory, all_coordinates, se, colormap,
    x_lab=x_lab, y_lab=y_lab,
    title=plot_title
  )
}

############################################
# .make_featExprPlot  ----
############################################

.make_featExprPlot <- function(id, all_memory, all_coordinates, se, colormap)
# Makes a gene expression plot.
{
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
    data_cmds[["x"]] <- sprintf(
       "plot.data$X <- colData(se)[,%s];", 
       deparse(x_lab)
    )

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
    x_lab <- .gene_axis_label(se, gene_selected_x, assay_choice, multiline = FALSE)
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

  # Generating the plot.
  .create_plot(
    data_cmds, param_choices, all_memory, all_coordinates, se, colormap,
    x_lab=x_lab, y_lab=y_lab,
    title = plot_title
  )
}

############################################
# .make_rowDataPlot  ----
############################################

.make_rowDataPlot <- function(id, all_memory, all_coordinates, se, colormap)
# Makes a plot of column data variables.
{
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

  # Generating the plot.
  .create_plot(
    data_cmds, param_choices, all_memory, all_coordinates, se, colormap,
    x_lab=x_lab, y_lab=y_lab,
    by_row=TRUE,
    title=plot_title
  )
}

############################################
# Internal functions: central plotter ----
############################################

.create_plot <- function(
  data_cmds, param_choices, all_memory, all_coordinates, se, colormap, ...,
  by_row=FALSE)
# This function will generate plotting commands appropriate to
# each type of X/Y. It does so by evaluating 'plot.data' to
# determine the nature of X/Y, and then choosing the plot to match.
#
# Note that we need 'all_coordinates' to be passed as an argument
# for the evaluations to execute in this environment. All evaluations
# are to take place in this function, not in the calling environment
# or in child environments. This constrains the scope of 'eval' calls.
{
  if (by_row) {
    color_out <- .process_colorby_choice_for_row_plots(param_choices, all_memory, se, colormap)
  } else {
    color_out <- .process_colorby_choice_for_column_plots(param_choices, all_memory, se, colormap)
  }
  data_cmds[["color"]] <- color_out$cmd
  color_label <- color_out$label

  # Evaluating to check the grouping status of various fields. It is important that 
  # non-numeric X/Y become explicit factors here, which simplifies downstream 
  # processing (e.g., coercion to integer, no lost levels upon subsetting).
  eval_env <- new.env()
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

  # Choosing a color scale based on the nature of ColorBy.
  coloring <- eval_env$plot.data$ColorBy
  color_cmd <- NULL
  if (!is.null(coloring)) {
    if (!.is_groupable(coloring)) {
      more_data_cmds[["more_color"]] <- .coerce_to_numeric(coloring, "ColorBy")
      color_cmd <- color_out$FUN(NA_integer_)
    } else {
      more_data_cmds[["more_color"]] <- "plot.data$ColorBy <- as.factor(plot.data$ColorBy);"
      color_cmd <- color_out$FUN(.nlevels(coloring))
    }
  }

  # Creating the command to define BrushBy.
  # Note that 'all_brushes' or 'all_lassos' is needed for the ultimate eval() to obtain BrushBy.
  # This approach relatively easy to deparse() in the code tracker, rather than
  # having to construct the brush object or lasso waypoints manually.
  brush_out <- .process_brushby_choice(param_choices, all_memory)
  brush_cmd <- brush_out$cmd
  eval_env$all_brushes <- brush_out$data
  eval_env$all_lassos <- brush_out$data
  
  # Dispatch to different plotting commands, depending on whether X/Y are groupable.
  if (group_X && group_Y) {
    extra_cmds <- .griddotplot(param_choices=param_choices, ..., 
      color_label=color_label, color_cmd=color_cmd, brush_cmd=brush_cmd)

  } else if (group_X && !group_Y) {
    extra_cmds <- .violin_plot(param_choices=param_choices, ..., 
      color_label=color_label, color_cmd=color_cmd, brush_cmd=brush_cmd)

  } else if (!group_X && group_Y) {
    extra_cmds <- .violin_plot(param_choices=param_choices, ..., 
      color_label=color_label, color_cmd=color_cmd, brush_cmd=brush_cmd,
      horizontal=TRUE)

  } else {
    extra_cmds <- .scatter_plot(param_choices=param_choices, ..., 
      color_label=color_label, color_cmd=color_cmd, brush_cmd=brush_cmd)

  }
  extra_cmds$data <- c(more_data_cmds, extra_cmds$data)
  
  # Evaluating the early commands to get something to store for brushing.
  to_eval <- unlist(extra_cmds[c("data", "lim", "brush")])
  if (length(to_eval)) {
    eval(parse(text=to_eval), envir=eval_env)
  }
  plot_data <- eval_env$plot.data 
  
  # If the plot_data is not empty, remove the extra geom_blank from the plot
  # commands, as well as the creation of plot.data.all in the earlier brushing
  # command list (since the plot.data.all is only used in geom_blank)
  if (nrow(plot_data) > 0 && !is.null(extra_cmds$brush[["full"]])) {
    rm(list="plot.data.all", envir=eval_env)
    extra_cmds$brush[["full"]] <- NULL
    extra_cmds$plot[["brush_blank"]] <- NULL
  }

  # Evaluating the remaining commands.
  to_eval <- unlist(extra_cmds[c("setup", "plot")])
  plot_out <- eval(parse(text=to_eval), envir=eval_env)

  # Adding back the originally executed commands and returning the lot.  
  extra_cmds$data <- c(data_cmds, extra_cmds$data)
  return(list(cmd = extra_cmds, xy = plot_data, plot = plot_out))
}

############################################
# Internal functions: scatter plotter ----
############################################

.scatter_plot <- function(
  param_choices, x_lab, y_lab, color_label, color_cmd, brush_cmd, 
  title)
# Creates a scatter plot of numeric X/Y. This function should purely
# generate the plotting commands, with no modification of 'cmds'.
{
  plot_cmds <- list()
  plot_cmds[["ggplot"]] <- "ggplot() +"

  # Adding points to the plot (with/without colors).
  new_aes <- .build_aes(color = !is.null(color_cmd))
  point_out <- .create_points(param_choices, brush_cmd, new_aes)
  brush_cmds <- point_out$brush  
  plot_cmds <- c(plot_cmds, point_out$plot)

  # Add axes labels
  plot_cmds[["labs"]] <- .build_labs(
    x = x_lab,
    y = y_lab,
    color = color_label,
    title = title
  )

  # Defining boundaries if zoomed.
  lim_cmds <- list()
  bounds <- param_choices[[.zoomData]][[1]]
  if (!is.null(bounds)) {
    plot_cmds[["coord"]] <- sprintf(
      "coord_cartesian(xlim = c(%.5g, %.5g), ylim = c(%.5g, %.5g), expand = FALSE) +", # FALSE, to get a literal zoom.
      bounds["xmin"], bounds["xmax"], bounds["ymin"],  bounds["ymax"]
    )
  } else {
    lim_cmds[["limits"]] <- "xbounds <- range(plot.data$X, na.rm = TRUE);
ybounds <- range(plot.data$Y, na.rm = TRUE);"
    plot_cmds[["coord"]] <- "coord_cartesian(xlim = xbounds, ylim = ybounds, expand = TRUE) +"
  }

  # Both of these are NULL if not defined.
  plot_cmds[["scale_color"]] <- color_cmd

  plot_cmds[["theme_base"]] <- "theme_bw() +"
  plot_cmds[["theme_custom"]] <- "theme(legend.position = 'bottom')"

  # lim_cmds must be executed before setup_cmds when brushing to restrict!
  return(list(data=list(), lim=lim_cmds, brush=brush_cmds, setup=list(), plot=plot_cmds))
}

############################################
# Internal functions: violin plotter ----
############################################

.violin_plot <- function(
  param_choices, x_lab, y_lab, color_label, color_cmd, brush_cmd, 
  horizontal = FALSE, title)
# Generates a vertical violin plot. This function should purely
# generate the plotting commands, with no modification of 'cmds'.
{
  plot_cmds <- list()
  plot_cmds[["ggplot"]] <- "ggplot() +" # do NOT put aes here, it does not play nice with shiny brushes.
  color_set <- !is.null(color_cmd)
  
  setup_cmds <- list()
  setup_cmds[["group"]] <- "plot.data$GroupBy <- plot.data$X;"
  plot_cmds[["violin"]] <- sprintf(
    "geom_violin(%s, alpha = 0.2, data=plot.data, scale = 'width', width = 0.9) +", 
    .build_aes(color = FALSE, group = TRUE)
  )

  # Switching X and Y axes if we want a horizontal violin plot.
  # This is done in lim_cmds to guarantee sensible limits, though
  # it would technically be more appropriate to put in setup_cmds.
  data_cmds <- list()
  if (horizontal) {
    data_cmds[["swap"]] <- c("tmp <- plot.data$X;
plot.data$X <- plot.data$Y;
plot.data$Y <- tmp;")
    tmp <- y_lab
    y_lab <- x_lab
    x_lab <- tmp
  }
  data_cmds[["na.rm"]] <- "plot.data <- subset(plot.data, !is.na(X) & !is.na(Y));"

  # Figuring out the scatter. This is done ahead of time to guarantee the
  # same results regardless of the subset used for brushing. Note adjust=1
  # for consistency with geom_violin (differs from geom_quasirandom default).
  setup_cmds[["seed"]] <- "set.seed(100);"
  setup_cmds[["calcX"]] <- "plot.data$jitteredX <- vipor::offsetX(plot.data$Y,
    x=plot.data$X, width=0.4, varwidth=FALSE, adjust=1,
    method='quasirandom', nbins=NULL) + as.integer(plot.data$X);"

  # Adding the points to the plot (with/without brushing).
  new_aes <- .build_aes(color = color_set, alt=c(x="jitteredX"))
  point_out <- .create_points(param_choices, brush_cmd, new_aes)
  brush_cmds <- point_out$brush  
  plot_cmds <- c(plot_cmds, point_out$plot)

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

  lim_cmds <- list()
  if (!is.null(bounds)) {
    plot_cmds[["coord"]] <- sprintf(
      "%s(xlim = c(%.5g, %.5g), ylim = c(%.5g, %.5g), expand = FALSE) +", # FALSE, to get a literal zoom.
      coord_cmd, bounds["xmin"], bounds["xmax"], bounds["ymin"], bounds["ymax"]
    )
  } else {
    lim_cmds[["limits"]] <- "ybounds <- range(plot.data$Y, na.rm = TRUE);"
    plot_cmds[["coord"]] <- sprintf("%s(xlim = NULL, ylim = ybounds, expand = TRUE) +", coord_cmd)
  }

  # Both of these are just NULL if no color/brush is defined.
  plot_cmds[["scale_color"]] <- color_cmd

  plot_cmds[["scale_x"]] <- "scale_x_discrete(drop = FALSE) +" # preserving the x-axis range.
  plot_cmds[["theme_base"]] <- "theme_bw() +"
  plot_cmds[["theme_custom"]] <- "theme(legend.position = 'bottom')"

  # lim_cmds must be executed before setup_cmds, to ensure bounds are correctly defined.
  return(list(data=data_cmds, lim=lim_cmds, brush=brush_cmds, setup=setup_cmds, plot=plot_cmds))
}

############################################
# Internal functions: rectangle plotter ----
############################################

.griddotplot <- function(
  param_choices, x_lab, y_lab, color_label, color_cmd, brush_cmd, 
  title)
# Generates a grid dot plot. This function should purely
# generate the plotting commands, with no modification of 'cmds'.
{
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

  plot_cmds <- list()
  plot_cmds[["ggplot"]] <- "ggplot(plot.data) +"
  plot_cmds[["tile"]] <- "geom_tile(aes(x = X, y = Y, height = 2*Radius, width = 2*Radius),
    summary.data, color = 'black', alpha = 0, size = 0.5) +"

  # Adding the points to the plot (with/without brushing).
  new_aes <- .build_aes(color = !is.null(color_cmd), alt=c(x="jitteredX", y="jitteredY"))
  point_out <- .create_points(param_choices, brush_cmd, new_aes)
  brush_cmds <- point_out$brush  
  plot_cmds <- c(plot_cmds, point_out$plot)

  plot_cmds[["scale"]] <- "scale_size_area(limits = c(0, 1), max_size = 30) +"

  # Adding the commands to color the points and the brushing box (NULL if undefined).
  plot_cmds[["scale_color"]] <- color_cmd

  # Creating labels.
  plot_cmds[["labs"]] <- .build_labs(
    x = x_lab,
    y = y_lab,
    color = color_label,
    title = title
  )

  # Defining boundaries if zoomed.
  bounds <- param_choices[[.zoomData]][[1]]
  if (!is.null(bounds)) {
    plot_cmds[["coord"]] <- sprintf(
      "coord_cartesian(xlim = c(%.5g, %.5g), ylim = c(%.5g, %.5g), expand = FALSE) +",
      bounds["xmin"], bounds["xmax"], bounds["ymin"], bounds["ymax"]
    )
  }

  plot_cmds[["scale_x"]] <- "scale_x_discrete(drop = FALSE) +"
  plot_cmds[["scale_y"]] <- "scale_y_discrete(drop = FALSE) +"

  plot_cmds[["guides"]] <- "guides(size = 'none') +"
  plot_cmds[["theme_base"]] <- "theme_bw() +"
  plot_cmds[["theme_custom"]] <- "theme(legend.position = 'bottom', legend.box = 'vertical')"

  return(list(data=list(), lim=list(), brush=brush_cmds, setup=setup_cmds, plot=plot_cmds))
}

############################################
# Internal functions: coloring ----
############################################

.process_colorby_choice_for_column_plots <- function(param_choices, all_memory, se, colormap) 
# This function defines the colour-by choices for column plots,
# i.e., where each point represents a sample.
{
  output <- list(cmd=NULL, label=NA_character_, FUN=NULL)
  color_choice <- param_choices[[.colorByField]]
  colormap_cmd <- NULL

  if (color_choice==.colorByColDataTitle) {
    covariate_name <- param_choices[[.colorByColData]]
    covariate_as_string <- deparse(covariate_name)
    output$cmd <-  sprintf("plot.data$ColorBy <- colData(se)[,%s];", covariate_as_string)
    output$label <- covariate_name
    colormap_cmd <- sprintf("colDataColorMap(colormap, %s, discrete=%%s)(%%i)", covariate_as_string)

  } else if (color_choice==.colorByRowTableTitle || color_choice==.colorByFeatNameTitle) {

    # Set the color to the selected gene
    if (color_choice==.colorByRowTableTitle) {
      chosen_tab <- .decoded2encoded(param_choices[[.colorByRowTable]])
      chosen_gene <- all_memory$rowStatTable[chosen_tab, .rowStatSelected]
      validate(need(
        length(chosen_gene)==1L,
        sprintf("Invalid '%s' > '%s' input", .colorByField, .colorByRowTableTitle)
      ))
      assay_choice <- param_choices[[.colorByRowTableAssay]]

    } else {
      chosen_gene <- param_choices[[.colorByFeatName]]
      validate(need(
        chosen_gene %in% rownames(se),
        sprintf("Invalid '%s' > '%s' input", .colorByField, .colorByFeatNameTitle)
      ))
      assay_choice <- param_choices[[.colorByFeatNameAssay]]

    }

    assay_name <- assayNames(se)[assay_choice]
    assay_access <- ifelse(
      identical(assay_name, ""),
      assay_choice,
      sprintf("'%s'",assay_name )
    )
    
    output$cmd <- sprintf("plot.data$ColorBy <- assay(se, %i)[%s,];", 
                          assay_choice, deparse(chosen_gene))
    output$label <- .gene_axis_label(se, chosen_gene, assay_choice, multiline = TRUE)
    colormap_cmd <- sprintf("assayColorMap(colormap, %s, discrete=%%s)(%%i)", assay_access)

  }

  output$FUN <- .create_color_function_chooser(colormap_cmd)
  return(output)
}

.process_colorby_choice_for_row_plots <- function(param_choices, all_memory, se, colormap) 
# This function defines the colour-by choices for row-based plots,
# i.e., where each point represents a feature.
{
  output <- list(cmd=NULL, label=NA_character_, FUN=NULL)
  color_choice <- param_choices[[.colorByField]]
  colormap_cmd <- NULL

  if (color_choice==.colorByRowDataTitle) {
    covariate_name <- param_choices[[.colorByRowData]]
    covariate_as_string <- deparse(covariate_name)
    output$cmd <-  sprintf("plot.data$ColorBy <- rowData(se)[,%s];", covariate_as_string)
    output$label <- covariate_name
    colormap_cmd <- sprintf("rowDataColorMap(colormap, %s, discrete=%%s)(%%i)", covariate_as_string)
    output$FUN <- .create_color_function_chooser(colormap_cmd)

  } else if (color_choice==.colorByRowTableTitle || color_choice==.colorByFeatNameTitle) {

    # Set the color to the selected gene
    if (color_choice==.colorByRowTableTitle) {
      chosen_tab <- .decoded2encoded(param_choices[[.colorByRowTable]])
      chosen_gene <- all_memory$rowStatTable[chosen_tab, .rowStatSelected]
      validate(need(
        length(chosen_gene)==1L,
        sprintf("Invalid '%s' > '%s' input", .colorByField, .colorByRowTableTitle)
      ))
      col_choice <- param_choices[[.colorByRowTableColor]]

    } else {
      chosen_gene <- param_choices[[.colorByFeatName]]
      validate(need(
        chosen_gene %in% rownames(se),
        sprintf("Invalid '%s' > '%s' input", .colorByField, .colorByFeatNameTitle)
      ))
      col_choice <- param_choices[[.colorByFeatNameColor]]

    }

    output$cmd <- sprintf("plot.data$ColorBy <- FALSE;
plot.data[%s, 'ColorBy'] <- TRUE;
plot.data <- plot.data[order(plot.data$ColorBy),]", deparse(chosen_gene)) # To ensure it is plotted last.
    output$label <- .gene_axis_label(se, chosen_gene, assay_id=NULL)
    output$FUN <- function(nlevels) {
       # Argument is ignored, as we should know the number of levels beforehand.
       sprintf("scale_color_manual(values=c(`FALSE`='black', `TRUE`=%s), drop=FALSE) +", deparse(col_choice))
    }
  }
  return(output)
}

.create_color_function_chooser <- function(command) 
# Generating a function that returns a ggplot coloring function 
# depending on the number of levels.
{
    if (is.null(command)) {
        return(function(nlevels) NULL)
    }
    function(nlevels) {
        if (is.finite(nlevels)) {
            cm_command <- sprintf(command, "TRUE", nlevels)
            return(list(sprintf("scale_color_manual(values=%s, na.value='grey50', drop=FALSE) +", cm_command),
                        sprintf("scale_fill_manual(values=%s, na.value='grey50', drop=FALSE) +", cm_command)))
        } else {
            cm_command <- sprintf(command, "FALSE", 21L)
            return(list(sprintf("scale_color_gradientn(colors=%s, na.value='grey50') +", cm_command),
                        sprintf("scale_fill_gradientn(colors=%s, na.value='grey50') +", cm_command)))
        }
    }
}

############################################
# Internal functions: brushing ----
############################################

.process_brushby_choice <- function(param_choices, all_memory) {
  brush_in <- param_choices[[.brushByPlot]]
  cmd <- NULL
  brush_obj <- list()

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
        cmd <- sprintf("brushed_pts <- shiny::brushedPoints(%s, all_brushes[['%s']])",
                       source_data, transmitter)
        cmd <- c(cmd, "plot.data$BrushBy <- rownames(plot.data) %in% rownames(brushed_pts);")
        cmd <- paste(cmd, collapse="\n")

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
            cmd <- sprintf("to_check <- subset(%s, !is.na(X) & !is.na(Y))", source_data)
            cmd <- c(cmd,
                    sprintf("brushed_pts <- mgcv::in.out(all_lassos[['%s']], cbind(as.numeric(to_check$%s), as.numeric(to_check$%s)))",
                            transmitter, v1, v2))
            cmd <- c(cmd, "plot.data$BrushBy <- rownames(plot.data) %in% rownames(to_check)[brushed_pts]")
            cmd <- paste(cmd, collapse="\n")
        }
    }
  }

  return(list(cmd=cmd, data=brush_obj))
}

.create_points <- function(param_choices, brush_cmd, aes) 
# Implementing the brushing effect.
{
  brush_cmds <- list()
  brush_cmds[["init"]] <- brush_cmd
  plot_cmds <- list()

  if (!is.null(brush_cmd)) {
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
      # Duplicate plot.data before brushing, to make sure that axes are retained
      # even in case of an empty brushed subset. This does _not_ replace coord_cartesian
      # (necessary for zooming, flipping and avoiding plot expansion due to brushing box),
      # or scale_*_discrete (necessary to avoid dropping empty levels).
      brush_cmds[["full"]] <- "plot.data.all <- plot.data;"
      brush_cmds[["subset"]] <- "plot.data <- subset(plot.data, BrushBy);"
      plot_cmds[["brush_blank"]] <- "geom_blank(data = plot.data.all, inherit.aes = FALSE, aes(x = X, y = Y)) +"
      plot_cmds[["brush_restrict"]] <- sprintf(
        "geom_point(%s, alpha = 0.6, plot.data) +", 
        aes
      )
    }
  } else {
    plot_cmds[["point"]] <- sprintf(
      "geom_point(%s, alpha = 0.6, plot.data) +", 
      aes
    )
  }
  
  return(list(brush=brush_cmds, plot=plot_cmds))
}

############################################
# Internal functions: aesthetics ----
############################################

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

.build_aes <- function(x = TRUE, y = TRUE, color = FALSE, shape = FALSE, fill = FALSE, group = FALSE, alt=NULL) {
    active_aes <- .all_aes_values[c(x, y, color, shape, fill, group)]
    if (!is.null(alt)) {
        active_aes <- c(active_aes, alt)
        active_aes <- active_aes[!duplicated(names(active_aes), fromLast=TRUE)]
    }
    aes_specs <- mapply(FUN = .make_single_aes, names(active_aes), active_aes, USE.NAMES = FALSE)
    aes_specs <- paste(aes_specs, collapse = ", ")
    return(sprintf("aes(%s)", aes_specs))
}

.make_single_aes <- function(name, value){
    sprintf("%s = %s", name, value)
}

.build_labs <- function(
  x = NA_character_, y = NA_character_,
  color = NA_character_, shape = NA_character_,
  fill = NA_character_, group = NA_character_,
  title = NA_character_
){
    labs_specs <- c(x, y, color, shape, fill, group, title)
    names(labs_specs) <- .all_labs_names
    labs_specs <- labs_specs[!is.na(labs_specs)]
    if (identical(length(labs_specs), 0L)){
      return(NULL)
    }
    labs_specs <- mapply(FUN = .make_single_lab, names(labs_specs), labs_specs, USE.NAMES = FALSE)
    labs_specs <- paste(labs_specs, collapse = ", ")
    return(sprintf("labs(%s) +", labs_specs))
}

.make_single_lab <- function(name, value){
    sprintf("%s = %s", name, deparse(value))
}

############################################
# Internal functions: grouping ----
############################################

.nlevels <- function(covariate){
  # numeric covariates are assume to have infinite levels
  if (is.numeric(covariate)){
    return(Inf)
  }
  # default answer for factors
  if (is.factor(covariate)){
    return(nlevels(covariate))
  }
  # default answer for character would be NULL
  return(length(unique(covariate)))
}

.is_groupable <- function(x, max_levels = 24){
  return(.nlevels(x) <= max_levels)
}

.coerce_to_numeric <- function(values, field, warn=TRUE) {
  if (!is.numeric(values)) {
    if (warn) {
      warning("coloring covariate has too many unique values, coercing to numeric")
    }
    if (is.factor(values)) {
      extra_cmd <- sprintf("plot.data$%s <- as.numeric(plot.data$%s)", field, field)
    } else {
      extra_cmd <- sprintf("plot.data$%s <- as.numeric(as.factor(plot.data$%s))", field, field)
    }
    return(extra_cmd)
  }
  return(NULL)
}

############################################
# Plot update functions ----
############################################

.self_brush_box <- function(mode, i, memory, flip=FALSE) { 
    current <- memory[[mode]][,.brushData][[i]]
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
    
    plot_name <- paste0(mode, i)
    cmd <- sprintf("geom_rect(aes(xmin = %s, xmax = %s, ymin = %s, ymax = %s), color='%s', alpha=0, 
    data=do.call(data.frame, all_brushes[['%s']][c('xmin', 'xmax', 'ymin', 'ymax')]), inherit.aes=FALSE)",
                                         xmin, xmax, ymin, ymax, panel_colors[mode], plot_name)
    
    data <- list()
    data[[plot_name]] <- current
    return(list(cmd=cmd, data=data))
}

.self_lasso_path <- function(mode, i, memory, flip=FALSE) {
    current <- memory[[mode]][,.lassoData][[i]]
    if (is.null(current) || !is.null(memory[[mode]][,.brushData][[i]])) {
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
  
    plot_name <- paste0(mode, i)
    
    if (identical(nrow(current), 1L)) { # lasso has only a start point
      point_cmd <- sprintf("geom_point(aes(x = %s, y = %s), 
    data=data.frame(x = all_lassos[['%s']][,1], y = all_lassos[['%s']][,2]),
    inherit.aes=FALSE, alpha=1, stroke = 1, color = '%s', size = %s, shape = %s)",
        x, y, plot_name, plot_name, panel_colors[mode],
        .lassoStartSize, .lassoStartShape)
      full_cmd_list <- list(point_cmd)
      
    } else if (is_closed){ # lasso is closed
      polygon_cmd <- sprintf("geom_polygon(aes(x = %s, y = %s), alpha=%s, color='%s', 
    data=data.frame(x = all_lassos[['%s']][,1], y = all_lassos[['%s']][,2]), 
    inherit.aes=FALSE, fill = '%s')", 
          x, y ,
          .brushFillOpacity, panel_colors[mode],
          plot_name, plot_name, brush_fill_color[mode])
    
        scale_fill_cmd <- sprintf(
          "scale_fill_manual(values = c('TRUE' = '%s', 'FALSE' = '%s'))",
          panel_colors[mode], brush_fill_color[mode])

        guides_cmd <- "guides(shape = 'none')"
        full_cmd_list <- list(polygon_cmd, scale_fill_cmd, guides_cmd)

    } else { # lasso is still open
      path_cmd <- sprintf("geom_path(aes(x = %s, y = %s), 
    data=data.frame(x = all_lassos[['%s']][,1], y = all_lassos[['%s']][,2]),
    inherit.aes=FALSE, alpha=1, color='%s', linetype = 'longdash')", 
        x, y, plot_name, plot_name, panel_colors[mode])
    
        point_cmd <- sprintf("geom_point(aes(x = %s, y = %s, size = First, shape = First), 
    data=data.frame(x = all_lassos[['%s']][,1], y = all_lassos[['%s']][,2], 
                    First = seq_len(nrow(all_lassos[['%s']]))==1L),
    inherit.aes=FALSE, alpha=1, stroke = 1, color = '%s')",
        x, y, plot_name, plot_name, plot_name, panel_colors[mode])

        scale_shape_cmd <- sprintf(
          "scale_shape_manual(values = c('TRUE' = %s, 'FALSE' = %s))",
          .lassoStartShape, .lassoWaypointShape
        )
        scale_size_cmd <- sprintf(
          "scale_size_manual(values = c('TRUE' = %s, 'FALSE' = %s))",
          .lassoStartSize, .lassoWaypointSize
        )
        guides_cmd <- "guides(shape = 'none', size = 'none')"
        full_cmd_list <- list(
          path_cmd, point_cmd, scale_shape_cmd, scale_size_cmd, guides_cmd)
        
    }
   
    data <- list()
    data[[plot_name]] <- current
    return(list(cmd=full_cmd_list, data=data))
}

# Lasso constants
.lassoStartShape <- 22
.lassoWaypointShape <- 21
.lassoStartSize <- 1.5
.lassoWaypointSize <- 0.5
