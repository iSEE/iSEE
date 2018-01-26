############################################
# Plotting constants -----
############################################

.all_aes_names <- c("x", "y", "color", "shape", "fill", "group")
.all_aes_values <-
  c("X", "Y", "ColorBy", "ShapeBy", "FillBy", "GroupBy")
.all_labs_values <- .all_aes_values

names(.all_aes_values) <- .all_aes_names
names(.all_labs_values) <- .all_aes_names

############################################
# .make_redDimPlot  ----
############################################

.make_redDimPlot <- function(id, se, input, all_coordinates, all_memory, color_map)
# Makes the dimension reduction plot.
{
  param_choices <- all_memory$redDim[id,]
  data_cmds <- list()
  data_cmds[["reducedDim"]] <- sprintf(
    "red.dim <- reducedDim(se, %i);", param_choices[[.redDimType]])
  data_cmds[["xy"]] <- sprintf(
    "plot.data <- data.frame(X = red.dim[, %i], Y = red.dim[, %i], row.names=colnames(se));",
    param_choices[[.redDimXAxis]],
    param_choices[[.redDimYAxis]]
  )

  # Adding colour data (and commands in color_FUN).
  setup_cmds <- list()
  color_out <- .process_colorby_choice(param_choices, se, input, color_map)
  setup_cmds[["color"]] <- color_out$cmd
  color_label <- color_out$label
  color_FUN <- color_out$FUN

  # Adding brushing data (plot-specific commands will be added later).
  brush_out <- .process_brushby_choice(param_choices, input, all_memory, color=brush_stroke_color_full["redDim"])

  # Generating the plotting commands.
  .create_plot(
    data_cmds, setup_cmds, se, all_coordinates,
    param_choices=param_choices,
    x_lab=sprintf("Dimension %s", param_choices[[.redDimXAxis]]),
    y_lab=sprintf("Dimension %s", param_choices[[.redDimYAxis]]),
    color_FUN=color_FUN, color_label=color_label, 
    brush_cmd=brush_out$cmd, brush_show_cmd=brush_out$show
  )
}

############################################
# .make_colDataPlot  ----
############################################

.make_colDataPlot <- function(id, se, input, all_coordinates, all_memory, color_map)
# Makes a plot of column data variables.
{
  param_choices <- all_memory$colData[id,]
  data_cmds <- list()
  y_lab <- param_choices[[.colDataYAxis]]
  data_cmds[["y"]] <- sprintf(
    "plot.data <- data.frame(Y = colData(se)[,%s], row.names=colnames(se));", 
    deparse(y_lab) # deparse() automatically adds quotes, AND protects against existing quotes/escapes.
  )
  
  # Prepare X-axis data.
  if (param_choices[[.colDataXAxis]]==.colDataXAxisNothingTitle) {
    x_lab <- ''
    data_cmds[["x"]] <- "plot.data$X <- factor(integer(ncol(se)))"
  } else {
    x_lab <- param_choices[[.colDataXAxisColData]]
    data_cmds[["x"]] <- sprintf("plot.data$X <- colData(se)[,%s];", deparse(x_lab))
  }

  # Adding colour commands.
  setup_cmds <- list()
  color_out <- .process_colorby_choice(param_choices, se, input, color_map)
  setup_cmds[["color"]] <- color_out$cmd
  color_FUN <- color_out$FUN
  color_label <- color_out$label

  # Adding brushing commands.
  brush_out <- .process_brushby_choice(param_choices, input, all_memory, color=brush_stroke_color_full["colData"])

  # Generating the plot.
  .create_plot(
    data_cmds, setup_cmds, se, all_coordinates,
    param_choices=param_choices, x_lab=x_lab, y_lab=y_lab,
    color_FUN=color_FUN, color_label=color_label, 
    brush_cmd=brush_out$cmd, brush_show_cmd=brush_out$show
  )
}

############################################
# .make_geneExprPlot  ----
############################################

.make_geneExprPlot <- function(id, se, input, all_coordinates, all_memory, color_map)
# Makes a gene expression plot.
{
  param_choices <- all_memory$geneExpr[id,]
  data_cmds <- list()

  ## Setting up the y-axis:
  y_choice <- param_choices[[.geneExprYAxis]]
  if (y_choice==.geneExprYAxisGeneTableTitle) {
    gene_selected_y <- .find_linked_gene(param_choices[[.geneExprYAxisGeneTable]], input)
    validate(need( 
      length(gene_selected_y)==1L,
      sprintf("Invalid '%s' > '%s' input", .geneExprYAxis, y_choice)
    ))

  } else if (y_choice==.geneExprYAxisGeneTextTitle) {
    gene_selected_y <- param_choices[[.geneExprYAxisGeneText]]
    validate(need( 
      gene_selected_y %in% rownames(se),
      sprintf("Invalid '%s' > '%s' input", .geneExprYAxis, y_choice)
    ))
  }

  assay_choice <- param_choices[[.geneExprAssay]]
  y_lab <- .gene_axis_label(se, gene_selected_y, assay_choice, multiline = FALSE)
  data_cmds[["y"]] <- sprintf(
    "plot.data <- data.frame(Y=assay(se, %i)[%s,], row.names = colnames(se))",
    assay_choice, deparse(gene_selected_y) # deparse() also handles integer selections correctly.
  )

  ## Checking X axis choice:
  x_choice <- param_choices[[.geneExprXAxis]]

  if (x_choice==.geneExprXAxisColDataTitle) { # colData column selected
    x_lab <- param_choices[[.geneExprXAxisColData]]
    data_cmds[["x"]] <- sprintf(
       "plot.data$X <- colData(se)[,%s];", 
       deparse(x_lab)
    )

  } else if (x_choice==.geneExprXAxisGeneTableTitle || x_choice==.geneExprXAxisGeneTextTitle) { # gene selected

    if (x_choice==.geneExprXAxisGeneTableTitle) {
      gene_selected_x <- .find_linked_gene(param_choices[[.geneExprXAxisGeneTable]], input)
      validate(need( 
        length(gene_selected_y)==1L,
        sprintf("Invalid '%s' > '%s' input", .geneExprXAxis, x_choice)
      ))

    } else if (x_choice==.geneExprXAxisGeneTextTitle) {
      gene_selected_x <- param_choices[[.geneExprXAxisGeneText]]
      validate(need(
        gene_selected_x %in% rownames(se),
        sprintf("Invalid '%s' > '%s' input", .geneExprXAxis, x_choice)
      ))
    }

    x_lab <- .gene_axis_label(se, gene_selected_x, assay_choice, multiline = FALSE)
    data_cmds[["x"]] <- sprintf(
      "plot.data$X <- assay(se, %i)[%s,];",
      assay_choice, deparse(gene_selected_x)
    )

  } else { # no x axis variable specified: show single violin
    x_lab <- ''
    data_cmds[["x"]] <- "plot.data$X <- factor(integer(ncol(se)))"
  }

  # Adding colour commands.
  setup_cmds <- list()
  color_out <- .process_colorby_choice(param_choices, se, input, color_map)
  setup_cmds[["color"]] <- color_out$cmd
  color_FUN <- color_out$FUN
  color_label <- color_out$label

  # Adding brushing commands.
  brush_out <- .process_brushby_choice(param_choices, input, all_memory, color=brush_stroke_color_full["geneExpr"])

  # Generating the plot.
  .create_plot(
    data_cmds, setup_cmds, se, all_coordinates,
    param_choices=param_choices, x_lab=x_lab, y_lab=y_lab,
    color_FUN=color_FUN, color_label=color_label, 
    brush_cmd=brush_out$cmd, brush_show_cmd=brush_out$show
  )
}

############################################
# Internal functions: central plotter ----
############################################

.create_plot <- function(data_cmds, setup_cmds, se, all_coordinates, ...)
# This function will generate plotting commands appropriate to
# each type of X/Y. It does so by evaluating 'plot.data' to
# determine the nature of X/Y, and then choosing the plot to match.
#
# Note that we need 'se' and 'all_coordinates' to be passed as arguments
# for the evaluations to execute in this environment. All evaluations
# are to take place in this function, not in the calling environment
# or in child environments. This constrains the scope of 'eval' calls.
{
  eval_env <- new.env()
  eval(parse(text=unlist(data_cmds)), envir=eval_env)
  more_data_cmds <- list() 

  # Cleaning up the grouping status of various fields. It is important that 
  # non-numeric X/Y become explicit factors here, which simplifies downstream 
  # processing (e.g., coercion to integer, no lost levels upon subsetting).
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

  coloring <- eval_env$plot.data$ColorBy
  color_discrete <- FALSE
  if (!is.null(coloring)) {
    color_discrete <- .is_groupable(coloring)
    if (!color_discrete) {
      setup_cmds[["more_color"]] <- .coerce_to_numeric(coloring, "ColorBy")
    }
  }

  # Dispatch to different plotting commands, depending on whether X/Y are groupable.
  if (group_X && group_Y) {
    extra_cmds <- .griddotplot(..., color_discrete=color_discrete)

  } else if (group_X && !group_Y) {
    extra_cmds <- .violin_plot(..., color_discrete=color_discrete)

  } else if (!group_X && group_Y) {
    extra_cmds <- .violin_plot(..., color_discrete=color_discrete, horizontal=TRUE)

  } else {
    extra_cmds <- .scatter_plot(..., color_discrete=color_discrete)

  }
  extra_cmds$data <- c(more_data_cmds, extra_cmds$data)
  extra_cmds$setup <- c(setup_cmds, extra_cmds$setup)
  print(extra_cmds)

  # Evaluating the early commands to get something to store for brushing.
  to_eval <- unlist(extra_cmds[c("data", "lim", "brush")])
  if (length(to_eval)) {
    eval(parse(text=to_eval), envir=eval_env)
  }
  plot_data <- eval_env$plot.data 

  # Evaluating the remaining commands.
  to_eval <- unlist(extra_cmds[c("setup", "plot")])
  plot_out <- eval(parse(text=to_eval), envir=eval_env)
  
  extra_cmds$data <- c(data_cmds, extra_cmds$data)
  return(list(cmd = extra_cmds, xy = plot_data, plot = plot_out))
}

############################################
# Internal functions: scatter plotter ----
############################################

.scatter_plot <- function(param_choices, x_lab, y_lab, color_label, color_FUN, color_discrete, brush_cmd, brush_show_cmd)
# Creates a scatter plot of numeric X/Y. This function should purely
# generate the plotting commands, with no modification of 'cmds'.
{
  plot_cmds <- list()
  plot_cmds[["ggplot"]] <- "ggplot() +"
  
  # Determining the colors to use.
  color_cmd <- color_FUN(color_discrete)
  color_set <- !is.null(color_cmd)

  # Implementing the brushing effect.
  all_brush_cmds <- list(init=brush_cmd)
  if (!is.null(brush_cmd)) {
    brush_effect <- param_choices[[.brushEffect]]
    if (brush_effect==.brushColorTitle) {
      plot_cmds[["brush_other"]] <- sprintf(
        "geom_point(%s, subset(plot.data, !BrushBy)) +",
        .build_aes(color = color_set)
      )
      plot_cmds[["brush_color"]] <- sprintf(
        "geom_point(%s, data = subset(plot.data, BrushBy), color = %s) +",
        .build_aes(color = color_set), 
        deparse(param_choices[[.brushColor]])
      )
    }
    if (brush_effect==.brushTransTitle) {
      plot_cmds[["brush_other"]] <- sprintf(
        "geom_point(%s, subset(plot.data, !BrushBy), alpha = %.2f) +",
        .build_aes(color = color_set), param_choices[[.brushTransAlpha]]
      )
      plot_cmds[["brush_alpha"]] <- sprintf(
        "geom_point(%s, subset(plot.data, BrushBy)) +",
        .build_aes(color = color_set)
      )
    }
    if (brush_effect==.brushRestrictTitle) {
      all_brush_cmds[["subset"]] <- "plot.data <- subset(plot.data, BrushBy);"
      plot_cmds[["brush_restrict"]] <- sprintf(
        "geom_point(%s, plot.data) +",
        .build_aes(color = color_set)
      )
    }
  } else {
    plot_cmds[["point"]] <- sprintf(
      "geom_point(%s, plot.data) +",
      .build_aes(color = color_set)
    )
  }

  # Add axes labels
  plot_cmds[["labs"]] <- .build_labs(
    x = x_lab,
    y = y_lab,
    color = color_label
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

  if (color_set){
    plot_cmds[["scale_color"]] <- color_cmd
  }
  plot_cmds[["brush_tile"]] <- brush_show_cmd

  plot_cmds[["theme_base"]] <- "theme_bw() +"
  plot_cmds[["theme_custom"]] <- "theme(legend.position = 'bottom')"

  # lim_cmds must be executed before setup_cmds when brushing to restrict!
  return(list(data=list(), lim=lim_cmds, brush=all_brush_cmds, setup=list(), plot=plot_cmds))
}

############################################
# Internal functions: violin plotter ----
############################################

.violin_plot <- function(param_choices, x_lab, y_lab, color_label, color_FUN, color_discrete, brush_cmd, brush_show_cmd, horizontal = FALSE)
# Generates a vertical violin plot. This function should purely
# generate the plotting commands, with no modification of 'cmds'.
{
  setup_cmds <- list()
  color_cmd <- color_FUN(color_discrete)
  color_set <- !is.null(color_cmd)
  fill_set <- (color_set && color_discrete)
  if (fill_set) {
    setup_cmds[["fill"]] <- "plot.data$FillBy <- plot.data$ColorBy;"
  }

  plot_cmds <- list()
  plot_cmds[["ggplot"]] <- sprintf(
    "ggplot(plot.data, %s) +",
    .build_aes(color = color_set, fill = fill_set, group = TRUE)
  )
  plot_cmds[["violin"]] <- "geom_violin(alpha = 0.2, scale = 'width') +"

  # Switching X and Y axes if we want a horizontal violin plot.
  # This is done in lim_cmds to guarantee sensible limits, though
  # it would technically be more appropriate to put in setup_cmds.
  data_cmds <- list()
  if (horizontal) {
    data_cmds[["swap"]] <- c("tmp <- plot.data$X;
plot.data$X <- plot.data$Y;
plot.data$Y <- tmp;")
  }
  data_cmds[["na.rm"]] <- "plot.data <- subset(plot.data, !is.na(X) & !is.na(Y));"

  # Setting up grouping status for the violins.
  setup_cmds <- list()
  setup_cmds[["group"]] <- "plot.data$GroupBy <- plot.data$X;"

  # Figuring out the scatter. This is done ahead of time to guarantee the
  # same results regardless of the subset used for brushing. Note adjust=1
  # for consistency with geom_violin (differs from geom_quasirandom default).
  setup_cmds[["seed"]] <- "set.seed(100);"
  setup_cmds[["calcX"]] <- "plot.data$jitteredX <- vipor::offsetX(plot.data$Y,
    x=plot.data$X, width=0.4, varwidth=FALSE, adjust=1,
    method='quasirandom', nbins=NULL) + as.integer(plot.data$X);"

  # Implementing the brushing effect.
  new_aes <- .build_aes(color = color_set, alt=c(x="jitteredX"))
  all_brush_cmds <- list(init=brush_cmd)
  if (!is.null(brush_cmd)) {
    brush_effect <- param_choices[[.brushEffect]]
    if (brush_effect==.brushColorTitle) {
      plot_cmds[["brush_other"]] <- sprintf("geom_point(%s, subset(plot.data, !BrushBy)) +", new_aes)
      plot_cmds[["brush_color"]] <- sprintf(
        "geom_point(%s, data = subset(plot.data, BrushBy), color = %s) +",
        new_aes, deparse(param_choices[[.brushColor]])
      )

    } else  if (brush_effect==.brushTransTitle) {
      plot_cmds[["brush_other"]] <- sprintf(
        "geom_point(%s, subset(plot.data, !BrushBy), alpha = %.2f) +",
        new_aes, param_choices[[.brushTransAlpha]]
      )
      plot_cmds[["brush_alpha"]] <- sprintf("geom_point(%s, subset(plot.data, BrushBy)) +", new_aes)

    } else if (brush_effect==.brushRestrictTitle) {
      # Need to subset explicitly, to adjust the density calculations and ensure
      # doesntream brushes are correct. Note, subsetting BEFORE vipor calculations.
      all_brush_cmds[["subset"]] <- "plot.data <- subset(plot.data, BrushBy);"
      plot_cmds[["violin"]] <- "geom_violin(data = plot.data, alpha = 0.2, scale = 'width') +"
      plot_cmds[["brush_restrict"]] <- sprintf("geom_point(%s, plot.data) +", new_aes)
    }
  } else {
    plot_cmds[["point"]] <- sprintf("geom_point(%s, alpha = 0.6, size = 1) +", new_aes)
  }

  plot_cmds[["labs"]] <- .build_labs(
    x = x_lab,
    y = y_lab,
    color = color_label,
    fill = color_label
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

  if (color_set){
    plot_cmds[["scale_color"]] <- color_cmd
  }
  plot_cmds[["brush_tile"]] <- brush_show_cmd

  plot_cmds[["scale_x"]] <- "scale_x_discrete(drop = FALSE) +" # preserving the x-axis range.
  plot_cmds[["theme_base"]] <- "theme_bw() +"
  plot_cmds[["theme_custom"]] <- "theme(legend.position = 'bottom')"

  # lim_cmds must be executed before setup_cmds, to ensure bounds are correctly defined.
  return(list(data=data_cmds, lim=lim_cmds, brush=all_brush_cmds, setup=setup_cmds, plot=plot_cmds))
}

############################################
# Internal functions: rectangle plotter ----
############################################

.griddotplot <- function(param_choices, x_lab, y_lab, color_label, color_FUN, color_discrete, brush_cmd, brush_show_cmd)
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

  # Determining the colors to use.
  color_cmd <- color_FUN(color_discrete)
  color_set <- !is.null(color_cmd)

  # Implementing the brushing effect.
  new_aes <- .build_aes(color = color_set, alt=c(x="jitteredX", y="jitteredY"))
  all_brush_cmds <- list(init=brush_cmd)
  if (!is.null(brush_cmd)) {
    brush_effect <- param_choices[[.brushEffect]]

    if (brush_effect==.brushColorTitle) {
      plot_cmds[["point"]] <-
        "geom_tile(aes(x = X, y = Y, height = 2*Radius, width = 2*Radius), summary.data, color = 'black', alpha = 0, size = 0.5) +"
      plot_cmds[["brush_other"]] <- sprintf(
        "geom_point(%s, subset(plot.data, !BrushBy), width = 0.2, height = 0.2) +",
        new_aes
      )
      plot_cmds[["brush_color"]] <- sprintf(
        "geom_point(%s, data = subset(plot.data, BrushBy), color = %s, width = 0.2, height = 0.2) +",
        new_aes, deparse(param_choices[[.brushColor]])
      )
    }
    if (brush_effect==.brushTransTitle) {
      plot_cmds[["point"]] <-
        "geom_tile(aes(x = X, y = Y, height = 2*Radius, width = 2*Radius), summary.data, color = 'black', alpha = 0, size = 0.5) +"
      plot_cmds[["brush_other"]] <- sprintf(
        "geom_point(%s, subset(plot.data, !BrushBy), alpha = %.2f, width = 0.2, height = 0.2) +",
        new_aes, param_choices[[.brushTransAlpha]]
      )
      plot_cmds[["brush_alpha"]] <- sprintf(
        "geom_point(%s, subset(plot.data, BrushBy)) +",
        new_aes
      )
    }
    if (brush_effect==.brushRestrictTitle) {
      # Note subsetting must occur before all other calculations.
      all_brush_cmds[["subset"]] <- "plot.data <- subset(plot.data, BrushBy);"
      plot_cmds[["point"]] <-
        "geom_tile(aes(x = X, y = Y, height = 2*Radius, width = 2*Radius), summary.data, color = 'black', alpha = 0, size = 0.5) +"
      plot_cmds[["brush_restrict"]] <- sprintf(
        "geom_point(%s, plot.data, width = 0.2, height = 0.2) +",
        new_aes
      )
    }
  } else {
    plot_cmds[["point"]] <-
      "geom_tile(aes(x = X, y = Y, height = 2*Radius, width = 2*Radius), summary.data, color = 'black', alpha = 0, size = 0.5) +"
    plot_cmds[["jitter"]] <- sprintf(
      "geom_point(%s, plot.data, width = 0.2, height = 0.2, alpha = 0.4) +",
      new_aes
    )
  }

  plot_cmds[["scale"]] <- "scale_size_area(limits = c(0, 1), max_size = 30) +"

  # Adding the commands to color the points and the brushing box.
  if (color_set){
    plot_cmds[["scale_color"]] <- color_cmd
  }
  plot_cmds[["brush_tile"]] <- brush_show_cmd

  # Creating labels.
  plot_cmds[["labs"]] <- .build_labs(
    x = x_lab,
    y = y_lab,
    color = color_label,
    fill = color_label
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

  return(list(data=list(), lim=list(), brush=all_brush_cmds, setup=setup_cmds, plot=plot_cmds))
}

############################################
# Internal functions: coloring/brushing ----
############################################

.process_colorby_choice <- function(param_choices, se, input, color_map) {
  output <- list(cmd=NULL, label=NA_character_, FUN=NULL)
  color_choice <- param_choices[[.colorByField]]

  if (color_choice==.colorByColDataTitle) {
    covariate_name <- param_choices[[.colorByColData]]
    output$cmd <-  sprintf("plot.data$ColorBy <- colData(se)[,%s];", deparse(covariate_name))
    output$label <- covariate_name
    output$FUN <- .create_color_function_chooser(colDataColorMap(color_map, covariate_name))

  } else if (color_choice==.colorByGeneTableTitle || color_choice==.colorByGeneTextTitle) {

    # Set the color to the selected gene
    if (color_choice==.colorByGeneTableTitle) {
      chosen_gene <- .find_linked_gene(param_choices[[.colorByGeneTable]], input)
      validate(need(
        length(chosen_gene)==1L,
        sprintf("Invalid '%s' > '%s' input", .colorByField, .colorByGeneTableTitle)
      ))
      assay_choice <- param_choices[[.colorByGeneTableAssay]]

    } else {
      chosen_gene <- param_choices[[.colorByGeneText]]
      validate(need(
        chosen_gene %in% rownames(se),
        sprintf("Invalid '%s' > '%s' input", .colorByField, .colorByGeneTextTitle)
      ))
      assay_choice <- param_choices[[.colorByGeneTextAssay]]

    }

    output$cmd <- sprintf("plot.data$ColorBy <- assay(se, %i)[%s,];", 
                          assay_choice, deparse(chosen_gene))
    output$label <- .gene_axis_label(se, chosen_gene, assay_choice, multiline = TRUE)
    output$FUN <- .create_color_function_chooser(assayColorMap(color_map, assay_choice))

  } else {
    # No color; function just returns NULL all the time.
    output$FUN <- function(is_discrete) NULL
  }

  return(output)
}

.process_brushby_choice <- function(param_choices, input, all_memory, color="dodgerblue") {
  brush_in <- param_choices[[.brushByPlot]]
  output <- list(cmd=NULL)

  # Adding a box around the brush coordinates in the _current_ plot (not transmitter).
  current <- param_choices[,.brushData][[1]]
  if (!is.null(current)) {
    output$show <- sprintf("geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color='%s', alpha=0, 
    data=data.frame(xmin = %.5g, xmax=%.5g, ymin = %.5g, ymax = %.5g), inherit.aes=FALSE) +",
                           color, current$xmin, current$xmax, current$ymin, current$ymax)
  }

  # Checking what points are brushed from the transmitting plot.
  if (brush_in != "") {
    brush_by <- .encode_panel_name(brush_in)
    brush_val <- all_memory[[brush_by$Type]][,.brushData][[brush_by$ID]]

    if (!is.null(brush_val)) {
        transmitter <- paste0(brush_by$Type, "Plot", brush_by$ID)
        if (rownames(param_choices)==transmitter) {
            source_data <- 'plot.data'
        } else {
            source_data <- sprintf("all_coordinates[['%s']]", transmitter)
        }

        cmd <- sprintf("brushed_pts <- shiny::brushedPoints(%s,
    list(xmin=%.5g, xmax=%.5g, ymin=%.5g, ymax=%.5g,
         direction='%s', mapping=list(x='%s', y='%s')));",
        source_data,
        brush_val$xmin, brush_val$xmax, brush_val$ymin, brush_val$ymax,
        brush_val$direction, brush_val$mapping$x, brush_val$mapping$y)

        cmd <- c(cmd, "plot.data$BrushBy <- rownames(plot.data) %in% rownames(brushed_pts);")
        output$cmd <- paste(cmd, collapse="\n")
    }
  }

  return(output)
}

.create_color_function_chooser <- function(colors_scale) {
    to_use <- paste(deparse(colors_scale), collapse = "")
    function(is_discrete) {
        sprintf(ifelse(!is_discrete,
                       "scale_color_gradientn(colours = %s) +",
                       "scale_colour_manual(values = %s) +"),
                to_use)
    }
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

    assay_name <- assayNames(se)[assay_id]
    if (assay_name=="") {
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
    fill = NA_character_, group = NA_character_
){
    labs_specs <- c(x, y, color, shape, fill, group)
    names(labs_specs) <- .all_aes_names
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
# Internal functions: other ----
############################################

.find_linked_gene <- function(link, input)
# Convenience function to identify the selected gene from the linked table.
{
  if (link=="") {
    return(NULL)
  }
  tab.id <- .encode_panel_name(link)$ID
  linked.tab <- paste0("geneStatTable", tab.id, "_rows_selected")
  input[[linked.tab]]
}
