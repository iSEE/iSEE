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

.make_redDimPlot <- function(se, param_choices, input, all.coordinates)
# Makes the dimension reduction plot.
{
  force(se)
  cmds <- list(
    todo = list(),
    done = character(0)
  )

  # Store the command to prepare X and Y axes data (required)
  cmds$todo[["reducedDim"]] <- sprintf(
    "red.dim <- reducedDim(se, '%s');", param_choices[[.redDimType]])
  cmds$todo[["xy"]] <- sprintf(
    "plot.data <- data.frame(X = red.dim[, %s], Y = red.dim[, %s], row.names=colnames(se));",
    param_choices[[.redDimXAxis]],
    param_choices[[.redDimYAxis]]
  )

  # Adding colour commands.
  color_out <- .process_colorby_choice(param_choices, se, input)
  cmds$todo[["color"]] <- color_out$cmd
  color_label <- color_out$label
  color_set <- !is.null(color_out$cmd)

  # Adding brushing commands.
  brush_out <- .process_brushby_choice(param_choices, input)
  cmds$todo[["brush"]] <- brush_out$cmd
  brush_set <- !is.null(brush_out$cmd)

  # Generating the plot.
  .create_plot(cmds, se, all.coordinates, 
               param_choices=param_choices, 
               x_lab=sprintf("Dimension %s", param_choices[[.redDimXAxis]]),
               y_lab=sprintf("Dimension %s", param_choices[[.redDimYAxis]]),
               color_set=color_set, brush_set=brush_set, color_label=color_label)
}

############################################
# .make_colDataPlot  ----
############################################

.make_colDataPlot <- function(se, param_choices, input, all.coordinates)
# Makes a plot of column data variables.
{
  cmds <- list(
    todo = list(),
    done = character(0)
  )

  # Store the command to prepare Y-axis data (required)
  y_lab <- param_choices[[.colDataYAxis]]
  cmds$todo[["y"]] <- sprintf("plot.data <- data.frame(Y = colData(se)[,'%s'], row.names=colnames(se));", y_lab)

  # Prepare X-axis data.
  if (param_choices[[.colDataXAxis]]==.colDataXAxisNothingTitle) {
    x_lab <- ''
    cmds$todo[["x"]] <- "plot.data$X <- factor(integer(ncol(se)))"
  } else {
    x_lab <- param_choices[[.colDataXAxisColData]]
    cmds$todo[["x"]] <- sprintf("plot.data$X <- colData(se)[,'%s'];", x_lab)
  }

  # Adding colour commands.
  color_out <- .process_colorby_choice(param_choices, se, input)
  cmds$todo[["color"]] <- color_out$cmd
  color_label <- color_out$label
  color_set <- !is.null(color_out$cmd)

  # Adding brushing commands.
  brush_out <- .process_brushby_choice(param_choices, input)
  cmds$todo[["brush"]] <- brush_out$cmd
  brush_set <- !is.null(brush_out$cmd)

  # Generating the plot.
  .create_plot(cmds, se, all.coordinates, 
               param_choices=param_choices, x_lab=x_lab, y_lab=y_lab, 
               color_set=color_set, brush_set=brush_set, color_label=color_label)
}

############################################
# .make_geneExprPlot  ----
############################################

.make_geneExprPlot <- function(se, param_choices, input, all.coordinates)
# Makes a gene expression plot.
{
  ## Checking y-axis choice:
  y_choice <- param_choices[[.geneExprYAxis]]
  if (y_choice==.geneExprYAxisGeneTableTitle) {
    gene_selected_y <- .find_linked_gene(se, param_choices[[.geneExprYAxisGeneTable]], input)
  } else if (y_choice==.geneExprYAxisGeneTextTitle) {
    gene_selected_y <- param_choices[[.geneExprYAxisGeneText]]
  }
  validate(need(
      gene_selected_y %in% rownames(se),
      sprintf("Invalid '%s' > '%s' input", .geneExprYAxis, y_choice)
    ))

  ## Checking X axis choice:
  x_choice <- param_choices[[.geneExprXAxis]]
  if (x_choice==.geneExprXAxisGeneTableTitle) {
    gene_selected_x <- .find_linked_gene(se, param_choices[[.geneExprXAxisGeneTable]], input)
  } else if (x_choice==.geneExprXAxisGeneTextTitle) {
    gene_selected_x <- param_choices[[.geneExprXAxisGeneText]]
  }
  if (x_choice==.geneExprXAxisGeneTableTitle || x_choice==.geneExprXAxisGeneTextTitle) {
    validate(need(
      gene_selected_x %in% rownames(se),
      sprintf("Invalid '%s' > '%s' input", .geneExprXAxis, x_choice)
    ))
  }

  # List of commands to evaluate
  cmds <- list(
    todo = list(),
    done = character(0)
  )

  #########################################
  #### Setting up the X/Y-axis data ----
  #########################################

  assay.choice <- param_choices[[.geneExprAssay]]

  if (x_choice==.geneExprXAxisColDataTitle) { # colData column selected
    x_lab <- param_choices[[.geneExprXAxisColData]]
    cmds$todo[["x"]] <- sprintf(
       "plot.data <- data.frame(X = colData(se)[,'%s'], row.names = colnames(se));", x_lab
    )

  } else if (x_choice==.geneExprXAxisGeneTableTitle || x_choice==.geneExprXAxisGeneTextTitle) { # gene selected
    x_lab <- .gene_axis_label(gene_selected_x, assay.choice, multiline = FALSE)
    cmds$todo[["x"]] <- sprintf(
      "plot.data <- data.frame(X = assay(se, '%s')['%s',], row.names = colnames(se));",
      assay.choice, gene_selected_x
    )

  } else { ## no x axis variable specified: show single violin
    x_lab <- ''
    cmds$todo[["x"]] <- sprintf(
      "plot.data <- data.frame(X = factor(rep('%s', ncol(se))), row.names = colnames(se));", gene_selected_y
    )
  }

  # Set Y-axis data and axis label.
  y_lab <- .gene_axis_label(gene_selected_y, assay.choice, multiline = FALSE)
  cmds$todo[["y"]] <- sprintf(
    "plot.data$Y <- assay(se, '%s')['%s',];",
    assay.choice, gene_selected_y
  )

  #########################################
  #### Setting up the plot aesthetics ----
  #########################################

  # Adding colour commands.
  color_out <- .process_colorby_choice(param_choices, se, input)
  color_set <- !is.null(color_out$cmd)
  cmds$todo[["color"]] <- color_out$cmd
  color_label <- color_out$label

  # Adding brushing commands.
  brush_out <- .process_brushby_choice(param_choices, input)
  cmds$todo[["brush"]] <- brush_out$cmd
  brush_set <- !is.null(brush_out$cmd)

  # Generating the plot.
  .create_plot(cmds, se, all.coordinates, 
               param_choices=param_choices, x_lab=x_lab, y_lab=y_lab,
               color_set=color_set, brush_set=brush_set, color_label=color_label)
}

############################################
# Internal functions: central plotter ----
############################################

.create_plot <- function(cmds, se, all.coordinates, ..., color_set) 
# This function will generate plotting commands appropriate to 
# each type of X/Y. It does so by evaluating 'plot.data' to 
# determine the nature of X/Y, and then choosing the plot to match.
#
# Note that we need 'se' and 'all.coordinates' to be passed as arguments
# for the evaluations to execute in this environment. All evaluations
# are to take place in this function, not in the calling environment
# or in child environments. This constrains the scope of 'eval' calls.
{ 
  eval_out <- new.env()
  executed <- .evaluate_remainder(cmd_list=cmds, eval_env=eval_out)
  cmds <- executed$cmd_list

  # Cleaning up the grouping status of various fields.
  coloring <- eval_out$plot.data$ColorBy
  if (!is.null(coloring)) { 
    group_color <- .is_groupable(coloring)
    if (!group_color) {
      cmds$todo[["more_color"]] <- .coerce_to_numeric(coloring, "ColorBy")
    }
  }
  
  xvals <- eval_out$plot.data$X
  group_X <- .is_groupable(xvals)
  if (!group_X) {
    cmds$todo[["more_X"]] <- .coerce_to_numeric(xvals, "X")
  }

  yvals <- eval_out$plot.data$Y
  group_Y <- .is_groupable(yvals)
  if (!group_Y) { 
    cmds$todo[["more_Y"]] <- .coerce_to_numeric(yvals, "Y")
  }

  # Dispatch to different plotting commands, depending on whether X/Y are groupable.
  if (group_X && group_Y) {
    # Something with circles, as discussed
    validate(FALSE, "ARGHH!")

  } else if (group_X && !group_Y) {
    # Vertical violin plots.
    cmds$todo[["group"]] <- "plot.data$GroupBy <- plot.data$X;"
    fill_set <- (color_set && group_color)
    if (fill_set) {
      cmds$todo[["fill"]] <- "plot.data$FillBy <- plot.data$ColorBy"
    }
    plot_cmds <- .violin_plot(..., color_set=color_set, fill_set=fill_set) 

  } else if (!group_X && group_Y) {
    # Need horizontal violin plots (just using this as a placeholder for the time being).
    cmds$todo[["more_Y"]] <- .coerce_to_numeric(yvals, "Y")
    plot_cmds <- .scatter_plot(..., color_set=color_set)

  } else {
    plot_cmds <- .scatter_plot(..., color_set=color_set)

  }
  cmds$todo <- c(cmds$todo, plot_cmds)

  # Combine all the commands to evaluate
  executed <- .evaluate_remainder(cmd_list=cmds, eval_env=eval_out)
  return(list(cmd = .build_cmd_eval(cmds),
              xy = eval_out$plot.data,
              plot = executed$output))
}

.scatter_plot <- function(param_choices, x_lab, y_lab, color_set, color_label, brush_set) 
# Creates a scatter plot of numeric X/Y. This function should purely
# generate the plotting commands, with no modification of 'cmds'.
{
  plot_cmds <- list()
  plot_cmds[["ggplot"]] <- "ggplot() +"

  # Implementing the brushing effect.
  if (brush_set) {
    brush_effect <- param_choices[[.brushEffect]]
    if (brush_effect==.brushColorTitle) {
      plot_cmds[["brush_other"]] <- sprintf(
        "geom_point(%s, subset(plot.data, !BrushBy)) +",
        .build_aes(color = color_set)
      )
      plot_cmds[["brush_color"]] <- sprintf(
        "geom_point(%s, data = subset(plot.data, BrushBy), color = '%s') +",
        .build_aes(color = color_set), param_choices[[.brushColor]]
      )
    }
    if (brush_effect==.brushTransTitle) {
      plot_cmds[["brush_other"]] <- sprintf(
        "geom_point(%s, subset(plot.data, !BrushBy), alpha = %s) +",
        .build_aes(color = color_set), param_choices[[.brushTransAlpha]]
      )
      plot_cmds[["brush_alpha"]] <- sprintf(
        "geom_point(%s, subset(plot.data, BrushBy)) +",
        .build_aes(color = color_set)
      )
    }
    if (brush_effect==.brushRestrictTitle) {
      plot_cmds[["brush_restrict"]] <- sprintf(
        "geom_point(%s, subset(plot.data, BrushBy)) +",
        .build_aes(color = color_set)
      )
    }
  } else {
    plot_cmds[["point"]] <- sprintf(
      "geom_point(%s, plot.data, size = 1.5) +",
      .build_aes(color = color_set)
    )
  }

  # Add axes labels
  plot_cmds[["labs"]] <- .build_labs(
    x = x_lab,
    y = y_lab,
    color = color_label
  )

  plot_cmds[["theme_base"]] <- "theme_bw() +"
  plot_cmds[["theme_custom"]] <- "theme(legend.position = 'bottom')"
  return(plot_cmds)
}

.violin_plot <- function(param_choices, x_lab, y_lab, color_set, color_label, fill_set, brush_set)
# Generates a vertical violin plot. This function should purely
# generate the plotting commands, with no modification of 'cmds'.
{ 
  plot_cmds <- list()
  plot_cmds[["ggplot"]] <- sprintf(
    "ggplot(plot.data, %s) +",
    .build_aes(color = color_set, fill = fill_set, group = TRUE)
  )
  plot_cmds[["violin"]] <- "geom_violin(alpha = 0.2, scale = 'width') +"

  plot_cmds[["point"]] <-
    "geom_jitter(alpha = 0.6, position = position_jitter(height = 0, width = 0.25)) +"

  plot_cmds[["labs"]] <- .build_labs(
    x = x_lab,
    y = y_lab,
    color = color_label,
    fill = color_label
  )
  plot_cmds[["theme_base"]] <- "theme_bw() +"
  plot_cmds[["theme_custom"]] <- "theme(legend.position = 'bottom')"
  return(plot_cmds)
}

############################################
# Internal functions: coloring/brushing ----
############################################

.process_colorby_choice <- function(param_choices, se, input) {
  output <- list(cmd=NULL, label=NA_character_)
  color_choice <- param_choices[[.colorByField]]

  if (color_choice==.colorByColDataTitle) {
    covariate.name <- param_choices[[.colorByColData]]
    output$cmd <-  sprintf("plot.data$ColorBy <- colData(se)[,'%s'];", covariate.name)
    output$label <- covariate.name

  } else if (color_choice==.colorByGeneTableTitle || color_choice==.colorByGeneTextTitle) {

    # Set the color to the selected gene
    if (color_choice==.colorByGeneTableTitle) {
      covariate.name <- .find_linked_gene(se, param_choices[[.colorByGeneTable]], input)
      validate(need(
        covariate.name %in% rownames(se),
        sprintf("Invalid '%s' > '%s' input", .colorByField, .colorByGeneTableTitle)
      ))
      assay.choice <- param_choices[[.colorByGeneTableAssay]]

    } else {
      covariate.name <- param_choices[[.colorByGeneText]]
      validate(need(
        covariate.name %in% rownames(se),
        sprintf("Invalid '%s' > '%s' input", .colorByField, .colorByGeneTextTitle)
      ))
      assay.choice <- param_choices[[.colorByGeneTextAssay]]

    }

    if (identical(covariate.name, "")){
      # The initial validation code is meant to ensure that this never happens
      warning("Color mode is gene expression, but none selected.")
    } else {
      output$cmd <- sprintf("plot.data$ColorBy <- assay(se, '%s')['%s',];", assay.choice, covariate.name)
      output$label <- .gene_axis_label(covariate.name, assay.choice, multiline = TRUE)
    }
  }

  return(output)
}

.process_brushby_choice <- function(param_choices, input) {
  brush_in <- param_choices[[.brushByPlot]]
  output <- list(cmd=NULL)

  if (!isTRUE(param_choices[[.brushActive]])){
    return(output)
  }

  if (brush_in != "") {
    brush_by <- .encode_panel_name(brush_in)
    brush_val <- input[[paste0(brush_by$Type, .brushField, brush_by$ID)]]
    if (!is.null(brush_val)) {
        cmd <- sprintf("brushedPts <- shiny::brushedPoints(all.coordinates[['%s']],
    list(xmin=%.5g, xmax=%.5g, ymin=%.5g, ymax=%.5g,
         direction='%s', mapping=list(x='%s', y='%s')));",
        paste0(brush_by$Type, "Plot", brush_by$ID),
        brush_val$xmin, brush_val$xmax, brush_val$ymin, brush_val$ymax,
        brush_val$direction, brush_val$mapping$x, brush_val$mapping$y)
        cmd <- c(cmd, "plot.data$BrushBy <- rownames(plot.data) %in% rownames(brushedPts);")
        output$cmd <- paste(cmd, collapse="\n")
    }
  }

  return(output)
}

############################################
# Internal functions: aesthetics ----
############################################

.gene_axis_label <- function(gene_id, assay_name, multiline=FALSE){
    sep = ifelse(multiline, "\\n", " ")
    sprintf("%s%s(%s)", gene_id, sep, assay_name)
}

.build_aes <- function(x = TRUE, y = TRUE, color = FALSE, shape = FALSE, fill = FALSE, group = FALSE){
    active_aes <- .all_aes_values[c(x, y, color, shape, fill, group)]
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
    sprintf("%s = '%s'", name, value)
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

.evaluate_remainder <- function(cmd_list, eval_env) {
    out <- eval(parse(text=unlist(cmd_list$todo)), envir=eval_env)
    cmd_list$done <- c(cmd_list$done, unlist(cmd_list$todo))
    cmd_list$todo <- list()
    return(list(cmd_list=cmd_list, output=out))
}

.build_cmd_eval <- function(cmds){
  all_cmds <- c(cmds$done, unlist(cmds$todo))

  multi_line <- grep("\\+$", all_cmds) + 1 # indenting next line 
  all_cmds[multi_line] <- paste0("    ", all_cmds[multi_line])

  paste(all_cmds, collapse="\n")
}

.find_linked_gene <- function(se, link, input)
  # Convenience function to identify the selected gene from the linked table.
{
  if (link=="") {
    return(NULL)
  }
  tab.id <- .encode_panel_name(link)$ID
  linked.tab <- paste0("geneStatTable", tab.id, "_rows_selected")
  rownames(se)[input[[linked.tab]]]
}



