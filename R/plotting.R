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
  cmds <- list(
    header = c(
      strrep("#", 77),
      "Header section for .make_redDimPlot",
      strrep("#", 77)
    ),
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

  # Figuring out what to do with brushing.
  brush_out <- .process_brushby_choice(param_choices, input)
  cmds$todo[["brush"]] <- brush_out$cmd
  brush_set <- !is.null(brush_out$cmd)

  # Store the ggplot commands
  cmds$todo[["ggplot"]] <- "ggplot() +"

  # Implementing the brushing effect.
  if (brush_set) {
    brush_effect <- param_choices[[.brushEffect]]
    if (brush_effect==.brushColorTitle) {
      cmds$todo[["brush_other"]] <- sprintf(
        "geom_point(%s, subset(plot.data, !BrushBy)) +",
        .build_aes(color = color_set)
      )
      cmds$todo[["brush_color"]] <- sprintf(
        "geom_point(%s, data = subset(plot.data, BrushBy), color = '%s') +",
        .build_aes(color = color_set), param_choices[[.brushColor]]
      )
    }
    if (brush_effect==.brushTransTitle) {
      cmds$todo[["brush_other"]] <- sprintf(
        "geom_point(%s, subset(plot.data, !BrushBy), alpha = %s) +",
        .build_aes(color = color_set), param_choices[[.brushTransAlpha]]
      )
      cmds$todo[["brush_alpha"]] <- sprintf(
        "geom_point(%s, subset(plot.data, BrushBy)) +",
        .build_aes(color = color_set)
      )
    }
    if (brush_effect==.brushRestrictTitle) {
      cmds$todo[["brush_restrict"]] <- sprintf(
        "geom_point(%s, subset(plot.data, BrushBy)) +",
        .build_aes(color = color_set)
      )
    }
  } else {
    cmds$todo[["point"]] <- sprintf(
      "geom_point(%s, plot.data, size = 1.5) +",
      .build_aes(color = color_set)
    )
  }

  # Add axes labels
  cmds$todo[["labs"]] <- .build_labs(
    x = sprintf("Dimension %s", param_choices[[.redDimXAxis]]),
    y = sprintf("Dimension %s", param_choices[[.redDimYAxis]]),
    color = color_label
  )

  cmds$todo[["theme_base"]] <- "theme_bw() +"
  cmds$todo[["theme_custom"]] <- "theme(legend.position = 'bottom')"

  # Combine all the commands to evaluate
  eval_out <- new.env()
  executed <- .evaluate_remainder(cmd_list=cmds, eval_env=eval_out)
  return(list(cmd = .build_cmd_eval(cmds),
              xy = eval_out$plot.data,
              plot = executed$output))
}

############################################
# .make_colDataPlot  ----
############################################

.make_colDataPlot <- function(se, param_choices, input)
# Makes a plot of column data variables.
{
  cmds <- list(
    header = c(
      strrep("#", 77),
      "Header section for .make_colDataPlot",
      strrep("#", 77)
    ),
    todo = list(),
    done = character(0)
  )

  # Store the command to prepare Y-axis data (required)
  covariate_y <- colData(se)[, param_choices[[.colDataYAxis]]]
  is_groupable <- .is_groupable(covariate_y)
  message("is.character: ", is.character(covariate_y))
  if (!is_groupable){
    if (is.character(covariate_y)){
      cmds$todo[["y"]] <- sprintf(
        "plot.data <- data.frame(Y = as.numeric(as.factor(colData(se)[,'%s'])), row.names=colnames(se));",
        param_choices[[.colDataYAxis]]
      )
    } else {
      cmds$todo[["y"]] <- sprintf(
        "plot.data <- data.frame(Y = as.numeric(colData(se)[,'%s']), row.names=colnames(se));",
        param_choices[[.colDataYAxis]]
      )
    }

  } else {
    cmds$todo[["y"]] <- sprintf(
      "plot.data <- data.frame(Y = colData(se)[,'%s'], row.names=colnames(se));",
      param_choices[[.colDataYAxis]]
    )
  }

  # Prepare X-axis data (optional; if absent, rank by Y value)
  if (identical(param_choices[[.colDataXAxis]], .colDataXAxisNothingTitle)) {
    # TODO: allow toggling rank decreasing/increasing, note that factors cannot be negated
    # In that case, do not show any X-axis label (applies below)
    x_lab <- "Rank"
    cmds$todo[["x"]] <- 'plot.data$X <- rank(plot.data$Y, ties.method = "first");'
  } else {
    # Set X-axis to the selected colData name
    x_lab <- param_choices[[.colDataXAxisColData]]
    cmds$todo[["x"]] <- sprintf("plot.data$X <- colData(se)[,'%s'];", x_lab)
  }

  # Adding colour commands.
  color_out <- .process_colorby_choice(param_choices, se, input)
  cmds$todo[["color"]] <- color_out$cmd
  color_label <- color_out$label
  color_set <- !is.null(color_out$cmd)

  # Store the ggplot commands
  cmds$todo[["ggplot"]] <- sprintf("ggplot(plot.data, %s) +", .build_aes(color = color_set))
  cmds$todo[["point"]] <- "geom_point() +"
  cmds$todo[["labs"]] <- .build_labs(
      x = x_lab,
      y = param_choices[[.colDataYAxis]],
      color = color_label
    )
  cmds$todo[["theme_base"]] <- "theme_bw() +"
  cmds$todo[["theme_custom"]] <- "theme(legend.position = 'bottom')"

  # Combine all the commands to evaluate
  eval_out <- new.env()
  executed <- .evaluate_remainder(cmd_list=cmds, eval_env=eval_out)
  return(list(cmd = .build_cmd_eval(cmds),
              xy = eval_out$plot.data,
              plot = executed$output))
}

############################################
# .make_geneExprPlot  ----
############################################

.make_geneExprPlot <- function(se, param_choices, input)
# Makes a gene expression plot.
{
  # Do not plot if gene name input is not a valid rownames(se)
  ## Y axis (gene table)
  if (identical(param_choices[[.geneExprYAxis]], .geneExprYAxisGeneTableTitle)){
    gene_selected_y <- .find_linked_gene(se, param_choices[[.geneExprYAxisGeneTable]], input)
    validate(need(
      gene_selected_y %in% rownames(se),
      sprintf("Invalid '%s' > '%s' input", .geneExprYAxis, .geneExprYAxisGeneTableTitle)
    ))
  }
  ## Y axis (gene text)
  if (identical(param_choices[[.geneExprYAxis]], .geneExprYAxisGeneTextTitle)){
    gene_selected_y <- param_choices[[.geneExprYAxisGeneText]]
    validate(need(
      gene_selected_y %in% rownames(se),
      sprintf("Invalid '%s' > '%s' input", .geneExprYAxis, .geneExprYAxisGeneTextTitle)
    ))
  }
  ## X axis (gene table)
  if (identical(param_choices[[.geneExprXAxis]], .geneExprXAxisGeneTableTitle)){
      gene_selected_x <- .find_linked_gene(se, param_choices[[.geneExprXAxisGeneTable]], input)
      validate(need(
        gene_selected_x %in% rownames(se),
        sprintf("Invalid '%s' > '%s' input", .geneExprXAxis, .geneExprXAxisGeneTableTitle)
      ))
  }
  ## X axis (gene text)
  if (identical(param_choices[[.geneExprXAxis]], .geneExprXAxisGeneTextTitle)){
    gene_selected_x <- param_choices[[.geneExprXAxisGeneText]]
    validate(need(
      gene_selected_x %in% rownames(se),
      sprintf("Invalid '%s' > '%s' input", .geneExprXAxis, .geneExprXAxisGeneTextTitle)
    ))
  }

  # Shorthand
  assay.choice <- param_choices[[.geneExprAssay]]

  # List of commands to evaluate
  cmds <- list(
    header = c(
      strrep("#", 77),
      "Header section for .make_geneExprPlot",
      strrep("#", 77)
    ),
    todo = list(),
    done = character(0)
  )

  #########################################
  #### Setting up the X/Y-axis data ----
  #########################################

  xchoice <- param_choices[[.geneExprXAxis]]

  if (xchoice==.geneExprXAxisColDataTitle) { # colData column selected
    x_lab <- param_choices[[.geneExprXAxisColData]]
    cmds$todo[["x"]] <- sprintf(
       "plot.data <- data.frame(X = colData(se)[,'%s'], row.names = colnames(se));", x_lab
    )

  } else if (xchoice==.geneExprXAxisGeneTableTitle || xchoice==.geneExprXAxisGeneTextTitle) { # gene selected
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
  if (color_set) {
    color_out$cmd <- paste0(color_out$cmd, "\nplot.data$FillBy <- plot.data$ColorBy")
  }
  cmds$todo[["color"]] <- color_out$cmd
  color_label <- color_out$label

  ## PLOT EVALUATION POINT ##
  eval_out <- new.env()
  executed <- .evaluate_remainder(cmd_list=cmds, eval_env=eval_out)
  cmds <- executed$cmd_list
  covariate_x <- eval_out$plot.data$X
  covariate_color <- eval_out$plot.data$ColorBy

  # Determining whether the data are groupable.
  is_groupable <- .is_groupable(covariate_x)
  fill_set <- FALSE
  if (is_groupable) {
    cmds$todo[["group"]] <- "plot.data$GroupBy <- plot.data$X;"
    if (color_set && .is_groupable(covariate_color)) {
      fill_set <- TRUE
    }
  }

  # Store the ggplot commands
  cmds$todo[["ggplot"]] <- sprintf(
    "ggplot(plot.data, %s) +",
    .build_aes(color = color_set, fill = fill_set, group = is_groupable)
  )

  if (is_groupable){
    cmds$todo[["violin"]] <-
      "geom_violin(alpha = 0.2, scale = 'width') +"
  }

  if (is_groupable) {
    are_factor <- vapply(list(covariate_x, covariate_color), "is.factor", logical(1))
    cmds$todo[["point"]] <-
      "geom_jitter(alpha = 0.6, position = position_jitter(height = 0, width = 0.25)) +"
  } else {
    cmds$todo[["point"]] <- "geom_point(alpha = 0.6) +"
  }
  cmds$todo[["labs"]] <- .build_labs(
    x = x_lab,
    y = y_lab,
    color = color_label,
    fill = color_label
  )
  cmds$todo[["theme_base"]] <- "theme_bw() +"
  cmds$todo[["theme_custom"]] <- "theme(legend.position = 'bottom')"

  # Combine all the commands to evaluate
  executed <- .evaluate_remainder(cmd_list=cmds, eval_env=eval_out)
  return(list(cmd = .build_cmd_eval(cmds),
              xy = eval_out$plot.data,
              plot = executed$output))
}

############################################
# Internal functions ----
############################################

.process_colorby_choice <- function(param_choices, se, input) {
  output <- list(cmd=NULL, label=NA_character_)
  color_choice <- param_choices[[.colorByField]]

  if (color_choice==.colorByColDataTitle) {
    covariate.name <- param_choices[[.colorByColData]]
    output$cmd <- sprintf("plot.data$ColorBy <- colData(se)[,'%s'];", covariate.name)
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
#  covariates <- list(x = x, color = color)
#  covariate_types <- vapply(covariates, "class", character(1), USE.NAMES = TRUE)
  return(.nlevels(x) <= max_levels)
}

.evaluate_remainder <- function(cmd_list, eval_env) {
    out <- eval(parse(text=unlist(cmd_list$todo)), envir=eval_env)
    cmd_list$done <- c(cmd_list$done, unlist(cmd_list$todo))
    cmd_list$todo <- list()
    return(list(cmd_list=cmd_list, output=out))
}

.build_cmd_eval <- function(cmds){
  cmds$header <- paste("##", cmds$header, sep = " ")

  all_cmds <- c(cmds$done, unlist(cmds$todo))
  multi_line <- grep("\\+$", all_cmds) + 1 # indenting next line # indenting next line.
  all_cmds[multi_line] <- paste0("    ", all_cmds[multi_line])

  paste(c(cmds$header, all_cmds), collapse="\n")
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
