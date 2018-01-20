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

.make_redDimPlot <- function(se, param_choices, input, all.coordinates, se_name)
# Makes the dimension reduction plot.
{
  # Do not plot if gene name input is not a valid rownames(se)
  if (identical(param_choices[[.colorByField]], .colorByGeneTableTitle)){
      gene_selected <- .find_linked_gene(se, param_choices[[.colorByGeneTable]], input)
    validate(need(
      gene_selected %in% rownames(se),
      sprintf("Invalid '%s' > '%s' input", .colorByField, .colorByGeneTableTitle)
    ))
  }
  if (identical(param_choices[[.colorByField]], .colorByGeneTextTitle)){
    gene_selected <- param_choices[[.colorByGeneText]]
    validate(need(
      param_choices[[.colorByGeneText]] %in% rownames(se),
      sprintf("Invalid '%s' > '%s' input", .colorByField, .colorByGeneTextTitle)
    ))
  }

  # List of commands to evaluate
  cmds <- list(
    header = c(
      strrep("#", 77),
      "Header section for .make_redDimPlot",
      strrep("#", 77)
    ),
    data = character(),
    plot = character()
  )


  cmds$data[["reducedDim"]] <- sprintf(
    "red.dim <- reducedDim(%s, '%s');", se_name, param_choices[[.redDimType]])
  # Store the command to prepare X and Y axes data (required)
  cmds$data[["xy"]] <- sprintf(
    "plot.data <- data.frame(X = red.dim[, %s], Y = red.dim[, %s], row.names=colnames(%s));",
    param_choices[[.redDimXAxis]],
    param_choices[[.redDimYAxis]],
    se_name
  )

  # Process the color choice (if any)
  color_choice <- param_choices[[.colorByField]]
  # Set a boolean to later build an aes with color
  # Assume that it is the case, unset later if false
  color_set <- TRUE
  if (color_choice==.colorByColDataTitle) {
    # Save the selected colData name to later set the plot label
    covariate.name <- param_choices[[.colorByColData]]
    # Store the command to add color data
    cmds$data[["color"]] <- sprintf(
        "plot.data$ColorBy <- colData(%s)[,'%s'];", se_name, covariate.name
    )
  } else if (color_choice==.colorByGeneTableTitle || color_choice==.colorByGeneTextTitle) {

    if (color_choice==.colorByGeneTableTitle) {
      # Save the selected gene and assay names to later set the plot label
      covariate.name <-  .find_linked_gene(se, param_choices[[.colorByGeneTable]], input)
      assay.choice <- param_choices[[.colorByGeneTableAssay]]
    } else {
      # Save the selected gene and assay names to later set the plot label
      covariate.name <- gene_selected
      assay.choice <- param_choices[[.colorByGeneTextAssay]]
    }
    # Set the color to the selected gene
    if (identical(covariate.name, "")){
        # The initial validation code is meant to ensure that this never happens
        warning("Color mode is gene expression, but none selected.")
        # Fallback on 'no color' if the selected gene is not found
        covariate.name <- NA_character_ # plot label
        color_set <- FALSE # to later build an aes without color
    } else {
        cmds$data[["color"]] <- sprintf(
            "plot.data$ColorBy <- assay(%s, '%s')['%s',];",
            se_name, assay.choice, covariate.name
        )
        covariate.name <- .gene_axis_label(
        covariate.name, assay.choice, multiline = TRUE)
    }
  } else {
    # Color choice is therefore 'None'
    covariate.name <- NA_character_ # do not plot label
    color_set <- FALSE # to later build an aes without color
  }

  # Figuring out what to do with brushing.
  brush.in <- param_choices[[.brushByPlot]]
  if (brush.in!="") {
    brushed <- .define_brush(brush.in, input)
    if (!is.null(brushed)) { 
      cmds$data[["brush"]] <- brushed
    }
  }

  # Store the ggplot commands
  cmds$plot["ggplot"] <- sprintf("ggplot(plot.data, %s) +", .build_aes(color = color_set))
  cmds$plot["point"] <- "geom_point(size = 1.5) +"
  # An empty labs() command may be generated if no covariate is selected
  labs_add <- .build_labs(
      x = NA_character_, # or param_choices[[.redDimXAxis]], if shown
      y = NA_character_, # or param_choices[[.redDimYAxis]], if shown
      color = covariate.name
    )
  if (!is.null(labs_add)){
    cmds$plot[["labs"]] <- .build_labs(
      x = NA_character_, # or param_choices[[.redDimXAxis]], if shown
      y = NA_character_, # or param_choices[[.redDimYAxis]], if shown
      color = covariate.name
    )
  }
  cmds$plot[["theme_base"]] <- "theme_void() + "
  cmds$plot[["theme_custom"]] <- "theme(legend.position = 'bottom')"

  # Combine all the commands to evaluate
  cmds_eval <- .build_cmd_eval(cmds)

  eval.out <- new.env()
  plot.out <- eval(parse(text = cmds_eval), envir=eval.out)
  return(list(cmd = cmds_eval, xy = eval.out$plot.data, plot = plot.out))
}

############################################
# .make_colDataPlot  ----
############################################

.make_colDataPlot <- function(se, param_choices, input, se_name)
# Makes a plot of column data variables.
{
  # Do not plot if text field is not a valid rownames(se)
  if (identical(param_choices[[.colorByField]], .colorByGeneTableTitle)){
    gene_selected <- .find_linked_gene(se, param_choices[[.colorByGeneTable]], input)
    validate(need(
      gene_selected %in% rownames(se),
      sprintf("Invalid '%s' > '%s' input", .colorByField, .colorByGeneTableTitle)
    ))
  }
  if (identical(param_choices[[.colorByField]], .colorByGeneTextTitle)){
    gene_selected <- param_choices[[.colorByGeneText]]
    validate(need(
      gene_selected %in% rownames(se),
      sprintf("Invalid '%s' > '%s' input", .colorByField, .colorByGeneTextTitle)
    ))
  }

  # List of commands to evaluate
  cmds <- list(
    header = c(
      strrep("#", 77),
      "Header section for .make_colDataPlot",
      strrep("#", 77)
    ),
    data = character(),
    plot = character()
  )

  # Store the command to prepare Y-axis data (required)
  cmds$data[["y"]] <- sprintf(
    "plot.data <- data.frame(Y = colData(%s)[,'%s'], row.names=colnames(%s));",
    se_name, param_choices[[.colDataYAxis]], se_name
  )

  # Prepare X-axis data (optional; if absent, rank by Y value)
  if (identical(param_choices[[.colDataXAxis]], .colDataXAxisNothingTitle)) {
    # TODO: allow toggling rank decreasing/increasing, note that factors cannot be negated
    # In that case, do not show any X-axis label (applies below)
    x_lab <- "Rank"
    # Store the command to add X data
    cmds$data[["x"]] <- 'plot.data$X <- rank(plot.data$Y, ties.method = "first");'
  } else {
    # Set X-axis to the selected colData name
    x_lab <- param_choices[[.colDataXAxisColData]]
    # Store the command to add X data
    cmds$data[["x"]] <- sprintf("plot.data$X <- colData(%s)[,'%s'];", se_name, x_lab)
  }

  # Process the color choice (if any)
  color_choice <- param_choices[[.colorByField]]
  # Set a boolean to later build an aes with color
  # Assume that it is the case, unset later if false
  color_set <- TRUE
  if (identical(color_choice, .colorByColDataTitle)) {
    # Save the selected colData name to later set the plot label
    covariate.name <- param_choices[[.colorByColData]]
    # Store the command to add color data
    cmds$data[["color"]] <- sprintf(
        "plot.data$ColorBy <- colData(%s)[,'%s'];", se_name, covariate.name
    )
  } else if (identical(color_choice, .colorByGeneTableTitle) || identical(color_choice, .colorByGeneTextTitle)){
    if (identical(color_choice, .colorByGeneTableTitle)) {
      # Save the selected gene and assay names to later set the plot label
      covariate.name <- .find_linked_gene(se, param_choices[[.colorByGeneTable]], input)
      assay.choice <- param_choices[[.colorByGeneTableAssay]]
    } else if (identical(color_choice, .colorByGeneTextTitle)) {
      # Save the selected gene and assay names to later set the plot label
      covariate.name <- gene_selected
      assay.choice <- param_choices[[.colorByGeneTextAssay]]
    } else {
      stop("Not possible!")
    }
    # Set the color to the selected gene
    if (identical(covariate.name, "")){
        # The initial validation code is meant to ensure that this never happens
        warning("Color mode is gene expression, but none selected.")
        # Fallback on 'no color' if the selected gene is not found
        covariate.name <- NA_character_ # plot label
        color_set <- FALSE # to later build an aes without color
    } else {
        cmds$data[["color"]] <- sprintf(
            "plot.data$ColorBy <- assay(%s, '%s')['%s',];",
            se_name, assay.choice, covariate.name
        )
        covariate.name <- .gene_axis_label(
          covariate.name, assay.choice, multiline = TRUE)
    }
  } else {
      # Color choice is therefore 'None'
        covariate.name <- NA_character_ # do not plot label
        color_set <- FALSE # to later build an aes without color
  }

  # Store the ggplot commands
  cmds$plot[["ggplot"]] <- sprintf("ggplot(plot.data, %s) +", .build_aes(color = color_set))
  cmds$plot[["point"]] <- "geom_point() +"
  cmds$plot[["labs"]] <- .build_labs(
      x = x_lab,
      y = param_choices[[.colDataYAxis]],
      color = covariate.name
    )
  cmds$plot[["theme_base"]] <- "theme_bw() +"
  cmds$plot[["theme_custom"]] <- "theme(legend.position = 'bottom')"

  # Combine all the commands to evaluate
  cmds_eval <- .build_cmd_eval(cmds)

  return(list(cmd = cmds_eval, plot = eval(parse(text = cmds_eval))))
}

############################################
# .make_geneExprPlot  ----
############################################

.make_geneExprPlot <- function(se, param_choices, input, se_name)
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
  ## Colour (gene table)
  if (identical(param_choices[[.colorByField]], .colorByGeneTableTitle)){
      gene_selected_color <- .find_linked_gene(se, param_choices[[.colorByGeneTable]], input)
      validate(need(
        gene_selected_color %in% rownames(se),
        sprintf("Invalid '%s' > '%s' input", .colorByField, .colorByGeneTableTitle)
      ))
  }
  ## Colour (gene text)
  if (identical(param_choices[[.colorByField]], .colorByGeneTextTitle)){
      gene_selected_color <- param_choices[[.colorByGeneText]]
      validate(need(
        gene_selected_color %in% rownames(se),
        sprintf("Invalid '%s' > '%s' input", .colorByField, .colorByGeneTextTitle)
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
    data = character(),
    plot = character()
  )

  # Prepare X-axis data and axis label
  xchoice <- param_choices[[.geneExprXAxis]]
  if (xchoice==.geneExprXAxisColDataTitle) { # colData column selected
    # Set X-axis to the selected colData name
    x_lab <- param_choices[[.geneExprXAxisColData]]
    # needed to later decided whether to group data in violins
    covariate_x <- colData(se)[,x_lab]
    # Store the command to add X data
    if (is.character(covariate_x)){
      # turn characters into factors, as ggplot will during plotting
      cmds$data[["x_factor"]] <- paste(
        sprintf("## character colData '%s' coerced to factor for X value", x_lab)
      )
      cmds$data[["x"]] <- sprintf(
        "plot.data <- data.frame(X = factor(colData(%s)[,'%s']), row.names = colnames(%s));",
        se_name, x_lab, se_name
      )
      covariate_x <- factor(covariate_x)
    } else {
      cmds$data[["x"]] <- sprintf(
        "plot.data <- data.frame(X = colData(%s)[,'%s'], row.names = colnames(%s));",
        se_name, x_lab, se_name
      )
    }
  } else if (xchoice==.geneExprXAxisGeneTableTitle || xchoice==.geneExprXAxisGeneTextTitle) { # gene selected
    # Store the command to add X data
    cmds$data[["x"]] <- sprintf(
      "plot.data <- data.frame(X = assay(%s, '%s')['%s',], row.names = colnames(%s));",
      se_name, assay.choice, gene_selected_x, se_name
    )
    # Set X-axis to the selected gene identified
    x_lab <- .gene_axis_label(gene_selected_x, assay.choice, multiline = FALSE)
    # needed to later decided whether to group data in violins
    covariate_x <- assay(se, assay.choice)[gene_selected_x,]
  } else { ## no x axis variable specified: show single violin
    x_lab <- '' # to hide the X axis label
    cmds$data[["x"]] <- sprintf(
      "plot.data <- data.frame(X = factor(rep('%s', ncol(%s))), row.names = colnames(%s));",
      gene_selected_y, se_name, se_name
    )
    # needed to later decided whether to group data in violins
    covariate_x <- factor(rep(gene_selected_y, ncol(se)))
  }

  # Store the Y-axis label (note that input was previously validated)
  y_lab <- .gene_axis_label(gene_selected_y, assay.choice, multiline = FALSE)
  # Store the command to prepare Y-axis data (required)
  cmds$data[["y"]] <- sprintf(
    "plot.data$Y <- assay(%s, '%s')['%s',];",
    se_name, assay.choice, gene_selected_y
  )

  # Process the color choice (if any)
  color_choice <- param_choices[[.colorByField]]
  # Set a boolean to later build an aes with color
  # Assume that it is the case, unset later if false
  color_set <- TRUE; fill_set <- color_set
  if (identical(color_choice, .colorByColDataTitle)) {
    # Save the selected colData name to later set the plot label
    covariate.name <- param_choices[[.colorByColData]]
    # Store the command to add color data
    covariate_color <- colData(se)[,covariate.name]
    if (is.character(covariate_color)){
      # turn characters into factors, as ggplot will during plotting
      cmds$data[["color_factor"]] <- paste(
        sprintf("## character colData '%s' coerced to factor for color value", x_lab)
      )
      cmds$data[["color"]] <- sprintf(
        "plot.data$ColorBy <- factor(colData(%s)[,'%s']);", se_name, covariate.name)
      cmds$data[["fill"]] <- "plot.data$FillBy <- plot.data$ColorBy;"
      covariate_color <- factor(covariate_color)
    } else {
      cmds$data[["color"]] <- sprintf(
        "plot.data$ColorBy <- colData(%s)[,'%s'];",
        se_name, covariate.name)
      fill_set <- FALSE
    }
  } else if (identical(color_choice, .colorByGeneTableTitle) ||
      identical(color_choice, .colorByGeneTextTitle)) {
    if (identical(color_choice, .colorByGeneTableTitle)) {
      # Save the selected gene and assay names to later set the plot label
      covariate.name <- .find_linked_gene(se, param_choices[[.colorByGeneTable]], input)
      assay.choice <- param_choices[[.colorByGeneTableAssay]]
    } else {
       # Save the selected gene and assay names to later set the plot label
      covariate.name <- param_choices[[.colorByGeneText]]
      assay.choice <- param_choices[[.colorByGeneTextAssay]]
    }
    cmds$data[["color"]] <- sprintf(
      "plot.data$ColorBy <- assay(%s, '%s')['%s',];",
      se_name, assay.choice, covariate.name)
    cmds$data[["fill"]] <- "plot.data$FillBy <- plot.data$ColorBy;"
     # to later decide whether data can be grouped
    covariate_color <- assay(se, assay.choice)[covariate.name,]
  } else {
    # Color choice is therefore 'None'
    covariate.name <- NA_character_ # do not plot label
    covariate_color <- NULL # to later decide whether data can be grouped
    color_set <- FALSE; fill_set <- color_set # to later build an aes without color
  }

  is_groupable <- .is_groupable(covariate_x, covariate_color)

  if (!is_groupable) {
    fill_set <- FALSE
  } else {
    cmds$data[["group"]] <- "plot.data$GroupBy <- plot.data$X;"
  }

  # Store the ggplot commands
  cmds$plot[["ggplot"]] <- sprintf(
    "ggplot(plot.data, %s) +",
    .build_aes(color = color_set, fill = fill_set, group = is_groupable)
  )

  if (is_groupable){
    cmds$plot[["violin"]] <-
      "geom_violin(alpha = 0.2, scale = 'width') +"
  }

  if (is_groupable) {
    are_factor <- vapply(list(covariate_x, covariate_color), "is.factor", logical(1))
    cmds$plot[["point"]] <-
      "geom_jitter(alpha = 0.6, position = position_jitter(height = 0, width = 0.25)) +"
  } else {
    cmds$plot[["point"]] <- "geom_point(alpha = 0.6) +"
  }
  cmds$plot[["labs"]] <- .build_labs(
    x = x_lab,
    y = y_lab,
    color = covariate.name,
    fill = covariate.name
  )
  cmds$plot[["theme_base"]] <- "theme_bw() +"
  cmds$plot[["theme_custom"]] <- "theme(legend.position = 'bottom')"
  
  # Combine all the commands to evaluate
  cmds_eval <- .build_cmd_eval(cmds)

  return(list(cmd = cmds_eval, plot = eval(parse(text = cmds_eval))))

}

############################################
# Internal functions ----
############################################

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

.is_groupable <- function(x, color, max_levels = 24){
  covariates <- list(x = x, color = color)
  covariate_types <- vapply(covariates, "class", character(1), USE.NAMES = TRUE)
  
  if (is.factor(x)){
    return(nlevels(x) <= max_levels)
  }
  
  if (is.numeric(x)){
    return(FALSE)
  }

  # fallback
  return(TRUE)
}

.build_cmd_eval <- function(cmds){
  cmds$header <- paste("##", cmds$header, sep = " ")
  
  final_cmd <- paste(
    paste(cmds$header, collapse = "\n"),
    paste(cmds$data, collapse = "\n"),
    paste(cmds$plot, collapse = "\n\t"),
    sep  = "\n"
  )
  
  return(final_cmd)
}

.define_brush <- function(brush_in, input) {
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
        print(cmd)
        return(paste(cmd, collapse="\n"))
    } else {
        return(NULL)
    }
}
