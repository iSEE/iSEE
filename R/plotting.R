############################################
# .make_redDimPlot  ----
############################################

.make_redDimPlot <- function(se, param_choices, input, all.coordinates)
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
      data = character(),
      plot = character()
  )


  cmds$data[["reducedDim"]] <- sprintf(
    "red.dim <- reducedDim(se, '%s');", param_choices[[.redDimType]])
  # Store the command to prepare X and Y axes data (required)
  cmds$data[["xy"]] <- sprintf(
    "plot.data <- data.frame(X = red.dim[, %s], Y = red.dim[, %s]);",
    param_choices[[.redDimXAxis]],
    param_choices[[.redDimYAxis]]
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
        "plot.data$ColorBy <- colData(se)[,'%s'];", covariate.name
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
            "plot.data$ColorBy <- assay(se, '%s')['%s',];",
            assay.choice, covariate.name
        )
        covariate.name <- .gene_axis_label(
        covariate.name, assay.choice, multiline = TRUE)
    }
  } else {
    # Color choice is therefore 'None'
    covariate.name <- NA_character_ # do not plot label
    color_set <- FALSE # to later build an aes without color
  }

  # TODO: Figuring out what to do with brushing.
  brush.in <- param_choices[[.brushByPlot]]
  if (brush.in!="") {
    brush.by <- .encode_panel_name(brush.in)
    brush.id <- input[[paste0(brush.by$Type, .brushField, brush.by$ID)]]
    brushed.pts <- brushedPoints(all.coordinates[[paste0(brush.by$Type, "Plot", brush.by$ID)]], brush.id)
    if (!is.null(brushed.pts) && nrow(brushed.pts)) {
      print("YAY, brushing!")
      print(brushed.pts)
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
  cmds$plot[["theme_custom"]] <- "theme(legend.position = 'bottom')\n"

  # Combine all the commands to evaluate
  cmds_eval <- paste(
      paste(cmds$data, collapse = "\n"),
      paste(cmds$plot, collapse = "\n\t"),
      sep  = "\n"
  )

  return(list(cmd = cmds_eval, plot = eval(parse(text = cmds_eval))))
}

############################################
# .make_colDataPlot  ----
############################################

.make_colDataPlot <- function(se, param_choices, input)
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
      data = character(),
      plot = character()
  )

  # Store the command to prepare Y-axis data (required)
  cmds$data[["y"]] <- sprintf(
    "plot.data <- data.frame(Y = colData(se)[,'%s']);",
    param_choices[[.colDataYAxis]]
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
    cmds$data[["x"]] <- sprintf("plot.data$X <- colData(se)[,'%s'];", x_lab)
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
        "plot.data$ColorBy <- colData(se)[,'%s'];", covariate.name
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
            "plot.data$ColorBy <- assay(se, '%s')['%s',];",
            assay.choice, covariate.name
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
  cmds_eval <- paste(
      paste(cmds$data, collapse = "\n"),
      paste(cmds$plot, collapse = "\n\t"),
      sep  = "\n"
  )

    return(list(cmd = cmds_eval, plot = eval(parse(text = cmds_eval))))
}

############################################
# .make_geneExprPlot  ----
############################################

.make_geneExprPlot <- function(se, param_choices, input)
# Makes a gene expression plot.
{
  # Do not plot if gene name input is not a valid rownames(se)
  ## Y-axis (always a gene table)
  gene_selected_y <- .find_linked_gene(se, param_choices[[.geneExprID]], input)
  validate(need(
      gene_selected_y %in% rownames(se),
      sprintf("Invalid Y-axis '%s' input", .geneExprID)
  ))
  ## X axis (gene table)
  if (identical(param_choices[[.geneExprXAxis]], .geneExprXAxisGeneExprsTitle)){
      gene_selected_color <- .find_linked_gene(se, param_choices[[.geneExprXAxisGeneExprs]], input)
      validate(need(
        gene_selected_color %in% rownames(se),
        sprintf("Invalid '%s' > '%s' input", .geneExprXAxis, .geneExprXAxisGeneExprsTitle)
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
  ## Colour (gene table)
  if (identical(param_choices[[.colorByField]], .colorByGeneTextTitle)){
      gene_selected_color <- param_choices[[.colorByGeneText]]
      validate(need(
        gene_selected_color %in% rownames(se),
        sprintf("Invalid '%s' > '%s' input", .colorByField, .colorByGeneTableTitle)
      ))
  }

  # Shorthand
  assay_choice <- param_choices[[.geneExprAssay]]

  # List of commands to evaluate
  cmds <- list(
      data = character(),
      plot = character()
  )

  # Prepare X-axis data and axis label
  xchoice <- param_choices[[.geneExprXAxis]]
  if (xchoice==.geneExprXAxisColDataTitle) { # colData column selected
    # Set X-axis to the selected colData name
    byx <- param_choices[[.geneExprXAxisColData]]
    # Store the command to add X data
    cmds$data[["x"]] <- sprintf(
      "plot.data <- data.frame(X = colData(se)[,'%s', drop = FALSE]);",
      byx
    )
    group_by_x <- .group_by_x(colData(se)[,byx])
    numeric_x <- is.numeric(colData(se)[,byx])
  } else if (xchoice==.geneExprXAxisGeneExprsTitle) { # gene selected
    # Set X-axis to the selected gene identified
    byx <- .find_linked_gene(se, param_choices[[.geneExprXAxisGeneExprs]], input)
    # Store the command to add X data
    cmds$data[["x"]] <- sprintf(
      "plot.data <- data.frame(X = assay(se, '%s')['%s',], row.names = colnames(se));",
      assay_choice,
      byx
    )
    group_by_x <- .group_by_x(assay(se, assay_choice)[byx,])
    numeric_x <- is.numeric(assay(se, assay_choice)[byx,])
  } else { ## no x axis variable specified: show single violin
    byx <- '' # to hide the X axis label
    cmds$data[["x"]] <-
      "plot.data <- data.frame(X = rep('Feature', ncol(se)), row.names = colnames(se));"
    group_by_x <- TRUE
    numeric_x <- FALSE
  }

  # Store the Y-axis label (note that input was previously validated)
  y_lab <- .gene_axis_label(gene_selected_y, assay_choice, multiline = FALSE)
  # Store the command to prepare Y-axis data (required)
  cmds$data[["y"]] <- sprintf(
    "plot.data$Y <- assay(se, '%s')['%s',];", assay_choice, gene_selected_y
  )

  # Process the color choice (if any)
  color_choice <- param_choices[[.colorByField]]
  # Set a boolean to later build an aes with color
  # Assume that it is the case, unset later if false
  color_set <- TRUE
  if (identical(color_choice, .colorByColDataTitle)) {
    # Save the selected colData name to later set the plot label
    covariate.name <- param_choices[[.colorByColData]]
    # Store the command to add color data
    cmds$data[["color"]] <- sprintf("plot.data$ColorBy <- colData(se)[,'%s'];", covariate.name)
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
      "plot.data$ColorBy <- assay(se, '%s')['%s',];",
      assay.choice, covariate.name)
  } else {
    # Color choice is therefore 'None'
    covariate.name <- NA_character_ # do not plot label
    color_set <- FALSE # to later build an aes without color
  }

  # Prepare grouping if applicable
  if (group_by_x){
    cmds$data[["groupBy"]] <- "plot.data$GroupBy <- plot.data$X;"
  }

  # Store the ggplot commands
  cmds$plot[["ggplot"]] <- sprintf(
    "ggplot(plot.data, %s) +",
    .build_aes(color = color_set, group = group_by_x)
  )
  #
  if (group_by_x){
    cmds$plot[["violin"]] <-
      "geom_violin(aes(fill = GroupBy), color = 'gray60', alpha = 0.2, scale = 'width') +"
  } else {
    cmds$plot[["violin"]] <-
      "geom_violin(color = 'gray60', alpha = 0.3, fill = 'gray80', scale = 'width') +"
  }
  if (numeric_x) {
    cmds$plot[["point"]] <- "geom_point(alpha = 0.6) +"
  } else {
    cmds$plot[["point"]] <-
      "geom_jitter(alpha = 0.6, position = position_jitter(height = 0)) +"
  }
  cmds$plot[["labs"]] <- .build_labs(
      x = byx,
      y = y_lab,
      color = covariate.name
  )
  cmds$plot[["theme_base"]] <- "theme_bw() +"
  cmds$plot[["theme_custom"]] <- "theme(legend.position = 'bottom')"

  print(cmds)

  # Combine all the commands to evaluate
  cmds_eval <- paste(
      paste(cmds$data, collapse = "\n"),
      paste(cmds$plot, collapse = "\n\t"),
      sep  = "\n"
  )

  return(list(cmd = cmds_eval, plot = eval(parse(text = cmds_eval))))

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

.group_by_x <- function(covariate, max_levels = 12){
  # numeric values don't get grouped
  if (is.numeric(covariate)){
    return(FALSE)
  }
  # factors are grouped if fewer than max_levels levels
  if (is.factor(covariate)){
    return(nlevels(covariate) <= max_levels)
  }
  # characters are grouped if fewer than max_levels unique values
  return(length(unique(covariate)) <= max_levels)
}
