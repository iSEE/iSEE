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
    # Store the command to add X data
    cmds$data[["x"]] <- 'plot.data$X <- rank(plot.data$Y, ties.method = "first");'
    # In that case, do not show any X-axis label (applies below)
    x_lab <- "Rank"
  } else {
    # Store the command to add X data
    cmds$data[["x"]] <- sprintf(
      "plot.data$X <- colData(se)[,'%s'];", param_choices[[.colDataXAxisColData]]
    )
    # Set X-axis to the selected colData name
    x_lab <- param_choices[[.colDataXAxisColData]]
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

.make_geneExprPlot <- function(se, param_choices, input)
# Makes a gene expression plot.
{
  # Do not plot if gene name input is not a valid rownames(se)
  ## Y-axis (always a gene table)
  gene_selected <- .find_linked_gene(se, param_choices[[.geneExprID]], input)
  validate(need(
      gene_selected %in% rownames(se),
      sprintf("Invalid Y-axis '%s' input", .geneExprID)
  ))
  # X axis (gene table)
  if (identical(param_choices[[.geneExprXAxis]], .geneExprXAxisGeneExprsTitle)){
      gene_selected <- .find_linked_gene(se, param_choices[[.geneExprXAxisGeneExprs]], input)
      validate(need(
        gene_selected %in% rownames(se),
        sprintf("Invalid '%s' > '%s' input", .geneExprXAxis, .geneExprXAxisGeneExprsTitle)
      ))
  }
  # Colour (gene table)
  if (identical(param_choices[[.colorByField]], .colorByGeneTableTitle)){
      gene_selected <- .find_linked_gene(se, param_choices[[.colorByGeneTable]], input)
      validate(need(
        gene_selected %in% rownames(se),
        sprintf("Invalid '%s' > '%s' input", .colorByField, .colorByGeneTableTitle)
      ))
  }

  # Get x axis
  xchoice <- param_choices[[.geneExprXAxis]]
  if (xchoice==.geneExprXAxisColDataTitle) { # colData column
    byx <- param_choices[[.geneExprXAxisColData]]
    show_violin <- TRUE
    cmd_x <- sprintf("xcoord <- se[['%s']];",
                     param_choices[[.geneExprXAxisColData]])
  } else if (xchoice==.geneExprXAxisGeneExprsTitle) { # gene
    byx <- .find_linked_gene(se, param_choices[[.geneExprXAxisGeneExprs]], input)
    show_violin <- FALSE
    cmd_x <- sprintf("xcoord <- assay(se, '%s')['%s', ];",
                     param_choices[[.geneExprAssay]],
                     byx)
  } else { ## no x axis variable specified
    byx <- NULL
    show_violin <- TRUE
    cmd_x <- NULL
  }

  # Get variable to color by
  color_choice <- param_choices[[.colorByField]]
  if (color_choice==.colorByColDataTitle) {
    covariate.name <- param_choices[[.colorByColData]]
    covariate <- colData(se)[,covariate.name]
    cmd_col <- sprintf("covariate <- se[['%s']];", covariate.name)
  } else if (color_choice==.colorByGeneTableTitle || color_choice==.colorByGeneTextTitle) {
    if (color_choice==.colorByGeneTableTitle) {
      covariate.name <- .find_linked_gene(se, param_choices[[.colorByGeneTable]], input)
      covariate.assay.choice <- param_choices[[.colorByGeneTableAssay]]
    } else {
      covariate.name <- param_choices[[.colorByGeneText]]
      if (!covariate.name %in% rownames(se)) {
        covariate.name <- NULL
      }
      covariate.assay.choice <- param_choices[[.colorByGeneTextAssay]]
    }
    if (!is.null(covariate.name)) {
      covariate <- assay(se, covariate.assay.choice)[covariate.name,]
      cmd_col <- sprintf("covariate <- assay(se, '%s')['%s', ];",
                         covariate.assay.choice, covariate.name)
    } else {
      covariate.name <- NULL
      covariate <- NULL
      cmd_col <- NULL
    }
  } else {
    covariate.name <- NULL
    covariate <- NULL
    cmd_col <- NULL
  }
  cmd_samp <- "samples.long <- data.frame(row.names = colnames(se));"
  if (!is.null(covariate.name)) {
    cmd_samp <- paste(cmd_samp,
                      sprintf("samples.long[['%s']] <- covariate;",
                              covariate.name),
                      sep = "\n")
  }
  if (!is.null(byx)) {
    cmd_samp <- paste(cmd_samp,
                      sprintf("samples.long[['%s']] <- xcoord;", byx),
                      sep = "\n")
  }

  # Getting the gene choice for the y axis
  cur.gene <- .find_linked_gene(se, param_choices[[.geneExprID]], input)

  if (!is.null(cur.gene)) {
    # Get expression values and melt
    ylab <- .gene_axis_label(cur.gene, param_choices[[.geneExprAssay]], multiline = FALSE)

    cmd_y <- sprintf("exprs.mat <- as.matrix(assay(se, '%s'))['%s', , drop = FALSE];\nevals.long <- reshape2::melt(exprs.mat, value.name = 'evals');\ncolnames(evals.long) <- c('Feature', 'Cell', 'evals');",
                     param_choices[[.geneExprAssay]], cur.gene)

    ## Evaluate data generation part of final command
    cmd_prep <- paste(cmd_x, cmd_col, cmd_samp, cmd_y)
    eval(parse(text = cmd_prep))

    if (!is.null(evals.long)) {
      cmd_obj <- sprintf("object <- cbind(evals.long, samples.long);")
      eval(parse(text = cmd_obj))

      # Define aesthetics
      aesth <- list()
      if (is.null(byx)) { # no x axis variable specified
        aesth$x <- "Feature"
        xlab <- NULL
      } else if (xchoice==.geneExprXAxisGeneExprsTitle){ # gene expression on x axis
        aesth$x <- byx
        xlab <- sprintf("%s (%s)", byx, param_choices[[.geneExprAssay]])
      } else { # colData column
        aesth$x <- byx
        xlab <- byx
      }

      aesth$y <- "evals"
      if (!is.null(covariate)) {
        aesth$color <- covariate.name
      }
      if (is.null(aesth$color) && is.null(byx)) {
        aesth$color <- "Feature"
      }

      # Group values if x axis is categorical with at most 5 categories
      group_by_x <- (show_violin &&
                       (!is.numeric(object[[as.character(aesth$x)]]) ||
                          nlevels(as.factor(object[[as.character(aesth$x)]])) <= 5))
      if (group_by_x) {
        aesth$group <- aesth$x
      } else {
        aesth$group <- 1
        show_violin <- FALSE
      }

      cmd_plot <- sprintf("ggplot(object, aes(x = `%s`, y = `%s`%s, group = %s)) + \n\txlab('%s') + ylab('%s')",
                          aesth$x, aesth$y,
                          ifelse(is.null(aesth$color), "", paste0(", color = \`", aesth$color, "\`")),
                          ifelse(aesth$group==1, 1, paste0("\`", as.character(aesth$group), "\`")),
                          ifelse(is.null(xlab), "", xlab),
                          ylab)

      if (is.numeric(aesth$x)) {
        cmd_plot <- paste0(cmd_plot,
                           sprintf(" + \n\tgeom_point(alpha = 0.6)"))
      } else {
        cmd_plot <- paste0(cmd_plot,
                           sprintf(" + \n\tgeom_jitter(alpha = 0.6, position = position_jitter(height = 0))"))
      }
      if (show_violin) {
        if (!is.null(aesth$color) && aesth$color == as.symbol("Feature")) {
          cmd_plot <- paste0(cmd_plot,
                             sprintf(" + \n\tgeom_violin(aes(fill = Feature), color = 'gray60', alpha = 0.2, scale = 'width')"))
        } else {
          cmd_plot <- paste0(cmd_plot,
                             sprintf(" + \n\tgeom_violin(color = 'gray60', alpha = 0.3, fill = 'gray80', scale = 'width')"))
        }
      }

      if (is.null(covariate.name)) {
        cmd_plot <- paste0(
          cmd_plot, "+ \n\tguides(fill = 'none', color = 'none')"
        )
      } else {
          if (color_choice==.colorByGeneTableTitle || color_choice==.colorByGeneTextTitle){
            color_lab <- .gene_axis_label(
              covariate.name, covariate.assay.choice, multiline = TRUE)
            cmd_plot <- paste0(
              cmd_plot, sprintf(
                "+ \n\tlabs(fill = '%s', color = '%s')", # TODO: evaluates OK; prints dirty
                color_lab, color_lab
              )
            )
          }
      }

      cmd_plot <- paste0(cmd_plot, "+ \n\ttheme_bw() + theme(legend.position = 'bottom')\n")
      cmd <- paste(cmd_x, cmd_col, cmd_samp, cmd_y, cmd_obj, cmd_plot, sep = "\n")

      return(list(cmd = cmd, plot = eval(parse(text = cmd_plot))))
    }
  }
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
