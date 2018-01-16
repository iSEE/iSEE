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
    validate(need(
      gene_selected %in% rownames(se),
      sprintf("Invalid '%s' > '%s' input", .colorByField, .colorByGeneTextTitle)
    ))
  }

    color_choice <- param_choices[[.colorByField]]
    if (color_choice==.colorByColDataTitle) {
      covariate.name <- param_choices[[.colorByColData]]
      covariate <- colData(se)[,covariate.name]
      astr <- "aes(x=Dim1, y=Dim2, color=Covariate)"
      cov.str <- sprintf(";\nplot.data$Covariate <- colData(se)[,'%s']", covariate.name)
    } else if (color_choice==.colorByGeneTableTitle || color_choice==.colorByGeneTextTitle) {

      if (color_choice==.colorByGeneTableTitle) {
        covariate.name <- .find_linked_gene(se, param_choices[[.colorByGeneTable]], input)
        assay.choice <- param_choices[[.colorByGeneTableAssay]]
      } else {
        covariate.name <- param_choices[[.colorByGeneText]]
        assay.choice <- param_choices[[.colorByGeneTextAssay]]
      }
      if (!is.null(covariate.name)) {
        covariate <- assay(se, assay.choice)[covariate.name,]
        astr <- "aes(x=Dim1, y=Dim2, color=Covariate)"
        cov.str <- sprintf(
          ";\nplot.data$Covariate <- assay(se, '%s')['%s',]",
          assay.choice, covariate.name)
        covariate.name <- .gene_axis_label(
          covariate.name, assay.choice, multiline = TRUE)
      } else {
        covariate.name <- 'NULL'
        covariate <- NULL
        astr <- "aes(x=Dim1, y=Dim2)"
        cov.str <- ""
        }
    } else {
      covariate.name <- 'NULL'
      covariate <- NULL
      astr <- "aes(x=Dim1, y=Dim2)"
      cov.str <- ""
    }

    # Figuring out what to do with brushing.
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

    cmd_prep <- paste(sprintf("red.dim <- reducedDim(se, '%s');",
                              param_choices[[.redDimType]]),
                      sprintf("plot.data <- data.frame(Dim1 = red.dim[, %s], Dim2 = red.dim[, %s])%s;",
                              param_choices[[.redDimXAxis]],
                              param_choices[[.redDimYAxis]],
                              cov.str),
                      sep = "\n")
    eval(parse(text = cmd_prep))

    cmd_plot <- paste(sprintf("ggplot(data = plot.data, %s) + ", astr),
                      "geom_point(size = 1.5) + ",
                      sprintf("labs(color = '%s') + ", covariate.name),
                      "theme_void() + ",
                      "theme(legend.position = 'bottom')\n",
                      sep = "\n\t")
    cmd <- paste(cmd_prep, cmd_plot, sep = "\n")
    list(xy = plot.data, cmd = cmd, plot = eval(parse(text = cmd_plot)))
}

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
    validate(need(
      gene_selected %in% rownames(se),
      sprintf("Invalid '%s' > '%s' input", .colorByField, .colorByGeneTextTitle)
    ))
  }

    # Process Y-axis
    cmd_y <- sprintf(
        "plot.data <- data.frame(Y = colData(se)[,'%s']);",
        param_choices[[.colDataYAxis]]
    )

    # Process X-axis
    if (param_choices[[.colDataXAxis]] == .colDataXAxisNothingTitle) {
        # TODO: allow toggling rank decreasing/increasing, note that factors cannot be negated
        x_lab <- NULL
        cmd_x <- 'plot.data$X <- rank(plot.data$Y, ties.method = "first");'
    } else {
        x_lab <- param_choices[[.colDataXAxisColData]]
        cmd_x <- sprintf(
            "plot.data$X <- colData(se)[,'%s'];",
            param_choices[[.colDataXAxisColData]]
        )
    }

    # general case; color aes dropped later if applicable
    cmd_aes <- "aes(X, Y, color = Covariate)"
    # process the color choice if any
    color_choice <- param_choices[[.colorByField]]
    if (color_choice == .colorByColDataTitle) {
        covariate.name <- param_choices[[.colorByColData]]
        cmd_color <- sprintf(
            "plot.data$Covariate <- colData(se)[,'%s'];", covariate.name
        )
    } else if (color_choice == .colorByGeneTableTitle || color_choice == .colorByGeneTextTitle){
        if (color_choice == .colorByGeneTableTitle) {
            covariate.name <- .find_linked_gene(se, param_choices[[.colorByGeneTable]], input)
            assay.choice <- param_choices[[.colorByGeneTableAssay]]
        } else if (color_choice == .colorByGeneTextTitle) {
            covariate.name <- param_choices[[.colorByGeneText]]
            assay.choice <- param_choices[[.colorByGeneTextAssay]]
        } else {
            stop("Impossible color choice!")
        }
        # Set the color to the selected gene

        if (identical(covariate.name, "")){
            warning("Color mode is gene expression, but none selected.")
        } else {
            cmd_color <- sprintf(
                "plot.data$Covariate <- assay(se, '%s')['%s',];",
                assay.choice, covariate.name
            )
            covariate.name <- .gene_axis_label(
              covariate.name, assay.choice, multiline = TRUE)
        }
    } else {
        covariate.name <- NULL
        cmd_color <- "# No coloring data"
        cmd_aes <- "aes(X, Y)"
    }


    # Creating the plot command
    gg_cmd <- paste(
        sprintf("ggplot(plot.data, %s) +", cmd_aes),
        "geom_point() +",
        sprintf(
            "labs(x = %s, y = '%s', color = %s) +",
            ifelse(is.null(x_lab), "NULL", sprintf("'%s'", x_lab)),
            param_choices[[.colDataYAxis]],
            ifelse(is.null(covariate.name), "NULL", sprintf("'%s'", covariate.name))
        ),
        "theme_bw() +",
        "theme(legend.position = 'bottom')\n",
        sep = "\n\t"
    )

    cmd <- paste(cmd_y, cmd_x, cmd_color, gg_cmd, sep = "\n")
    # message(cmd)

    return(list(cmd = cmd, plot = eval(parse(text = cmd)))) # xy = plot.data,
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

      return(list(xy = object, cmd = cmd, plot = eval(parse(text = cmd_plot))))
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
