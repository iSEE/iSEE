#' Makes a heatmap
#' 
#' Make a heatmap with features on the Y axis and samples on the X axis.
#'
#' @param id Integer specifying the index of the heatmap plot panel
#' @param all_memory A \code{list} of \code{\linkS4class{DataFrame}}s, where each
#' \code{\linkS4class{DataFrame}} corresponds to a panel type and contains the
#' initial settings for each individual panel of that type.
#' @param all_coordinates A \code{list} of \code{data.frame}s that contain
#' the coordinates and covariates of data points visible in each of the plots;
#' in particular data points excluded by "restrict" brushes are not included in
#' the corresponding \code{data.frame}.
#' @param se A \code{\linkS4class{SingleCellExperiment}} object.
#' @param colormap An \code{\linkS4class{ExperimentColorMap}} object that defines
#' custom color maps to apply to individual \code{assays}, \code{colData},
#' and \code{rowData} covariates.
#'
#' @return A \code{list} that includes the following elements:
#' \describe{
#'   \item{cmd}{A \code{list} of \code{list} of commands
#'   as \code{character} vectors to parse and evaluate to produce the final plot.
#'   Each list element groups functionally related commands with a common purpose
#'   that should be evaluated together, yet separately from other groups
#'   for certains tasks).
#'   }
#'   \item{xy}{A \code{data.frame} that includes coordinates and covariates of
#'   the plot.}
#'   \item{plot}{A \code{\link{ggplot}} object that results of the evaluation
#'   of \code{cmd} items.}
#'   \item{legends}{A list of \code{\link{ggplot}} objects that contains color
#'   legends for the column data annotations.}
#' }
#' 
#' @author Charlotte Soneson, Aaron Lun, Kevin Rue-Albrecht 
#' @rdname INTERNAL_make_heatMapPlot
#' 
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom cowplot get_legend plot_grid
#' @importFrom scales rescale
#' @importFrom reshape2 melt
#' @importFrom dplyr arrange
.make_heatMapPlot <- function(id, all_memory, all_coordinates, se, colormap) {
  param_choices <- all_memory$heatMapPlot[id,]
  eval_env <- new.env()

  genes_selected_y <- param_choices[[.heatMapFeatName]][[1]]
  validate(need( 
    length(genes_selected_y) > 0L,
    sprintf("need at least one feature for heat map") 
  ))
  assay_choice <- param_choices[[.heatMapAssay]]
  
  # Extract genes to be included and melt the matrix to long format
  data_cmds <- c(
    sprintf("value.mat <- as.matrix(assay(se, %i)[%s, , drop=FALSE]);", 
            assay_choice, paste(deparse(genes_selected_y), collapse="\n")),
    sprintf("value.mat <- t(scale(t(value.mat), center = %s, scale = %s));", 
            .heatMapCenterTitle %in% param_choices[[.heatMapCenterScale]],
            .heatMapScaleTitle %in% param_choices[[.heatMapCenterScale]]),
    "plot.data <- reshape2::melt(value.mat, varnames = c('Y', 'X'));"
  )

  # Arrange cells according to the selected colData columns
  orderBy <- param_choices[[.heatMapColData]][[1]]
  validate(need( 
      length(orderBy) > 0L,
      sprintf("need at least one column annotation for heat map") 
  ))
  setup_cmds <- c(
      vapply(seq_along(orderBy), FUN=function(i) {
          if (.is_groupable(colData(se)[[orderBy[i]]])) {
              sprintf("plot.data[['OrderBy%i']] <- factor(colData(se)[['%s']][match(plot.data$X, rownames(colData(se)))]);",
                      i, orderBy[i])
          } else {
              sprintf("plot.data[['OrderBy%i']] <- colData(se)[['%s']][match(plot.data$X, rownames(colData(se)))];",
                      i, orderBy[i])
          }
      }, FUN.VALUE=character(1)),
      sprintf("plot.data <- dplyr::arrange(plot.data, %s);", 
              paste0("OrderBy", seq_along(orderBy), collapse = ",")),
      sprintf("plot.data$X <- factor(plot.data$X, levels = unique(plot.data$X));")
  )
  data_cmds <- c(data_cmds, "", setup_cmds)

  # Evaluate to create plot.data, and determining the colour range of the full data.
  eval(parse(text=data_cmds), envir=eval_env)
  min.obs <- min(eval_env$plot.data$value, na.rm=TRUE)
  max.obs <- max(eval_env$plot.data$value, na.rm=TRUE)

  # Processing the brushing choice.
  # Note that the cell names are not in the rownames(plot.data), but in plot.data$X, hence the sub().
  alpha_cmd <- ""
  alpha_legend_cmd <- NULL
  brush_out <- .process_brushby_choice(param_choices, all_memory)
  brush_cmds <- brush_out$cmds

  if (length(brush_cmds)) { 
      brush_cmds[["select"]] <- sub("rownames(plot.data)", "plot.data$X", brush_cmds[["select"]], fixed=TRUE)
      eval_env$all_brushes <- brush_out$data
      eval_env$all_lassos <- brush_out$data
      if (param_choices[[.brushEffect]]==.brushTransTitle) {
          alpha_cmd <- ", alpha=BrushBy"
          alpha_legend_cmd <- "guides(alpha=FALSE) +"
      } else if (param_choices[[.brushEffect]]==.brushColorTitle) {
          ## Add annotation bar
          orderBy <- c(orderBy,"BrushBy")
      }
  }

  # Define zoom command. Transform the ranges given by zoomData to the actual coordinates in the heatmap
  bounds <- param_choices[[.zoomData]][[1]]
  if (!is.null(bounds)) {
      zoom_cmds <- sprintf("plot.data <- subset(plot.data, Y %%in%% rownames(value.mat)[c(%s)]); # zooming in", 
                           paste0(bounds, collapse = ","))
  } else {
      zoom_cmds <- NULL
  }

  # Heatmap. Get the colorbar separately to make it easier to guess the real
  # heatmap coordinates from a brush on the final combined plot
  plot_cmds <- c(
    "p0 <- ggplot(plot.data, aes(x = X, y = Y)) +",
    "geom_raster(aes(fill = value)) +",
    "labs(x='', y='') +",
    .get_heatmap_fill_cmd(param_choices, colormap, min.obs, max.obs),
    "scale_y_discrete(expand=c(0, 0)) +", 
    "theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line=element_blank());",
    "heatlegend <- cowplot::get_legend(p0 + theme(legend.position='bottom'));"
  )
  
  # Annotations
  annot_cmds <- "legends <- list()"
  annot_cmds0 <- lapply(seq_along(orderBy), function(i) {
      if (orderBy[i] != "BrushBy") {
          if (is.numeric(eval_env$plot.data[[paste0("OrderBy", i)]])) {
              color_cmd <- sprintf("scale_fill_gradientn(colors=colDataColorMap(colormap, '%s', discrete=FALSE)(21L), na.value='grey50', name='%s') +", 
                                   orderBy[i], orderBy[i])
          } else {
              color_cmd <- sprintf("scale_fill_manual(values=colDataColorMap(colormap, '%s', discrete=TRUE)(%i), na.value='grey50', drop=FALSE, name='%s') +", 
                                   orderBy[i], .nlevels(factor(eval_env$plot.data[[paste0("OrderBy", i)]])), orderBy[i])
          }
          
          c("",
            sprintf("p%i <- ggplot(plot.data, aes(x = X, y = 1)) +", i) ,
            sprintf("geom_raster(aes(fill = OrderBy%i%s)) +", i, alpha_cmd), 
            "labs(x='', y='') +", 
            alpha_legend_cmd,
            sprintf("scale_y_continuous(breaks=1, labels='%s') +", orderBy[i]), 
            color_cmd,
            "theme(axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), 
    rect=element_blank(), line=element_blank(), axis.title.y=element_blank(), 
    plot.margin = unit(c(0,0,-0.5,0), 'lines'));",
            sprintf("legends[[%i]] <- cowplot::get_legend(p%i + theme(legend.position='bottom', plot.margin = unit(c(0,0,0,0), 'lines')))", i, i)
          )
      } else {
          c("",
            sprintf("p%i <- ggplot(plot.data, aes(x = X, y = 1)) +", i),
            "geom_raster(aes(fill = BrushBy)) +",
            "labs(x='', y='') +",
            "scale_y_continuous(breaks=1, labels='Brushed points') +",
            sprintf("scale_fill_manual(values=c(`TRUE`='%s', `FALSE`='white')) +",param_choices[[.brushColor]]),
            "theme(axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), 
    rect=element_blank(), line=element_blank(), axis.title.y=element_blank(), 
            plot.margin = unit(c(0,0,-0.5,0), 'lines'));")
      }
  })
  annot_cmds <- c(annot_cmds, unlist(annot_cmds0)) 
  
  # Evaluate to get the individual legends
  plot_part <- eval(parse(text=c(brush_cmds, zoom_cmds, plot_cmds, annot_cmds)), envir=eval_env)
  legends <- eval_env$legends

  # Put heatmap and annotations together
  grid_cmds <- c(
        sprintf("cowplot::plot_grid(
    cowplot::plot_grid(
        %s,
        ncol=1, align='v', rel_heights=c(%s)), 
    heatlegend, ncol=1, rel_heights=c(%s, %s))", 
            paste0("p", c(seq_along(orderBy), 0), 
                   c(rep(" + theme(legend.position='none')", length(orderBy)), 
                     " + theme(legend.position='none')"), collapse = ",\n        "),
            paste(c(rep(.heatMapRelHeightAnnot, length(orderBy)), .heatMapRelHeightHeatmap), 
                  collapse = ", "),
            1-.heatMapRelHeightColorBar, .heatMapRelHeightColorBar)
  )
  
  plot_out <- eval(parse(text=grid_cmds), envir=eval_env)
  return(list(cmd_list=list(data=data_cmds, brush=brush_cmds, zoom=zoom_cmds, plot=plot_cmds, annot=annot_cmds, grid=grid_cmds), 
              xy=eval_env$value.mat, plot=plot_out, legends=legends))
}

.get_colorscale_limits <- function(min.value, max.value, lower.bound, upper.bound, include.zero) {
  if (is.finite(lower.bound)) v1 <- lower.bound
  else v1 <- min.value
  if (is.finite(upper.bound)) v2 <- upper.bound
  else v2 <- max.value
  if (include.zero) sort(c(v1, v2, 0))
  else c(v1, v2)
}

#' @importFrom shiny showNotification req
#' @importFrom stats dist hclust as.dendrogram order.dendrogram
.cluster_genes <- function(X) {
     if (is.null(dim(X)) || nrow(X) < 2L) {
         showNotification("must have at least 2 features for clustering", type="error")
         req(FALSE)
     }
     D <- dist(X)
     hc <- hclust(D)
     d <- as.dendrogram(hc)
     rownames(X)[order.dendrogram(d)]
}

.transform_global_to_local_y <- function(yg, n.annot, n.genes) {
    k <- n.genes*(.heatMapRelHeightAnnot*n.annot+.heatMapRelHeightHeatmap)/(1-.heatMapRelHeightColorBar)
    m <- 0.5-.heatMapRelHeightColorBar*k
    v <- round(k*yg+m)
    # Has to be between 1 and n.genes
    v <- min(v, n.genes)
    v <- max(1, v)
    v
}

#' @importFrom scales rescale
.get_heatmap_fill_cmd <- function(param_choices, colormap, min.obs, max.obs) {
    # Set limits of color bar
    limits <- range(min.obs, param_choices[[.heatMapLower]], 
                    max.obs, param_choices[[.heatMapUpper]], finite=TRUE, na.rm=TRUE)

    # Get values to define range of colors
    centered <- .heatMapCenterTitle %in% param_choices[[.heatMapCenterScale]][[1]]
    break.vec <- .get_colorscale_limits(min.value=min.obs, max.value=max.obs, 
                                        lower.bound=param_choices[[.heatMapLower]], 
                                        upper.bound=param_choices[[.heatMapUpper]],
                                        include.zero=centered)
    
    if (centered) {
        validate(need( 
            is.na(param_choices[[.heatMapLower]]) || param_choices[[.heatMapLower]] < 0L,
            sprintf("Lower bound must be negative") 
        ))
        validate(need( 
            is.na(param_choices[[.heatMapUpper]]) || param_choices[[.heatMapUpper]] > 0L,
            sprintf("Upper bound must be positive") 
        ))

        # Centered values - use selected color scale
        col.vec <- strsplit(param_choices[[.heatMapCenteredColors]], "-")[[1]]
        if (param_choices[[.heatMapLower]] > min.obs && is.finite(param_choices[[.heatMapLower]])) {
            break.vec <- c(min.obs, break.vec)
            col.vec <- c(col.vec[1], col.vec)
        }
        if (param_choices[[.heatMapUpper]] < max.obs && is.finite(param_choices[[.heatMapUpper]])) {
            break.vec <- c(break.vec, max.obs)
            col.vec <- c(col.vec, col.vec[length(col.vec)])
        }
        break.vec <- scales::rescale(break.vec, to=c(0, 1), from=limits)
        fill_cmd <- sprintf("scale_fill_gradientn(colors=c('%s'), 
                            values=c(%s), 
                            limits=c(%s), na.value='grey50') +", 
                            paste0(col.vec, collapse="','"),
                            paste0(break.vec, collapse=","),
                            paste0(limits, collapse=","))
    } else {
        validate(need( 
            param_choices[[.heatMapLower]] < max.obs,
            sprintf("Lower bound must be lower than largest value in matrix") 
        ))
        validate(need( 
            param_choices[[.heatMapUpper]] > min.obs,
            sprintf("Upper bound must be higher than smallest value in matrix") 
        ))
        col.vec <- assayColorMap(colormap, param_choices[[.heatMapAssay]], discrete=FALSE)(21L)
        col.add.before <- col.add.after <- break.add.before <- break.add.after <- ""
        if (param_choices[[.heatMapLower]] > min.obs && is.finite(param_choices[[.heatMapLower]])) {
            col.add.before <- sprintf("c('%s',", col.vec[1])
            break.add.before <- sprintf("c(%s,", min.obs)
        }
        if (param_choices[[.heatMapUpper]] < max.obs && is.finite(param_choices[[.heatMapUpper]])) {
            col.add.after <- sprintf(",'%s')", col.vec[length(col.vec)])
            break.add.after <- sprintf(",%s)", max.obs)
        }
        if (col.add.before=="" && col.add.after != "") col.add.before <- "c("
        if (break.add.before=="" && break.add.after != "") break.add.before <- "c("
        if (col.add.after=="" && col.add.before != "") col.add.after <- ")"
        if (break.add.after=="" && break.add.before != "") break.add.after <- ")"
        
        fill_cmd <- sprintf("scale_fill_gradientn(colors=%sassayColorMap(colormap, '%s', discrete=FALSE)(21L)%s, 
                            values=%s, 
                            limits=c(%s), na.value='grey50') +",
                            col.add.before,
                            param_choices[[.heatMapAssay]],
                            col.add.after,
                            paste0("scales::rescale(", break.add.before,
                                   "seq(", min(break.vec), ",", max(break.vec), ",length.out=21L)",
                                   break.add.after, ", to=c(0,1), from=c(", paste0(limits, collapse=","), "))"),
                            paste0(limits, collapse=","))
    }
    fill_cmd
}
