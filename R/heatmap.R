.make_heatMapPlot <- function(id, all_memory, all_coordinates, se, colormap)
  # Makes a heatmap.
{
  param_choices <- all_memory$heatMapPlot[id,]
  data_cmds <- list()
  
  genes_selected_y <- param_choices[[.heatMapFeatName]][[1]]
  validate(need( 
    length(genes_selected_y) > 0L,
    sprintf("Invalid genes for heat map input") # NEED BETTER ERROR MESSAGE HERE?
  ))
  
  assay_choice <- param_choices[[.heatMapAssay]]
  
  # Extract genes to be included and melt the matrix to long format
  data_cmds[["y"]] <- list(
    sprintf("value.mat <- as.matrix(assay(se, %i)[%s, , drop=FALSE]);", 
            assay_choice, paste0('c(', paste(genes_selected_y, collapse = ','), ')')),
    sprintf("value.mat <- t(scale(t(value.mat), center = %s, scale = %s));", 
            ifelse(param_choices[[.heatMapCentering]] == .heatMapYesTitle, 
                   "TRUE", "FALSE"),
            ifelse(param_choices[[.heatMapScaling]] == .heatMapYesTitle, 
                   "TRUE", "FALSE")),
    sprintf("plot.data <- reshape2::melt(value.mat, varnames = c('Y', 'X'));")
  )
  
  # Arrange cells according to the selected colData columns
  orderBy <- unlist(param_choices[[.heatMapColData]])
  data_cmds[["order"]] <- 
    c(
      lapply(seq_along(orderBy), function(i) {
        sprintf("plot.data[['OrderBy%i']] <- colData(se)[['%s']][match(plot.data$X, rownames(colData(se)))];",
                i, orderBy[i])
      }),
      sprintf("plot.data <- dplyr::arrange(plot.data, %s);", 
              paste0("OrderBy", seq_along(orderBy), collapse = ",")),
      sprintf("plot.data$X <- factor(plot.data$X, levels = unique(plot.data$X));")
    )

  eval_env <- new.env()
  eval(parse(text=unlist(data_cmds[c("y", "order")])), envir=eval_env)

  if (param_choices[[.heatMapCentering]] == .heatMapYesTitle) {
    colors.to.use <- strsplit(param_choices[[.heatMapCenteredColors]], "-")[[1]]
    fill_cmd <- sprintf("scale_fill_gradient2(low='%s', mid='%s', high='%s', na.value='grey50') +",
                        colors.to.use[1], colors.to.use[2], colors.to.use[3])
  } else {
    fill_cmd <- sprintf("scale_fill_gradientn(colors=assayColorMap(colormap, '%s', discrete=FALSE)(21L), na.value='grey50') +", assay_choice)
  }
    
  # Define plotting commands
  extra_cmds <- list()
  # Heatmap
  extra_cmds[["heatmap"]] <- list(
    sprintf("p0 <- ggplot(plot.data, aes(x = X, y = Y)) +"),
    sprintf("geom_raster(aes(fill = value)) +"),
    sprintf("labs(x='', y='') +"),
    fill_cmd, 
    sprintf("theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line=element_blank()) +"),
    sprintf("theme(legend.position='bottom');")
  )
  
  # Annotations
  extra_cmds[["annotations"]] <- lapply(seq_along(orderBy), function(i) {
    if (is.numeric(eval_env$plot.data[[paste0("OrderBy", i)]])) {
      color_cmd <- sprintf("scale_fill_gradientn(colors=colDataColorMap(colormap, '%s', discrete=FALSE)(21L), na.value='grey50', name='%s') +", orderBy[i], orderBy[i])
    } else {
      color_cmd <- sprintf("scale_fill_manual(values=colDataColorMap(colormap, '%s', discrete=TRUE)(%i), na.value='grey50', drop=FALSE, name='%s') +", orderBy[i], .nlevels(factor(eval_env$plot.data[[paste0("OrderBy", i)]])), orderBy[i])
    }
    
    list(
      sprintf("p%i <- ggplot(plot.data, aes(x = X, y = 1)) +", i) ,
      sprintf("geom_raster(aes(fill = OrderBy%i)) +", i), 
      sprintf("labs(x='', y='') +"), 
      sprintf("scale_y_continuous(breaks=1, labels='%s') +", orderBy[i]), 
      color_cmd,
      sprintf("theme(axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), rect=element_blank(), line=element_blank(), axis.title.y=element_blank(), plot.margin = unit(c(0,0,-0.5,0), 'lines'));"),
      sprintf("legend%i <- cowplot::get_legend(p%i)", i, i)
    )
  })
  
  extra_cmds[["legends"]] <- list(
    sprintf("legends <- list(%s);", paste0("legend", seq_len(length(orderBy)), collapse=","))
  )
  
  # Evaluate to get the individual legends
  to_eval <- unlist(extra_cmds)
  plot_part <- eval(parse(text=to_eval), envir=eval_env)
  
  legends <- eval_env$legends

  extra_cmds[["grid"]] <- list(
    sprintf("cowplot::plot_grid(%s, ncol=1, align='v', rel_heights=c(%s))", 
            paste0("p", c(seq_along(orderBy), 0), 
                   c(rep("+theme(legend.position='none')", length(orderBy)), ""), collapse = ","),
            paste(c(rep(0.1, length(orderBy)), 1), collapse = ","))
  )
  
  to_eval <- extra_cmds[["grid"]]
  plot_out <- eval(parse(text=to_eval), envir=eval_env)

  return(list(cmd=c(data_cmds, extra_cmds), xy=NULL, plot=plot_out, legends=legends))
}

