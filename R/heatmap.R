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

  # Define explicit colors for each ordering column
  data_cmds[["colmaps"]] <- list()
  for (i in seq_along(orderBy)) {
    data_cmds[["colmaps"]] <- 
      c(data_cmds[["colmaps"]],
        list(sprintf("plot.data[['ColorBy%i']] <- colDataColorMap(colormap, '%s', discrete=%s)(%i)[as.numeric(factor(plot.data[['OrderBy%i']]))];", 
                     i, orderBy[i], 
                     ifelse(is.numeric(eval_env$plot.data[[paste0("OrderBy", i)]]), FALSE, TRUE), 
                     length(unique(eval_env$plot.data[[paste0("OrderBy", i)]])),
                     i),
             sprintf("plot.data[['ColorBy%i']][is.na(plot.data[['ColorBy%i']])] <- 'grey50'", i, i)
             )
    )
  }

  # Define plotting commands
  extra_cmds <- c(list(
    sprintf("ggplot(plot.data, aes(x = X, y = Y)) +"),
    sprintf("geom_raster(aes(fill = value)) +")
  ),
  lapply(seq_len(length(orderBy)), function(i) {
    sprintf("geom_segment(aes(x=as.integer(X)-0.5, xend=as.integer(X)+0.5, y=nrow(value.mat)+(%s), yend=nrow(value.mat)+(%s), color=%s)) +", 1+nrow(eval_env$value.mat)/10+(length(orderBy)-i)*nrow(eval_env$value.mat)/(4*length(orderBy)), 1+nrow(eval_env$value.mat)/10+(length(orderBy)-i+1)*nrow(eval_env$value.mat)/(4*length(orderBy)), paste0("ColorBy", i))
  }),
  list(sprintf("labs(x='', y='') +"),
       sprintf("scale_color_identity() +"),
       sprintf("theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +"),
       "theme(legend.position = 'bottom')"
  ))

  to_eval <- unlist(c(data_cmds["colmaps"], extra_cmds))
  plot_out <- eval(parse(text=to_eval), envir=eval_env)

  return(list(cmd = c(data_cmds, extra_cmds), xy = NULL, plot = plot_out))
}

