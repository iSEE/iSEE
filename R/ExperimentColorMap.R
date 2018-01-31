isColorMapCompatible <- function(colormap, se, error = FALSE){
  
  # type-checking; is this worth a generic method?
  stopifnot(is(colormap, "ExperimentColorMap"))
  stopifnot(inherits(se, "SummarizedExperiment"))
  
  # The count of color maps cannot exceed the count of assays
  num_assay_maps <- length(colormap@assays)
  num_assay_se <- length(se@assays)
  if (num_assay_maps > num_assay_se){
    if (error){
      stop(
        "More assays in color map (",
        num_assay_maps,
        ") than experiment (",
        num_assay_se,
        ")")
    } else {
      return(FALSE)
    }
  }
  
  # Named color maps must map to existing data in the experiment
  names_assays_maps <- names(colormap@assays)
  names_coldata_maps <- names(colormap@colData)
  names_rowdata_maps <- names(colormap@rowData)
  
  names_assays_se <- assayNames(se)
  names_coldata_se <- names(colData(se))
  names_rowdata_se <- names(rowData(se))
  
  # process assays
  names_assays_maps <- names_assays_maps[names_assays_maps != ""]
  check_assay_names <- names_assays_maps %in% names_assays_se
  if (!all(check_assay_names)){
    if (error){
      stop(
        sprintf(
          "assay `%s` in color map missing in experiment",
          names_assays_maps[!check_assay_names]
        )
      )
    } else {
      return(FALSE)
    }
  }
  
  # process colData
  check_coldata_names <- names_coldata_maps %in% names_coldata_se
  if (!all(check_coldata_names)){
    if (error){
      stop(
        sprintf(
          "colData `%s` in color map missing in experiment",
          names_coldata_maps[!check_coldata_names]
        )
      )
    } else {
      return(FALSE)
    }
  }
  
  # process rowData
  check_rowdata_names <- names_rowdata_maps %in% names_rowdata_se
  if (!all(check_rowdata_names)){
    if (error){
      stop(
        sprintf(
          "rowData `%s` in color map missing in experiment",
          names_rowdata_maps[!check_rowdata_names]
        )
      )
    } else {
      return(FALSE)
    }
  }
  
  return(TRUE)
}
