new_panel_addin = function() {
  sys.source(system.file(package = 'iSEE', 'scripts', 'new_panel.R'))
}

#' @importFrom methods extends getClasses
collect_parents <- function() {
	x <- getClasses("package:iSEE")
	is_panel <- function(Class) {
		extends(Class, "Panel")
	}
	keep <- vapply(x, is_panel, FUN.VALUE = logical(1))
	x[keep]
}

#' Create a new panel file
#'
#' Opens a new R script in the editor after substituting
#'
#' @param encoded
#' @param decoded
#' @param parent
#'
#' @importFrom rstudioapi documentNew
new_panel_file <- function(encoded, decoded, parent) {
  template_file <- system.file(package = "iSEE", "templates", "NewPanel.R")
  template_content <- scan(template_file, "character", sep = "\n")
  template_content <- paste0(template_content, collapse = "\n")
  template_content <- gsub("__ENCODED__", encoded, template_content, fixed = TRUE)
  template_content <- gsub("__DECODED__", decoded, template_content, fixed = TRUE)
  template_content <- gsub("__PARENT__", parent, template_content, fixed = TRUE)
  documentNew(template_content, type = "r")
}
