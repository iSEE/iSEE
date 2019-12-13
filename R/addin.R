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
#' Opens a template R script in the editor, to define a new iSEE panel class .
#'
#' @param encoded Name of the new panel class.
#' @param decoded Extended name of the new panel class (for display).
#' @param parent Name of the parent panel class
#'
#' @export
#'
#' @author Kevin Rue-Albrecht
#'
#' @seealso \linkS4class{Panel}
#'
#' @examples
#' new_panel_file("NewRedDimPlot", "New reduced dimension plot", "RedDimPlot")
new_panel_file <- function(encoded, decoded, parent="Panel") {
  template_file <- system.file(package = "iSEE", "templates", "NewPanel.R")
  template_content <- scan(template_file, "character", sep = "\n", quiet = TRUE)
  template_content <- paste0(template_content, collapse = "\n")
  template_content <- gsub("__ENCODED__", encoded, template_content, fixed = TRUE)
  template_content <- gsub("__DECODED__", decoded, template_content, fixed = TRUE)
  template_content <- gsub("__PARENT__", parent, template_content, fixed = TRUE)
  rstudioapi::documentNew(template_content, type = "r")
}
