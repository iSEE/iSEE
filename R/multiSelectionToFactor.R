#' Convert multiple selections into a factor
#'
#' Convert multiple selection information into a factor, typically for use as a covariate or for coloring.
#'
#' @param selected A named list of character vectors, containing the names of selected observations for different selections.
#' Vectors for different selections may overlap.
#' @param all.names Character vector of all observations.
#' 
#' @return A factor containing the set(s) to which each observation is assigned.
#' Multiple sets are encoded as comma-separated strings.
#' Unselected observations are listed as \code{"unselected"}.
#' 
#' @author Aaron Lun
#'
#' @examples
#' multiSelectionToFactor(list(active=c("A", "B"), 
#'     saved1=c("B", "C"), saved2=c("D", "E", "F")),
#'     all.names=LETTERS[1:10])
#' 
#' @export
multiSelectionToFactor <- function(selected, all.names) {
    output <- character(length(all.names))
    names(output) <- all.names

    for (x in names(selected)) {
        current <- selected[[x]]
        chosen <- output[current]
        present <- chosen!=""
        output[current][present] <- sprintf("%s,%s", chosen[present], x)
        output[current][!present] <- x 
    }

    output[output==""] <- "unselected"
    factor(output)
}
