#' Jitter points for categorical variables
#'
#' Adds quasi-random jitter on the x-axis for violin plots when the x-axis variable is categorical.
#' Alternatively, adds random jitter within a rectangular area for square plots when both x- and y-axis variables are categorical.
#'
#' @param X A factor corresponding to a categorical variable.
#' @param Y A numeric vector of the same length as \code{X} for violin plots, or a factor of the same length as \code{X} for square plots.
#' @param grouping A character vector or factor as the same length as \code{X}, specifying how elements should be grouped.
#' @param ... Further arguments to be passed to \code{\link{offsetX}}.
#'
#' @details
#' If \code{Y} is numeric, this function calls \code{\link{offsetX}} to obtain quasi-random jittered x-axis values.
#' This reflects the area occupied by a violin plot, though some tuning of arguments in \code{...} may be required to get an exact match.
#' 
#' If both \code{Y} and \code{X} are factors, points are uniformly jittered on both the x- and y-axes. 
#' The jitter area is a square with area proportional to the frequency of the paired levels in \code{X} and \code{Y}.
#' If either factor only has one level, the jitter area becomes a rectangle that can be interpreted as a bar plot.
#'
#' If \code{grouping} is specified (e.g., corresponding to faceting variables), jitter is computed within each level of \code{grouping}.
#' This is useful for obtaining appropriate jittering when points are visualized by group, e.g., during faceting.
#'
#' @return
#' If \code{Y} is a numeric vector, a numeric vector is returned containing the jittered x-axis coordinates for all points.
#' 
#' Otherwise, a list is returned with numeric vectors \code{X} and \code{Y}, containing jittered coordinates on the x- and y-axes respectively for all points;
#' and \code{summary}, a data.frame of rectangle coordinates and side lengths for each X/Y-level pairing. 
#' 
#' @author Aaron Lun
#' @export
#' @importFrom vipor offsetX
#' @importFrom stats runif
#'
#' @examples
#' X <- factor(sample(LETTERS[1:4], 100, replace=TRUE))
#' Y <- rnorm(100)
#' (out1 <- jitterPoints(X=X, Y=Y))
#' 
#' Y2 <- factor(sample(letters[1:3], 100, replace=TRUE)
#' (out2 <- jitterPoints(X=X, Y=Y))
#'
#' grouped <- sample(5, 100, replace=TRUE)
#' (out3 <- jitterPoints(X=X, Y=Y, grouping=grouped))
jitterPoints <- function(X, Y, grouping=NULL, ...) {
    stopifnot(length(X)==length(Y))

    # Groupings, usually for faceting.
    if (is.null(grouping)) {
        by_group <- list(seq_along(Y))
    } else {
        stopifnot(length(grouping)==length(X))
        by_group <- split(seq_along(Y), grouping)
    }

    if (is.factor(Y)) {
        # X/Y-jitter for square plots. 
        jittered_X <- jittered_Y <- numeric(length(Y))
        for (g in by_group) {
			current <- data.frame(X=X[g], Y=Y[g])
    		summary_data <- as.data.frame(table(X=current$X, Y=current$Y))
			norm_freq <- summary_data$Freq / max(summary_data$Freq)

            # Collapsing to a bar plot if there is only one level on either axis.
	    	if (nlevels(Y)==1L && nlevels(X)!=1L) {
				summary_data$XWidth <- 0.4
				summary_data$YWidth <- 0.49 * norm_freq
			} else if (nlevels(Y)!=1L && nlevels(X)==1L) {
                summary_data$XWidth <- 0.49 * norm_freq
                summary_data$YWidth <- 0.4
            } else {
                summary_data$XWidth <- summary_data$YWidth <- 0.49 * norm_freq
            }

            current$Marker <- seq_len(nrow(current))
            combined <- merge(current, summary_data, by=c('X', 'Y'), all_x=TRUE)

            o <- order(combined$Marker)
            width_x <- combined$XWidth[o]
            width_y <- combined$YWidth[o]
            jittered_X[g] <- width_x*runif(nrow(current), -1, 1)
            jittered_Y[g] <- width_y*runif(nrow(current), -1, 1)
        }
        return(list(X=jittered_X + as.integer(X), Y=jittered_Y + as.integer(Y), summary=summary_data))
    } else {
        # X-jitter for violin plots.
        jittered_X <- numeric(length(Y))
        for (g in by_group) {
            jittered_X[g] <- vipor::offsetX(Y[g], x=X[g], ...) 
        }
        return(jittered_X + as.integer(X))
    }
}

