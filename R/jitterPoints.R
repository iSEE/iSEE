#' Jitter points for categorical variables
#'
#' Add quasi-random jitter on the x-axis for violin plots when the x-axis variable is categorical.
#' Add random jitter within a rectangular area for square plots when both x- and y-axis variables are categorical.
#'
#' @param X A factor corresponding to a categorical variable.
#' @param Y A numeric vector of the same length as \code{X} for violin plots, or a factor of the same length as \code{X} for square plots.
#' @param grouping A named list of vectors or factors as the same length as \code{X}, specifying how elements should be grouped.
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
#' If \code{grouping} is specified (e.g., corresponding to faceting variables), jitter is computed within each unique combination of all vectors in \code{grouping}.
#' This is useful for obtaining appropriate jittering when points are visualized by group, e.g., during faceting.
#'
#' If \code{grouping!=NULL}, the statistics in the returned \code{summary} data.frame will also be stratified by unique combinations of factors in \code{grouping}. 
#' To avoid clashes with existing fields, the names in \code{grouping} should not be \code{"X"}, \code{"Y"}, \code{"Freq"}, \code{"XWidth"} or \code{"YWidth"}.
#'
#' @return
#' If \code{Y} is a numeric vector, a numeric vector is returned containing the jittered x-axis coordinates for all points.
#' 
#' Otherwise, a list is returned with numeric vectors \code{X} and \code{Y}, containing jittered coordinates on the x- and y-axes respectively for all points;
#' and \code{summary}, a data.frame of frequencies and side lengths for each unique X/Y-level pairing.
#' 
#' @author Aaron Lun
#' @importFrom vipor offsetX
#' @importFrom stats runif
#'
#' @rdname INTERNAL_jitter_points
#' @examples
#' X <- factor(sample(LETTERS[1:4], 100, replace=TRUE))
#' Y <- rnorm(100)
#' (out1 <- jitterPoints(X=X, Y=Y))
#' 
#' Y2 <- factor(sample(letters[1:3], 100, replace=TRUE))
#' (out2 <- jitterPoints(X=X, Y=Y))
#'
#' grouped <- sample(5, 100, replace=TRUE)
#' (out3 <- jitterPoints(X=X, Y=Y, grouping=list(FacetRow=grouped)))
jitterPoints <- function(X, Y, grouping=NULL, ...) {
    stopifnot(length(X)==length(Y))

    # Groupings, usually for faceting.
    if (is.null(grouping)) {
        by_group <- list(seq_along(Y))
    } else {
        stopifnot(all(lengths(grouping)==length(X)))

        o <- do.call(order, grouping)
        nvals <- length(o)
        is_first <- logical(nvals)
        if (nvals) {
            for (grp in grouping) {
                grp <- grp[o]
                is_first <- is_first | c(TRUE, grp[-1]!=grp[-nvals])
            }
        }

        overall_group <- cumsum(is_first)
        by_group <- split(o, overall_group)
    }

    if (is.factor(Y)) {
        jittered_X <- jittered_Y <- numeric(length(Y))
        all_summary <- vector("list", length(by_group))

        # X/Y-jitter for square plots. 
        for (g in seq_along(by_group)) {
            grp <- by_group[[g]]
			current <- data.frame(X=X[grp], Y=Y[grp])
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
            jittered_X[grp] <- width_x*runif(nrow(current), -1, 1)
            jittered_Y[grp] <- width_y*runif(nrow(current), -1, 1)

            # Adding current bits and pieces regarding the grouping.
            if (!is.null(grouping)) {
                for (mode in names(grouping)) {
                    summary_data[[mode]] <- rep(grouping[[mode]][grp[1]], nrow(summary_data))
                }
            }
            all_summary[[g]] <- summary_data
        }
        return(list(X=jittered_X + as.integer(X), Y=jittered_Y + as.integer(Y), 
                    summary=do.call(rbind, all_summary)))
    } else {
        # X-jitter for violin plots.
        jittered_X <- numeric(length(Y))
        for (g in by_group) {
            jittered_X[g] <- vipor::offsetX(Y[g], x=X[g], ...) 
        }
        return(jittered_X + as.integer(X))
    }
}

