#' Create a landing page
#'
#' Define a function to create a landing page in which users can specify or upload \link{SummarizedExperiment} objects.
#'
#' @param seUI Function that accepts a single \code{id} argument and returns a UI element for specifying the SummarizedExperiment.
#' @param seLoad Function that accepts the input value of the UI element from \code{seUI}
#' and returns a \linkS4class{SummarizedExperiment} object.
#' @param initUI Function that accepts a single \code{id} argument and returns a UI element for specifying the initial state.
#' @param initLoad Function that accepts the input value of the UI element from \code{initUI}
#' and returns a list of \linkS4class{Panel}s.
#' @param requireButton Logical scalar indicating whether the app should require an explicit button press to initialize,
#' or if it should initialize upon any modification to the UI element in \code{seUI}.
#'
#' @return A function that generates a landing page upon being passed to \code{\link{iSEE}}
#' as the \code{landingPage} argument.
#'
#' @details
#' By default, this function creates a landing page in which users can upload an RDS file containing a SummarizedExperiment,
#' which is subsequently read by \code{\link{readRDS}} to launch an instance of \code{\link{iSEE}}.
#' However, any source of SummarizedExperiment objects can be used;
#' for example, we can retrieve them from databases by modifying \code{seUI} and \code{seLoad} appropriately.
#'
#' The default landing page also allows users to upload a RDS file containing a list of \linkS4class{Panel}s,
#' which specifies the initial state of the \code{\link{iSEE}} instance (effectively replacing the \code{initial} argument).
#' Again, any source can be used to create this list if \code{initUI} and \code{initLoad} are modified appropriately.
#' Note that only \link{Panel}s classes that were in the original \code{\link{iSEE}} call
#' (as the original \code{initial} or in the \code{extra}) will be used for security and other reasons.
#'
#' The UI elements for the SummarizedExperiment and the initial state are named \code{se} and \code{initial} respectively.
#' This can be used in Shiny bookmarking to initialize an \code{\link{iSEE}} in a desired state by simply clicking a link,
#' provided that \code{requireButton=FALSE} so that the reactive expressions are triggered by \code{initialize_INTERNAL_se}.
#' We do not use bookmarking to set parameters directly as we will run afoul of URL character limits.
#'
#' @section Defining a custom landing page:
#' We note that \code{createLandingPage} is just a limited wrapper around the landing page API.
#' In \code{\link{iSEE}}, \code{landingPage} can be any function with the following argument signature:
#' \itemize{
#' \item \code{FUN}, a function to initialize the \code{\link{iSEE}} observer architecture.
#' This function expects to be passed \code{SE}, a SummarizedExperiment object;
#' and \code{INITIAL}, a list of \linkS4class{Panel} objects describing the initial application state.
#' If \code{INITIAL=NULL}, the initial state from \code{initial} is used instead.
#' \item \code{input}, the Shiny input list.
#' \item \code{output}, the Shiny output list.
#' \item \code{session}, the Shiny session object.
#' }
#'
#' The function should render UI elements into \code{output$allPanels}
#' containing the required widgets to allow a user to set up an \code{\link{iSEE}} session interactively.
#' It should also define observers to respond to those elements and call \code{FUN} with appropriate arguments.
#' We suggest that all elements have IDs prefixed with \code{"initialize_INTERNAL"} to avoid conflicts with other elements.
#'
#' @author Aaron Lun
#' @examples
#' createLandingPage()
#'
#' # Alternative approach:
#'
#' @export
#' @importFrom shiny showNotification fileInput HTML observeEvent textInput actionButton
#' @importFrom shinyjs hidden
createLandingPage <- function(seUI=NULL, seLoad=NULL, initUI=NULL, initLoad=NULL, requireButton=TRUE) {
    if (is.null(seUI)) {
        seUI <- function(id) fileInput(id, "SummarizedExperiment RDS file:", multiple = FALSE)
    }
    if (is.null(seLoad)) {
        seLoad <- function(x) readRDS(x$datapath)
    }
    if (is.null(initUI)) {
        initUI <- function(id) fileInput(id, "Initial state RDS file:", multiple=FALSE)
    }
    if (is.null(initLoad)) {
        initLoad <- function(x) readRDS(x$datapath)
    }
    force(requireButton)

    function (FUN, input, output, session) {
        output$allPanels <- renderUI({
            tagList(
               seUI(.initializeSE),
               initUI(.initializeInitial),
               if (requireButton) actionButton(.initializeLaunch, label="Launch", style=.actionbutton_biocstyle)
            )
        })

        target <- if (requireButton) .initializeLaunch else .initializeSE

        observeEvent(input[[target]], {
            se2 <- try(seLoad(input[[.initializeSE]]), silent=TRUE)
            if (is(se2, "try-error")) {
                showNotification("must upload a valid RDS file", type="error")
            } else if (!is(se2, "SummarizedExperiment")) {
                showNotification("must upload a SummarizedExperiment object", type="error")
            } else {
                init <- try(initLoad(input[[.initializeInitial]], silent=TRUE))
                if (is(init, "try-error")) {
                    init <- NULL
                }
                FUN(SE=se2, INITIAL=init)
            }
        }, ignoreNULL=TRUE, ignoreInit=TRUE)
    }
}

# Using the same names as iSEE() for convenience.
.initializeSE <- "se"
.initializeInitial <- "initial"
.initializeLaunch <- "iSEE_INTERNAL_launch_init"
