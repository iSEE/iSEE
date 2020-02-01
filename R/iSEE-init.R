#' By default, the maximum request size for file uploads defaults to 5MB (\url{https://shiny.rstudio.com/reference/shiny/0.14/shiny-options.html}).
#' To raise the limit (e.g., 50MB), run \code{options(shiny.maxRequestSize=50*1024^2)}.
#'
#' @export
#' @importFrom shiny showNotification fileInput HTML observeEvent textInput
#' @importFrom shinyjs hidden
createUploadLandingPage <- function(uiFUN=NULL, seFUN=readRDS) {
    if (is.null(uiFUN)) {
        uiFUN <- function(id) fileInput(id, "Choose RDS file containing a SummarizedExperiment:", multiple = FALSE)
    }

    function (FUN, input, output, session, ...) {
        output$allPanels <- renderUI({
            tagList(
               uiFUN("initialize_INTERNAL_new_se"),
               hidden(textInput("initialize_INTERNAL_encoded_param", "Base-64 encoded memory"))
            )
        })

        observeEvent(input$initialize_INTERNAL_new_se, {
            se2 <- try(seFUN(input$initialize_INTERNAL_new_se$datapath), silent=TRUE)
            if (is(se2, "try-error")) {
                showNotification("must upload a valid RDS file", type="error")
            } else if (!is(se2, "SummarizedExperiment")) {
                showNotification("must upload a SummarizedExperiment object", type="error")
            } else {
                init <- try({
                    out <- base64enc::base64decode(input$initialize_INTERNAL_encoded_param)
                    unserialize(out)
                }, silent=TRUE)
                if (is(init, "try-error")) {
                    init <- NULL
                }
                FUN(SE=se2, INITIAL=init, ..., input=input, output=output, session=session)
            }
        }, ignoreNULL=TRUE, once=TRUE)
    }
}
