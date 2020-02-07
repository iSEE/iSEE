.generalLinkGraph <- "iSEE_INTERNAL_link_graph"
.generalTrackedCode <- "iSEE_INTERNAL_tracked_code"
.generalPanelSettings <- "iSEE_INTERNAL_panel_settings"
.generalTourSteps <- "iSEE_INTERNAL_tour_steps"
.generalVignetteOpen <- "iSEE_INTERNAL_open_vignette"
.generalSessionInfo <- "iSEE_INTERNAL_session_info"
.generalCitationInfo <- "iSEE_INTERNAL_citation_info"
.generalExportOutput <- "iSEE_INTERNAL_export_content"
.generalExportOutputAll <- "iSEE_INTERNAL_export_content_all"

#nocov start

#' General observers for \code{\link{iSEE}}
#'
#' A function to set up observers for general (i.e., not panel-specific) observers used in the app.
#'
#' @param input The Shiny input object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#' @param tour A data.frame of tour steps to use in \code{\link{introjs}}.
#' @param runLocal A logical scalar indicating whether this app is run locally or on a server,
#' which determines which vignette to serve.
#' @param se_name,ecm_name Strings containing variable names to be passed to \code{\link{.track_it_all}}.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @importFrom utils read.delim sessionInfo citation browseURL
#' @importFrom shiny observeEvent showModal modalDialog
#' HTML br renderPrint tagList showNotification p
#' @importFrom rintrojs introjs
#' @importFrom shinyAce aceEditor
#'
#' @rdname INTERNAL_general_observers
.create_general_observers <- function(tour, runLocal, se_name, ecm_name, input, session, pObjects, rObjects) {
    observeEvent(input[[.generalTourSteps]], {
        if(is.null(tour)) {
            tour <- read.delim(system.file("extdata", "intro_firststeps.txt", package="iSEE"),
                sep=";", stringsAsFactors=FALSE, row.names=NULL, quote="")
        }
        introjs(session, options=list(steps=tour))
    })

    if (!is.null(tour)) {
        # Only triggers _after_ panels are fully setup, so observers are properly ID'd.
        session$onFlushed(function() { introjs(session, options=list(steps=tour)) })
    }

    observeEvent(input[[.generalTrackedCode]], {
        showModal(modalDialog(
            title="My code", size="l",fade=TRUE,
            footer=NULL, easyClose=TRUE,
            p("You can click anywhere in the code editor and select all the code using",
              "a keyboard shortcut that depends on your operating system (e.g. Ctrl/Cmd + A",
              "followed by Ctrl/Cmd + C).",
              "This will copy the selected parts to the clipboard."),

            aceEditor("report_all_cmds", mode="r", theme="solarized_light", autoComplete="live",
                value=paste0(.track_it_all(pObjects, se_name, ecm_name), collapse="\n"),
                height="600px")
        ))
    })

    observeEvent(input[[.generalPanelSettings]], {
        showModal(modalDialog(
            title="Panel settings", size="l", fade=TRUE,
            footer=NULL, easyClose=TRUE,
            aceEditor("acereport_r", mode="r", theme="solarized_light", autoComplete="live",
                value=paste(.report_memory(pObjects$memory), collapse="\n"),
                height="600px")
        ))
    })

    observeEvent(input[[.generalSessionInfo]], {
        showModal(modalDialog(
            title="Session information", size="l",fade=TRUE,
            footer=NULL, easyClose=TRUE,
            renderPrint({
                sessionInfo()
            })
        ))
    })

    observeEvent(input[[.generalCitationInfo]], {
        showModal(modalDialog(
            title="About iSEE", size="m", fade=TRUE,
            footer=NULL, easyClose=TRUE,
            tagList(
                iSEE_info, br(), br(),
                HTML("If you use this package, please use the following citation information:"),
                renderPrint({
                    citation("iSEE")
                })
            )
        ))
    })

    observeEvent(input[[.generalLinkGraph]], {
        showModal(modalDialog(
            title="Graph of inter-panel links", size="l",
            fade=TRUE, footer=NULL, easyClose=TRUE,
            renderPlot({
                .snapshot_graph_linkedpanels(pObjects$selection_links,
                    vapply(pObjects$memory, .getPanelColor, ""))
            })
        ))
    })

    observeEvent(input[[.generalExportOutput]], {
        showModal(modalDialog(
            title="Download panel contents", size="s",
            fade=TRUE, footer=NULL, easyClose=TRUE,
            downloadButton(.generalExportOutputAll, "All panels")
        ))
    })

    if (runLocal) {
        observeEvent(input[[.generalTourSteps]], {
            path <- system.file("doc", "basic.html", package="iSEE")
            if (path=="") {
                showNotification("vignette has not been built on this system", type="error")
            } else {
                browseURL(path)
            }
        })
    }

    invisible(NULL)
}

#nocov end
