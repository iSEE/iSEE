.generalLinkGraph <- "iSEE_INTERNAL_link_graph"
.generalLinkGraphPlot <- "iSEE_INTERNAL_link_graph_plot"
.generalTrackedCode <- "iSEE_INTERNAL_tracked_code"

.generalPanelSettings <- "iSEE_INTERNAL_panel_settings"
.generalTourSteps <- "iSEE_INTERNAL_tour_steps"
.generalVignetteOpen <- "iSEE_INTERNAL_open_vignette"
.generalDraftTour <- "iSEE_INTERNAL_draft_tour"
.generalDraftEditor <- "iSEE_INTERNAL_editor_tour"

.generalSessionInfo <- "iSEE_INTERNAL_session_info"
.generalCitationInfo <- "iSEE_INTERNAL_citation_info"
.generalMetadataInfo <- "iSEE_INTERNAL_metadata_info"

.generalCodeTracker <- "iSEE_INTERNAL_tracked_code"
.generalMemoryTracker <- "iSEE_INTERNAL_tracked_memory"
.generalMemoryExport <- "iSEE_INTERNAL_memory_export"
.generalMemoryIncludeSE <- "iSEE_INTERNAL_memory_se"
.generalMemoryIncludeECM <- "iSEE_INTERNAL_memory_ecm"
.generalMemoryCustomSave <- "iSEE_INTERNAL_custom_save"

.generalExportOutput <- "iSEE_INTERNAL_export_content"
.generalExportOutputUI <- "iSEE_INTERNAL_export_content_ui"
.generalExportOutputDownload <- "iSEE_INTERNAL_export_content_download"
.generalExportOutputAll <- "iSEE_INTERNAL_export_content_all"
.generalExportOutputNone <- "iSEE_INTERNAL_export_content_none"
.generalExportOutputChoices <- "iSEE_INTERNAL_export_content_choices"

#nocov start

#' General observers for \code{\link{iSEE}}
#'
#' A function to set up observers for general (i.e., not panel-specific) observers used in the app.
#'
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param mod_commands A character vector of commands performed to modify \code{se} before running the app proper.
#' @inheritParams .initialize_server
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @importFrom utils read.delim sessionInfo citation browseURL capture.output
#' @importFrom shiny observeEvent showModal modalDialog HTML br tagList showNotification p pre downloadButton
#' checkboxInput actionButton
#' @importFrom shinyAce aceEditor
#' @importFrom listviewer jsoneditOutput
#'
#' @rdname INTERNAL_general_observers
.create_general_observers <- function(se, runLocal, se_name, ecm_name, mod_commands, saveState, input, session, pObjects, rObjects) {
    observeEvent(input[[.generalTrackedCode]], {
        all_cmds <- .track_it_all(pObjects, se_name, ecm_name, mod_commands)
        all_cmds <- paste(all_cmds, collapse="\n")

        showModal(modalDialog(
            title="My code", size="l",fade=TRUE,
            footer=NULL, easyClose=TRUE,
            p("You can click anywhere in the code editor and select all the code using",
              "a keyboard shortcut that depends on your operating system (e.g. Ctrl/Cmd + A",
              "followed by Ctrl/Cmd + C).",
              "This will copy the selected parts to the clipboard."),
            aceEditor(.generalCodeTracker, mode="r", theme="solarized_light", autoComplete="live",
                value=all_cmds, height="600px")
        ))
    }, ignoreInit=TRUE)

    observeEvent(input[[.generalPanelSettings]], {
        showModal(modalDialog(
            title="Panel settings", size="l", fade=TRUE,
            footer=NULL, easyClose=TRUE,
            checkboxInput(.generalMemoryIncludeSE, "include SummarizedExperiment", value=TRUE),
            checkboxInput(.generalMemoryIncludeECM, "include ExperimentColorMap", value=TRUE),
            downloadButton(.generalMemoryExport, "Download RDS"),
            if (!is.null(saveState)) {
                actionButton(.generalMemoryCustomSave, "Save application state")
            },
            br(), br(),
            aceEditor(.generalMemoryTracker, mode="r", theme="solarized_light", autoComplete="live",
                value=paste(.report_memory(pObjects$memory), collapse="\n"),
                height="600px")
        ))
    }, ignoreInit=TRUE)

    # Arbitrary function to save the content.
    if (!is.null(saveState)) {
        observeEvent(input[[.generalMemoryCustomSave]], {
            saveState(.gather_current_memory(se, input, pObjects))
        }, ignoreInit=TRUE)
    }

    observeEvent(input[[.generalSessionInfo]], {
        showModal(modalDialog(
            title="Session information", size="l",fade=TRUE,
            footer=NULL, easyClose=TRUE,
            pre(paste(capture.output(sessionInfo()), collapse="\n"))
        ))
    }, ignoreInit=TRUE)

    observeEvent(input[[.generalCitationInfo]], {
        showModal(modalDialog(
            title="About iSEE", size="m", fade=TRUE,
            footer=NULL, easyClose=TRUE,
            tagList(
                iSEE_info, br(), br(),
                HTML("If you use this package, please use the following citation information:"),
                pre(paste(capture.output(citation("iSEE")), collapse="\n"))
            )
        ))
    }, ignoreInit=TRUE)
    
    observeEvent(input[[.generalMetadataInfo]], {
        
        showModal(modalDialog(
            title="About this dataset", size="m", fade=TRUE,
            footer=NULL, easyClose=TRUE,
            
            HTML("You can see here a schematic representation of the metadata included",
                 "in the provided <code>SummarizedExperiment</code> object. <br><br>"),
            # paste(names(mdd), collapse = "\n"),
            tagList(
                listviewer::jsoneditOutput("mdd"),
                actionButton("export_to_json", 
                             label = "Export metadata to json")
            )
            
        ))
    }, ignoreInit=TRUE)
    
    # observeEvent(input[[export_to_json]], {
    #   
    # }, ignoreInit=TRUE)
    
    observeEvent(input[[.generalLinkGraph]], {
        showModal(modalDialog(
            title="Graph of inter-panel links", size="l",
            fade=TRUE, footer=NULL, easyClose=TRUE,
            plotOutput(.generalLinkGraphPlot)
        ))
    }, ignoreInit=TRUE)

    if (runLocal) {
        observeEvent(input[[.generalVignetteOpen]], {
            path <- system.file("doc", "basic.html", package="iSEE")
            if (path=="") {
                showNotification("vignette has not been built on this system", type="error")
            } else {
                browseURL(path)
            }
        }, ignoreInit=TRUE)
    }

    .create_export_observers(input, session, pObjects)

    invisible(NULL)
}

#' Tour observer for \code{\link{iSEE}}
#'
#' A function to set up the observers for the tour.
#'
#' @inheritParams .initialize_server
#' @param memory A list of \linkS4class{Panel} objects specifying the current memory of the app.
#'
#' @return An observer is created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun
#' @importFrom rintrojs introjs
#' @importFrom shiny observeEvent
#' @rdname INTERNAL_create_tour_observer
.create_tour_observer <- function(se, memory, tour, input, session) {
    observeEvent(input[[.generalTourSteps]], {
        if (is.null(tour)) {
            tour <- .assemble_tour(se, memory)
        }
        introjs(session, options=list(steps=tour))
    }, ignoreInit=TRUE)

    if (!is.null(tour)) {
        # Only triggers _after_ panels are fully setup, so observers are properly ID'd.
        session$onFlushed(function() { introjs(session, options=list(steps=tour)) })
    }
}


#' Draft a tour backbone
#' 
#' Create a draft of a tour based on the current panel set
#'
#' @inheritParams .initialize_server
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' 
#' @return An observer is created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#' 
#' @author Federico Marini
#' @rdname INTERNAL_create_tour_drafter
.create_tour_drafter <- function(se, input, pObjects) {
    observeEvent(input[[.generalDraftTour]], {
        tour_draft <- c("element;intro")

        enc_names <- .define_memory_panel_choices(pObjects$memory)
        tour_draft <- c(tour_draft, paste0("#", enc_names, ";TOUR_TEXT"))
    
        showModal(modalDialog(
            title="This is a draft of the tour based on the current panel selection", size="m",
            fade=TRUE, footer=NULL, easyClose=TRUE,
            p("You can click anywhere in the code editor and select all the code using",
              "a keyboard shortcut that depends on your operating system (e.g., Ctrl/Cmd + A",
              "followed by Ctrl/Cmd + C).",
              "This will copy the selected parts to the clipboard.",
              "While this is not guaranteed to be a fully fledged tour, it will give you",
              "some pointers to work on the panels that you are likely to be mentioning",
              "in the tour for your instance of iSEE.",
              "The content of this editor should ideally reside in a text file that is read",
              "in and stored into a data.frame.",
              "For more details, refer to the section",
              a("Writing your own tour", href = "https://isee.github.io/iSEE/articles/configure.html#writing-your-own-tour"),
              "in our vignette 'Configuring iSEE apps'."),
            aceEditor(.generalDraftEditor, mode="r", theme="solarized_light", autoComplete="live",
                      value=tour_draft, height="300px")
        ))
    }, ignoreInit=TRUE)
    
    invisible(NULL)
}

#' Create the export observers
#'
#' Create observers that are dedicated to exporting panel content (e.g., as PDFs or CSVs).
#'
#' @inheritParams .create_general_observers
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @rdname INTERNAL_export_observers
#' @importFrom shiny observeEvent showModal modalDialog updateCheckboxGroupInput uiOutput
.create_export_observers <- function(input, session, pObjects) {
    observeEvent(input[[.generalExportOutput]], {
        showModal(modalDialog(
            title="Download panel contents", size="m",
            fade=TRUE, footer=NULL, easyClose=TRUE,
            uiOutput(.generalExportOutputUI)
        ))
    }, ignoreInit=TRUE)

    observeEvent(input[[.generalExportOutputAll]], {
        all_options <- .define_export_choices(pObjects$memory)
        updateCheckboxGroupInput(session, .generalExportOutputChoices,
            choices=all_options, selected=all_options)
    }, ignoreInit=TRUE)

    observeEvent(input[[.generalExportOutputNone]], {
        all_options <- .define_export_choices(pObjects$memory)
        updateCheckboxGroupInput(session, .generalExportOutputChoices, selected=character(0))
    }, ignoreInit=TRUE)

    invisible(NULL)
}

#nocov end

#' Gather current memory state
#'
#' Gather the bits and pieces necessary to describe the current state of the application.
#'
#' @inheritParams .create_general_observers
#'
#' @return A list containing \code{memory}, the list of \linkS4class{Panel} describing the current app state;
#' and possibly \code{se} and \code{colormap}, depending on whether their respective options are checked.
#'
#' @author Aaron Lun
#'
#' @rdname INTERNAL_gather_current_memory
#' @importFrom S4Vectors metadata
.gather_current_memory <- function(se, input, pObjects) {
    args <- list(memory=pObjects$memory)
    if (input[[.generalMemoryIncludeSE]]) {
        args$se <- se
    }
    if (input[[.generalMemoryIncludeECM]]) {
        args$colormap <- metadata(se)$colormap
    }
    args
}
