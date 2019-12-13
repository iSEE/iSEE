local({
  txt_input = function(..., width = '100%') shiny::textInput(..., width = width)
  sel_input = function(...) shiny::selectizeInput(
    ..., width = '98%', multiple = FALSE, options = list(create = TRUE)
  )
  parent_choices = sort(iSEE:::collect_parents())
  shiny::runGadget(
    miniUI::miniPage(miniUI::miniContentPanel(
      txt_input('encoded', 'Class encoded name', placeholder = 'MyNewPlot'),
      txt_input('decoded', 'Class decoded name', placeholder = 'My New Plot'),
      shiny::fillRow(
        sel_input('parentclass', 'Parent class', parent_choices, selected="Panel"),
        height = '70px'
      ),
      miniUI::gadgetTitleBar(NULL)
    )),
    server = function(input, output, session) {

      shiny::observeEvent(input$done, {
        encoded <- input$encoded
        decoded <- input$decoded
        parentclass <- input$parentclass

        if (grepl('^\\s*$', encoded)) return(
          warning('The decoded class name is empty!', call. = FALSE)
        )
        if (grepl('^\\s*$', decoded)) return(
          warning('The encoded class name is empty!', call. = FALSE)
        )
        if (grepl('^\\s*$', parentclass)) return(
          warning('The parent class name is empty!', call. = FALSE)
        )

        iSEE:::new_panel_file(encoded, decoded, parentclass)
        shiny::stopApp()
      })

      shiny::observeEvent(input$cancel, {
        shiny::stopApp()
      })
    },
    stopOnCancel = FALSE, viewer = shiny::dialogViewer('New Post', height = 500)
  )
})
