collapseBox <- function(id, title, ..., open = FALSE, style = NULL)
# A custom collapsible box, more or less stolen from shinyBS.
{
  if(is.null(style)) {
    style <- "default"
  }

  sub.id <- paste0("collapse_", id)
  bsTag <- tags$div(class = paste0("isee-collapse-box panel panel-", style), 
                    id=id, 
                    value = title,
                    tags$div(class = "panel-heading", 
                             role = "tab", 
                             id = paste0("heading_", sub.id),
                             tags$h4(class = "panel-title",
                                     shiny::tags$a("data-toggle" = "collapse", 
                                                   href = paste0("#", sub.id), 
                                                   title)
                                     )
                             ),
                    tags$div(id = sub.id, 
                             class = sprintf("panel-collapse %s", ifelse(open, "collapse in", "collapse")),
                             role = "tabpanel",
                             tags$div(class = "panel-body", list(...))
                             )
                    )

#  bsTag
  tagList(singleton(tags$head(tags$script(src = "iSEE/collapseBox.js"))), bsTag)
}  

