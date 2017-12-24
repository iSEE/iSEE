iSEE_footer <- function(){
  tags$div(
    class = "panel-footer",
    style = "text-align:center",
    tags$div(
      class = "foot-inner",
      list(
        # hr(),
        "iSEE is a project developed by
        Aaron Lun (CRUK Cambridge Institute, University of Cambridge),
        Charlotte Soneson (University of Zurich and SIB Swiss Institute of Bioinformatics),
        Kevin Rue-Albrecht (",
        tags$a(
            href="https://www.kennedy.ox.ac.uk",
            "Kennedy Institute of Rheumatology, University of Oxford"
        ),
        "), and Federico Marini in the Bioinformatics division of the ",
        tags$a(href="http://www.unimedizin-mainz.de/imbei","IMBEI"),
        "- Institute for Medical Biostatistics, Epidemiology and Informatics",br(),
        "License: ",tags$a(href="https://opensource.org/licenses/MIT","MIT"), br(),

        "Development of the iSEE package is on ",
        tags$a(href="https://github.com/csoneson/iSEE", "GitHub")
      )
    )
  )
}
