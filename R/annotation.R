#' Retrieve info on a gene
#' 
#' Generate HTML content with more information about a selected gene.
#'
#' @param annot.orgdb The OrgDb object
#' @param annot.keytype The keytype that matches the keys used
#' @param annot.keyfield The field to use as column from the \code{gene_data}
#' @param gene_data The data frame containing info on the genes/features of interest,
#' such as the one generated via \code{as.data.frame(rowData(se))}
#' @param chosen_gene The selected gene
#' 
#' @details This function relies on the code{entrez_summary} function, which 
#' requires internet access to retrieve updated information. Conversion of the 
#' ids is handled via OrgDb packages
#'
#' @return HTML content with more information about a selected gene
#' @author Federico Marini
#' @rdname INTERNAL_generate_annotation
.generate_annotation <- function(annot.orgdb, annot.keytype, annot.keyfield, gene_data, chosen_gene)
{
  if (is.null(annot.orgdb)) {
    return(HTML(""))
  }

  shiny::validate(
    need(!is.null(chosen_gene), "Select a gene from the table")
  )

  if (is.null(annot.keyfield)) {
    selectedGene <- rownames(gene_data)[chosen_gene]
  } else {
    selectedGene <- gene_data[chosen_gene,annot.keyfield]
  }

  if (annot.keytype!="ENTREZID") {
    selgene_entrez <- mapIds(annot.orgdb, selectedGene, "ENTREZID", annot.keytype)
  } else {
    selgene_entrez <- selectedGene
  }

  fullinfo <- entrez_summary("gene", selgene_entrez)
  link_pubmed <- paste0('<a href="http://www.ncbi.nlm.nih.gov/gene/?term=',
                        selgene_entrez,
                        '" target="_blank" >Click here to see more at NCBI</a>')

  if(fullinfo$summary == "") {
    return(HTML(paste0("<b>",fullinfo$name, "</b><br/><br/>",
                       fullinfo$description,"<br/><br/>",
                       link_pubmed
    )))
  } else {
    return(HTML(paste0("<b>",fullinfo$name, "</b><br/><br/>",
                       fullinfo$description, "<br/><br/>",
                       fullinfo$summary, "<br/><br/>",
                       link_pubmed
    )))
  }
}
