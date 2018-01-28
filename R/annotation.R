.generate_annotation <- function(annot.orgdb, annot.keytype, annot.keyfield, gene_data, chosen_gene)
#  A function to generate a HTML containing more information about a selected gene.
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
