## install script for R pkgs
source("https://bioconductor.org/biocLite.R")

library(BiocInstaller) # shouldn't be necessary
biocLite()


pkgs <- c(
  "AnnotationDbi",
  "BiocGenerics", 
  "BiocStyle",
  "colourpicker",
  "DT",
  "devtools",
  "ggplot2",
  "knitr",
  "igraph",
  "irlba",
  "RColorBrewer",
  "rentrez",
  "rintrojs", 
  "rmarkdown",
  "Rtsne",
  "S4Vectors",
  "scRNAseq",
  "scater",
  "SingleCellExperiment",
  "shiny",
  "shinydashboard",
  "shinyAce",
  "shinyjs",
  "SummarizedExperiment",
  "testthat",
  "utils",
  "vipor",
  "viridis"
)


ap.db <- available.packages(contrib.url(biocinstallRepos()))
ap <- rownames(ap.db)

pkgs_to_install <- pkgs[pkgs %in% ap]

biocLite(pkgs_to_install)

devtools::install_github("csoneson/iSEE")

## just in case there were warnings, we want to see them
## without having to scroll up:
warnings()

if (!is.null(warnings()))
{
  w <- capture.output(warnings())
  if (length(grep("is not available|had non-zero exit status", w)))
    quit("no", 1L)
}

suppressWarnings(BiocInstaller::biocValid(fix=TRUE, ask=FALSE))

