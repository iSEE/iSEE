library(iSEE)
library(scRNAseq)
data(allen)
class(allen)

# Example data ----

library(scater)
sce <- as(allen, "SingleCellExperiment")
counts(sce) <- assay(sce, "tophat_counts")
sce <- normalize(sce)

sce <- runPCA(sce, ncomponents=4)
sce <- runTSNE(sce)
rowData(sce)$ave_count <- rowMeans(counts(sce))
rowData(sce)$n_cells <- rowSums(counts(sce)>0)
sce

# launch the app itself ----

initialPanels <- DataFrame(
    Name=c("Reduced dimension plot 1", "Column data plot 1"),
    Width=c(4, 4)
)

app <- iSEE(sce, initialPanels = initialPanels, voice=TRUE)
if (interactive()) {
    shiny::runApp(app, port=1234, launch.browser = FALSE)
}

# devtools::reload(); app <- iSEE(sce, initialPanels = initialPanels, voice=TRUE); shiny::runApp(app, port=1234, launch.browser = FALSE)