stopifnot(
  require(scRNAseq),
  require(scater)
)
data(allen)

# Example data ----

sce <- as(allen, "SingleCellExperiment")
counts(sce) <- assay(sce, "tophat_counts")
sce <- normalize(sce)
sce <- runPCA(sce)
sce <- runTSNE(sce)

rowData(sce)$num_cells <- rowSums(counts(sce)>0)
rowData(sce)$mean_count <- rowMeans(counts(sce))

isSpike(sce, "random") <- sample(c(TRUE, FALSE), nrow(sce), replace = TRUE, prob = c(0.01, 0.99))
sizeFactors(sce, "ERCC")  <- runif(ncol(sce))
