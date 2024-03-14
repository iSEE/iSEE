stopifnot(
  require(scRNAseq),
  require(scater)
)

# Example data ----
sce <- ReprocessedAllenData(assays = "tophat_counts", legacy = TRUE)

sce <- logNormCounts(sce, exprs_values="tophat_counts")

ro <- order(rowVars(assay(sce, "logcounts")), decreasing = TRUE)
sce <- sce[head(ro, 100),]

sce <- runPCA(sce)
sce <- runTSNE(sce)

rowData(sce)$num_cells <- rowSums(assay(sce, "tophat_counts") > 0)
rowData(sce)$mean_count <- rowMeans(assay(sce, "tophat_counts"))
# Add a groupable field in rowData
rowData(sce)$letters <- factor(sample(letters[1:3], nrow(sce), TRUE))

sizeFactors(sce)  <- runif(ncol(sce))

colData(sce)[["nested"]] <- DataFrame(
        nested1 = runif(ncol(sce)),
        nested2 = sample(letters, ncol(sce), TRUE), row.names = colnames(sce)
    )

assay(sce, "letters") <- matrix(sample(letters[1:3], prod(dim(sce)), TRUE), nrow = nrow(sce), ncol = ncol(sce), dimnames = list(rownames(sce), colnames(sce)))
