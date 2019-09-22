context("ExperimentColorMap")

# Validity method ----

test_that("validity method recognizes valid objects", {

    # use `new` to bypass
    ecm <- ExperimentColorMap()

    expect_true(iSEE:::.valid.Colormap(ecm))

})

test_that("validity method catches colormaps that are not functions", {

    # use `new` to bypass
    ecm <- ExperimentColorMap()

    ecm@assays <- list(dummy1=1)
    msg <- iSEE:::.valid.Colormap(ecm)

    expect_match(msg, "Colormap `dummy1` in slot `assays` is not a function")

})

test_that("validity method catches unnamed colormaps", {

    ecm <- ExperimentColorMap()

    ecm@colData <- list(
        function(x){NULL},
        a=function(){NULL})
    msg <- iSEE:::.valid.Colormap(ecm)

    expect_match(msg, "Colormap #1 in slot `colData` must be named", fixed=TRUE)

})

test_that("validity method catches colormaps with controlled names", {

    ecm <- ExperimentColorMap()

    ecm@all_discrete <- list(
        wrong=function(x){NULL},
        again=function(){NULL})
    msg <- iSEE:::.valid.Colormap(ecm)

    expect_match(
        msg,
        "Colormap in slot `all_discrete` must be named c(\"assays\", \"colData\", \"rowData\")",
        fixed=TRUE)

})

# Constructors ----

test_that("Constructor produce a valid object",{

    ecm <- ExperimentColorMap(
        assays=list(
            counts=COUNT_COLORS,
            tophat_counts=COUNT_COLORS,
            cufflinks_fpkm=FPKM_COLORS,
            cufflinks_fpkm=FPKM_COLORS,
            rsem_tpm=TPM_COLORS
        ),
        colData=list(
            passes_qc_checks_s=QC_COLOR_FUN
        ),
        global_continuous=ASSAY_CONTINUOUS_COLORS
    )

    expect_s4_class(
        ecm,
        "ExperimentColorMap"
    )

})

test_that("Constructor catches unnamed colormaps",{

    expect_error(
        ExperimentColorMap(
            colData=list(function(x) {NULL}),
            rowData=list(function(x) {NULL}),
            all_discrete=list( function(x) {NULL} )
            ),
        "User-defined colormap must be a named list",
        fixed=TRUE
    )

})

# show method ----

test_that("show method displays expected content",{

    ecm <- ExperimentColorMap(
        assays=list(
            counts=COUNT_COLORS,
            tophat_counts=COUNT_COLORS,
            cufflinks_fpkm=FPKM_COLORS,
            cufflinks_fpkm=FPKM_COLORS,
            rsem_tpm=TPM_COLORS
        ),
        colData=list(
            passes_qc_checks_s=QC_COLOR_FUN
        ),
        global_continuous=ASSAY_CONTINUOUS_COLORS,
        global_discrete=iSEE:::.defaultDiscreteColorMap
    )

    expect_null(show(ecm))

    expect_output(show(ecm), "ExperimentColorMap")

})

# assays ----

test_that("assays returns appropriate values",{

    ecm <- ExperimentColorMap(
        assays=list(
            counts=COUNT_COLORS
        ),
        global_continuous=ASSAY_CONTINUOUS_COLORS
    )

    expect_identical(
        assays(ecm),
        ecm@assays
    )

})

test_that("assays<- sets appropriate values",{

    ecm <- ExperimentColorMap(
        assays=list(
            counts=COUNT_COLORS
        ),
        global_continuous=ASSAY_CONTINUOUS_COLORS
    )

    new_value <- list()
    assays(ecm) <- new_value

    expect_identical(
        assays(ecm),
        new_value
    )

})

# colData ----

test_that("colData returns appropriate values",{

    ecm <- ExperimentColorMap(
        colData=list(
            passes_qc_checks_s=QC_COLOR_FUN
        )
    )

    expect_identical(
        colData(ecm),
        ecm@colData
    )

})

test_that("colData<- sets appropriate values",{

    ecm <- ExperimentColorMap(
        colData=list(
            passes_qc_checks_s=QC_COLOR_FUN
        )
    )

    new_value <- list(
        new_coldata <- function(n){return("blue")}
    )
    colData(ecm) <- new_value

    expect_identical(
        colData(ecm),
        new_value
    )

})

# rowData ----

test_that("rowData returns appropriate values",{

    ecm <- ExperimentColorMap(
        rowData=list(
            passes_qc_checks_s=QC_COLOR_FUN
        )
    )

    expect_identical(
        rowData(ecm),
        ecm@rowData
    )

})

test_that("rowData<- sets appropriate values",{

    logical_colormap <- function(n){
        logical_colors <- c("forestgreen", "firebrick1")
        names(logical_colors) <- c("TRUE", "FALSE")
        return(logical_colors)
    }

    ecm <- ExperimentColorMap(
        rowData=list(
            is_MT=logical_colormap
        )
    )

    new_value <- list(
        new_rowData=function(n){return("blue")}
    )
    rowData(ecm) <- new_value

    expect_identical(
        rowData(ecm),
        new_value
    )

})

# assay ----

test_that("assay returns appropriate values",{

    ecm <- ExperimentColorMap(
        assays=list(
            counts=COUNT_COLORS
        ),
        global_continuous=ASSAY_CONTINUOUS_COLORS
    )

    # character
    expect_identical(
        assay(ecm, "counts"),
        ecm@assays$counts
    )

    # numeric
    expect_identical(
        assay(ecm, 1),
        ecm@assays[[1]]
    )

})

test_that("assay<- sets appropriate values with character indexing",{

    ecm <- ExperimentColorMap(
        assays=list(
            counts=COUNT_COLORS
        ),
        global_continuous=ASSAY_CONTINUOUS_COLORS
    )

    new_value <- function(n){return("red")}
    assay(ecm, "counts") <- new_value

    # character
    expect_identical(
        assay(ecm, "counts"),
        new_value
    )

})

# assayNames ----

test_that("assayNames returns appropriate values",{

    ecm <- ExperimentColorMap(
        assays=list(
            counts=COUNT_COLORS
        ),
        global_continuous=ASSAY_CONTINUOUS_COLORS
    )

    # character
    expect_identical(
        assayNames(ecm),
        "counts"
    )

})

test_that("assayNames<- sets appropriate values",{

    ecm <- ExperimentColorMap(
        assays=list(
            counts=COUNT_COLORS
        ),
        global_continuous=ASSAY_CONTINUOUS_COLORS
    )

    new_value <- "logcounts"
    assayNames(ecm) <- new_value

    # character
    expect_identical(assayNames(ecm), new_value)

})

test_that("assay<- sets appropriate values with numeric indexing",{

    ecm <- ExperimentColorMap(
        assays=list(
            counts=COUNT_COLORS
        ),
        global_continuous=ASSAY_CONTINUOUS_COLORS
    )

    new_value <- function(n){return("red")}
    assay(ecm, 1) <- new_value

    # character
    expect_identical(
        assay(ecm, 1),
        new_value
    )

})

# assayColorMap ----

test_that("assayColorMap returns appropriate values",{

    ecm <- ExperimentColorMap(
        assays=list(
            counts=COUNT_COLORS
        ),
        global_continuous=ASSAY_CONTINUOUS_COLORS
    )

    # specific
    expect_equal(
        assayColorMap(ecm, "counts")(21L),
        COUNT_COLORS(21L)
    )

    # specific > (continuous) all > global
    expect_equal(
        assayColorMap(ecm, "undefined", discrete=FALSE)(21L),
        ASSAY_CONTINUOUS_COLORS(21L)
    )

})

test_that(".assayAllColorMap returns the appropriate values", {

    # Non-NULL
    ecm <- ExperimentColorMap(all_discrete=list(
        assays=COUNT_COLORS
    ))

    out <- iSEE:::.assayAllColorMap(ecm, discrete=TRUE)
    expect_identical(out, COUNT_COLORS)

})

test_that("assay<- sets appropriate values with character indexing",{

    ecm <- ExperimentColorMap(
        assays=list(
            counts=COUNT_COLORS
        ),
        global_continuous=ASSAY_CONTINUOUS_COLORS
    )

    new_value <- function(n){return("red")}
    assay(ecm, "counts") <- new_value

    # character
    expect_identical(
        assay(ecm, "counts"),
        new_value
    )

})

test_that("assayColorMap<- sets appropriate values with character indexing",{

    ecm <- ExperimentColorMap(
        assays=list(
            counts=COUNT_COLORS
        ),
        global_continuous=ASSAY_CONTINUOUS_COLORS
    )

    new_value <- function(n){return("red")}
    assayColorMap(ecm, "counts") <- new_value

    # character
    expect_identical(
        assay(ecm, "counts"),
        new_value
    )

})

test_that("assay<- sets appropriate values with numeric indexing",{

    ecm <- ExperimentColorMap(
        assays=list(
            counts=COUNT_COLORS
        ),
        global_continuous=ASSAY_CONTINUOUS_COLORS
    )

    new_value <- function(n){return("red")}
    assay(ecm, 1) <- new_value

    # character
    expect_identical(
        assay(ecm, 1),
        new_value
    )

})

test_that("assayColorMap<- sets appropriate values with numeric indexing",{

    ecm <- ExperimentColorMap(
        assays=list(
            counts=COUNT_COLORS
        ),
        global_continuous=ASSAY_CONTINUOUS_COLORS
    )

    new_value <- function(n){return("red")}
    assayColorMap(ecm, 1) <- new_value

    # character
    expect_identical(
        assay(ecm, 1),
        new_value
    )

})

# colDataColorMap ----

test_that("colDataColorMap returns appropriate values",{

    ecm <- ExperimentColorMap(
        assays=list(
            counts=COUNT_COLORS
        ),
        global_continuous=ASSAY_CONTINUOUS_COLORS
    )

    # specific > (discrete) all > global > .defaultDiscreteColorMap
    expect_identical(
        colDataColorMap(ecm, "test", discrete=TRUE)(21L),
        .defaultDiscreteColorMap(21L)
    )

    # specific > (continuous) all > global
    expect_identical(
        colDataColorMap(ecm, "test", discrete=FALSE)(21L),
        ASSAY_CONTINUOUS_COLORS(21L)
    )

})

test_that(".colDataAllColorMap returns appropriate values",{

    ecm <- ExperimentColorMap(
        all_continuous=list(colData=QC_COLOR_FUN)
    )

    # specific > (continuous) all > global
    expect_identical(
        colDataColorMap(ecm, "test", discrete=FALSE),
        QC_COLOR_FUN
    )

})

test_that("colDataColorMap<- sets appropriate values with character indexing",{

    ecm <- ExperimentColorMap(
        colData=list(
            passes_qc_checks_s=QC_COLOR_FUN
        )
    )

    new_value <- function(n){return("red")}
    colDataColorMap(ecm, "passes_qc_checks_s") <- new_value

    # character
    expect_identical(
        colDataColorMap(ecm, "passes_qc_checks_s"),
        new_value
    )

})

# rowDataColorMap ----

test_that("rowDataColorMap returns appropriate values",{

    ecm <- ExperimentColorMap(
        assays=list(
            counts=COUNT_COLORS
        ),
        global_continuous=ASSAY_CONTINUOUS_COLORS
    )

    # specific > (discrete) all > global > .defaultDiscreteColorMap
    expect_identical(
        rowDataColorMap(ecm, "test", discrete=TRUE)(21L),
        .defaultDiscreteColorMap(21L)
    )

    # specific > (continuous) all > global
    expect_identical(
        rowDataColorMap(ecm, "test", discrete=FALSE)(21L),
        ASSAY_CONTINUOUS_COLORS(21L)
    )

})

test_that("rowDataColorMap returns appropriate values",{

    ecm <- ExperimentColorMap(
        all_continuous=list(rowData=QC_COLOR_FUN)
    )

   # specific > (continuous) all > global
    expect_identical(
        rowDataColorMap(ecm, "test", discrete=FALSE),
        QC_COLOR_FUN
    )

})

test_that("rowDataColorMap<- sets appropriate values with character indexing",{

    ecm <- ExperimentColorMap(
        rowData=list(
            passes_qc_checks_s=QC_COLOR_FUN
        )
    )

    new_value <- function(n){return("red")}
    rowDataColorMap(ecm, "passes_qc_checks_s") <- new_value

    # character
    expect_identical(
        rowDataColorMap(ecm, "passes_qc_checks_s"),
        new_value
    )

})

# Validity method ----

test_that("Invalid objects are not allowed to be created", {

    # colormaps must be functions
    expect_error(
        ExperimentColorMap(assays=list(dummy1='a')),
        "not a function",
        fixed=TRUE
    )
    expect_error(
        ExperimentColorMap(colData=list(dummy2=NULL)),
        "not a function",
        fixed=TRUE
    )
    expect_error(
        ExperimentColorMap(rowData=list(dummy2=NULL)),
        "not a function",
        fixed=TRUE
    )

    # colData and rowData colormaps must be named
    expect_error(
        ExperimentColorMap(
            colData=list(
                dummy1=function(x){NULL},
                function(x){NULL} # unnamed
            )
        ),
        "must be named",
        fixed=TRUE
    )
    expect_error(
        ExperimentColorMap(
            rowData=list(
                dummy1=function(x){NULL},
                function(x){NULL} # unnamed
            )
        ),
        "must be named",
        fixed=TRUE
    )

    # all_* slots have specific names
    expect_error(
        ExperimentColorMap(
            all_discrete=list(a=function(x){NULL}),
            all_continuous=list(assays=NULL, b=NULL, rowData=NULL)
        )
    )

})

# checkColormapCompatibility (many assays) ----

test_that("checkColormapCompatibility catches too many assays colormaps", {

    ecm_manyAssays <- ExperimentColorMap(
        assays=list(
            counts=COUNT_COLORS,
            tophat_counts=COUNT_COLORS,
            cufflinks_fpkm=FPKM_COLORS,
            cufflinks_fpkm=FPKM_COLORS,
            rsem_tpm=TPM_COLORS,
            another=TPM_COLORS,
            yet_another=TPM_COLORS,
            last_one_i_promise=TPM_COLORS,
            oh_well=TPM_COLORS
        )
    )

    out <- checkColormapCompatibility(ecm_manyAssays, sce)

    expect_identical(
        checkColormapCompatibility(ecm_manyAssays, sce),
        c(
            "More assays in colormap (9) than experiment (2)",
            "assay `counts` in colormap missing in experiment",
            "assay `cufflinks_fpkm` in colormap missing in experiment",
            "assay `cufflinks_fpkm` in colormap missing in experiment",
            "assay `rsem_tpm` in colormap missing in experiment",
            "assay `another` in colormap missing in experiment",
            "assay `yet_another` in colormap missing in experiment",
            "assay `last_one_i_promise` in colormap missing in experiment",
            "assay `oh_well` in colormap missing in experiment")
    )

})

# checkColormapCompatibility (superfluous assays) ----

test_that("checkColormapCompatibility catches superfluous assays colormap", {

    nullECM <- ExperimentColorMap(
        assays=list(
            dummy1=function(x){NULL}
        )
    )

    out <- checkColormapCompatibility(nullECM, sce)
    expect_identical(out, "assay `dummy1` in colormap missing in experiment")

})

# checkColormapCompatibility (superfluous colData) ----

test_that("checkColormapCompatibility catches superfluous colData colormap", {

    missingColData <- ExperimentColorMap(
        colData=list(
            dummy2=function(x){NULL}
        )
    )

    out <- checkColormapCompatibility(missingColData, sce)
    expect_identical(out, "colData `dummy2` in colormap missing in experiment")

})

# checkColormapCompatibility (superfluous rowData) ----


test_that("checkColormapCompatibility catches superfluous rowData colormap", {

    missingRowData <- ExperimentColorMap(
        rowData=list(
            dummy2=function(x){NULL}
        )
    )

    out <- checkColormapCompatibility(missingRowData, sce)
    expect_identical(out, "rowData `dummy2` in colormap missing in experiment")

})

# checkColormapCompatibility (valid) ----

test_that("checkColormapCompatibility accepts compatible colormap", {

    ecm <- ExperimentColorMap(
        assays=list(
            tophat_counts=COUNT_COLORS
        ),
        global_continuous=ASSAY_CONTINUOUS_COLORS
    )

    out <- checkColormapCompatibility(ecm, sce)
    expect_null(out)

})

# synchronizeAssays ----

test_that("synchronizeAssays works for fully named assays", {

    ecm <- ExperimentColorMap(
        assays=list(
            counts=COUNT_COLORS,
            tophat_counts=COUNT_COLORS,
            cufflinks_fpkm=FPKM_COLORS,
            rsem_tpm=FPKM_COLORS,
            orphan=COUNT_COLORS,
            orphan2=COUNT_COLORS,
            COUNT_COLORS,
            TPM_COLORS
        )
    )

    ecm_expected <- ExperimentColorMap(
        assays=list(
            tophat_counts=COUNT_COLORS,
            logcounts=iSEE:::.defaultContinuousColorMap
        )
    )

    expect_warning(
        synchronizeAssays(ecm, sce),
        "Unused assays dropped from ecm"
    )

    ecm_sync <- synchronizeAssays(ecm, sce)

    expect_identical(
        ecm_sync,
        ecm_expected
    )

    # The returned ECM must have named in the same order as SCE
    expect_identical(
        assayNames(sce),
        assayNames(ecm_sync)
    )

})

test_that("synchronizeAssays requires same number of unnamed assays", {

    sce_unnamed <- sce
    assayNames(sce_unnamed) <- rep("", length(assays(sce_unnamed)))

    # Different number of un/named colormap
    ecm_unmatched <- ExperimentColorMap(
        assays=list(
            COUNT_COLORS,
            test=COUNT_COLORS,
            test2=COUNT_COLORS
        )
    )

    expect_error(
        synchronizeAssays(ecm_unmatched, sce_unnamed),
        "Cannot synchronize assays",
        fixed=TRUE
    )

})


test_that("synchronizeAssays works for fully _un_named assays", {

    sce_unnamed <- sce
    assayNames(sce_unnamed) <- rep("", length(assays(sce_unnamed)))

    # same number of un/named colormaps
    ecm_matched <- ExperimentColorMap(
        assays=list(
            COUNT_COLORS,
            FPKM_COLORS
        )
    )

    ecm_sync <- synchronizeAssays(ecm_matched, sce_unnamed)

    # Expect the input ExperimentColorMap returned as is
    expect_identical(
        ecm_sync,
        ecm_matched
    )

    # assayNames may differ, if the input ExperimentColorMap had names
    expect_identical(
        length(assayNames(sce_unnamed)),
        length(assayNames(ecm_sync))
    )

})


test_that("synchronizeAssays works for partially named assays", {

    sce_some_names <- sce # tophat_counts, logcounts [1, 2]
    counts(sce_some_names) <- assay(sce_some_names, "tophat_counts") # counts [3]
    assayNames(sce_some_names)[3] <- ""

    ecm <- ExperimentColorMap(
        assays=list(
            tophat_counts=COUNT_COLORS,
            cufflinks_fpkm=FPKM_COLORS, # missing colormap
            rsem_tpm=FPKM_COLORS, # missing colormap
            orphan=COUNT_COLORS, # missing colormap
            orphan2=COUNT_COLORS, # missing colormap
            COUNT_COLORS,
            TPM_COLORS
        )
    )

    ecm_sync <- synchronizeAssays(ecm, sce_some_names)

    ecm_expected <- ExperimentColorMap(
        assays=list(
            tophat_counts=COUNT_COLORS,
            logcounts=iSEE:::.defaultContinuousColorMap,
            iSEE:::.defaultContinuousColorMap
        )
    )

    # Expect:
    # - named assays matched to have the appropriate colormap (tophat_counts)
    # - named assays unmatched to have the default continuous colormap (logcounts)
    # - unnamed assays to be assigned default continuous colormap (3rd assay)
    expect_identical(
        ecm_sync,
        ecm_expected
    )

    # The returned ECM must have named in the same order as SCE
    expect_identical(
        assayNames(sce_some_names),
        assayNames(ecm_sync)
    )

})
