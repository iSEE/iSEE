
test_that("nested DataFrame are extracted correctly", {

    df_info <- iSEE:::.extract_nested_DF(colData(sce))
    
    nested_colnames <- colnames(colData(sce)[,"nested"])
    
    expected_getters <- sprintf("[[\"nested\"]][[\"%s\"]]", nested_colnames)
    expected_setters <- sprintf("nested:%s", nested_colnames)
    
    expect_identical(df_info$getter, expected_getters)
    expect_identical(df_info$setter, expected_setters)
})

test_that(".sanitize_SE_input returns expected commands and object", {
    colData(sce)$nested <- DataFrame(nested=1:ncol(sce))
    rowData(sce)$nested <- DataFrame(nested=1:nrow(sce))
    sizeFactors(sce) <- runif(ncol(sce))
    sizeFactors(sce, "ERCC") <- runif(ncol(sce))
    isSpike(sce, "ERCC") <- sample(nrow(sce), 10)

    sanitized_list <- iSEE:::.sanitize_SE_input(sce)
    sanitized_cmds <- sanitized_list$cmds
    sanitized_sce <- sanitized_list$object

    # Checking that the extracted values are correct.
    expect_identical(colData(sanitized_sce)[["nested:nested"]], sce$nested$nested)
    expect_identical(rowData(sanitized_sce)[["nested:nested"]], rowData(sce)$nested$nested)
    expect_identical(colData(sanitized_sce)[["sizeFactors(se)"]], sizeFactors(sce))
    expect_identical(colData(sanitized_sce)[['sizeFactors(se, "ERCC")']], sizeFactors(sce, "ERCC"))
    expect_identical(rowData(sanitized_sce)[['isSpike(se, "ERCC")']], isSpike(sce, "ERCC"))
    
    # emulate the function behaviour to obtain the expected
    eval_env <- new.env()
    eval_env$se <- sce
    eval(parse(text=sanitized_cmds), envir=eval_env)
    expected_se <- eval_env$se
    
    for (f in colnames(colData(expected_se))) {
        cur_field <- colData(expected_se)[[f]]
        if (!is.numeric(cur_field) && !is.factor(cur_field) 
            && !is.character(cur_field) && !is.logical(cur_field)) {
            colData(expected_se)[[f]] <- NULL
        }
    }

    for (f in colnames(rowData(expected_se))) {
        cur_field <- rowData(expected_se)[[f]]
        if (!is.numeric(cur_field) && !is.factor(cur_field) 
            && !is.character(cur_field) && !is.logical(cur_field)) {
            rowData(expected_se)[[f]] <- NULL
        }
    }

    expect_identical(sanitized_sce, expected_se)
}) 
