
test_that("nested DataFrame are extracted correctly", {

    df_info <- iSEE:::.extract_nested_DF(colData(sce))
    
    nested_colnames <- colnames(colData(sce)[,"nested"])
    
    expected_getters <- sprintf("[[\"nested\"]][[\"%s\"]]", nested_colnames)
    expected_setters <- sprintf("nested:%s", nested_colnames)
    
    expect_identical(df_info$getter, expected_getters)
    expect_identical(df_info$setter, expected_setters)
})

test_that(".sanitize_SE_input returns expected commands and object", {

    sanitized_list <- iSEE:::.sanitize_SE_input(sce)
    sanitized_cmds <- sanitized_list$cmds
    sanitized_sce <- sanitized_list$object
    
    # emulate the function behaviour to obtain the expected
    eval_env <- new.env()
    eval_env$se <- sce
    eval(parse(text=sanitized_cmds), envir=eval_env)
    
    expected_se <- eval_env$se
    
    for (f in colnames(colData(expected_se))) {
        cur_field <- colData(tmp_se)[[f]]
        if (!is.numeric(cur_field) & !is.factor(cur_field) 
            & !is.character(cur_field) & !is.logical(cur_field)) {
            colData(expected_se)[[f]] <- NULL
        }
    }
    
    # fails!!! does not add the last field?
    expect_identical(sanitized_sce, expected_se)
    
    # emulate the invalid sce produced: this should fail!
    colData(expected_se)[,"nested:nested2"] <- NULL
    expect_identical(sanitized_sce, expected_se)
}) 


