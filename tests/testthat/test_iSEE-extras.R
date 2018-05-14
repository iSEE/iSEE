
test_that("nested DataFrame are extracted correctly", {

    df_info <- iSEE:::.extract_nested_DF(colData(sce))
    
    nested_colnames <- colnames(colData(sce)[,"nested"])
    
    expected_getters <- sprintf("[[\"nested\"]][[\"%s\"]]", nested_colnames)
    expected_setters <- sprintf("nested:%s", nested_colnames)
    
    expect_identical(df_info$getter, expected_getters)
    expect_identical(df_info$setter, expected_setters)
}) 
