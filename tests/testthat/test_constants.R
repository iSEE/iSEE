

test_that(".encode_panel_name can convert plot names", {
  
  array_length <- 5
  
  ## redDim
  
  encoded_redDim <- iSEE:::.encode_panel_name(
    paste("Reduced dimension plot", seq_len(array_length))
  )
  
  expected_type <- rep("redDimPlot", array_length)
  names(expected_type) <- rep("Reduced dimension plot", array_length)
  expected_ID <- seq_len(array_length)
  expected_out <- list(Type = expected_type, ID = expected_ID)
  
  expect_identical(encoded_redDim, expected_out)
  
  ## colData
  
  encoded_colData <- iSEE:::.encode_panel_name(
    paste("Column data plot", seq_len(array_length))
  )
  
  expected_type <- rep("colDataPlot", array_length)
  names(expected_type) <- rep("Column data plot", array_length)
  expected_ID <- seq_len(array_length)
  expected_out <- list(Type = expected_type, ID = expected_ID)
  
  expect_identical(encoded_colData, expected_out)
  
  ## featExpr
  
  encoded_featExpr <- iSEE:::.encode_panel_name(
    paste("Feature expression plot", seq_len(array_length))
  )
  
  expected_type <- rep("featExprPlot", array_length)
  names(expected_type) <- rep("Feature expression plot", array_length)
  expected_ID <- seq_len(array_length)
  expected_out <- list(Type = expected_type, ID = expected_ID)
  
  expect_identical(encoded_featExpr, expected_out)
  
})

test_that(".encode_panel_name catches invalid inputs", {
  
  expect_error(
    iSEE:::.encode_panel_name("invalid plot name"),
    "is not a legal panel name"
  )
  
})

test_that(".decode_panel_name works as expected", {
    expect_identical(iSEE:::.decode_panel_name("featExprPlot", 1), "Feature expression plot 1")
    expect_identical(iSEE:::.decode_panel_name("redDimPlot", 2), "Reduced dimension plot 2")
    expect_identical(iSEE:::.decode_panel_name("colDataPlot", 3), "Column data plot 3")
    expect_identical(iSEE:::.decode_panel_name("rowStatTable", 4), "Row statistics table 4")

    # Works on vectors.
    expect_identical(iSEE:::.decode_panel_name(c("featExprPlot", "redDimPlot"), 1:2), 
                     c("Feature expression plot 1", "Reduced dimension plot 2"))
    expect_identical(iSEE:::.decode_panel_name(character(0), integer(0)), character(0))
})

test_that(".decoded2encoded works as expected", {
    expect_identical(iSEE:::.decoded2encoded("Feature expression plot 1"), "featExprPlot1")
    expect_identical(iSEE:::.decoded2encoded(c("Feature expression plot 1", "Reduced dimension plot 1")), 
                                             c("featExprPlot1", "redDimPlot1"))
    expect_identical(iSEE:::.decoded2encoded(character(0)), character(0))
    expect_error(iSEE:::.decoded2encoded(c("Feature expression plot 1", "")),
                 "'' is not a legal panel name")
    # expect_identical(iSEE:::.decoded2encoded(c("Feature expression plot 1", "")), 
    #                                          c("featExprPlot1", ""))
})

test_that(".split_encoded works as expected", {
    expect_identical(iSEE:::.split_encoded("featExprPlot1"), list(Type="featExprPlot", ID=1L))
    expect_identical(iSEE:::.split_encoded(c("featExprPlot1", "redDimPlot1")),
                     list(Type=c("featExprPlot", "redDimPlot"), ID=c(1L, 1L)))
    expect_identical(iSEE:::.split_encoded(character(0)), list(Type=character(0), ID=integer(0)))
})

