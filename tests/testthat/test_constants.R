

test_that(".encode_panel_name can convert plot names", {
  
  array_length <- 5
  
  ## redDim
  
  encoded_redDim <- iSEE:::.encode_panel_name(
    paste("Reduced dimension plot", seq_len(array_length))
  )
  
  expected_type <- rep("redDim", array_length)
  names(expected_type) <- rep("Reduced dimension plot", array_length)
  expected_ID <- seq_len(array_length)
  expected_out <- list(Type = expected_type, ID = expected_ID)
  
  expect_identical(encoded_redDim, expected_out)
  
  ## colData
  
  encoded_colData <- iSEE:::.encode_panel_name(
    paste("Column data plot", seq_len(array_length))
  )
  
  expected_type <- rep("colData", array_length)
  names(expected_type) <- rep("Column data plot", array_length)
  expected_ID <- seq_len(array_length)
  expected_out <- list(Type = expected_type, ID = expected_ID)
  
  expect_identical(encoded_colData, expected_out)
  
  ## geneExpr
  
  encoded_geneExpr <- iSEE:::.encode_panel_name(
    paste("Gene expression plot", seq_len(array_length))
  )
  
  expected_type <- rep("geneExpr", array_length)
  names(expected_type) <- rep("Gene expression plot", array_length)
  expected_ID <- seq_len(array_length)
  expected_out <- list(Type = expected_type, ID = expected_ID)
  
  expect_identical(encoded_geneExpr, expected_out)
  
})

test_that(".encode_panel_name catches invalid inputs", {
  
  expect_error(
    iSEE:::.encode_panel_name("invalid plot name"),
    "is not a legal panel name"
  )
  
})

test_that(".decode_panel_name works as expected", {
    expect_identical(iSEE:::.decode_panel_name("geneExpr", 1), "Gene expression plot 1")
    expect_identical(iSEE:::.decode_panel_name("redDim", 2), "Reduced dimension plot 2")
    expect_identical(iSEE:::.decode_panel_name("colData", 3), "Column data plot 3")
    expect_identical(iSEE:::.decode_panel_name("geneStat", 4), "Gene statistics table 4")

    # Works on vectors.
    expect_identical(iSEE:::.decode_panel_name(c("geneExpr", "redDim"), 1:2), 
                     c("Gene expression plot 1", "Reduced dimension plot 2"))
    expect_identical(iSEE:::.decode_panel_name(character(0), integer(0)), character(0))
})

test_that(".decoded2encoded works as expected", {
    expect_identical(iSEE:::.decoded2encoded("Gene expression plot 1"), "geneExprPlot1")
    expect_identical(iSEE:::.decoded2encoded(c("Gene expression plot 1", "Reduced dimension plot 1")), 
                                             c("geneExprPlot1", "redDimPlot1"))
    expect_identical(iSEE:::.decoded2encoded(character(0)), character(0))
    expect_identical(iSEE:::.decoded2encoded(c("Gene expression plot 1", "")), 
                                             c("geneExprPlot1", ""))
})

test_that(".split_encoded works as expected", {
    expect_identical(iSEE:::.split_encoded("geneExprPlot1"), list(Type="geneExpr", ID=1L))
    expect_identical(iSEE:::.split_encoded(c("geneExprPlot1", "redDimPlot1")),
                     list(Type=c("geneExpr", "redDim"), ID=c(1L, 1L)))
    expect_identical(iSEE:::.split_encoded(character(0)), list(Type=character(0), ID=integer(0)))
})

