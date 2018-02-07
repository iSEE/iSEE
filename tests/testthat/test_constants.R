

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
