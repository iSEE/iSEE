
# .set_default_assay

test_that(".set_default_assay returns 1L there is no logcounts assay", 
  
  expect_identical(
    iSEE:::.set_default_assay(sce_noLogCount),
    1L
  )
  
)
