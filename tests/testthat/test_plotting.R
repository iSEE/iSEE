

# Constructors ----

test_that(".make_redDimPlot produce a valid list",{
  
  p.out <- iSEE:::.make_redDimPlot(id = 1, all_memory, all_coordinates, sce, ecm)
  
  # return value is a named list
  expect_type(
    iSEE:::.make_redDimPlot(id = 1, all_memory, all_coordinates, sce, ecm),
    "list"
  )
  expect_named(
    iSEE:::.make_redDimPlot(id = 1, all_memory, all_coordinates, sce, ecm),
    c("cmd", "xy", "plot")
  )
  
  # cmd value is a named list
  expect_type(
    p.out$cmd,
    "list"
  )
  expect_named(
    p.out$cmd,
    c("data","lim","brush","setup","plot")
  )
  
  # xy value is a data frame
  expect_s3_class(
    p.out$xy,
    "data.frame"
  )
  expect_named(
    p.out$xy,
    c("X","Y","ColorBy")
  )
  
  #plot
  expect_s3_class(
    p.out$plot,
    c("gg", "ggplot")
  )

})

