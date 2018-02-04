

# Constructors ----

test_that(".make_redDimPlot/.scatter_plot produce a valid list",{
  
  p.out <- iSEE:::.make_redDimPlot(id = 1, all_memory, all_coordinates, sce, ecm)
  
  # return value is a named list
  expect_type(
    p.out,
    "list"
  )
  expect_named(
    p.out,
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

test_that(".make_colDataPlot/.violin_plot produce a valid list",{
  
  p.out <- iSEE:::.make_colDataPlot(id = 1, all_memory, all_coordinates, sce, ecm)
  
  # return value is a named list
  expect_type(
    p.out,
    "list"
  )
  expect_named(
    p.out,
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
    c("Y","X","ColorBy")
  )
  
  #plot
  expect_s3_class(
    p.out$plot,
    c("gg", "ggplot")
  )

})

test_that(".make_colDataPlot/.griddotplot produce a valid list",{
  
  p.out <- iSEE:::.make_colDataPlot(id = 2, all_memory, all_coordinates, sce, ecm)
  
  # return value is a named list
  expect_type(
    p.out,
    "list"
  )
  expect_named(
    p.out,
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
    c("Y","X","ColorBy")
  )
  
  #plot
  expect_s3_class(
    p.out$plot,
    c("gg", "ggplot")
  )

})

