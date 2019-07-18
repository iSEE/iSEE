context("modes")

####################
# Tests start here #
####################

# mode_gating ----

test_that("mode_gating returns a Shiny app", {

  plot_count <- 6
  # rv <- rowVars(logcounts(sce))
  # top_var <- head(order(rv, decreasing = TRUE), plot_count*2)
  # top_var_genes <- rownames(sce)[top_var]

  plot_features <- data.frame(
      x = head(rownames(sce), plot_count),
      y = tail(rownames(sce), plot_count),
      stringsAsFactors = FALSE
   )

  # launch the app itself ----

  app <- mode_gating(sce, features = plot_features, featAssayMax = 12)

  # return value is a named list
  expect_s3_class(
    app,
    "shiny.appobj"
  )

})

# mode_void ----

test_that("mode_void returns a Shiny app", {

  # launch the app itself ----

  app <- mode_void(sce)

  # return value is a named list
  expect_s3_class(
    app,
    "shiny.appobj"
  )

})
