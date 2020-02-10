# This tests the custom panel creation functions.
# library(iSEE); library(testthat); source('setup_sce.R'); source('test_custom.R')

test_that("createCustomPlot works as expected", {
    library(ggplot2)
    FUNNY <- function(se, rows, columns, some_str1="A", some_str2="B",
        some_number1=1, some_number2=2, some_flag1=TRUE, some_flag2=FALSE)
    {

        ggplot(as.data.frame(colData(se))) + ggtitle(paste0(some_str1, some_str2))  
    }

    GEN <- createCustomPlot(FUNNY, 
        argStrings=c("some_str1", "some_str2"),
        argNumbers=c("some_number1", "some_number2"),
        argFlags=c("some_flag1", "some_flag2")
    )
    x <- GEN(PanelId=1L)

    # Checking that the custom methods work.
    expect_identical(.fullName(x), "Custom plot")

    expect_error(ui <- .defineDataInterface(x, sce, NULL), NA)
    expect_match(as.character(ui), "some_str1")
    expect_match(as.character(ui), "some_flag1")
    expect_match(as.character(ui), "some_number2")

    output <- .generateOutput(x, sce, all_memory=list(), all_contents=list())
    expect_s3_class(output$contents, "ggplot")
    expect_match(output$commands$plot, "FUNNY")
})

test_that("createCustomTable works as expected", {
    library(ggplot2)
    FUNNY <- function(se, rows, columns, some_str1="A", some_str2="B",
        some_number1=1, some_number2=2, some_flag1=TRUE, some_flag2=FALSE)
    {
        as.data.frame(colData(se))[c(some_number1, some_number2),]
    }

    GEN <- createCustomTable(FUNNY, 
        argStrings=c("some_str1", "some_str2"),
        argNumbers=c("some_number1", "some_number2"),
        argFlags=c("some_flag1", "some_flag2")
    )
    x <- GEN(PanelId=1L)

    # Checking that the custom methods work.
    expect_identical(.fullName(x), "Custom table")

    expect_error(ui <- .defineDataInterface(x, sce, NULL), NA)
    expect_match(as.character(ui), "some_str2")
    expect_match(as.character(ui), "some_flag2")
    expect_match(as.character(ui), "some_number1")

    env <- new.env()
    env$se <- sce
    output <- .generateTable(x, env)
    expect_match(output, "FUNNY")
    expect_true(is.data.frame(env$tab))
})
