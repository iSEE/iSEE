# This tests the custom panel creation functions.
# library(iSEE); library(testthat); source('setup_sce.R'); source('test_custom.R')

test_that("createCustomPlot works as expected", {
    library(ggplot2)
    FUNNY <- function(se, rows, columns, some_str1="A", some_str2=c("B", "C"),
        some_number1=1, some_number2=2, some_flag1=TRUE, some_flag2=FALSE)
    {
        ggplot(as.data.frame(colData(se))) + ggtitle(paste0(some_str1, some_str2))
    }

    GEN <- createCustomPlot(FUNNY)
    x <- GEN(PanelId=1L)

    # Checking that the custom methods work.
    expect_identical(.fullName(x), "Custom plot")

    expect_error(ui <- .defineDataInterface(x, sce, NULL), NA)
    expect_match(as.character(ui), "some_str1")
    expect_match(as.character(ui), "some_flag1")
    expect_match(as.character(ui), "some_number2")

    out <- .generateOutput(x, sce, all_memory=list(), all_contents=list())
    expect_s3_class(out$contents, "ggplot")
    expect_match(out$commands$plot, "FUNNY")

    out <- .defineOutput(x)
    expect_is(out, "shiny.tag")

    output <- new.env()
    pObjects <- new.env()
    rObjects <- new.env()
    out <- .renderOutput(x, sce, output=output, pObjects=pObjects, rObjects=rObjects)
    expect_is(out, "shiny.render.function")
    expect_named(output, "CustomPlot1")

})

test_that("createCustomTable works as expected", {
    FUNNY <- function(se, rows, columns, some_str1="A", some_str2=c("B", "C"),
        some_number1=1, some_number2=2, some_flag1=TRUE, some_flag2=FALSE)
    {
        as.data.frame(colData(se))[c(some_number1, some_number2),]
    }

    GEN <- createCustomTable(FUNNY)
    x <- GEN(PanelId=1L)

    # Checking that the custom methods work.
    expect_identical(.fullName(x), "Custom table")

    expect_error(ui <- .defineDataInterface(x, sce, NULL), NA)
    expect_match(as.character(ui), "some_str2")
    expect_match(as.character(ui), "some_flag2")
    expect_match(as.character(ui), "some_number1")

    env <- new.env()
    env$se <- sce
    out <- .generateTable(x, env)
    expect_match(out, "FUNNY")
    expect_true(is.data.frame(env$tab))

    out <- .refineParameters(x, sce)
    expect_identical(out[[iSEE:::.TableSelected]], "")

    # .refineParameters handles NULL x
    FUN <- selectMethod(".refineParameters", signature="CustomTable")
    out <- FUN(NULL, sce)
    expect_null(out, NULL)

    expect_true(.hideInterface(x, iSEE:::.selectColType))
    expect_false(.hideInterface(x, iSEE:::.TableSelected))

    input <- new.env()
    session <- new.env()
    pObjects <- new.env()
    rObjects <- new.env()
    expect_null(.createObservers(x, sce, input, session, pObjects, rObjects))
    expect_named(rObjects, c(
        "CustomTable1_INTERNAL_relinked_select",
        "CustomTable1_INTERNAL_saved_choices",
        "CustomTable1",
        "CustomTable1_INTERNAL_multi_select",
        "CustomTable1_INTERNAL_single_select"))
})

test_that(".grab_all_args works with restriction", {
    FUNNY <- function(se, rows, columns, some_str1="A", some_str2=c("B", "C"), some_str3=character(0),
        some_number1=1, some_number2=1:10, some_flag1=TRUE, some_flag2=logical(0), some_null=NULL)
    {
        NULL
    }

    out <- iSEE:::.grab_all_args(FUNNY)
    expect_identical(out, list(some_str1="A", some_str2=c("B", "C"), some_number1=1, some_flag1=TRUE))

    out <- iSEE:::.grab_all_args(FUNNY, restrict=c("some_str1"))
    expect_identical(out, list(some_str1="A"))
})

test_that(".execute_custom_function works with incoming selections", {
    library(ggplot2)
    FUNNY <- function(se, rows, columns, some_str1="A", some_str2=c("B", "C"),
        some_number1=1, some_number2=2, some_flag1=TRUE, some_flag2=FALSE)
    {
        ggplot()
    }

    GEN <- createCustomPlot(FUNNY)
    x <- GEN(PanelId=1L)

    plot_env <- new.env()
    plot_env$row_selected <- head(rownames(sce))
    plot_env$col_selected <- head(rownames(sce))

    out <- .execute_custom_function(x, FUNNY,
            fn_name="FUNNY", assigned="gg", envir=plot_env,
            fn_args=character(0))

    expect_identical(out, "gg <- FUNNY(se , row_selected , col_selected)")
})
