context("Plots")

test_that(".create_selection_info_output works as expected", {

    output = new.env()
    pObjects = new.env()
    rObjects = new.env()

    pObjects$cached[["RedDimPlot1"]] <- list(
        commands="ggplot()",
        contents=data.frame(X=1, Y=1),
        plot=ggplot()
    )

    out <- .create_plot_output("RedDimPlot1", sce, output, pObjects, rObjects)
    expect_null(out)
    expect_is(output$RedDimPlot1, "shiny.render.function")

})
