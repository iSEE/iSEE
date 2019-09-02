context("voice")

# prepareSpeechRecognition ----

test_that("prepareBugsEasterEgg loads", {

    out <- .prepareBugsEasterEgg(use = FALSE)
    expect_identical(
        out,
        list()
    )

    out <- .prepareBugsEasterEgg(use = TRUE)
    expect_identical(
        names(out),
        c("name", "attribs", "children")
    )

    out <- .prepareBugsEasterEgg(use = c(bugs=3, spiders=10))
    expect_identical(
        names(out),
        c("name", "attribs", "children")
    )

    out <- .prepareBugsEasterEgg(use = c(spiders=10, bugs=3))
    expect_identical(
        names(out),
        c("name", "attribs", "children")
    )

    out <- .prepareBugsEasterEgg(use = c(spiders=10))
    expect_identical(
        names(out),
        c("name", "attribs", "children")
    )

    expect_error(
        .prepareBugsEasterEgg(use = c(bugs=3)),
        "'use' must be TRUE, FALSE, or an integer vector named c('bugs', 'spiders')",
        fixed=TRUE
    )

})

