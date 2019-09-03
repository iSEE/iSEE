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

    out <- .prepareBugsEasterEgg(use = c(bugs=3L, spiders=10L))
    expect_identical(
        names(out),
        c("name", "attribs", "children")
    )

    # both values must be defined
    expect_error(
        .prepareBugsEasterEgg(use = c(bugs=3L)),
        "'use' must be TRUE, FALSE, or an integer vector named c('bugs', 'spiders')",
        fixed=TRUE
    )

})

