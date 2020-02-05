context("Commands")

test_that(".initialize_cmd_store behaves as expected", {

    cmds <- .initialize_cmd_store()

    expect_identical(cmds, list(pending = character(0), processed = character(0)))
})

test_that(".add_command behaves as expected", {

    cmds <- .initialize_cmd_store()
    cmds <- .add_command(cmds, "ggplot()", "ggplot")
    expect_identical(cmds$pending, c("ggplot"="ggplot()"))

    cmds <- .initialize_cmd_store()
    cmds <- .add_command(cmds, list("ggplot()", "sessionInfo()"), c("ggplot", "sessionInfo"))
    expect_identical(cmds$pending, c("ggplot"="ggplot()", "sessionInfo"="sessionInfo()"))

})

test_that(".evaluate_commands behaves as expected", {

    eval_env <- new.env()

    cmds <- .initialize_cmd_store()
    cmds <- .add_command(cmds, "gg <- ggplot()", "ggplot")
    cmds <- .evaluate_commands(cmds, eval_env)

    expect_identical(cmds$processed, c("ggplot"="gg <- ggplot()"))
    expect_is(eval_env$gg, "ggplot")

})
