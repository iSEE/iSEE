context("brush")

# .identical_brushes ----

test_that(".identical_brushes works with empty brush data", {

    expect_false(.identical_brushes(old_brush=NULL, new_brush=list(a=1)))
    expect_false(.identical_brushes(old_brush=list(a=1), new_brush=NULL))

})
