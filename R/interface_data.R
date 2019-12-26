.create_data_param_box <- function(x, se, select_info) {
    do.call(.collapseBoxHidden, 
        c(
            list(x=x, field=.dataParamBoxOpen, title="Data parameters"),
            .defineDataInterface(x, se, select_info)
        )
    )
}
