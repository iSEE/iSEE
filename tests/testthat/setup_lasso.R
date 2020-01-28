LASSO_OPEN <- list(
    lasso=NULL,
    closed=FALSE,
    panelvar1=NULL, panelvar2=NULL,
    mapping=list(x="X", y="Y"),
    coord=matrix(c(1, 2, 2, 1, 1, 1, 2, 2), ncol=2))

CLICK_CLOSING <- list(
    x=1, y=1,
    mapping=list(x="X", y="Y"),
    domain=list(left=-14.1, right=10.9, bottom=-12, top=16.4),
    range=list(left=38.7, right=379, bottom=466, top=23.8),
    log=list(x=NULL, y=NULL)
)

CLICK_WAYPOINT <- list(
    x=3, y=4,
    mapping=list(x="X", y="Y"),
    domain=list(left=-14.1, right=10.9, bottom=-12, top=16.4),
    range=list(left=38.7, right=379, bottom=466, top=23.8),
    log=list(x=NULL, y=NULL)
)

LASSO_CLOSED <- list(
    lasso=NULL,
    closed=TRUE,
    panelvar1=NULL, panelvar2=NULL,
    mapping=list(x="X", y="Y"),
    coord=matrix(c(1, 2, 2, 1, 1, 1, 1, 2, 2, 1), ncol=2))

CLOSED_LASSO <- iSEE:::.update_lasso(CLICK_CLOSING, LASSO_OPEN)
