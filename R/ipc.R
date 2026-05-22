#' identifiable principle components
#'
#' 
#' wrap  R's  prcomp()  to  ensure  maximum  positive  span,  so  the  principal
#' components become identifiable.
#'
#' @param x data matrix to apply IPC, or a prcomp object from R's PCA.
#' @param mxp ensure positive maximum span? (def=1)
#' @param ... arguments to pass to prcomp().
ipc <- function(x, mxp=1, ...)
{
    ## if x is a PCA object, convert it to an IPC object
    pca <- if(inherits(x, "prcomp")) x else prcomp(x, ...)
    pcs <- pca$x        # score vectors (PCs)
    sdv <- pca$sdev     # SD of scores
    ldv <- pca$rotation # loading vectors use
    cnt <- pca$center   # centers
    ## naming 
    colnames(pcs) <- sprintf("P%02X", 1:NCOL(pcs)) #
    colnames(ldv) <- colnames(pcs)
    names(sdv)    <- colnames(pcs)

    ## origin in the PC-Score space
    org <- -drop(crossprod(cnt, ldv)) # origin in PCS

    ## enforce identifiablity by miximizing positive span on each scores (PCs) 
    if(mxp)
    {
        for(i in seq(ncol(pcs)))
        {
            ## ldv[, i] <- ldv[, i] * sign(max(pcs[, i])^2 - min(pcs[, i])^2)
            ldv[, i] <- ldv[, i] *
                sign((max(pcs[, i]) - org[i])^2 - (min(pcs[, i]) - org[i])^2)
        }
        pcs <- scale(x, cnt, FALSE) %*% ldv
    }
    structure(list(pcs=pcs, ldv=ldv, sdv=sdv, cnt=cnt), class=c("ipc", "list"))
}

#' convenient indexing of PCA resluts.
#'
#' @examples
#' res <- ipc(USArrests)
#' res[c("Arkansas", "Delaware", "Ohio"), ]        # 3 states
#' res[, c(1, 3)]                                  # 2 scores
#' res[c("Arkansas", "Delaware", "Ohio"), c(1, 3)] # 2D index
`[.ipc` <- function(x, ...)
{
    .i. <- match.call(expand.dots = FALSE)[["..."]]
    .f. <- as.name("[")
    .e. <- alist(,)[1] # empty arg
    ## by default, do not drop matrics to vectors.
    if("drop" %in% names(.i.))
    {
        .d. <- .i.["drop"]
        .i.["drop"] <- NULL
    }
    else
        .d. <- alist(drop=FALSE)
    if(length(.i.) > 1)
    {
        call.pcs <- as.call(c(.f., as.name("pcs"), .i., .d.))
        call.ldv <- as.call(c(.f., as.name("ldv"), .e., .i.[2], .d.))
        call.sdv <- as.call(c(.f., as.name("sdv"), .i.[2], .d.))
    }
    else
    {
        call.pcs <- as.call(c(.f., as.name("pcs"), .e., .i.[1], .d.))
        call.ldv <- as.call(c(.f., as.name("ldv"), .e., .i.[1], .d.))
        call.sdv <- as.call(c(.f., as.name("sdv"), .i.[1], .d.))
    }
    x$pcs <- eval(call.pcs, x, parent.frame())
    x$ldv <- eval(call.ldv, x, parent.frame())
    x$sdv <- eval(call.sdv, x, parent.frame())
    x
}

#' extrapolate PCA to new data
#'
#' @examples
#' res <- ipc(USArrests[+1:40])   # PCA based on the first 40 states
#' res <- predict(res, USArrests) # extrapolate to all states
predict.ipc <- function(x, ...)
{
    dot <- list(...)
    if(length(dot) < 1)
        x
    else
        within(x, pcs <- scale(dot[[1]], cnt, FALSE) %*% ldv)
}

#' principal component label
#'
#' A wrapper of R's [reorder()] to ensure consistant order of factor levels with
#' principle components or any weighted coordinates.
#'
#' The ordering of levels is dertermined by the *aggregated proximity* of points
#' in each level, to  the origin of PC-Score space (i.e.,  score of the centroid
#' of data), with dimensions inversely weighted by `sdv`.
#'
#' By default, *proximity* to the origin is measured by Euclidian distance while
#' *aggregation* is done by passing [FUN=median()] to [reorder()].
#'
#' @param lbl N points labeled by factor levels.
#' @param x (N x P) data matrix, or a "prcomp", or an "ipc" object.
#' @param FUN function to calculated an aggregated proximity, def=median().
#' @param USE PC-scores to use to calculate proximity, def=1:`num_of_PC`.
pcl <- function(lbl, x, FUN=median, USE=NULL, ...)
{
    ## if x is a PCA object, convert it to an IPC object
    pca <- if(inherits(x, "ipc")) x else ipc(x, ...)
    ##
    USE %||% 1:ncol(pca$pcs)
    ldv <- pca$ldv[, USE, drop=FALSE]
    pcs <- pca$pcs[, USE, drop=FALSE]
    sdv <- pca$sdv[USE]
    org <- -drop(crossprod(pca$cnt, ldv)) # origin in PCS
    off <- scale(pcs, org, FALSE)         # offset from the origin
    dst <- sqrt((off^2 %*% (1/sdv^2)))    # weighted Euclidian from the origin
    lbl <- reorder(lbl, dst, FUN)         # levels re-ordered
    lbl
}
