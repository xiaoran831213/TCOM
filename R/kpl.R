## kernel player function

KPL <- new.env()

#' pairwise completion counts
#'
#' @param x M x P data matrix 1.
#' @param y N x P data matrix 2 (def=x).
#' @return M x N matrix of pairwise completion counts.
#' @examples
#' . <- NA
#' x <- rbind(c(0, 1, ., 3), c(2, 3, 0, 1), c(0, 1, 0, .))
#' y <- rbind(c(1, 2, ., 2), c(2, ., 3, 2), c(3, 2, 1, 0), c(0, 0, 1, 0))
#' pcc(x, y)
KPL$pcc <- function(x, y=NULL)
{
    x <- !is.na(x)
    y <- !is.na(y) %||% x
    tcrossprod(x, y) |> as.integer() |> matrix(NROW(x), NROW(y))
}

#' Zero fill missing values in x.
#'
#' @param x M x P data matrix 1.
#' @param y N x P data matrix 2 (def=x).
#' @param ... reserved to specify imputation type.
#' 
#' @return M x N identity matrix
KPL$zfx <- function(x, y=NULL, ...)
{
    x[is.na(x)] <- 0
    x
}

#' Zero fill missing values in y.
#'
#' @param x M x P data matrix 1.
#' @param y N x P data matrix 2 (def=x).
#' @param ... reserved to specify imputation type.
#' 
#' @return M x N identity matrix
KPL$zfy <- function(x, y=NULL, ...)
{
    y[is.na(y)] <- 0
    y
}

#' kernel function: identity
#'
#' @param x M x P data matrix 1.
#' @param y N x P data matrix 2 (def=x).
#' @param d values to fill in.
#' @param ... not used
#' 
#' @return M x N identity matrix
KPL$idn <- function(x, y=NULL, d=NULL, ...)
{
    y %:-% x
    d %:-% 1
    diag(d, NROW(x), NROW(y))
}


#' Euclidean Distance Squared
#'
#' Faster calculation by matrix product tricks.
#'
#' @param x M x P data matrix 1 w/o missing.
#' @param y N x P data matrix 2 w/o missing (def=x).
#' @return M x N distance matrix.
#'
#' @examples
#' x <- rbind(c(0, 1, 2, 3), c(2, 3, 0, 1), c(0, 1, 0, 1))
#' y <- rbind(c(1, 2, 3, 2), c(2, 3, 3, 2), c(3, 2, 1, 0), c(0, 0, 1, 0))
#' euc(x, y)
KPL$euc <- function(x, y=NULL, ...)
{
    D <- outer(0:9, 0:9, `-` %.% `^`(2))
    x <- as.matrix(x + 1)
    y <- as.matrix(y + 1) %&!% is.null(y)
    d <- matrix(0L, nrow(x), nrow(y))
    for(j in seq(ncol(x)))
        d <- d + D[x[, j], y[, j]]
    d
}

#' Manhattan Distance
#'
#' @param x M x P data matrix 1 w/o missing.
#' @param y N x P data matrix 2 w/o missing (def=x).
#' @return M x N kernel matrix
#'
#' @examples
#' x <- rbind(c(0, 1, 2, 3), c(2, 3, 0, 1), c(0, 1, 0, 1))
#' y <- rbind(c(1, 2, 3, 2), c(2, 3, 3, 2), c(3, 2, 1, 0), c(0, 0, 1, 0)
#' man(x, y)
KPL$man <- function(x, y=NULL, ...)
{
    D <- outer(0:9, 0:9, `-` %.% abs)
    x <- as.matrix(x + 1)
    y <- as.matrix(y + 1) %&!% is.null(y)
    d <- matrix(0L, nrow(x), nrow(y))
    for(j in seq(ncol(x)))
        d <- d + D[x[, j], y[, j]]
    d
}

#' kernel functition: Gaussian
#'
#' @param x N x P data matrix 1.
#' @param y M x P data matrix 2 (def=NULL).
#' @param sigma kernel width or variation length scale (def=1).
#' @param log return logged kernel (def=0)?
#' @param ... not used
#' @return M x N kernel matrix
KPL$gsn <- function(x, y=NULL, sigma=1, ...)
{
    n0 <- pcc(x, y)
    x1 <- zfs(x)
    y1 <- zfs(y)
    ## kernel logged
    kl <- -euc(x1, y1) * (.5 * sigma^2) - n0 * log(sqrt(2 * pi) * sigma)
    if(lgk) kl else exp(kl)
}

#' kernel function: polynomial
#' @param x N x P data matrix 1.
#' @param y M x P data matrix 2 (def=NULL).
#' @param a: polynomial offset (def=0).
#' @param b: polynomial degree (def=1).
#' @param ... not used.
#' @return  N x M kernel matrix.
#' @details k(x, y) = (x'y / p + a)^b;
KPL$ply <- function(x, y=NULL, a=0, b=1, ...)
{
    n0 <- pcc(x, y)
    c0 <- colMeans(rbind(x, y), na.rm=TRUE)
    x1 <- scale(x, c0, FALSE)
    if(is.null(y))
        y1 <- x1
    else
        y1 <- scale(y, c0, FALSE)
    (tcrossprod(x1, y1) / n0 + a)^b
}

#' kernel function: Identity By State
#'
#' For genomic dosage data, the similarity between two individual
#' i and j, contributed to the k th. variants is:
#'
#' $ s_{ij} = 2 - |g_{ik} - g_{jk}| $
#'
#' @param x N matrix of N row individuals and P column variants
#' @param l level of genomic variation (def = 2, i.e., allele dosage.)
#' @return NxN IBS kernel matrix
KPL$ibs <- function(x, l=2, ...)
{
    if(is.null(l))
        x <- apply(x, function(.) . / max(.))
    else
        x <- x / l
    1 - as.matrix(dist(x, 'man')) / ncol(x)
}

#' kernel summed
#'
#' @param k list of R kernels
#' @param w vector of R weights
#' @return w_1 * k_1 + ... + w_R * k_R
KPL$ksm <- function(k, w)
{
    Reduce(`+`, mapply(`*`, k, as.list(w), SIMPLIFY = FALSE))
}


#' kernel scaled
#'
#' @param k \code{N * N} kernel matrix.
#' @param center center the kernel (def=T)?
#' @param scale scale the kernel (def=F)?
KPL$ksc <- function(k, center=TRUE, scale=FALSE)
{
    if(center)
        k <- k - outer(rowMeans(k), colMeans(k), `+`) + mean(k)
    if(scale)
        k <- k / mean(diag(k))
    k
}

if("TCOM:KPL" %in% search())
    detach("TCOM:KPL")
attach(KPL, name="TCOM:KPL")
rm(KPL)
