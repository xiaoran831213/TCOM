xpls <- function (x, y, ncomp=NULL, center=TRUE, scale=FALSE)
{
    ## x <- as.matrix(x)
    ## y <- as.matrix(y)
    x <- scale(x, center, scale)
    y <- scale(y, center, scale)
    nobj <- nrow(x)
    npred <- ncol(x)
    nresp <- ncol(y)
    if(length(ncomp) == 0)
        ncomp <- min(nobj, npred)
    S <- crossprod(x, y)
    M <- crossprod(x)
    B <- array(0, dim = c(npred, ncomp, nresp))
    Q <- matrix(0, nrow = nresp, ncol = ncomp)
    R <- V <- P <- matrix(0, nrow = npred, ncol = ncomp)
    T <- U <- matrix(0, nrow = nobj, ncol = ncomp)
    for (a in seq_len(ncomp))
    {
        r <- svd(S, nu = 1, nv = 0)$u
        t <- x %*% r
        tnorm <- sqrt(sum(t * t))
        t <- t/tnorm
        r <- r/tnorm
        p <- crossprod(x, t)
        q <- crossprod(y, t)
        u <- y %*% q
        v <- p
        if (a > 1)
        {
            v <- v - V %*% crossprod(V, p)
            u <- u - T %*% crossprod(T, u)
        }
        v <- v/sqrt(sum(v * v))
        R[, a] <- r
        V[, a] <- v
        P[, a] <- p
        T[, a] <- t
        U[, a] <- u
        Q[, a] <- q
        B[, a, ] <- tcrossprod(R[, seq_len(a), drop = FALSE], 
            Q[, seq_len(a), drop = FALSE])
        M <- M - tcrossprod(p)
        S <- S - v %*% crossprod(v, S)
    }

    xeigenvals <- colSums(T^2)/(nobj - 1)
    yeigenvals <- colSums(U^2)/(nobj - 1)
    ldv <- R %*% solve(crossprod(P, R)) # actual loading of X
    pcs <- x %*% ldv                    # actual scores
    
    list(coeffs = B, weights = R, xloadings = P, xscores = T, 
         yloadings = Q, yscores = U, ncomp = a,
         xeigenvals=xeigenvals, yeigenvals=yeigenvals,
         ldv=ldv, pcs=pcs)
}
