## factor analysis assistance
FAA <- new.env()

#' Rho Circle
#'
#' Display correlations in a circule plot
FAA$rho_ccl <- function(rho) {
    library(ggraph)
    library(igraph)
    library(reshape2)
    ## treat off-triangle correlation in the long-format as connections.
    cnn <- cbind(expand.grid(LHS=rownames(rho), RHS=colnames(rho)), RHO=as.vector(rho))
    cnn <- subset(cnn, as.integer(LHS) < as.integer(RHS))
    cnn <- arrange(cnn, LHS, RHS)
    cnn <- within(cnn, {
        LHS <- as.character(LHS)
        RHS <- as.character(RHS)
    })

    ## item as veterices
    vtx <- data.frame(
        ISN=rownames(rho),
        DOM=sub("^(.+?)_(.+)$", "\\1", rownames(rho)),
        ITM=sub("^(.+?)_(.+)$", "\\2", rownames(rho)),
        NFO=(rowSums(rho^2) - 1) / sum(eigen(rho, TRUE, TRUE)[["values"]] > 1),
        row.names=NULL)

    ## computing the label features that will be displayed all around the circle,
    ## next to the nodes:
    ## - angle: vertical on top and botton, horizontal on the side, and so on.
    ## - flip it:  on the left hand side must be 180° flipped to be readable
    ## - alignment: if labels are flipped, they must be right aligned
    vtx <- within(vtx, {
        id <- seq_along(ISN)
        ## flip angle BY to make them readable
        angle <- 90 - 360 * id / sum(!is.na(id))
        ## calculate the alignment of labels: right or left If I am on the left
        ## part of the plot, my labels have currently an angle < -90
        hjust <- ifelse(angle < -90, 1, 0)
        ## flip angle BY to make them readable
        angle <- ifelse(angle < -90, angle+180, angle)
    })

    ## Create a graph object
    gob <- igraph::graph_from_data_frame(subset(cnn, RHO > 0.50), vertices=vtx)
    
    ## Basic usual argument
    g <- ggraph(gob, layout='linear', circular=TRUE)
    g <- g + geom_edge_arc(aes(linewidth=RHO), color="blue", alpha=0.3)
    g <- g + scale_linewidth_identity(NULL)
    g <- g + geom_node_point(aes(color=DOM, size=NFO))
    g <- g + scale_color_brewer(NULL, type="qua", palette=3)
    g <- g + geom_node_text(aes(x=x*1.04, y=y*1.04, color=DOM,
                                label=name, angle=angle, hjust=hjust), size=2)
    g <- g + coord_fixed()
    g <- g + theme_void()
    g <- g + theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm"))
    g <- g + expand_limits(x = c(-1.5, 1.5), y = c(-1.5, 1.5))
    ggsave(file.path(OUT, "rho_idh.png"), g, bg = "white")

}


#' fix factor names
#'
#' Rename and reorder factors as F followed by  2 digits.
#'
#' Prior to rotation, the K factors, are actually K principal components ordered
#' by accounted variance, and named as "MR" followed by 1 ...  K.
#'
#' The rotation changes accounted variance but retains the original names of the
#' factors, so the new order may not match the number in the name.
#'
#' The fix rename (up to 99) factors as "F##" then restore the ordering prior to
#' rotation.
#'
#' @param x an object with named elements/dimensions corresponding to factors.
#' @return an object with fixed factor names and ordering.
FAA$fix_fct <- function(x, ...) {
    ## matching and fixing
    PTN <- "^MR([0-9]+)$"
    .m. <- function(n) length(n) && is.character(n) && all(grepl(PTN, n))
    .f. <- function(n) sprintf("F%02d", as.integer(sub(PTN, "\\1", n)))
    ## fix
    if(.m.(names(x))) {
        names(x) <- .f.(names(x))
        x <- x[order(names(x))]
    }
    if(.m.(x)) {
        x <- .f.(x)
        x <- x[order(x)]
    }
    if(.m.(rownames(x))) {
        rownames(x) <- .f.(rownames(x))
        x <- x[order(rownames(x)), , drop=FALSE]
    }
    if(.m.(colnames(x))) {
        colnames(x) <- .f.(colnames(x))
        x <- x[, order(colnames(x)), drop=FALSE]
    }
    ## done
    x
}

#' loading blocs
#'
#' Given a loading matrix, find blocs of large loadings.
#' 
#' @param mob model object of class "FA" or a loading matrix.
#' @param MIN minimum size of a primary loading.
#' @param GAP minimum gap to separate loading groups.
#' @param FSZ a funtion to measure the size of a loading.
#' @return M-K numerical mask: 1 as primary, 2+ as abiguity, and 0 as negligibe
#'     loading
FAA$lds_blc <- function(mob, MIN=0.35, GAP=0.10, FSZ=abs, ...) {
    ## prepare
    if("fa" %in% class(mob)) {
        lds <- mob[["loadings"]] # loadings
    } else {
        lds <- as.matrix(mob)
    }
    M <- nrow(lds)           # number of items
    K <- ncol(lds)           # number of factors
    FCT <- seq(K)            # index of factors
    
    ## per-item, rank loadings and corresponding factors by loadings sizes
    lsz <- FSZ(lds)         # loading sizes
    rnk <- list()           # loading ranks
    rls <- matrix(.0, M, K) # loading sizes - rank ordered
    for(m in seq(M)) {
        r <- order(lsz[m, ], decreasing=TRUE)
        rls[m, ] <- lsz[m, r]
        rnk[[m]] <- r
    }

    ## differential loading sizes: d_r = s_{r-1} - s_r (r = 2 .. K); d_1 = s_1
    dls <- abs(rls - cbind(0, rls[, -K])) #
    ## mum-mask ranked loadings as blocks formed by large gaps in loading sizes
    msk <- apply(col(rls) > 1 & dls >  GAP, 1, cumsum)
    msk <- matrix(msk, M, K, byrow=TRUE, dimnames=dimnames(lds))
    ## elevate the mask-numbers by count of ambiguous loadings in the 1st block
    msk <- msk + rowSums(msk==0)
    ## zero-fill loadings < primary loading threshold
    msk <- msk * (rls > MIN)
    ## mask == 1 denote a primary loading, mask >= 2 denote ambiguities.

    ## restore the block mask numbers to the original order of factors
    blc <- matrix(0L, M, K, dimnames=dimnames(lds))
    for(m in seq(M)) {
        r <- rnk[[m]]
        blc[m, r] <- msk[m, ]
    }
    blc
}

#' correlation of factors
#'
#' Extract correlation among factors.
#'
#' Prioritize  asymptotic inter  factor correlation  ("Phi") which  is available
#' when rotations were used, otherwise,  return the correlation among calculated
#' factor scores ("r.score").
#'
#' @param mob model object of class "FA".
#' @return a correlation matrix of K factors.
FAA$cor_fct <- function(mob, ...) {
    ## key elements
    if(mob[["rotation"]] == "none" || is.null(mob$Phi)) {
        res <- mob[["r.scores"]]
    } else {
        res <- mob[["Phi"]]
    }
    res
}

#' omega statistics summary
#'
#' Summarize omega statistics for a factor analysis model
#'
#' @param mob model object of class "fa"
FAA$sum_omg <- function(mob, ...) {
    ## key items in the model object
    lds <- mob[["loadings"]] # loadings
    N <- mob[["n.obs"]]      # number of observations
    ## correlation among factors
    phi <- cor_fct(mob)
    ## calculate Omega statistics (without re-run FA)
    omg <- omega(lds, Phi=phi, flip=FALSE, plot=FALSE, n.obs=N, ...)
    omg
}

#' analysis call summary
#'
#' Summarize analytical parameters.
#'
#' @param mob model object of class "fa"
#' @param ... additional information to carry over.
#' @return a 1-line data frame of configuration, and dimension names.
FAA$sum_cal <- function(mob, ...) {
    ## call summary
    data.frame(
        ...,
        NOB=mob[["n.obs"]],          # number of observations
        NIT=nrow(mob[["loadings"]]), # number of items
        NFC=ncol(mob[["loadings"]]), # number of factors
        ROT=mob[["rotation"]],       # rotation
        MTD=mob[["fm"]],             # factoring method
        row.names=NULL)
}

#' model level summary
#'
#' Summarize a factor analysis at model level.
#'
#' @details the following statistics are reported
#' \itemize{
#'   \item{BIC}:
#'   \item{FIT}: 1 - sum((r - f'f)^2) / sum(r^2); (r=rho, f=factors)
#'   \item{TLI}: Tucker Lewis Index of reliability
#'   \item{CFI}: Comparative Fit Index
#'   \item{ALP}: Cronbach Alpha
#'   \item{GT6}: Gutman's Alpha 6
#'   \item{OMG}: Omega (Total)
#'   \item{ECV}: Explained Common Variance (Total)
#' }
#' @param mob model object of class "fa"
#' @return a 1-line data frame with model descriptive and fitness stats.
FAA$sum_mdl <- function(mob, ...) {
    ## flood the model object into local environment
    for(. in names(mob)) {
        assign(., mob[[.]])
    }
    ## [n.obs, loadings, r, BIC, TLI, CFI, rms, crms, RMSEA]
    rho <- r        # observed corr
    lds <- loadings #
    K <- ncol(lds)
    M <- nrow(lds)
    N <- n.obs

    ## ---- goodness of fit ----
    ## crude fitness: 1 - sum((r - f'f)^2)/sum(r^2)
    FIT <- factor.fit(rho, lds)
    ## variance accounted (proportion)
    VCC <- sum(Vaccounted["Proportion Var", ])
    ## Root Mean Square Error of Approximation
    RMSEA <- RMSEA[1]

    ## ---- validity ----
    ## Mean Interitem Correlation
    MIC <- mean(rho[upper.tri(rho)])
    ## added statists: omega, alpha, and ECV
    omg <- sum_omg(mob, ...)
    ## flood added stats into local environment
    for(. in names(omg)) {
        assign(., omg[[.]]) # [alpha, G6, omega.group, ECV]
    }
    OMG <- omega.group[1, 1] # Omega Total

    ## ---- pack up ----
    data.frame(
        BIC,       #
        FIT,       # 1 - sum((r - f'f)^2)/sum(r^2)
        TLI,       # Tucker Lewis Index of reliability
        CFI,       # Comparative Fit Index
        SRMS=rms,  # standard root-mean-square residual
        ARMS=crms, # adjusted root-mean-square residual
        RMSEA,     # Root Mean Square Error of Approx.
        ECV,       # Explained Common Variance Total
        VCC,       # variance accounted
        MIC,       # Mean Interitem Correlation
        ALP=alpha, # Cronbach Alpha
        GT6=G6,    # Gutman's Alpha 6
        OMG,       # Omega Total
        row.names=NULL)
}

#' factor level summary
#'
#' Summarize a factor analysis at factor level.
#'
#' The following statistics are reported for each factor
#' \itemize{
#'   \item{FCT}: factor names
#'   \item{VCC}: prop variance accounted (post-rotation)
#'   \item{R2S}: Multi-R2 of factor ~ factor score
#'   \item{VLD}: validity coef of coarse coded factor score (Grice, 2001)
#'   \item{NPL}: number of primary loadings
#'   \item{ALP}: Cronbach Alpha
#'   \item{GT6}: Gutman's Alpha 6
#'   \item{OMG}: Omega (Total)
#'   \item{ECV}: Explained Common Variance (Total)
#' }
#'
#' @param mob model object of class "fa"
#' @param ibc item blocs by loadings, can be NULL (default) to keep the loadings
#'     as is, a bloc matrix returned by [lds_blc()] showing primary loadings, or
#'     a function to return the said bloc matrix.
#' @return a data frame of descriptive & fitness stats for each factor.
FAA$sum_fct <- function(mob, blc=lds_blc, ...) {
    ## key items in the model
    lds <- mob[["loadings"]][] # loadings (pattern matrix)
    rho <- mob[["r"]]          # observed item correlation
    K <- ncol(lds)             # number of factors
    M <- nrow(lds)             # number of items
    N <- mob[["n.obs"]]        # number of observations
    FCT <- colnames(lds)       # factor names
    ## primary loadings as the first loading bloc
    if(is.null(blc)) {
        blc <- lds_blc
    }
    if(is.function(blc)) {
        msk <- blc(mob, ...) # calculate loading blocs
    } else {
        msk <- blc # directly supply the loading blocs
    }
    msk <- msk == 1     # keep primary loadings only
    NPL <- colSums(msk) # number of primary loadings

    ## -------------------------- factor level summary ---------------------------
    VCC <- mob$Vaccounted[2, ]       # prop variance accounted (post-rotation)
    R2S <- mob$R2                    # Multi-R2 of factor ~ factor score
    ## validity coef of coarse coded factor score (Grice, 2001)
    VLD <- rep(NA, K)
    VLD[!is.na(diag(mob$score.cor))] <- mob$valid
    ## Cronbach alpha and Gutman alpha 6
    tmp <- apply(msk, 2, \(m) {
        if(sum(m) > 1) {
            unlist(alpha(rho[m, m, drop=FALSE], n.obs=N)$total[c(1, 3)])
        } else {
            c("raw_alpha"=NA, "G6(smc)"=NA) # less than 2 primary loadings
        }
    })
    ALP <- tmp["raw_alpha", ]
    GT6 <- tmp["G6(smc)",   ]
    ## Omega (1XT: not entirely sure)
    omg <- sum_omg(mob)
    OMG <- omg$omega.group[-1, 1] # omega coef of sub-scales (a.k.a. per-factor)

    ## ---- pack up ----
    data.frame(NPL, VCC, R2S, VLD, ALP, GT6, OMG)
}

#' Item Level Summary
#'
#' Summarize a factor analysis at item level.
#'
#' The following statistics are included
#' - complexity: Hoffman's index of complexity for each item.  This is just
#'   {(Sigma a_i^2)^2}/{Sigma a_i^4} where a_i is the factor loading on the
#'   ith factor. From Hofmann (1978), MBR. See also Pettersson and Turkheimer
#'   (2010).
#' - uniqueness: 1 - communality = 1 - sum of squared factor loadings.
#'
#' @param mob model object of class "fa"
#' @return a data frame of stats for each item.
FAA$sum_itm <- function(mob, ...) {
    ## key items in the model
    CPL <- mob[["complexity"]]
    UNQ <- mob[["uniquenesses"]]
    ## pack up
    data.frame(CPL, UNQ)
}

#' Bass-Ackward - heritage matrix
#'
#' Per-level, summarize inheritance of the new factor from its parents.
FAA$bss_hmx <- function(lvl, fct, phc) {
    ## fortify: convert msking vectors as R-factor
    LVL <- if(is.factor(lvl)) lvl else factor(lvl, unique(lvl))
    FCT <- if(is.factor(fct)) fct else factor(fct, unique(fct))
    ## order the level and factors
    i <- order(LVL, FCT)
    LVL <- LVL[i]
    FCT <- FCT[i]
    phc <- phc[i, , drop=FALSE]
    ## mark new factors
    tmp <- split(FCT, LVL)
    NEW <- list(!is.na(tmp[[1]]))
    for(i in seq.int(2, length.out=length(tmp) - 1)) {
        NEW[[i]] <- !tmp[[i]] %in% tmp[[i - 1]]
    }
    NEW <- 0L + unlist(NEW)
    ## heritage matrix: heritage of new i (row) from old j (col).
    hmx <- phc[NEW==1, ]
    hmx <- data.frame(hmx, row.names=levels(FCT))
    hmx <- abs(as.matrix(hmx))
    hmx[is.na(hmx)] <- 0
    names(dimnames(hmx)) <- c("New", "Old")
    hmx
}

#' Bass-Ackward - build nodes
#'
#' @param lvl vector of models
#' @param fct vactor of factors
#' @param txt descriptive text of each factor
FAA$bss_nds <- function(lvl, fct, txt=NULL) {
    ##
    LVL <- if(is.factor(lvl)) lvl else factor(lvl, unique(lvl))
    FCT <- if(is.factor(fct)) fct else factor(fct, unique(fct))
    ## sort by levels and factors
    i <- order(LVL, FCT)
    LVL <- LVL[i]
    FCT <- FCT[i]
    ## factors in integer form
    LVN <- as.integer(LVL)
    FCN <- as.integer(FCT)
    ##
    TXT <- if(length(txt)) txt else sprintf("%s.%s", LVL, FCT)
    data.frame(LVL, LVN, FCT, FCN, TXT)
}

#' Bass-Ackward - build trees
#'
#' @param lvl vector of _models (e.g., 1, 2, 2, 3, 3, 3, 4, 4, ..)
#' @param fct vactor of factors (e.e., A, A, B, A, B, C, A, B, ..)
#' @param phc pseudo-hierachy-correlation
FAA$bss_trs <- function(lvl, fct, phc) {
    ## fortify #1: convert msking vectors as R-factor
    LVL <- if(is.factor(lvl)) lvl else factor(lvl, unique(lvl))
    FCT <- if(is.factor(fct)) fct else factor(fct, unique(fct))
    ## order the level and factors
    i <- order(LVL, FCT)
    LVL <- LVL[i]
    FCT <- FCT[i]
    phc <- phc[i, , drop=FALSE]
    ## mark new factors
    tmp <- split(FCT, LVL)
    NEW <- list(!is.na(tmp[[1]]))
    for(i in seq.int(2, length.out=length(tmp) - 1)) {
        NEW[[i]] <- !tmp[[i]] %in% tmp[[i - 1]]
    }
    NEW <- 0L + unlist(NEW)
    ## tree in long format: level, factor, New, COR, pseudo-parent.
    r <- cbind(LVL, FCT, NEW, phc)
    r <- melt(r, names(r)[1:3], variable.name="PPF", value.name="COR")
    r <- within(subset(r, !is.na(COR)), {
        ## fortify and rank pseudo-parents
        PPF <- factor(PPF, levels(FCT))
        PPR <- xgf(-abs(COR), interaction(LVL, FCT), rank)
        ## interger typed levels, factor, and parents
        LVN <- as.integer(LVL)
        FCN <- as.integer(FCT)
        PPN <- as.integer(PPF)
        ## display texts
        TXT <- sprintf("%.2f", COR)
    })
    ## pack up & return
    r <- r[, c("LVL", "LVN",        # levels
               "FCT", "FCN", "NEW", # factors
               "COR", "TXT",        # corr()
               "PPF", "PPN", "PPR"  # parents
               )]
    r <- arrange(r, LVL, FCT, PPF)
    r
}

#' confirmatory 1-factor analysis - model selection
FAA$cfa_m1s <- function(rho, RHS=NULL, LHS="LHS", RMS=0.08, CFI=0.95, TLI=0.90, ...) {
    ## fitness stats to use
    FTS <- c("rmsea", "cfi", "tli")
    ## sanity check
    stopifnot(!is.null(rownames(rho)), !is.null(colnames(rho)))
    stopifnot(NROW(rho) == NCOL(rho), rownames(rho) == colnames(rho))
    ## item set
    if(is.null(RHS)) {
        RHS <- rownames(rho)
    } else {
        stopifnot(RHS %in% rownames(rho))
    }
    stopifnot((M <- length(RHS)) > 3) # must have more than 3 items

    ## go through combinations of items - perform 1-factor CFA
    res <- list()
    MNO <- 0L
    for(NFS in seq.int(4L, M)) {
        r <- list()
        for(f in combn(RHS, NFS, simplify=FALSE)) {
            MNO <- MNO + 1
            f <- paste0(LHS, " =~ ", paste0(f, collapse = " + "))
            m <- cfa(f, sample.cov=rho, sample.nobs=NOB, std.lv=TRUE)
            ops <- subset(summary(m)[["pe"]], TRUE, -c(exo, z))
            fts <- data.frame(as.list(fitMeasures(m, FTS)))
            r[[f]] <- data.frame(MNO, NFS, fts, ops)
        }
        r <- do.call(rbind, r)
        r <- subset(r, rmsea <= RMS & cfi >= CFI & tli >= TLI)
        if(nrow(r) < 1) {
            break
        } else {
            res[[NFS]] <- r
        }
    }
    res <- data.frame(do.call(rbind, res), row.names=NULL)
    names(res) <- sub("^pvalue", "p", names(res))
    res
}


if("TCOM:FAA" %in% search())
    detach("TCOM:FAA")
attach(FAA, name="TCOM:FAA", warn.conflicts = TRUE)
rm(FAA)
