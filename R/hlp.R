## TCOM Tool related helpers
HLP <- new.env()

#' assign if not already exists
HLP$"%:-%" <- function(x, y) {
    Var <- deparse(substitute(x))
    if (!exists(Var, parent.frame(), inherits=FALSE) || length(x) == 0L) {
        assign(Var, y, parent.frame())
    }
    invisible(NULL)
}

#' y if x is none.
HLP$"%||%" <- function(x, y) if(length(x)) x else y
if(exists("%||%", "package:base"))
    HLP$"%||%" <- NULL

#' x if not y.
HLP$"%&!%" <- function(x, y) if(y) NULL else x

#' x if y.
HLP$"%&&%" <- function(x, y) if(y) x else NULL

#' swap values in x and y
#'
#' @param x one symbol of the values to swap
#' @param y one symbol of the values to swap
#' @examples
#' a <- "A"
#' b <- "9"
#' cat("a=", a, "; b=", b, " (prior) \n", sep="")
#' swp(a, b)
#' cat("a=", a, "; b=", b, " (after) \n", sep="")
#' swp(a, b)
#' cat("a=", a, "; b=", b, " (again) \n", sep="")
HLP$swp <- function(x, y) {
    env %:-% parent.frame()
    a <- deparse(substitute(x))
    b <- deparse(substitute(y))
    x <- force(x) # local copy of original x
    y <- force(y) # local copy of original y
    assign(a, y, parent.frame())
    assign(b, x, parent.frame())
    invisible(list(x=x, y=y))
}


#' make and return deep directories without warning
HLP$mkdir <- function(...) {
    d <- file.path(...)
    dir.create(d, FALSE, TRUE)
    d
}

#' cache evaluation 
#'
#' Idealy, a cached expression should only be evaluated once and the future call
#' return the cached result.
#'
#' @param .rds the R dataset (*.rds) to store the cache
#' @param .rxp the R expression to evaluate.
#' @param over overwrite existing cache? (def = N)
#' @param here evaluate {.rxp} locally? (def=N)
#' @param pack automatically pack up new / changed objects?
#'
#' Bewere not  to write R expression  relying on external objects  named {over},
#' {here}, or {pack}.
#' @examples
#' r <- cache("ex1.rds",
#' {
#'     a <- rnorm(9)
#'     b <- rnorm(9)
#'     a * b
#' }, over=0)
HLP$cache <- function(.rds, .rxp, over=0, here=0, pack=0) {
    .out. <- parent.frame() # outside
    .prt. <- NULL           # print out
    env <- new.env()        # sandbox
    if(file.exists(.rds) && !over) {
        ret <- readRDS(.rds)
        .prt. <- attr(ret, "stdout")
        cat(.prt., sep="\n")
    } else {
        .prt. <- capture.output({
            ret <- eval(substitute(.rxp), env, .out.)
        }, split = TRUE)
        if(pack) {
            ret <- as.list(env)
        }
        attr(ret, "stdout") <- .prt.
        saveRDS(ret, .rds)
    }
    if(here == 0) {
        for(n in names(env))
            assign(n, env[[n]], .out.)
    }
    invisible(ret)
}

#' flood objects from a list into an environment
#'
#' Unpack objects from a list into an environment.
#'
#' @param pck the named list of objects.
#' @param env the target environment (def=<caller>).
#' @return the same list of objects (silently).
HLP$upk <- function(pck, env=NULL, vbs=0) {
    env %:-% parent.frame()
    enm <- environmentName(env)
    pnm <- as.character(substitute(pck))
    for(. in names(pck)) {
        env[[.]] <- pck[[.]]
        if(vbs)
            PL("- %s: %7d x %5d, %16s", ., NROW(env[[.]]), NCOL(env[[.]]),
               format(as.integer(object.size(env[[.]])), big.mark=","))
    }
    invisible(pck)
}

#' Repack objects from an environment into a list.
#'
#' Repack objects from an environment into a list.
#' 
#' @param pck a list of named objects to pack.
#' @param env the environment (def=<caller>).
#' @return a packed list of objects.
HLP$rpk <- function(pck, env=NULL) {
    env %:-% parent.frame()
    ret <- list()
    for(. in intersect(names(pck), names(env))) {
        ret[[.]] <- env[[.]]  # pack up
        rm(list=., envir=env) # cleanup
    }
    invisible(ret)
}

#' Write TSV file.
#'
#' A wrapper of  [write.table()] that always uses "\t" as  separator, by default
#' uses no quotation and blank for NA.
HLP$write.tsv <- function(x, f, quote=FALSE, na="", row.names=FALSE, ...) {
    #' wrapper of write.table(...)
    #' - sep="\t",
    #' - row.names=FALSE,
    #' - na="",
    #' - quote=FALSE
    write.table(x, f, quote=quote, sep="\t", na=na, row.names=row.names, ...)
}
HLP$saveTSV <- HLP$Pwrite.tsv

#' Read TSV file.
#'
#' A wrapper of [read.delim()], use no quotation and treat blank as NA.
HLP$read.tsv <- read.delim
formals(HLP$read.tsv)$check.names <- FALSE
formals(HLP$read.tsv)$na.strings=""
HLP$readTSV <- HLP$read.tsv

#' split vector x by group g, apply function f, then unsplit.
HLP$xgf <- function(x, g, f, ...) unsplit(lapply(split(x, g), f, ...), g)

#' split {x} by {g}, tabluate unique values.
#'
#' When {g} is NULL, xgf() degenrated to a dummy variable encoder.
#' @param na TRUE to count NA (def=1), or a string to name the count.
HLP$xgt <- function(x, g=NULL, na=NULL) {
    if(!is.factor(x))
        x <- factor(x, unique(x))
    r <- 0 + outer(x, levels(x), `==`)
    r[is.na(x)] <- NA
    colnames(r) <- levels(x)
    rownames(r) <- names(x)
    if(!is.null(na) && na != FALSE) {
        if(na == TRUE)
            na <- NA
        r <- cbind(r, matrix(is.na(x), dimnames=list(NULL, na)))
    }
    if(length(g))
        r <- rowsum(r, g, na.rm=TRUE)
    r
}

#' code {x} as dummay variables
#'
#' {x} is treated as a factor.
#'
#' @param x a vector to recode
#' @param useNA (0=No, 1=ifAny, 2=Always)
HLP$xdm <- function(x, useNA=0) {
    if(!is.factor(x))
        x <- factor(x)
    if(useNA > 0)
        m <- 0 + is.na(x)
    else
        m <- NULL
    if(useNA == 1 && sum(m) == 0)
        m <- NULL
    ##
    l <- setdiff(levels(x), NA)
    n <- names(x)
    x <- 0 + outer(x, l, `==`)
    colnames(x) <- l
    rownames(x) <- n
    ##
    x <- cbind(x, `NA`=m)
    x
}

#' code NA as N/A in a factor
HLP$n2n <-function(x, i=NULL, s="N/A") {
    if(!is.factor(x))
        x <- factor(x)
    l <- levels(x)
    if(is.null(i))
        i <- length(l)
    a <- append(l, NA, i) -> b
    b[is.na(a)] <- s
    factor(x, a, b, exclude=NULL)
}

#' formated tally of counts and proportions of unique values.
#'
#' A wrapper of R function [table()].
#'
#' @param ... variable to calculate proportions.
#' @param mrg.prp margins to add to proportions
#' @param mrg.sum margins to add to sum(s)
#' @param mrk marker (def=","), the thousand separator.
HLP$ppf <- function(..., mrg.prp=NULL, mrg.sum=NULL, rnd=1) {
    ## tabulate
    num <- table(...)       # basic tabulation
    prp <- proportions(num) # basic proportion
    cls <- class(num)
    if(!is.null(mrg.sum)) {
        num <- addmargins(num, mrg.sum)
        prp <- addmargins(prp, mrg.sum)
    }
    att <- attributes(num)
    ## thousand mark and percentage
    num <- format(num, big.mark=",")
    fmt <- paste0("%5.", rnd, "f%%")
    prp <- format(sprintf(fmt, prp * 100))
    ## output
    res <- sprintf("%s(%s)", num, prp)
    attributes(res) <- att
    res
}

#' lengh of unique values in {x}
HLP$lux <- function(x, na.rm=FALSE) {
    #' length of unique values
    if(na.rm)
        x <- x[!is.na(x)]
    length(unique(x))
}

#' difference between values in {x} padded with an initial value.
#'
#' A wrapper of [diff()] which pads an  initial value (def=0) to the results, so
#' it has the same length with {x}.
HLP$xdf <- function(x, ini=0, lag=1L, dif=1L) {
    if(is.character(x)) {
        x <- factor(x, unique(x))
    }
    if(is.factor(x)) {
        x <- as.integer(x)
    }
    c(ini, diff(x, lag=lag, difference=dif))
}

#' append data frame into another data frame
HLP$append.df <- function(x, b, i=length(b)) {
    if(is.character(i))
        i <- pmatch(i, names(x), length(x))
    data.frame(append(x, b, i), row.names=rownames(x))
}

#' emulated printf
HLP$PF <- function(fmt, ...) {
    msg <- if(missing(fmt)) "" else sprintf(fmt, ...)
    cat(msg)
    invisible(c(msg, ..., recursive=FALSE))
}

#' emulated printf with new line
HLP$PL <- function(fmt, ...) {
    msg <- if(missing(fmt)) "" else sprintf(fmt, ...)
    ## msg <- format(msg)
    cat(msg, sep="\n")
    invisible(c(msg, ..., recursive=FALSE))
}

#' short hand for sprintf
HLP$SP <- sprintf

#' short hand for file.path
HLP$FP <- function(..., md=0, fsep=.Platform$file.sep) {
    fp <- file.path(..., fsep=fsep)
    if(md)
        mkdir(dirname(fp))
    fp
}

#' short hand for data.frame
HLP$DF <- data.frame
formals(HLP$DF)$check.names <- FALSE

#' compactaly list elements in a container
HLP$LL <- function(x, fmt=1) {
    rpt <- data.frame(name=names(x),
                      nrow=sapply(x, NROW),
                      ncol=sapply(x, NCOL),
                      size=as.numeric(sapply(x, object.size)),
                      row.names=NULL)
    if(fmt)
        rpt <- format(rpt, justify="right", big.mark=",")
    rpt
}

#' list names in a container in wide format
HLP$LW <- function(x) {
    cat("[")
    cat(names(x), sep=", ")
    cat("]\n")
}


#' praint a horizontal line
#'
#' @param chr char to fill up the line.
#' @param bgn beginning pattern
#' @param end ending pattern
#' @param ttl title in between.
#' @param len length in num of Char or % of screen width.
HLP$HL <- function(chr="-", bgn="##", end="##", ttl=NULL, len=NULL, sep=NULL) {
    if(is.null(len))
        len <- options()$width
    if(len < 0)
        len <- options()$width + len
    if(is.double(len) && 0.0 < len && len <= 1.0)
        len <- options()$width * len

    ## defaults and paddings
    sep <- " "  %&!% length(sep)
    bgn <- if(length(bgn)) paste0(bgn, sep) else ""
    end <- if(length(end)) paste0(sep, end) else ""
    ttl <- if(length(ttl)) paste0(sep, ttl, sep) else ""
    ttl <- format(ttl, justify="c")

    ## messages
    fl <- len - nchar(ttl) - nchar(bgn) - nchar(end) # fill length
    ll <- round(fl / 2)                              # left length
    rl <- fl - ll                                    # rest
    ##
    lm <- substr(strrep(chr, 1 + ll %/% nchar(chr)), 1, ll) # left
    rm <- substr(strrep(chr, 1 + rl %/% nchar(chr)), 1, rl) # rest
    ##
    msg <- paste0(bgn, lm, ttl, rm, end)
    cat(msg, sep="\n")
    invisible(msg)
}

#' concatenate two strings by ":".
#'
#' wrapper of `paste(a, b, sep=":")`.
HLP$`%:%` <- function(a, b) paste(a, b, sep=":")

#' helper: linear regression coefficients
#'
#' Get coefficients, standard error, Z/T-values, P-values, and 95% CI.
#'
#' @param mdl a linear (logistic) regression model
#' @param o.r for logistics reg, convert bta to Odds Ratio (def=Y)?
HLP$rcf <- function(mdl, o.r=TRUE) {
    if(inherits(mdl, "lmerMod") && attr(class(mdl), "package") == "lme4") {
        bta <- setNames(DF(coef(summary(mdl))), c("EST", "STD", "TST"))
        bta <- within(bta, {
            MPZ <- nrow(bta)
            NSZ <- nobs(mdl)
            PVL <- 2 * pt(abs(TST), NSZ - MPZ, lower=FALSE)
            CUP <- EST + STD * qt(0.975, NSZ - MPZ)
            CLW <- EST - STD * qt(0.975, NSZ - MPZ)
        })
    } else {
        c95 <- DF(confint.default(mdl))
        nsz <- dim(model.matrix(mdl))
        names(c95) <- c("CLW", "CUP")
        bta <- DF(summary(mdl)$coef)[names(coef(mdl)), ] # keep NA beta
        colnames(bta) <- c("EST", "STD", "TST", "PVL")
        rownames(bta) <- names(coef(mdl))
        bta <- cbind(bta[, 1, drop=FALSE], c95, bta[, -1])
        if("glm" %in% class(mdl) && family(mdl)$fam == "binomial" && o.r)
            bta[, 1:4] <- exp(bta[, 1:4])
        bta <- cbind(bta, NSZ=nsz[1], MPZ=nsz[2])
    }
    ret <- DF(RHS=rownames(bta), bta, row.names=NULL)
    structure(ret, class=c("rcf", class(ret)))
}
print.rcf <- function(x, digits=1, ...) {
    print.data.frame(x, digits=digits, ...)
}

#' helper: two class confusion matrices
HLP$cfx <- function(ref, est) {
    ## confusion of case
    r <- addmargins(table(factor(0+ref, 0:1), factor(0+est, 0:1)))
    ## ret <- rbind(TPC=c(2, 2), TNF=c(1, 1), FPC=c(1, 2), FNC=c(2, 1))
    ## F1S = TP / (TP + 0.5 (FP + FN))
    ret <- c(
        TPF=r[2, 2] / r[2, 3], TNF=r[1, 1] / r[1, 3],
        FPF=r[1, 2] / r[1, 3], FNF=r[2, 1] / r[2, 3],
        PRC=r[2, 2] / (r[2, 2] + r[1, 2]), # precision=TPC/(TPC+FPC)
        RCL=r[2, 2] / (r[2, 2] + r[2, 1]), # recall   =TPC/(TPC+FNG)
        ACC = (r[1, 1] + r[2, 2]) / r[3, 3])
    ## Micro F1 score for cases
    F1S <- r[2, 2] / (r[2, 2] + 0.5 * (r[1, 2] + r[2, 1]))

    ## confusion of ctrl
    r <- addmargins(table(factor(1-ref, 0:1), factor(1-est, 0:1)))
    F2S <- r[2, 2] / (r[2, 2] + 0.5 * (r[1, 2] + r[2, 1]))

    ## Macro Average Score
    FAS <- (F1S * sum(ref==1) + F2S * sum(1-ref==0)) / length(ref)
    c(ret, FAS=FAS)
}

#' sigmoid function
#'
#' wrapper of [stats::binomial()].
HLP$sgm <- binomial()$linkinv

#' convert YYYY-MM-DD to yyyy-qN (year-quater).
HLP$y2q <- function(x) {
    YMD <- "^(....)-(..)-(..)$"
    m <- sub(YMD, "\\2", x)
    y <- sub(YMD, "\\1", x)
    q <- (as.integer(m) - 1) %/% 3L + 1L
    sprintf("%s Q%d", y, q)
    ifelse(is.na(x), NA, sprintf("%s Q%d", y, q))
}

## tools for data clean up 
#' word check
#'
#' A initial quality summary of a data as words in bytes.
#' 
#' For each field, summerize the size  of words in bytes, missing rate, uniques,
#' followed by a preview of few non-missing words.
#'
#' Ill-formated data may have abnormally long  words for non-text fields such as
#' age, sex, and uid, due to  line shifting; highly missing and highly identical
#' fields may be considered to be dropped.
#'
#' Caution: the few values previewed here are not from the same records, because
#' the purpose of wor-check is to showcase some unique values in each field. Use
#' [head()] instead to preview a few compelte records.
#'
#' @param dat the table to check, typically a R [data.frame].
#' @param few how many elements to preview (def=4)
#' @param len maximum word character length to preview (def=10).
#' @param nas na-strings, def=c("", "NA", "N/A", "NULL").
HLP$wck <- function(dat, few=4, len=10, nas=c(NA, NaN, "", "N/A", "NULL"),
                    msg=0) {
    ## missingness patterns
    if(is.null(nas)) {
        nas <- c(NA, NaN, "", "N/A", "NULL")
    }
    nas <- unique(paste0(nas)) # turn Inf, NaN and NA to strings
    NAS <- sprintf("(?i)^\\s*(%s)\\s*$", paste0(nas, collapse = "|"))
    ## class codes
    CLS <- c(character="C", numeric="N", double="N", integer="I",
             Date="T", logical="B", factor="F")[sapply(dat, class)]
    rpt <- list()
    for(j in seq_len(ncol(dat))) {
        tic <- proc.time()
        n <- colnames(dat)[j]
        x <- dat[, j]
        v <- paste0(x) # NaN/NA/Inf as strings
        ## count and handle error encodings
        err <- c(ERR=sum(is.na(iconv(v))) - sum(is.na(v)))
        v <- iconv(v, sub="?")
        v <- v[!grepl(NAS, v)] # drop string typed NA
        Encoding(v) <- "bytes" # treat data as bytes
        ## prop of missing
        pms <- c(PMS = (length(x) - length(v)) / length(x))
        ## word sizes summary
        ## QSZ <- c(LMN=0, LQ1=1, LMD=2, LQ3=3, LMX=4) / 4 # min, q1, med, q3, max
        QSZ <- c(LMN=0, LMD=1, LMX=2) / 2 # min, med, max
        wsz <- quantile(nchar(v, "bytes"), QSZ)
        names(wsz) <- names(QSZ)
        ## number of unique values
        nux <- c(NUX = length(unique(v)))
        ## preview a few unique values, truncated
        val <- substr(v, 1, len) |> unique() |> rep(length.out=few)
        names(val) <- sprintf("x%02d", 1:few)
        ##
        rpt[[n]] <- data.frame(c(wsz, nux, pms, err, as.list(val)))
        tok <- proc.time()
        if(msg > 0) {
            lps <- (tok - tic)[3]
            MSG <- sprintf("[%4d/%4d] %6.3fs: %s", j, ncol(dat), lps, n)
            cat(MSG, "\n", sep="")
        }
    }
    data.frame(CLS, do.call(rbind, rpt), row.names=colnames(dat))
}

#' overlap check
#'
#' check column overlap of two data 
#'
#' @param x date 1
#' @param y data 2
#' @param ... arguments pass to [merge()] except "suffixes".
HLP$ock <- function(x, y, ...) {
  r <- merge(x, y, ..., suffixes=c(".LHS", ".RHS"))
  r <- sapply(intersect(names(x), names(y)), \(n)
  {
      a <- paste0(n, ".LHS")
      b <- paste0(n, ".RHS")
      if(!a %in% names(r) || !b %in% names(r))
          return(NULL)
      m <- r[, a] == r[, b]
      DF(VAR=n,
         NCP =  sum(!is.na(m)),
         PCP = mean(!is.na(m)),
         NEQ =  sum(m, na.rm=TRUE),
         PEQ = mean(m, na.rm=TRUE))
  })
  do.call(rbind, r)
}

#' identifiable principle components
#'
#' wrap R prcomp() to ensure maximum  positive span, so the principal components
#' become identifiable.
#'
#' @param x an N * P data matrix, or and object returned by prcomp().
#' @param y an N * Q data matrix, trigger partial least square (def=NULL)
#' @param mxp maximum positive span (i.e., enforing identifiability, def=1)?
#' @param ncomp number of components to retain (def=NULL - retain all).
#' @param center center x (def=TRUE)?
#' @param scale scale x (def=FALSE)?
#' @param ... arguments to pass to prcomp().
#' @examples
#' par(mfrow=c(1, 2))             # side by side comparison
#'
#' ## (a) default R-prcomp(), the usual PCA
#' pca <- prcomp(iris[, 1:4])
#' p00 <- predict(pca, iris[1, 1:4] * 0) # origin in the PC space
#' pcs <- pca$x
#' plot(pcs[, 1:2], main="Default R prcomp()",
#'      xlim=range(c(p00[1], pcs[, 1])), ylim=range(c(p00[2], pcs[, 2])))
#' points(p00[1], p00[2], pch=23, cex=1.5, col="red")
#'
#' ## (b) maxed positive span, identifiable-PCA
#' pca <- ipc(iris[, 1:4])
#' p00 <- pca$p00                        # origin in the PC space
#' pcs <- pca$pcs
#' plot(pcs[, 1:2], main="identifiable (maximize positive)",
#'      xlim=range(c(p00[1], pcs[, 1])), ylim=range(c(p00[2], pcs[, 2])))
#' points(p00[1], p00[2], pch=23, cex=1.5, col="red")
HLP$ipc <- function(x, y=NULL, ncomp=NULL, center=TRUE, scale=FALSE, mxp=1, ...) {
    if(inherits(x, "ipc")) { # update an existing "ipc" object
        upk(x)
    } else if(inherits(x, "prcomp")) { # build from "prcomp"
        pca <- x
        pcs <- pca$x        # score vectors (PCs)
        sdv <- pca$sdev     # SD of each score vector.
        ldv <- pca$rotation # loading vectors used
        x00 <- pca$center   # 
    } else if(is.null(y)) { # regular PCA
        x <- scale(x, center, scale)
        ncomp <- ncomp %||% min(dim(x))
        pca <- prcomp(x, rank.=ncomp, ...)
        pcs <- pca$x        # score vectors (PCs)
        sdv <- pca$sdev     # SD of each score vector.
        ldv <- pca$rotation # loading vectors used
        x00 <- pca$center   # 
    } else { # partial least square
        x <- scale(x, center, scale)
        y <- scale(y, center, scale)
        ncomp <- ncomp %||% (min(dim(x)) - min(dim(y)) - 1)
        library(mdatools) # for partial least square
        res <- mdatools:::pls.simpls(x, y, ncomp=ncomp, cv=FALSE)
        if("package:mdatools" %in% search())
            detach("package:mdatools", unload=TRUE)
        ## de facto loading
        ldv <- with(res, weights %*% solve(crossprod(xloadings, weights)))
        ldv <- ldv / sd(ldv) # better scales
        ## sdv <- sqrt(res$cal$xdecomp$expvar)
        pcs <- x %*% ldv # training data only
        sdv <- rowSums(abs(cor(pcs, cbind(x, y))))      # pasudo-sdv
        x00 <- attr(x, "scaled:center") %||% 0 * x[1, ] # original space
    }

    ## naming 
    colnames(pcs) <- sprintf("P%02X", 1:NCOL(pcs)) #
    colnames(ldv) <- colnames(pcs)
    if(!is.null(colnames(x)))
        rownames(ldv) <- colnames(x)
    names(sdv)    <- colnames(pcs)

    ## center in the original space <-> origin in the PC-score space
    p00 <- -drop(crossprod(x00, ldv))
    
    ## enforce maximum positve span (i.e., indentifiability)?
    if(mxp) {
        for(i in seq(ncol(pcs))) {
            sgn <- sign((max(pcs[, i]) - p00[i])^2 - (min(pcs[, i]) - p00[i])^2)
            ldv[, i] <- ldv[, i] * sgn
            pcs[, i] <- pcs[, i] * sgn
        }
        ## pcs <- x %*% ldv
    }
    p00 <- -drop(crossprod(x00, ldv)) # origin in PCS - updated!
    ##
    structure(list(pcs=pcs, ldv=ldv, sdv=sdv, p00=p00, x00=x00),
              class=c("ipc", "list"))
}

#' convenient indexing of PCA resluts.
#'
#' @examples
#' res <- ipc(USArrests)
#' res[c("Arkansas", "Delaware", "Ohio"), ]        # 3 states
#' res[, c(1, 3)]                                  # 2 scores
#' res[c("Arkansas", "Delaware", "Ohio"), c(1, 3)] # 2D index
`[.ipc` <- function(x, ...) {
    .i. <- match.call(expand.dots = FALSE)[["..."]]
    .f. <- as.name("[")
    .e. <- alist(,)[1] # empty arg
    ## by default, do not drop matrics to vectors.
    if("drop" %in% names(.i.)) {
        .d. <- .i.["drop"]
        .i.["drop"] <- NULL
    } else {
        .d. <- alist(drop=FALSE)
    }
    if(length(.i.) > 1) {
        call.pcs <- as.call(c(.f., as.name("pcs"), .i., .d.))
        call.ldv <- as.call(c(.f., as.name("ldv"), .e., .i.[2], .d.))
        call.sdv <- as.call(c(.f., as.name("sdv"), .i.[2], .d.))
        call.p00 <- as.call(c(.f., as.name("p00"), .i.[2], .d.))
    } else {
        call.pcs <- as.call(c(.f., as.name("pcs"), .e., .i.[1], .d.))
        call.ldv <- as.call(c(.f., as.name("ldv"), .e., .i.[1], .d.))
        call.sdv <- as.call(c(.f., as.name("sdv"), .i.[1], .d.))
        call.p00 <- as.call(c(.f., as.name("p00"), .i.[1], .d.))
    }
    x$pcs <- eval(call.pcs, x, parent.frame())
    x$ldv <- eval(call.ldv, x, parent.frame())
    x$sdv <- eval(call.sdv, x, parent.frame())
    x$p00 <- eval(call.p00, x, parent.frame())
    x
}

#' extrapolate PCA to new data
#'
#' @examples
#' res <- ipc(USArrests[+1:40])   # PCA based on the first 40 states
#' res <- predict(res, USArrests) # extrapolate to all states
predict.ipc <- function(x, ...) {
    dot <- list(...)
    if(length(dot) < 1)
        x
    else
        within(x, pcs <- scale(dot[[1]][, names(x00)], x00, FALSE) %*% ldv)
}

dim.ipc <- function(x) {
    dim(x$pcs)
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
#' @param p00 the "origin" in the PC-space
#' @examples
#' res <- ipc(USArrests)
#' p12 <- as.data.frame(res$pcs[, 1:2]) # pc1 and pc2
#' o12 <- t(res$p00[1:2])               # center in PC space
#' lbl <- kmeans(USArrests, centers=5)$cluster
#' lbl <- factor(lbl)
#' cts <- do.call(rbind, by(p12, lbl, colMeans))
#' LMT <- apply(rbind(p12, o12), 2, range) # limits in PC space
#'
#' ## default order of levels
#' plot(p12, col=lbl, xlim=LMT[, 1], ylim=LMT[, 2])
#' text(cts, labels=rownames(cts))
#' points(o12, pch=23, col="Purple", bg="Purple", cex=2)
#'
#' ## order levels by mean centroid on PC1
#' lbl <- pcl(lbl, res, mean, USE=1:1)
#' levels(lbl) <- 1:nlevels(lbl)
#' c12 <- do.call(rbind, by(p12, lbl, colMeans))
#' plot(p12, col=lbl, xlim=LMT[, 1], ylim=LMT[, 2])
#' text(c12, labels=rownames(c12))
#' points(o12, pch=23, col="Purple", bg="Purple", cex=2)
#' 
#' lbl <- pcl(lbl, res, mean, USE=1:2)
#' levels(lbl) <- 1:nlevels(lbl)
#' c12 <- do.call(rbind, by(p12, lbl, colMeans))
#' plot(p12, col=lbl, xlim=LMT[, 1], ylim=LMT[, 2])
#' text(c12, labels=rownames(c12))
#' points(o12, pch=23, col="Purple", bg="Purple", cex=2)
HLP$pcl <- function(lbl, x, FUN=median, USE=NULL, p00=NULL, ...) {
    ## if x is a PCA object, convert it to an IPC object
    pca <- if(inherits(x, "ipc")) x else ipc(x, ...)
    ##
    if(length(USE) == 0)
    USE <- USE %||% 1:ncol(pca$pcs)
    ldv <- pca$ldv[, USE, drop=FALSE]
    pcs <- pca$pcs[, USE, drop=FALSE]
    sdv <- pca$sdv[USE]
    p00 <- p00 %||% pca$p00
    p00 <- rep(p00, length.out=ncol(pca$pcs))[USE]
    ## p00 <- -drop(crossprod(pca$x00, ldv)) # origin in p-space: p0 = x0' ld
    off <- scale(pcs, p00, FALSE)         # offset from the origin
    dst <- sqrt((off^2 %*% (1/sdv^2)))    # weighted Euclidian from the origin
    ## dst <- abs(off) %*% (1/sdv)   # weighted manhattan distance
    lbl <- reorder(lbl, dst, FUN)         # levels re-ordered
    lbl
}

#' turn a number to a date with system default origin.
HLP$as_Date <- function(x, fmt=NULL, org="1900-01-01", ...) {
    if(is.character(x)) {
        if(length(fmt) < 1)
            fmt <- c("%Y-%m-%d", "%Y/%m/%d")
        ret <- as.Date.character(x, tryFormats=fmt, ...)
    } else if(is.numeric(x)) {
        if(length(org) < 1)
            org <- Sys.Date() - as.integer(Sys.Date())
        else if(grepl("^XLS", org, ignore.case=TRUE))
            org <- as.Date("1900-01-01")
        ret <- org + x
    } else {
        arg <- list(x, tryFormats=fmt, origin=org, ...)
        ret <- do.call(as.Date, arg)
    }
    ret
}

#' p-value to statistical significance by asterisks
#'
#' Use "." to highlight `p` < 0.05 and "*" the number of 0s past digit.
#' @param p p-values
#' @param d number of extra 0s to count (def=4).
HLP$pvs <- function(p, d=4) {
    as.character(cut(p, c(-1, 10^((-d:-1) - 1), 0.05, 1),
                     c(strrep("*", d:1) , ".", "")))
}

#' formula concatenate
#'
#' @param ... formulas in "LHS ~ RHS" format, allows charactor string.
#' @param env environment of the formula (def=parent.frame()).
#' @return a formula as cbind(lhs_1, lhs_2, ...) ~ rhs_1 + rhs_2 + ...
#'
#' Use "y ~ 0" to specify a stand alone LHS.
#'
#' @examples
#' baseline <- ~ age + is_female + non_white + is_hispanic # baseline
#' growth <- ~ strength_initial + strength_build * (years + I(years^2))
#' random <- ~ (1 + years + I(years^2) | child_id)
#' (model <- fcc(baseline, growth, random, autism ~ 0))
HLP$fcc <- function(..., env=parent.frame()) {
    ## drop "~" and extract left/right hand sides (LHS/RHS)
    obj <- lapply(list(...), function(.) as.character(as.formula(.))[-1])
    rhs <- mapply(`[`, obj, lengths(obj) - 0, SIMPLIFY=FALSE)
    lhs <- mapply(`[`, obj, lengths(obj) - 1, SIMPLIFY=FALSE)
    ## handle RHS
    rhs <- rhs[sapply(rhs, `!=`, "0")] # drop "0"
    rhs <- do.call(paste, c(rhs, sep=" + "))
    if(length(rhs) < 1) # use "0" if rhs is empty
        rhs <- "0"
    ## handle LHS
    lhs <- lhs[lengths(lhs) > 0] # drop empty(s)
    if(length(lhs) > 1)
        lhs <- sprintf("cbind(%s)", do.call(paste, c(lhs, sep=", ")))
    ## return "lhs ~ rhs" as a formula in the calling environment.
    as.formula(paste0(lhs, " ~ ", rhs), env)
}

#' wrapper to calculate percentile instead of quantile
HLP$percentile <- function(x, probs=seq(0, 1, 0.01)) {
    quantile(x, probs, na.rm=TRUE)
}

#' rounding numerical variables in a data frame
round.data.frame <- function(x, digits=0, minlength=digits, ...) {
    for(j in seq_along(x)) {
        if("numeric" %in% class(x[[j]])) {
            x[[j]] <- round(x[[j]], digits, ...)
        } else {
            x[[j]] <- abbreviate(x[[j]], minlength, ...)
        }
    }
    x
}

if("TCOM:HLP" %in% search())
    detach("TCOM:HLP")
attach(HLP, name="TCOM:HLP", warn.conflicts = TRUE)
rm(HLP)
