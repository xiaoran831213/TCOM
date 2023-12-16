## TCOM Tool related helpers
HLP <- new.env()

#' assign if not already exists
HLP$"%:-%" <- function(x, y) 
{
    Var <- deparse(substitute(x))
    if (!exists(Var, parent.frame()) || length(x) == 0L)
    {
        assign(Var, y, parent.frame())
    }
    invisible(NULL)
}

#' y if x is none.
HLP$"%||%" <- function(x, y) if(length(x)) x else y

#' x if not y.
HLP$"%&!%" <- function(x, y) if(y) NULL else x

#' x if y.
HLP$"%&&%" <- function(x, y) if(y) x else NULL


#' make and return deep directories without warning
HLP$mkdir <- function(...)
{
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
HLP$cache <- function(.rds, .rxp, over=0, here=0, pack=0)
{
    .out. <- parent.frame() # outside
    env <- new.env()      # sandbox
    if(file.exists(.rds) && !over) {
        ret <- readRDS(.rds)
    } else {
        ret <- eval(substitute(.rxp), env, .out.)
        if(pack)
            ret <- as.list(env)
        saveRDS(ret, .rds)
    }
    if(here == 0)
    {
        for(n in names(ret))
            assign(n, ret[[n]], .out.)
    }
    invisible(ret)
}

#' flood objects from a list into an environment
#'
#' Unpack objects from a list into an environment.
#'
#' @param pck the container of newer objects.
#' @param env the environment (def=<caller>).
HLP$upk <- function(pck, env=NULL, vbs=0)
{
    env %:-% parent.frame()
    enm <- environmentName(env)
    pnm <- as.character(substitute(pck))
    if(vbs)
        PL("unpack obj from \"%s\" into env [%s]:", pnm, enm)
    for(. in names(pck))
    {
        env[[.]] <- pck[[.]]
        if(vbs)
            PL("- %s: %7d x %5d, %16s", ., NROW(env[[.]]), NCOL(env[[.]]),
               format(as.integer(object.size(env[[.]])), big.mark=","))
    }
    invisible(NULL)
}

#' Repack objects from an environment into a list.
#'
#' @param pck the container of older objects.
#' @param env the environment (def=<caller>).
HLP$rpk <- function(pck, env=NULL)
{
    env %:-% parent.frame()
    enm <- environmentName(env)
    ret <- list()
    for(. in intersect(names(pck), names(env)))
        ret[[.]] <- env[[.]]
    ret
}

#' Write TSV file.
#'
#' A wrapper of  [write.table()] that always uses "\t" as  separator, by default
#' uses no quotation and blank for NA.
HLP$write.tsv <- function(x, f, quote=FALSE, na="", ...)
{
    #' wrapper of write.table(...)
    #' - sep="\t",
    #' - row.names=FALSE,
    #' - na="",
    #' - quote=FALSE
    write.table(x, f, quote=quote, sep="\t", na=na, row.names=FALSE, ...)
}
HLP$saveTSV <- HLP$Pwrite.tsv

#' Read TSV file.
#'
#' A wrapper of [read.delim()], use no quotation and treat blank as NA.
HLP$read.tsv <- function(f, ...)
{
    #' wrapper of read.delim(...)
    #' - na.strings="",
    #' - quote=NULL,
    #' - check.names=FALSE
    read.delim(f, na.strings="", quote=NULL, check.names=FALSE, ...)
}
HLP$readTSV <- HLP$read.tsv

#' split vector x by group g, apply function f, then unsplit.
HLP$xgf <- function(x, g, f, ...) unsplit(lapply(split(x, g), f, ...), g)

#' split {x} by {g}, tabluate unique values.
#'
#' When {g} is NULL, xgf() degenrated to a dummy variable encoder.
#' @param na TRUE to count NA (def=1), or a string to name the count.
HLP$xgt <- function(x, g=NULL, na=NULL)
{
    x <- factor(x, unique(x))
    r <- 0 + outer(x, levels(x), `==`)
    r[is.na(x)] <- NA
    colnames(r) <- levels(x)
    rownames(r) <- names(x)
    if(!is.null(na) && na != FALSE)
    {
        if(na == TRUE)
            na <- NA
        r <- cbind(r, matrix(is.na(x), dimnames=list(NULL, na)))
    }
    if(length(g))
        r <- rowsum(r, g, na.rm=TRUE)
    r
}

#' formated tally of counts and proportions of unique values.
#'
#' A wrapper of R function [table()].
#'
#' @param ... variable to calculate proportions.
#' @param mrg.prp margins to add to proportions
#' @param mrg.sum margins to add to sum(s)
#' @param mrk marker (def=","), the thousand separator.
HLP$ppf <- function(..., mrg.prp=NULL, mrg.sum=NULL, rnd=1)
{
    ## tabulate
    num <- table(...)
    prp <- proportions(num, mrg.prp)
    cls <- class(num)
    if(!is.null(mrg.sum))
    {
        num <- addmargins(num, mrg.sum)
        prp <- addmargins(prp, mrg.sum)
    }
    att <- attributes(num)
    ## thousand mark and percentage
    num <- format(num, big.mark=",")
    prp <- paste0(format(round(prp * 100, rnd)), "%")
    ## output
    res <- sprintf("%s(%s)", num, prp)
    attributes(res) <- att
    res
}

#' lengh of unique values in {x}
HLP$lux <- function(x, na.rm=FALSE)
{
    #' length of unique values
    if(na.rm)
        x <- x[!is.na(x)]
    length(unique(x))
}

#' difference between values in {x} padded with an initial value.
#'
#' A wrapper of [diff()] which pads an  initial value (def=0) to the results, so
#' it has the same length with {x}.
HLP$xdf <- function(x, ini=0) c(ini, diff(x))

#' append data frame into another data frame
HLP$append.df <- function(x, b, i=length(b))
{
    if(is.character(i))
        i <- pmatch(i, names(x), length(x))
    data.frame(append(x, b, i), row.names=rownames(x))
}

#' emulated printf
HLP$PF <- function(fmt, ...)
{
    msg <- if(missing(fmt)) "" else sprintf(fmt, ...)
    cat(msg)
    invisible(c(msg, ..., recursive=FALSE))
}

#' emulated printf with new line
HLP$PL <- function(fmt, ...)
{
    msg <- if(missing(fmt)) "" else sprintf(fmt, ...)
    cat(format(msg), sep="\n")
    invisible(c(msg, ..., recursive=FALSE))
}

#' short hand for sprintf
HLP$SP <- sprintf

#' short hand for file.path
HLP$FP <- file.path

#' short hand for data.frame
HLP$DF <- data.frame

#' compactaly list elements in a container
HLP$LL <- function(x, fmt=1)
{
    rpt <- data.frame(name=names(x),
                      nrow=sapply(x, NROW),
                      ncol=sapply(x, NCOL),
                      size=as.integer(sapply(x, object.size)),
                      row.names=NULL)
    if(fmt)
        rpt <- format(rpt, justify="right", big.mark=",")
    rpt
}


#' praint a horizontal line
#'
#' @param chr char to fill up the line.
#' @param bgn beginning pattern
#' @param end ending pattern
#' @param ttl title in between.
#' @param len length in num of Char or % of screen width.
HLP$HL <- function(chr="-", bgn="##", end="##", ttl=NULL, len=NULL, sep=NULL)
{
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

#' helper: two class confusion matrices
HLP$cfx <- function(ref, est) # confusion related
{
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
HLP$y2q <- function(x)
{
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
HLP$wck <- function(dat, few=4, len=10, nas=NULL)
{
    if(is.null(nas))
        nas <- c("", "NA", "N/A", "NULL", NA)
    CLS <- c(character="C", numeric="N", double="N", integer="I",
             Date="T", logical="B", factor="F")[sapply(dat, class)]
    rpt <- sapply(dat, \(x)
    {
        ## v <- iconv(a, "latin1", "UTF-8")
        v <- as.character(x)
        v <- v[!(is.na(v) | toupper(enc2native(v)) %in% nas)] # non-NA
        Encoding(v) <- "bytes"
        ## prop of missing
        pms <- c(PMS = round((length(x) - length(v)) / length(x), 3))
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
        c(wsz, nux, pms, val)
    }) |> t()
    data.frame(CLS, rpt, check.names=FALSE)
}

#' identifiable principle components
#'
#' 
#' wrap  R's  prcomp()  to  ensure  maximum  positive  span,  so  the  principal
#' components become identifiable.
#'
#' @param x data matrix to apply IPC.
#' @param mxp ensure positive maximum span? (def=1)
#' @param ... arguments to pass to prcomp().
HLP$ipc <- function(x, mxp=1, ...)
{
    pca <- prcomp(x, ...)
    pcs <- pca$x
    ldv <- pca$rotation
    sdv <- pca$sdev
    cnt <- pca$center
    colnames(pcs) <- sprintf("P%02X", 1:ncol(pcs))
    colnames(ldv) <- colnames(pcs)
    names(sdv)    <- colnames(pcs)
    if(mxp)
    {
        for(i in seq(ncol(pcs)))
            ldv[, i] <- ldv[, i] * sign(max(pcs[, i])^2 - min(pcs[, i])^2)
        pcs <- scale(x, cnt, FALSE) %*% ldv
    }
    structure(list(pcs=pcs, ldv=ldv, sdv=sdv, cnt=cnt), class=c("ipc", "list"))
}

#' identifiable class labels
#'
#' A wrapper of R's [reorder()] to ensure consistant order of factor levels with
#' principle components or any weighted coordinates.
#'
#' The order of  a level is dertermined by the  *aggregated proximity* of points
#' in that level to the origin of the space defined by `pcs`, with dimensions in
#' `pcs` weighted by `sdv`.
#'
#' By default, *proximity* to the origin is measured by Euclidian distance while
#' *aggregation* is done by passing [FUN=median()] to [reorder()].
#'
#' @param lbl {n} points labeled by factor levels.
#' @param pcs principle components or coordinates of label point.
#' @param sdv explanable standard deviations or dimension weights.
#' @param FUN function to calculated an aggregated proximity.
HLP$icl <- function(lbl, pcs, ldv, sdv, cnt, FUN=median)
{
    dst <- sqrt((pcs^2 %*% (1/sdv^2)))
    lbl <- reorder(lbl, dst, FUN)
    lbl
}
HLP$ilv <- HLP$icl

#' principal component label
#'
#' A wrapper of R's [reorder()] to ensure consistant order of factor levels with
#' principle components or any weighted coordinates.
#'
#' The order of  a level is dertermined by the  *aggregated proximity* of points
#' in that level to the origin of the space, defined by the projection of [0] to
#' the `pcs` space, with dimensions in inversely weighted by `sdv`.
#'
#' By default, *proximity* to the origin is measured by Euclidian distance while
#' *aggregation* is done by passing [FUN=median()] to [reorder()].
#'
#' @param lbl {n} points labeled by factor levels.
#' @param pcs principle components or coordinates of label point.
#' @param ldv PC vactor loadings or ratation matrix.
#' @param sdv explanable standard deviations or dimension weights.
#' @param cnt center of the original dimensions
#' @param FUN function to calculated an aggregated proximity.
HLP$pcl <- function(lbl, pcs, ldv, sdv, cnt, FUN=median)
{
    org <- drop(ldv %*% cnt)
    pcs <- scale(pcs, -org, FALSE) # anti-center
    dst <- sqrt((pcs^2 %*% (1/sdv^2)))
    lbl <- reorder(lbl, dst, FUN)
    lbl
}
HLP$ilv <- HLP$icl

#' turn a number to a date with system default origin.
HLP$as_Date <- function(x, fmt=NULL, org=NULL, ...)
{
    if(is.character(x))
    {
        if(length(fmt) < 1)
            fmt <- c("%Y-%m-%d", "%Y/%m/%d")
        ret <- as.Date.character(x, tryFormats=fmt, ...)
    }
    else if(is.numeric(x))
    {
        if(length(org) < 1)
            org <- Sys.Date() - as.integer(Sys.Date())
        ret <- org + x
    }
    else
    {
        arg <- list(x, tryFormats=fmt, origin=org, ...)
        ret <- do.call(as.Date, arg)
    }
    ret
}

#' wrapper to calculate percentile instead of quantile
HLP$percentile <- function(x, probs=seq(0, 1, 0.01))
{
    quantile(x, probs, na.rm=TRUE)
}

if("TCOM:HLP" %in% search())
    detach("TCOM:HLP")
attach(HLP, name="TCOM:HLP", warn.conflicts = TRUE)
rm(HLP)
