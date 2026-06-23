## TCOM Tool - helpers for CANS alike data (require "hlp.R")
CNS <- new.env()

#' abberivations in 3 letters
CNS$GLOSSARY <- local(
{
    nfo <- c(
        CID = "(cli) client id.",
        UCN = "(cli) unique client number.",
        SEX = "(cli) sex (SXM/F:male/female, TSM/F:trans-male/female.)",
        RAC = "(cli) race (EUR:White, AFR:Black, NAT:Native, ASN:Asian, OTH:Other)",
        ETH = "(cli) ethnicity (NSP:None-Hispanic, HSP:Hispanic)",
        DOB = "(cli) date of birth",
        YOB = "(cli) year of birth",
        AOE = "(cli) age of enrollment (numerical)",
        DOE = "(cli) date of enrollment",
        YOE = "(cli) year of enrollment",
        LOS = "(cli) length of stay so far.",
        NVS = "(cli) number of assessment so far.",
        AGE = "(cli) age group (cohort).",
        AYE = "(cli) age in years (cohort)",
        AID = "(asc) assessment id.",
        AOA = "(asc) age of assessment.",
        DOA = "(asc) date of assessment.",
        YOA = "(asc) year of assessment.",
        DYS = "(asc) days since enrollment.",
        BTW = "(asc) days between visits (days since previous visit).",
        FVC = "(asc) for-ward visit counter.",
        BVC = "(asc) backward visit counter.",
        RSD = "(asc) residence when assessed.")
    format(nfo, align="left")
})
CNS$GLS <- CNS$GLOSSARY

#' add client-wise visit count to assessment meta-data.
#' 
#' @param cid client ID
#' @param doa date of assessments.
#' @return data.frame of visit counters
CNS$visits <- function(cid, doa) {
    idx <- order(cid, doa) # for each client, sort by DoA.
    doa <- doa[idx]
    cid <- cid[idx]
    org <- as.Date(doa[1])            # origin of date
    idx <- seq_along(idx)[order(idx)] # restore order
    ## calculate vists
    lst <- lapply(split(as.Date(doa), cid), function(d) {
        nvs <- length(d)      # number of visits
        fvc <- 1:nvs          # forward visit
        bvc <- nvs:1          # backward visit
        doe <- rep(d[1], nvs) # date of enrollment
        btw <- c(0, diff(d))  # days between visits
        dys <- cumsum(btw)    # days so far
        los <- dys[nvs]       # len of stay
        cbind(nvs=nvs, fvc=fvc, bvc=bvc, doe=doe, btw=btw, dys=dys, los=los)
    })
    ## compile, restore date, restore order, and return
    lst <- as.data.frame(do.call(rbind, lst), row.names=cid %:% doa,
                         check.names=FALSE)
    lst <- within(lst,
    {
        doe <- as.Date(doe - doe[1], origin=org)
    })[idx, ]
    lst
}
CNS$vst <- CNS$visits
CNS$fvc <- function(cid, doa) {
    unsplit(lapply(split(as.Date(doa), cid), order, decreasing=FALSE), cid)
}
CNS$bvc <- function(cid, doa) {
    unsplit(lapply(split(as.Date(doa), cid), order, decreasing=TRUE),  cid)
}

#' Group assessments in short periods to enforce minimum separation in days.
#'
#' For each client, mask assessments occured too close to each other.
#'
#' The helper is a typicall step before collapsing events too closely located in
#' the time line, in order to meaningfully count the re-occurance of events.
#'
#' @param cid N x 1 vector of client id.
#' @param doa N x 1 Date of Assessment (YYYY-MM-DD).
#' @param msp minimum separation in days (def=30).
#' @param ret return type (def=1)
#' @return N x 1 vector of grouped DoA.
#'
#' About the return types
#' - 0 = begin_date - : - the earlist date of a group of assessment
#' - 1 = group_date - grouped_date (default)
CNS$msp <- function(cid, doa, msp=30, ret=1) {
    idx <- order(cid, doa) # for each client, sort by DoA.
    doa <- doa[idx]
    cid <- cid[idx]
    idx <- seq_along(idx)[order(idx)] # restore order
    ## groups
    btw <- xgf(as.Date(doa), cid, xdf)
    msk <- cid %:% SP("%03X", xgf(btw > msp, cid, cumsum))
    bgn <- xgf(doa, msk, `[`, 1)      # begin data
    prd <- cid %:% bgn                # periods
    ## return
    if(ret == 0)
        bgn[idx]
    else if(ret == 2)
        data.frame(prd, bgn)[idx, ]
    else if(ret == 3)
        data.frame(prd, bgn, doa, btw)[idx, ]
    else
        prd[idx]
}

#' Build pseudo-panel from longitudinal records
#'
#' This function  constructs pseudo-panels from irregularly  spaced longitudinal
#' assessments by  aligning follow-up  observations to regular  intervals (e.g.,
#' 6-month panels) within  a specified tolerance window.  Each assessment serves
#' as a baseline, and all subsequent assessments are mapped to the closest panel
#' interval. The function returns statistics,  lags, indices, and dates for each
#' baseline-to-panel mapping.
#' @param cid client identifier
#' @param doa days/dates of assessments
#' @param int desired interval in days (default: 6 months = 365.24/2)
#' @param wnd window of tolerance in days (default: ±45 days)
#' @return list with three components:
#'   \item{stt}{statistics for each assessment as a baseline with fields:
#'     \itemize{
#'       \item{CID}{client identifier, aligned with cid}
#'       \item{DOA}{date of assessment (baseline), aligned with doa}
#'       \item{NPL}{number of panels generated from this baseline}
#'       \item{MPL}{maximum panel number reached from this baseline}
#'       \item{LLG}{largest lag (in days) from any panel}
#'     }
#'   }
#'   \item{lag}{wide format matrix with lags (in days) for each panel}
#'   \item{idx}{wide format matrix with follow-up indices for each panel}
#'   \item{doa}{wide format data.frame with follow-up dates for each panel}
#' @examples
#' cid <- c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L,
#'          3L, 2L, 1L, 2L, 1L, 2L, 3L, 2L, 1L, 2L, 3L, 1L, 2L, 3L, 2L, 2L,
#'          3L, 2L, 3L, 2L, 3L, 3L, 3L, 3L, 3L, 3L)
#' cid <- c("Dave", "Olga", "Ryan")[cid]
#' doa <- as.Date(c(
#'   "2012-06-26", "2012-09-26", "2013-02-12", "2013-08-01", "2014-01-03",
#'   "2014-05-15", "2014-11-01", "2014-11-20", "2015-03-03", "2015-03-17",
#'   "2015-05-05", "2015-06-15", "2015-12-01", "2016-02-29", "2016-06-05",
#'   "2016-11-01", "2016-11-22", "2017-02-25", "2017-02-28", "2017-06-09",
#'   "2017-07-25", "2017-07-26", "2017-10-17", "2018-01-01", "2018-01-30",
#'   "2018-03-02", "2018-04-01", "2018-07-27", "2018-08-01", "2018-09-17",
#'   "2018-10-15", "2018-12-20", "2019-02-06", "2019-06-01", "2019-07-31",
#'   "2019-11-01", "2020-01-14", "2020-05-04", "2020-06-10", "2020-12-01",
#'   "2021-05-01", "2021-11-01"))
#' int <- 365.24 / 2
#' wnd <- c(-60, 60)
#' pps <- psp(cid, doa, int, wnd)
#' stopifnot(pps$stt$CID == cid, pps$stt$DOA == doa)
#' with(pps, cbind(stt, lag)) |> print(width=200)
#' with(pps, cbind(stt, doa)) |> print(width=200)
CNS$psp <- function(cid, doa, int = 365.24/2, wnd = c(-45, 45)) {
    require(reshape2)  # or use :: to avoid dependency
    ## sanity check
    stopifnot(is.numeric(doa) ||
              inherits(doa, c("POSIXlt", "POSIXt", "Date")))
    
    ## order by client then by dates
    . <- order(cid, doa)
    cid <- cid[.]            # client id
    doa <- doa[.]            # dates of assessment
    idx <- seq_along(doa)[.] # index of assessment
    stt <- list()            # statistics for each assessment
    . <- order(idx)          # restorer of the nature order of assessment

    ## ------------------------ days since baseline ------------------------- ##
    ## per-client, each assessment act as a baseline - the days since a baseline
    ## to all possible later assessments.
    dys <- mapply(function(CID, DOA, IDX) {
        M <- seq_along(DOA)             # visit counter
        BSL <- rep.int(M, rev(M))       # each assessment as a baseline
        FLW <- sequence(rev(M), M)      # later assessment as follow-up
        BSD <- DOA[BSL]                 # baselines - dates
        FLD <- DOA[FLW]                 # follow-up - dates
        BSI <- IDX[BSL]                 # index of baselines
        FLI <- IDX[FLW]                 # index of follow-up
        DYS <- as.integer(FLD - BSD)    # days since baseline
        data.frame(CID, BSI, FLI, BSD, FLD, DYS)  #
    },
    unique(cid), split(doa, cid), split(idx, cid), SIMPLIFY=FALSE)
    dys <- data.frame(do.call(rbind, dys), row.names=NULL)
    ## note: data is grouped by assessment/baseline; keep CID for debugging.

    ## ------------------------------ alignment ----------------------------- ##
    ## per-client, each assessment act as a  baseline - align every follow up to
    ## the closest panel; check the window of tolerance (WOT).
    mat <- within(dys, {
        PNL <- as.integer(round(DYS / int))  # panel closest to a follow-up
        LAG <- DYS - PNL * int               # lag since desired panel date
        WOT <- wnd[1] <= LAG & LAG <= wnd[2] # lag within wnd of tolerance?
        PNL <- 1L + PNL                      # let the panels be one based.
    })
    ## for multiple follow-ups (within window) matched to one panel, rank their
    ## closeness to the said panel.
    IxP <- with(mat, interaction(BSI, PNL)) # combo of Baseline & Panels
    mat <- within(cbind(mat, RNK=NA), {
        split(RNK, IxP) <- lapply(split(ifelse(WOT, abs(LAG), NA), IxP),
                                  rank, na.last=TRUE, ties.method="first")
        RNK[!WOT ] <- NA # do not rank out-of-window assessments
    })
    ## keep assessments (1) within the window of, and (2) closest to a panel.
    mat <- subset(mat, WOT & RNK == 1L, -c(WOT, RNK))

    ## -------------- stats for each assessment as a baseline --------------- ##
    mxm <- function(x) x[which.max(abs(x))] # helper: value of largest magnitude
    stt <- list(
        aggregate(cbind(NPL=PNL) ~ BSI, mat, length), # number of panels
        aggregate(cbind(MPL=PNL) ~ BSI, mat, max),    # maximum panel
        aggregate(cbind(LLG=LAG) ~ BSI, mat, mxm))    # largest lag
    stt <- Reduce(merge, stt)
    ## note: the records are already sorted by the original index.
    stt <- data.frame(CID=cid[.], DOA=doa[.], stt[, -1], row.names=stt[, 1])

    ## ---- convert to wide format - one row per assessment as a baseline --- ##
    lag <- acast(mat, BSI ~ PNL, value.var="LAG") # lags
    idx <- acast(mat, BSI ~ PNL, value.var="FLI") # follow up index
    doa <- acast(mat, BSI ~ PNL, value.var="FLD") # follow up dates
    ## note: records already sorted by the nature order
    if(inherits(doa, "Date")) {
        doa <- data.frame(apply(doa, 2, as.Date, simplify=FALSE))
        dimnames(doa) <- dimnames(idx)
    }
    ## note: data.frame holds dates safer than matrix

    ## ------------------------------ pack up ------------------------------- ##
    stopifnot(rownames(stt) == rownames(lag))
    stopifnot(rownames(stt) == rownames(idx))
    list(stt=stt, lag=lag, idx=idx, doa=doa)
}

#' collapse grouped CANS by maximum ratings.
#'
#' Item-wise, take the maximum CANS assessment rating by groups.
#'
#' Grouping and collapsing assessments happens during quality control, typically
#' when assessments of a client being too  close to each other in date which are
#' considered a single event.
#'
#' Such scenario may occur artificially due to data merger, or in reality due to
#' rellocation of a client in multiple places in a short period of time, e.g., a
#' child being transported into a number of facilities for treatment / check up,
#' each took a semi-duplicated CANS for record keeping.
#'
#' Th helper speed up the calculation knowing that CANS items are 4-level scores
#' coded as interger 0-3.   For each client, the collapsed rating  of an item is
#' NA when all ratings in a group are NA.
#'
#' @param val N x P matrix of CANS ratings.
#' @param msk N x 1 vector of CANS assessment group mask.
CNS$vmx <- function(val, msk, nlv=4) {
    ret <- 0L # ratings
    ret <- ret + (rowsum(0L + (val > 0L), msk, na.rm=TRUE) > 0L) # CANS = 1
    ret <- ret + (rowsum(0L + (val > 1L), msk, na.rm=TRUE) > 0L) # CANS = 2
    ret <- ret + (rowsum(0L + (val > 2L), msk, na.rm=TRUE) > 0L) # CANS = 3
    ret <- ret + (rowsum(0L + (val > 3L), msk, na.rm=TRUE) > 0L) # FAST = 4
    ret[!rowsum(1 - is.na(val), msk)] <- NA
    ret
}

#' CANS to Actionable.
#'
#' Dichotomize CANS rating in 0-4 to actionable item rating in 0/1.
#'
#' Compatable with items of only two levels (e.g., traumatic experience).
CNS$c2a <- function(x) {
    if(idf <- is.data.frame(x)) {
        x <- as.matrix(x)
        x <- matrix(as.integer(x), nrow(x), ncol(x), dimnames=dimnames(x))
    }
    msk <- is.na(x)
    x[msk] <- 0L
    x <- apply(x, 2, function(a) 0 + (a > max(a) / 2))
    x[msk] <- NA
    if(idf) {
        x <- as.data.frame(x)
    }
    x
}


#' summary by classes and domains.
#'
#' Tally the number and proportion of ratings per-level over classes and domains
#' 
#' Given a rating matrix of:
#' * L tiers of need (e.g., 0=none, 1=mild, 2=some, 3=dire),
#' * N reads (one per row) labeled with M classes,
#' * P items (one per col) grouped into Q domains,
#' tally the ratings in each level by class and domain.
#'  
#' @param val N x P matrix of L-ratings such as CANS.
#' @param clz N x 1 vector of M-classes over N reads.
#' @param dmn P x 1 vector of Q-domains over P items.
#' @param LVL L x 1 vector of L-tiers of rating (def=auto).
CNS$scd <- function(val, clz, dmn, LVL=NULL) {
    clz <- factor(clz)    # M classes over N reads
    dmn <- factor(dmn)    # Q domains over P items
    val <- as.matrix(val) # L levels of ratings
    if(is.null(LVL))
        LVL <- val |> as.vector() |> unique() |> sort() |> as.character()
    
    ## the # of hits over M classes, Q domains, and L levels.
    bxs <- list(clz=levels(clz), dmn=levels(dmn), lvl=LVL) # tally boxes
    num <- array(0, sapply(bxs, length), bxs)
    for(v in LVL)
        num[, , v] <- rowsum(0 + (val==v), clz) |> t() |> rowsum(dmn) |> t()

    ## re-arrange boxes into long table, then calculate proportions.
    rpt <- cbind(do.call(expand.grid, bxs), num=as.vector(num))
    rpt <- rpt[with(rpt, order(clz, dmn, lvl)), ]
    rpt <- within(rpt, prp <- xgf(num, clz %:% dmn, proportions))
    rpt
}

#' youth age group
#'
#' Break down age in years into commonly seen youth age groups.
#' 
#' @param x age in years
#' @param drop drop empty groups (def=1)
#' @return a R factor of age groups.
CNS$yag <- function(x, drop=1) {
    ## group age of enrollment / assessment
    lvl <- c("<0DAY"=-1, "0M-1Y"=1, "1Y-5Y"=5, "6Y-8Y"=8, "9Y-12"=12,
             "13-17"=17, "18-21"=21, "22Y&+"=Inf)
    x <- cut(x, c(-Inf, lvl), labels=names(lvl))
    if(drop)
        x <- factor(x)
    x
}

if("TCOM:CNS" %in% search())
    detach("TCOM:CNS")
attach(CNS, name="TCOM:CNS", warn.conflicts=FALSE)
rm(CNS)
