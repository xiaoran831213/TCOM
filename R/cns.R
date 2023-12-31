## TCOM Tool - helpers for CANS alike data (require "hlp.R")
CNS <- new.env()

#' abberivations in 3 letters
CNS$GLOSSARY <- 
{
    nfo <- c(
        CID = "client: unique client id.",
        UCN = "client: unique client number.",
        SEX = "client: sex-male/female, trans-male/female.",
        RAC = "client: race (EUR:White, AFR:Black, NAT:Native, ASN:Asian, RMX:mixed)",
        ETH = "client: ethnicity (NSP:None-Hispanic, HSP:Hispanic)",
        AOE = "client: age of enrollment",
        DOE = "client: date of enrollment",
        LOS = "client: total length of stay.",
        NVS = "client: number of assessment.",
        AOA = "visits: age  of assessment.",
        DOA = "visits: date of assessment.",
        DYS = "visits: days since enrollment.",
        BTW = "visits: days between visits (days since previous visit).",
        FVC = "visits: for-ward visit counter.",
        BVC = "visits: backword visit counter.",
        RSD = "visits: residence when assessed.")
    nfo
}
CNS$GLS <- CNS$GLOSSARY

#' add client-wise visit count to assessment meta-data.
#' 
#' @param cid client ID
#' @param doa date of assessments.
#' @return data.frame of visit counters
CNS$visits <- function(cid, doa)
{
    idx <- order(cid, doa) # for each client, sort by DoA.
    doa <- doa[idx]
    cid <- cid[idx]
    org <- as.Date(doa[1])            # origin of date
    idx <- seq_along(idx)[order(idx)] # restore order
    ## calculate vists
    lst <- lapply(split(as.Date(doa), cid), function(d)
    {
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
    lst <- as.data.frame(do.call(rbind, lst), row.names=cid %:% doa)
    lst <- within(lst,
    {
        doe <- as.Date(doe - doe[1], origin=org)
    })[idx, ]
    lst
}
CNS$vst <- CNS$visits

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
CNS$msp <- function(cid, doa, msp=30, ret=1)
{
    idx <- order(cid, doa) # for each client, sort by DoA.
    doa <- doa[idx]
    cid <- cid[idx]
    idx <- seq_along(idx)[order(idx)] # restore order
    ## groups
    btw <- xgf(as.Date(doa), cid, xdf)
    msk <- cid %:% SP("%03X", xgf(btw > MSP, cid, cumsum))
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
CNS$vmx <- function(val, msk, nlv=4)
{
    ret <- 0L # ratings
    ret <- ret + (rowsum(0L + (val > 0L), msk, na.rm=TRUE) > 0L)
    ret <- ret + (rowsum(0L + (val > 1L), msk, na.rm=TRUE) > 0L)
    ret <- ret + (rowsum(0L + (val > 2L), msk, na.rm=TRUE) > 0L)
    ret[!rowsum(1 - is.na(val), msk)] <- NA
    ret
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
CNS$scd <- function(val, clz, dmn, LVL=NULL)
{
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
CNS$yag <- function(x, drop=1)
{
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
