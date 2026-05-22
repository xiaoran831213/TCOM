## TCOM Tool related figures
library(ggplot2)
library(cowplot)
library(fcp)
FIG <- new.env()

#colorblind-friendly color palette
CBF <- c("#0072B2","#9467BD","#E69F00", "#D55E00","#CC79A7", "#E69F00",
         "#009E73", "#56B4E9")

#' wrapper of color scales
FIG$.cs <- function(v, n="hue", aes="fill", dct=NULL, ttl=waiver(), dir=1, nav="grey50")
{
    if("Scale" %in% class(n))
        return(n)
    ## handel breaks
    brk <- names(table(v)) # preserve the order of factor levels
    if(is.numeric(v))
        brk <- as.numeric(brk)

    ## handle dictionary translation
    if(length(dct) < 1)
        dct <- waiver()
    else
    {
        .dc <- brk; names(.dc) <- brk
        .dc[brk %in% names(dct)] <- dct[brk[brk %in% names(dct)]]
        dct <-  .dc
    }
    
    ## use ggplot's Hue or Brewer?
    n %:-% "0"
    if(n == "hue")
    {
        ret <- scale_color_hue(
            breaks=brk, labels=dct, na.value = nav, name=ttl, drop=0,
            direction=dir, aesthetics=aes)
    }
    else if(substr(n, 1, 1) == "g") # gr[0-9][0-9] gray scale
    {
        a <- substr(n, 2, 2)                                  # start
        a <- ifelse(grepl("[0-9]", a), as.integer(a)/10, 0.2) # 
        b <- substr(n, 3, 3)                                  # end
        b <- ifelse(grepl("[0-9]", b), as.integer(b)/10, 1-a) # 
        if(dir < 0)
            swp(a, b)
        ret <- scale_color_grey(
            start=a, end=b,
            breaks=brk, labels=dct, na.value=nav, name=ttl, aesthetics=aes)
    }
    else if(substr(n, 1, 1) == "b") # brewer: bd#, bq# or bs#
    {
        pal <- suppressWarnings(as.integer(substr(n, 3, 3))) # palette number
        typ <- c(d="div", q="qua", s="seq")[substr(n, 2, 2)] # palette type
        ret <- scale_colour_brewer(
            breaks=brk, labels=dct, na.value=nav, name=ttl, drop=0,
            type=typ, palette=pal, direction=dir, aesthetics=aes)
    }
    else if(substr(n, 1, 2) == "st") # steps: st#
    {
        if(substr(n, 3, 3) <= 1) # single gradient
        {
            ## by default, put lighter color ahead
            a <- formals(scale_color_gradient)$high # lighter color
            b <- formals(scale_color_gradient)$low  # darker color
            if(dir < 0)
                swp(a, b) 
            ret <- scale_color_steps(
                low=a, high=b,
                breaks=brk, labels=dct, na.value=nav, name=ttl, aesthetics=aes)
        }
        else # with mid-point
        {
            ret <- scale_color_steps2(
                breaks=brk, labels=dct, na.value=nav, name=ttl, aesthetics=aes,
                midpoint=median(v))
        }
    }
    else if(substr(n, 1, 1) == "v") # viridis_(c|d)([a-e]|[A-H])
    {
        opt <- substr(n, 3, 3)
        ## lighter color come first, unless opt is upper case
        a <- formals(scale_colour_viridis_c)$end   # lighter color
        b <- formals(scale_colour_viridis_c)$begin # darker color
        if(grepl("[A-Z]", opt))                  
            swp(a, b)
        if(substr(n, 2, 2) == "c")
        {
            ret <- scale_colour_viridis_c(
                begin=a, end=b, na.value=nav,
                direction=dir, option=toupper(opt), aesthetics=aes)
        }
        else
        {
            ret <- scale_colour_viridis_d(
                begin=a, end=b, na.value=nav,
                direction=dir, option=toupper(opt), aesthetics=aes)
        }
    }
    else
    {
        if(is.numeric(v)) # default for numeric x
        {
            ## lighter color come first (against ggplot default)
            a <- formals(scale_color_gradient)$high # lighter color
            b <- formals(scale_color_gradient)$low  # darker color
            if(dir < 0)                             # realize direction
                swp(a, b)
            ret <- scale_color_gradient(low=a, high=b, aesthetics=aes)
        }
        if(is.ordered(v)) # default for ordinal x
        {
            ## lighter color come first (against ggplot default)
            ret <- scale_colour_viridis_d(
                begin=1, end=0, na.value=nav,
                direction=dir, option="G", aesthetics=aes)
        }
        else # default for categorical x
        {
            ret <- scale_color_hue(
                breaks=brk, labels=dct, name=ttl, drop=0,
                direction=dir, aesthetics=aes)
        }
    }
    ret
}

#' default theme
FIG$.th <- function(lgp=c("none", "left", "right", "bottom", "top"))
{
    ## place the legend
    if(length(lgp) < 1) # empty
        lgp <- "none"
    if(is.numeric(lgp))    # inside the plotting frame
        lgp <- c(lgp, rep(0, 2 - length(lgp)))[1:2]
    if(is.character(lgp))
        lgp <- match.arg(lgp[1], c("none", "left", "right", "bottom", "top"))
    e <- theme_bw()

    ## plot title inside the plot
    h1 <- e$text$size * as.numeric(e$plot.title$size) - e$text$lineheight # - e$line$linewidth
    e <- e + theme(plot.title=element_text(hjust=0.5, vjust=.5, margin=margin(b=-h1)))
    
    ## axis title inside the plot
    h1 <- e$text$size                                # height of title
    h2 <- as.numeric(e$text$size * e$axis.text$size) # height of text
    h3 <- as.numeric(e$axis.ticks.length)            # length of ticks
    h4 <- as.numeric(e$line$linewidth)               # line width
    e <- e + theme(axis.title.x=element_text(margin=margin(t=-h1-h2-h3, b=+h2+h3),
                                             vjust=1.0))
    e <- e + theme(axis.title.y=element_text(margin=margin(l=h2+h3+h4, r=-h1-h2-h3-h4),
                                             vjust=0.0))
    
    ## other themes
    e <- e + theme(legend.position=lgp, legend.justification=lgp)
    if(is.numeric(lgp))
        e <- e + theme(legend.background = element_rect(color="black", linewidth=rel(0.5)))
    e <- e + theme(legend.margin = margin(1, 1, 1, 1))
    e <- e + theme(legend.box.margin = margin(0, 0, 0, 0))
    e <- e + theme(axis.text.y = element_text(angle=90, hjust=.5))
    ## e <- e + theme(plot.margin = margin(1, 1, 1, 1))
    e <- e + theme(plot.margin = margin(0, 0, 0, 0))
    e <- e + theme(strip.text=element_blank())
    e <- e + theme(panel.spacing=unit(0, "pt"))
    e <- e + theme(panel.spacing=unit(0, "pt"))
    e
}

#' fortified data for ploting
FIG$.fd <- function(x, y=NULL, z=NULL, a=NULL)
{
    ## assign default values
    y %:-% x
    z %:-% "ALL"
    a %:-% 1.0
    ## retain label z
    lvl <- levels(z)
    ## drop data frame or matrix format
    x <- drop(unlist(x))
    y <- drop(unlist(y))
    z <- drop(unlist(z))
    a <- drop(unlist(a))
    ## retore label z
    if(length(lvl))
        z <-factor(z, lvl)
    ## 
    N <- max(length(x), length(y), length(z), length(a))
    x <- rep(x, length.out = N)
    y <- rep(y, length.out = N)
    z <- rep(z, length.out = N) |> as.factor()
    a <- rep(a, length.out = N)
    ## combine
    data.frame(x, y, z, a)
}

#' X and Y as dots on 2D painted by Z.
#'
#' @param x x-axis of n samples
#' @param y y-axis of n samples
#' @param z labels of n samples (categorical).
#' @param a alpha-value (transparency).
#' @param lgp legend position (def="none")
#' @param fxy flip x and y axis (def=0).
#' @param ttl figure title (def=NULL, use symbol or column name of z)
#' @param xlb x-axis label (def=NULL, use symbol or column name of x)
#' @param ylb y-axis label (def=NULL, use symbol of column name of y)
#' @param zcl color scheme to paint labels (def="hue")
#' @param c2d width and opacity of contour 2D (def=c(0, 0))
#' @param dct label dictionary to translate legend and text.
#' @param ann label annotation to show up in the figure.
#'
#' `lgp` specifies the position of legend,  can be of c("none", "left", "right",
#' "bottom", "top") outside of the plotting area,  or two numbers from c(0, 0) -
#' bottom-left to c(1, 1) top-right inside of the plotting area.
FIG$xoy <- function(x, y, z=NULL, a=NULL, lgp=NULL, fxy=0, ttl=NULL, zcl="hue",
                    zsh=NULL, c2d=0, dct=NULL, ann=NULL, xlb=NULL, ylb=NULL, ...)
{
    ## ------------ fix data object and retrieve their symbols ------------- ##
    xlb %:-% names(eval(call("DF", substitute(x)), parent.frame()))
    ylb %:-% names(eval(call("DF", substitute(y)), parent.frame()))
    upk(.fd(x, y, z, a)) # unpack fixed x, y, z, and a
    
    ## ---------------------------- make figure ---------------------------- ##
    ttl %:-% ""
    csz <- .cs(z, zcl, "color", dct) # color scheme
    crd <- coord_flip() %&&% fxy     # flip x and y
    ## draw contour 2D?
    if(length(c2d) && c2d[1] > 0)
    {
        c2d <- rep(c2d, 2)
        gsz <- unsplit(lapply(split(z, z), length), z) # group size
        ## to ensure bandwidth.nrd "normally"  guessed the bandwidth, (1) break
        ## ties in x and y, (2) remove tiny and NA groups.
        c2d <- geom_density_2d(
            aes(x=jitter(x, sd(x) / 100, 0),
                y=jitter(y, sd(y) / 100, 0), group=z),
            data = data.frame(x, y, z)[!is.na(gsz) & gsz > 5, ], 
            linewidth=c2d[1], alpha=c2d[2], color="gray40",
            contour_var="ndensity")
    }
    else
        c2d <- NULL
    ## annotate?
    if(length(ann))
    {
        ax <- tapply(x, z, mean)
        ay <- tapply(y, z, mean)
        al <- names(ax)
        if(is.character(ann) && length(ann) >= length(al))
            al <- ann[al]
        ann <- annotate("text", x=ax, y=ay, label=al, hjust=.5, vjust=.5,
                        na.rm=TRUE)
        ## "na.rm = TRUE" handles empty classes due to merging
    }
    else
        ann <- NULL

    ## use ggplot
    if(nlevels(z) == 1 && levels(z) == "ALL")
    {
        geo <- geom_point(alpha=mean(a))
    }
    else
    {
        geo <- geom_point(aes(color=z, shape=zsh), alpha=mean(a))
    }
    g <- ggplot(mapping=aes(x, y)) + geo
    g <- g + csz + crd + c2d + ann
    g <- g + labs(x=xlb, y=ylb, title=ttl, color=NULL) + guides(alpha="none")
    g <- g + .th(lgp)
    ## return
    invisible(g)
}

## histogram
FIG$hst <- function(x, y, z=NULL, a=NULL, lgp=NULL, fxy=0, ttl=NULL)
{
    ttl %:-% ""
    x <- if(fxy) y else x   # y as x?
    pdt <- .fd(x, z=z, a=a) # plot data
    map <- aes(x=x)         # main aesthetics
    hue <- NULL             # color hue
    crd <- if(fxy) coord_flip() else NULL
    fct <- NULL
    z <- pdt$z
    if(!is.null(z))
    {
        map <- c(map, aes(fill=z))
        hue <- scale_fill_hue(breaks=levels(z), drop=FALSE)
        if(fxy == 0)
            fct <- facet_wrap(vars(z), ncol=1)
        if(fxy == 1)
            fct <- facet_wrap(vars(z), nrow=1)
    }
    class(map) <- "uneval"
    ## ---- make figure ----
    g <- ggplot(pdt, map) + hue + crd + fct
    g <- g + geom_histogram(bins=30)
    g <- g + labs(x=NULL, y=NULL, fill=NULL) + guides(alpha="none")
    ## ---- theme ----
    g <- g + .th(lgp)
    ## print
    invisible(g)
}

#' one dimensional density plot
#' @param x x-axis of n samples, act as support if fxy=0, otherwise unused.
#' @param y y-axis of n samples, act as support if fxy=1, otherwise unused.
#' @param z labels of n samples (categorical).
#' @param a alpha-value (transparency).
#' @param lgp legend position (bottom-left [0,0] to top-right [1,1], def=NULL).
#' @param fxy flip x and y (def=0, x is the support, y is unused).
#' @param ttl figure title (def=<symbol/hearder of x if fxy=0 or y if fxy=1>)
#' @param xlb x-axis label for support if fxy=0 (def="").
#' @param ylb y-axis label for support if fxy=1 (def="").
#' @param dlb density label (def="")
#' @param zcl color scheme to paint labels (def="hue").
FIG$dst <- function(x, y, z=NULL, a=NULL, lgp=NULL, fxy=0, ttl=NULL, zcl="hue",
                    xlb=NULL, ylb=NULL, dlb=NULL)
{
    ## -------------------------- retain symbols --------------------------- ##
    X %:-% names(eval(call("DF", substitute(x)), parent.frame()))
    Y %:-% names(eval(call("DF", substitute(y)), parent.frame()))
    ## -------------------------- fix data objects ------------------------- ##
    upk(.fd(x, y, z, a)) # unpack fixed x, y, z, and a
    ## ---------------------------- make figure ---------------------------- ##
    ttl %:-% ""
    crd <- NULL
    if(fxy) # flip x and y?
    {
        crd <- coord_flip()
        swp(x, y)
        swp(X, Y)
        swp(xlb, ylb)
    }
    ttl %:-% X # symbol/header of support as default title
    ylb <- dlb # density label as y-axis label.
    cs1 <- .cs(z, zcl, "fill")  # color to fill
    cs2 <- .cs(z, zcl, "color") # color to draw
    ## the main aesthenics is density
    if(nlevels(z) == 1 && levels(z) == "ALL")
    {
        geo <- geom_density(fill="black", alpha=mean(a))
    } else {
        geo <- geom_density(aes(color=z, fill=z), alpha=mean(a))
    } 
    ## compose ggplot
    g <- ggplot(NULL, aes(x=x)) + geo
    g <- g + cs1 + cs2 + crd
    g <- g + labs(x=xlb, y=ylb, fill=NULL, color=NULL, title=ttl) +
        guides(alpha="none")
    g <- g + .th(lgp)
    ## print
    invisible(g)
}

#' bar chart
#'
#' @param x (not used).
#' @param y (not used).
#' @param z class label.
#' @param zcl color scale for class label.
FIG$bar <- function(x, y, z=NULL, a=NULL, lgp=NULL, fxy=0, ttl=NULL, dct=NULL,
                    zcl="hue", xlb = NULL, ylb=NULL, pfm=NULL)
{
    ## x <- if(fxy) y else x                    # y as x?
    ## a <- if(is.null(a)) 1 else a             # alpha
    ## pdt <- .fd(x, z=z, a=a)                  # plot data
    ## a <- mean(pdt$a)                         # alpha of all bars
    upk(.fd("0", "0", z, a))
    ttl %:-% ""
    pfm %:-% ifelse(fxy, "# (%)", "#\n%")
    
    ## coordinate flip
    crd <- NULL
    if(fxy)
    {
        crd <- coord_flip()
        ## z <- factor(z, rev(levels(z)))
    }
    
    ## plotting data for counts, proportions, and percentages
    num <- table(z)
    prp <- proportions(num, margin=NULL)
    num <- as.data.frame(num, responseName="num") |> na.omit()
    prp <- as.data.frame(prp, responseName="prp") |> na.omit()
    pdt <- within(merge(num, prp),
    {
        cnt <- local(
        {
            ## pct <- sprintf("%.0f%%", prp * 100)
            hid <- prp < 0.01 # small values will be hidden
            pct <- format(sprintf("%.0f%%", prp * 100))
            num <- format(num, trim=TRUE, big.mark=",")
            prp <- format(round(prp, 2))
            lbl <- rep(pfm, length(pct))
            for(. in seq_along(lbl))
            {
                lbl[.] <- sub("#", num[.], lbl[.], fixed=TRUE) # "#" -> num
                lbl[.] <- sub("%", pct[.], lbl[.], fixed=TRUE) # "%" -> pct
                lbl[.] <- sub(".", prp[.], lbl[.], fixed=TRUE) # "." -> prp
            }
            lbl[hid] <- ""
            lbl
        })
    })

    ## customized y-axis breaks (counts) and labels
    ysc_lmt <- function(l, ...)
    {
        e <- l + diff(l) * c(-0.05, +0.05)            # default expansion
        b <- labeling::extended(e[1], e[2], m=5, ...) # default breaks
        ## is the upper limit too close to the last break?
        if(abs(e[2] - max(b)) / diff(b)[1] < 0.05)
            l[2] <- l[2] + diff(b)[1] * 0.1 # grow the limit a bit
        l
    }
    ysc_lbl <- function(b, ...) # show numbers as K, M, B, and T
    {
        U <- 10^c(K=3, M=6, B=9, T=12) # number unit
        u <- U[which.max(max(b, na.rm=TRUE) >= U)] # choose unit
        sprintf("%s%s", format(b / u), names(u))
    }
    
    ## plot elements
    ymx <- with(pdt, max(num)) # y-axis top
    if(!is.null(z))
    {
        map <- aes(x=z, y=num, fill=z) #
        ## csz <- .cs(z, zcl, "fill", dir=ifelse(fxy, -1, 1)) # fill by z (labels)
        csz <- .cs(z, zcl, "fill") # fill by z (labels)
        geo <- geom_col(alpha=mean(a), linewidth=1)
    }
    else
    {
        map <- aes(x=z) # aesthetics
        csz <- NULL
        geo <- geom_col(alpha=mean(a), linewidth=1, fill="gray50")
    }
    
    ## annotation and text
    ann <- NULL
    if(length(dct) && dct != "0")
    {
        mis <- levels(z)[!levels(z) %in% names(dct)] # missed entry
        names(mis) <- mis
        ann <- c(dct[levels(z) %in% names(dct)], mis)[levels(z)]
        ann <- annotate("text", x=names(ann), y=ymx, label=ann, hjust=1, vjust=.5,
                        angle=90 * (1 - fxy), lineheight=.8, family="mono")
    }
    txt <- geom_text(aes(label=cnt, y=0), hjust=0.5 * (1 - fxy), vjust=0.5 * fxy,
                     lineheight=.8) # , angle=90 * (1 - fxy))
    ## ---- make figure ----
    g <- ggplot(pdt, map) + csz + crd
    g <- g + geo + ann + txt
    g <- g + scale_y_continuous(limits=ysc_lmt, labels=ysc_lbl)
    g <- g + labs(x=xlb, y=ylb, fill=NULL, title=ttl) + guides(alpha="none")
    g <- g + .th(lgp)
    ## print
    invisible(g)
}

#' column plot by group
#'
#' wrapper for ggplot geom_col(), suitable for showing loading vector.
#' @param x values (i.e., loading for PC1).
#' @param y values (i.e., loading for PC2).
#' @param z group label (not used)
FIG$clp <- function(x, y, z=NULL, a=NULL, lgp=NULL, fxy=0, ttl=NULL, dct=NULL, zcl="bs2", tsz=NULL)
{
    if(is.null(names(x)))
        names(x) <- seq_along(x)
    if(is.null(names(y)))
        names(y) <- seq_along(y)
    x <- if(fxy) y else x                    # y as x?
    ## specially treated plot data
    if(is.numeric(z) && length(unique(z)) > 8)
        z <- cut(z, breaks=8, ordered=TRUE)
    if(is.null(a))
        a <- 1.0
    pdt <- .fd(x=factor(names(x), names(x)), y=x, z=z, a=a)

    ## labeling and annotation
    ymx <- max(0, pdt$y, na.rm=TRUE)
    ymn <- min(0, pdt$y, na.rm=TRUE)
    if(!is.null(dct) && is.null(names(dct)))
        names(dct) <- seq_along(dct)
    ann <- with(pdt, dct[match(levels(x), names(dct), 0)])
    ann <- annotate("text", x=names(ann), y=ymx, label=ann, family="mono",
                    hjust=1, vjust=.5, angle=90 * (1 - fxy), lineheight=.8)
    ann$aes_params$size <- tsz
    ##
    txt <- geom_text(aes(label=format(round(y, 2), digits=1), y=ymn),
                     hjust=0.5 * (1 - fxy), vjust=0.5, angle=90 * (1 - fxy),
                     lineheight=.8)
    txt$aes_params$size <- tsz
    
    ## figure elements
    map <- aes(x=x, y=y) # common aesthetics
    crd <- if(fxy) coord_flip() else NULL
    if(is.null(a))
        a <- mean(pdt$a) # alpha of all bars
    if(!is.null(z))
    {
        map <- c(map, aes(fill=z))
        csz <- .cs(z, zcl, "fill") # fill by z (labels)
        geo <- geom_col(alpha=a, linewidth=1)
    }
    else
    {
        csz <- NULL
        geo <- geom_col(alpha=a, linewidth=1, fill="gray50")
    }

    class(map) <- "uneval"
    ## ---- make figure ----
    g <- ggplot(pdt, map) + csz + crd
    g <- g + scale_x_discrete(labels=function(x) rep("", length(x)))
    g <- g + geo + txt + ann
    g <- g + labs(x=NULL, y=NULL, fill=NULL, title=ttl) + guides(alpha="none")
    g <- g + .th(lgp)
    ## print
    invisible(g)
}

#' box plot by group
#'
#' @param x values, use when fxy=0, otherwise unused
#' @param y values, use when fxy=1, otherwise unused
#' @param z group label.
#' @param zcl color scheme for z (group labels).
FIG$bxp <- function(x, y, z=NULL, a=NULL, lgp=NULL, fxy=0, ttl=NULL, zcl="hue")
{
    x <- if(fxy) y else x    # y as x?
    pdt <- .fd(x, z=z, a=a)  # plot data
    map <- aes(x=z, y=x)     # aesthetics: x-axis <- class; y-axis <- value
    crd <- if(fxy) coord_flip() else NULL

    ## colors and graphical elements
    a <- if(is.null(a)) mean(pdt$a) else mean(a) # one alpha for entire figure
    map <- c(map, aes(fill=z, color=z))
    fcs <- FIG$.cs(z, zcl, "fill")          # fill colour: z-class
    lcs <- FIG$.cs(z, zcl, "color")         # line colour: z-class
    geo <- geom_boxplot(alpha=a)
    ebr <- stat_boxplot(geom="errorbar", width = 0.20)
    class(map) <- "uneval"

    ## report and annotation
    rpt <- aggregate(x ~ z, pdt, \(x)
    {
        n <- c(NUM=length(x))
        q <- quantile(x, c(0, .25, .5, .75, 1), na.rm=TRUE)
        names(q) <- c("MIN", "Q25", "MED", "Q75", "MAX")
        m <- c(AVG=mean(x, na.rm=TRUE), STD=sd(x, na.rm=TRUE))
        c(n, q, m)
    })
    rpt <- with(rpt, cbind(z, data.frame(x)))
    aq1 <- annotate("text", x=rpt[, 1], y=rpt[, "Q25"], label=rpt[, "Q25"],
                    hjust=1.0 * (1 - fxy) + 1.0 * fxy,
                    vjust=1.0 * (1 - fxy) + 1.0 * fxy,
                    lineheight=0.8)
    amd <- annotate("text", x=rpt[, 1], y=rpt[, "MED"], label=rpt[, "MED"],
                    hjust=1.0 * (1 - fxy),
                    vjust=0.0 * (1 - fxy) + 1.0 * fxy,
                    lineheight=0.8)
    aq3 <- annotate("text", x=rpt[, 1], y=rpt[, "Q75"], label=rpt[, "Q75"],
                    hjust=1.0 * (1 - fxy) + 0.0 * fxy,
                    vjust=0.0 * (1 - fxy) + 0.0 * fxy,
                    lineheight=0.8)

    ## ---- make figure ----
    g <- ggplot(pdt, map) + fcs + lcs + crd
    g <- g + geo + ebr + amd + aq1 + aq3
    g <- g + scale_x_discrete(drop=FALSE)
    g <- g + labs(x=NULL, y=NULL, fill=NULL, title=ttl) + guides(alpha="none")
    g <- g + .th(lgp)
    ## return
    invisible(g)
}

#' proportions of class z given x (mosasics)
#'
#' Show proportions of categories in `z` given the conditions in `x`.
#'
#' @param x conditions such as age group for n samples
#' @param y (unused)
#' @param z categories such as disease  status for n samples
#' @param xcl x-color scheme (conditions)
#' @param zcl z-color scheme (categories)
#' @param xlb y-label for the conditions axis.
#' @param ylb y-label for the proportion axis.
#' @param pfm text format to show in the bars (def="#(%)").
#'
#' Useful to display conditional probabilities.
#'
#' `lgp` specifies the position of legend,  can be of c("none", "left", "right",
#' "bottom", "top") outside of the plotting area,  or two numbers from c(0, 0) -
#' bottom-left to c(1, 1) top-right inside of the plotting area.
#'
#' The text format `pfm` is a string with special character "#", "."  and "%" to
#' represent numbers,  proportions, and percentages.  For  example, "#(%)" shows
#' "1,234(12%)" in the bar, and "#[.]" shows "1,234[0.12]".
#'
#' Currently, proportions are  rounded up to two digits past  decimal point, and
#' percentages are round to intergers;  proportions (percentages) less than 0.01
#' (1%) are not printed.
FIG$pzx <- function(x, y=NULL, z=NULL, a=NULL, lgp=NULL, fxy=0, ttl=NULL,
                    xlb=NULL, ylb=NULL, xcl=NULL, zcl="hue",
                    pfm=NULL, ...)
{
    ## -------------------------- retain symbols --------------------------- ##
    X %:-% names(eval(call("DF", substitute(x)), parent.frame()))
    Y %:-% names(eval(call("DF", substitute(y)), parent.frame()))
    ## -------------------------- fix data objects ------------------------- ##
    upk(.fd(x, y, z, a)) # unpack fixed x, y, z, and a
    ## ---------------------------- make figure ---------------------------- ##
    ttl %:-% ""
    xcl %:-% "1"
    crd <- NULL
    if(fxy) # flip x and y?
        crd <- coord_flip()
    xlb %:-% X # symbol/header of conditions as default x-axis title
    ylb %:-% Y # symbol/header of y as default y-axis title
    csx <- .cs(x, xcl, "color") # , dir=ifelse(fxy, 1, -1)) # color to draw
    csz <- .cs(z, zcl, "fill")  # , dir=ifelse(fxy, 1, -1)) # color to fill
    ## csz$guide <- guide_legend(reverse = TRUE, direction = "horizontal")
    
    ## counts, proportions, and labels
    pfm %:-% "#(%)"
    num <- table(x, z)
    prp <- proportions(num, margin = 1) # prop of [x] by [z]
    num <- as.data.frame(num, responseName="num") |> na.omit()
    prp <- as.data.frame(prp, responseName="prp") |> na.omit()
    pdt <- within(merge(num, prp), # prop and %
    {
        cnt <- local(
        {
            ## pct <- sprintf("%.0f%%", prp * 100)
            hid <- prp < 0.01 # small values will be hidden
            pct <- format(sprintf("%.0f%%", prp * 100))
            num <- format(num, trim=TRUE, big.mark=",")
            prp <- format(round(prp, 2))
            lbl <- rep(pfm, length(pct))
            for(. in seq_along(lbl))
            {
                lbl[.] <- sub("#", num[.], lbl[.], fixed=TRUE) # "#" -> num
                lbl[.] <- sub("%", pct[.], lbl[.], fixed=TRUE) # "%" -> pct
                lbl[.] <- sub(".", prp[.], lbl[.], fixed=TRUE) # "." -> prp
            }
            lbl[hid] <- ""
            lbl
        })
    })
    
    ## y-scale labeler
    ysc_lbl <- function(b, ...)
    {
        b <- sprintf("%.0f", b * 100)
        b[1] <- paste0(b[1], "(%)")
        b
    }

    ## x-scale and x-color-scale
    if(any(grepl("discrete", class(csx), TRUE)))
        s_x <- scale_x_discrete(drop=FALSE)
    else
        pdt <- within(pdt, x <- as.numeric(x))

    map <- aes(x=x, y=prp, fill=z) # aesthetics
    if(xcl != "0")
        map <- c(map, aes(color=x))
    class(map) <- "uneval"
    geo <- geom_col(alpha=mean(a), width=0.80, position=position_stack(reverse=fxy),
                    linewidth=rel(1.5))
    txt <- geom_text(aes(label=cnt), color="black",
                     position=position_stack(vjust=0.5, reverse=fxy), lineheight=0.8)
    ## 
    g <- ggplot(pdt, map)
    g <- g + geo + txt + crd + csx + csz
    g <- g + scale_y_continuous(labels=ysc_lbl)
    g <- g + labs(x=xlb, y=ylb, fill=NULL, title=ttl)
    g <- g + guides(alpha="none", color="none")
    g <- g + .th(lgp)
    ## return
    invisible(g)
}

#' table plot
#'
#' print a table as it is.
#'
#' An entry may contain multiple row records.
#' @param x data frame to plot, each row is a record.
#' @param y data frame to identify entries (def=NULL, each row is an entry).
#' @param z vector of row labels that affects color (def=NULL, no color).
#' @param a alpha of table background transparency (def=0.3).
FIG$tbp <- function(x, y=NULL, z=NULL, a=0.3) {
    stopifnot(NROW(x) == NROW(y) || length(y) == 0)
    stopifnot(NROW(x) == NROW(z) || length(z) == 0)
    ## data enchancement
    x <- eval(call("DF", substitute(x), check.names=FALSE), parent.frame())
    x <- format(x, justify="l", big.mark=",")
    y <- eval(call("DF", substitute(y), check.names=FALSE), parent.frame())
    y <- format(y, justify="l", big.mark=",")
    Y <- do.call(paste, c(y, list(sep=":"))) # shared row coordinate
    if(!length(Y))
        Y <- rownames(x)
    if(is.null(a)) # alpha / transparency
        a <- 0.3
    ## common plot elements
    PD <- position_dodge2(width=1, preserve="total", padding=0)
    EX <- rep(0, 4)
    e <- theme_bw() +
        theme(
            panel.grid=element_blank(),        # no coord grids
            axis.title.y=element_blank(),      #
            axis.text.y=element_blank(),       #
            axis.ticks.y=element_blank(),      #
            axis.ticks.length.y=unit(0, "pt"), # no border space
            ## axis.ticks.x=element_line(color="#00000000"),
            axis.text.x=element_text(face="bold", family="sans"),
            axis.title.x=element_blank(),
            plot.margin = margin(0, 0, 0, 0, unit="pt"))

    ## plot leading column in y
    fgy <- sapply(names(y), function(.)
    {
        g <- ggplot(NULL, aes(x=0, y=Y, label=y[, .]), hjust=0)
        g <- g + geom_tile(fill="grey40", color="black", alpha=a)
        g <- g + geom_text(family="mono")
        g <- g + scale_x_continuous(breaks=0, labels=., expand=EX)
        g <- g + scale_y_discrete(expand=rep(0, 4))
        g + e
    }, simplify=FALSE)
    ncy <- sapply(y, nchar %.% max(na.rm=TRUE))
    ncy <- unlist(ncy) # in case y is an empty list
    
    ## plot measure column in x
    fgx <- sapply(names(x), function(.)
    {
        g <- ggplot(NULL, aes(x=0, y=Y, label=x[, .]), hjust=0)
        g <- g + geom_tile(aes(fill=z), alpha=a, position=PD) # one raw
        g <- g + geom_tile(color="black", fill=NA)            # one entry
        g <- g + geom_text(family="mono", position=PD)        # cell text
        g <- g + scale_x_continuous(breaks=0, labels=., expand=EX)
        g <- g + scale_y_discrete(expand=EX) + guides(fill="none")
        g + e
    }, simplify=FALSE)
    ncx <- sapply(x, nchar %.% max(na.rm=TRUE))
    ncx <- unlist(ncx) # in case y is an empty list
    
    ## combined plot
    plot_grid(plotlist=c(fgy, fgx), nrow=1, rel_widths=c(ncy, ncx))
}

#' odds ratio plot
#'
#' a forest plot of odds ratios and confidence intervals.
#'
#' An entry may contain multiple row records.
#' @param x data frame of odds ratio, upper and lower 95%CI.
#' @param y data frame to identify entries (def=NULL, each row is an entry).
#' @param z vector of row labels that affects color (def=NULL, no color).
#' @param cap cap odds ratio grater  then this (def=NULL, 95 percential round up
#'     to half-unit).
#' @param lgp legend position (def=NULL, invisible)
FIG$orp <- function(x, y=NULL, z=NULL, a=NULL, cap=NULL, lgp=NULL)
{
    stopifnot(NCOL(x) >= 3)
    stopifnot(NROW(x) == NROW(y) || length(y) == 0)
    stopifnot(NROW(x) == NROW(z) || length(z) == 0)
    ## data enchancement
    o.r <- x[, 1] # odds ratio
    clw <- x[, 2] # ci lower
    cup <- x[, 3] # ci upper
    y <- eval(call("DF", substitute(y), check.names=FALSE), parent.frame())
    y <- format(y, justify="l", big.mark=",")
    Y <- do.call(paste, c(y, list(sep=":"))) # shared row coordinate
    if(!length(Y))
        Y <- rownames(x)
    if(is.null(a)) # alpha / transparency
        a <- 0.5
    if(is.null(cap))
        cap <- ceiling(quantile(cup, .95) * 100 / 95 * 2) / 2
    ## common plot elements
    PD <- position_dodge2(width=.9, preserve="single", padding=0)
    EX <- rep(0, 4)        # coordinate expansion
    XB <- c(0, .5, 1, cap) # x-axis breaks and labels
    XL <- c("   0", "0.5", "1", paste0(cap, strrep(" ", nchar(cap)*2)))
    e <- theme_bw() +
        theme(
            panel.grid=element_blank(),        # no coord grids
            axis.title.y=element_blank(),      #
            axis.text.y=element_blank(),       #
            axis.ticks.y=element_blank(),      #
            axis.ticks.length.y=unit(0, "pt"), # no border space
            axis.text.x=element_text(face="bold", family="sans"),
            axis.title.x=element_blank(),
            plot.margin = margin(0, 0, 0, 0, unit="pt"))
    ## legend
    lgp <- if(length(lgp)) rep(lgp, length.out=2) else "none"
    e <- e + theme(legend.position=lgp, legend.justification=lgp,
                   legend.margin=margin(0, unit="pt"))
    ## make the forest plot
    g <- ggplot(NULL, aes(x=o.r, y=Y, color=z))
    g <- g + geom_vline(xintercept=c(0, .5, 1), linetype=c(1, 2, 1),
                        linewidth=c(.5, .2, .2), alpha=a)
    g <- g + geom_point(size=4, position=PD)
    g <- g + geom_errorbar(aes(xmin=clw, xmax=cup), position=PD, width=.8)
    g <- g + geom_tile(aes(x=cap/2, width=cap), color="black", fill=NA)
    g <- g + scale_x_continuous(breaks=XB, labels=XL, expand=EX)
    g <- g + scale_y_discrete(expand=EX)
    g <- g + labs(color=NULL)
    g <- g + coord_cartesian(xlim=c(0, cap))
    g <- g + e
    g
}

#' table plot (experimental)
#'
#' Attemp to re-implement `tbp()` with ggplot2 only.
FIG$.tbp. <- function(x, y=NULL, z=NULL)
{
    ## data enchancement
    library(reshape2)
    x <- format(as.data.frame(x, check.names=FALSE), justify="l", big.mark=",")
    dat <- DF(y, z, x)
    dat <- melt(dat, id.vars=1:2, variable.name="x", value.name="v")
    PD <- position_dodge(width=1, preserve="total")
    e <- theme_bw() +
        theme(
            panel.grid=element_blank(),        # no coord grids
            axis.title.y=element_blank(),      #
            axis.text.y=element_blank(),       #
            axis.ticks.y=element_blank(),      #
            axis.ticks.length.y=unit(0, "pt"), # no border space
            axis.text.x=element_text(face="bold", family="sans"),
            axis.title.x=element_blank(),
            plot.margin = margin(0, 0, 0, 0, unit="pt"))
    g <- ggplot(dat, aes(x=x, y=y, label=v, fill=z), alpha=.1, hjust=0)
    g <- g + geom_tile(alpha=.1, color="black", position=PD)
    g <- g + geom_text(aes(color=z), family="mono", position=PD)
    ## g <- g + geom_tile(aes(width=5, group=y), fill=NA, color="black")
    g <- g + scale_x_discrete(expand=rep(0, 4))
    g <- g + scale_y_discrete(expand=rep(0, 4))
    g <- g + e
    g
}


#' print only legend for categories in z.
#'
#' @param x unused.
#' @param y unused.
#' @param zcl z-color scheme (categories).
FIG$lgz <- function(x, y, z=NULL, a=NULL, fxy=0, ttl=NULL, dct=NULL, zcl="hue")
{
    x <- if(fxy) y else x                    # y as x?
    pdt <- .fd(x, z=z, a=a)                  # plot data
    map <- aes(x=z)                          # aesthetics
    crd <- if(fxy) coord_flip() else NULL
    if(is.null(a))
        a <- mean(pdt$a) # alpha of all bars
    
    ## counts, proportions, labels, and annotations
    num <- with(pdt, table(z))
    pdt <- as.data.frame(num, responseName="num") |> na.omit()
    
    if(!is.null(dct))
        dct <- sprintf("%s: %s",
                       format(levels(z)),
                       dct[match(levels(z), names(dct), 0)])

    ## plot elements
    map <- c(map, aes(x=0, y=0, fill=z))
    csz <- .cs(z, zcl, "fill", dct) # fill by z (labels)
    geo <- geom_col(alpha=a, linewidth=1)
    class(map) <- "uneval"
    ## ---- make figure ----
    g <- ggplot(pdt, map) + csz + crd
    g <- g + geo
    g <- g + labs(x=NULL, y=NULL, fill=ttl, title=NULL) + guides(alpha="none")

    M0 <- margin(0,0,0,0)
    EB <- element_blank()
    e <- theme_void()
    e <- e + theme(plot.title = EB)
    e <- e + theme(plot.margin = M0)
    e <- e + theme(legend.spacing=unit(0, "pt"))
    e <- e + theme(legend.position=c(0, 0), legend.justification=c(0, 0),
                   legend.direction=ifelse(fxy, "horizontal", "vertical"))
    e <- e + theme(legend.background = EB)
    e <- e + theme(legend.margin = M0)
    e <- e + theme(legend.box.margin = M0)
    e <- e + theme(axis.line = EB, axis.text = EB, axis.ticks = EB,
                   axis.title=EB)
    e <- e + theme(strip.text=EB)
    e <- e + theme(panel.border=EB)
    e <- e + theme(panel.spacing=unit(0, "pt"))

    ## print
    g <- g + e
    invisible(g)
}

#' wrapper for ggsave
#'
#' Special care is taken to allow saving multi-page PDF.
FIG$gsv <- function(g, out=NULL, dim=NULL, rsz=NULL, dpi=NULL, new=0, bg=NULL, ...)
{
    if(!is.null(out) && (!file.exists(out) || new))
    {
        arg <- list(filename=out, plot=g) # gather arguments
        if(is.null(dim)) # dimension
            dim <- 7
        dim <- rep(dim, length.out = 2)
        arg <- c(arg, width=dim[1], height=dim[2])
        if(is.null(rsz)) # resize
            rsz <- 1
        arg <- c(arg, scale=sqrt(1/rsz))
        ## device
        sfx <- tolower(regmatches(out, regexec("[.]([^.]+)$", out))[[1]][-1])
        if(sfx == "pdf")
            arg <- c(arg, device=cairo_pdf, onefile=TRUE)
        ## save
        arg <- c(arg, dpi=dpi, bg=bg, ...) # additional arguments
        do.call(ggsave, arg)
    }
    invisible(g)
}

if("TCOM:FIG" %in% search())
    detach("TCOM:FIG")
attach(FIG, name="TCOM:FIG", warn.conflicts = FALSE)
FIG$col <- FIG$clp
## rm(FIG)
