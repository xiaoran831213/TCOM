## pairwise figures
library(ggplot2)

FIG <- local(
{
    #' wrapper of color scales
    .cs <- function(v, n="hue", aes="fill", dct=NULL, ttl=waiver())
    {
        pal <- substr(n, 3, 3) # palettte number
        typ <- substr(n, 2, 2) # type
        fun <- substr(n, 1, 1) # function name
        brk <- names(table(v)) # preserve the order of factor levels
        dct <- if(length(dct) < 1) waiver() else dct
        ## use ggplot's Hue or Brewer?
        if(fun == "h") # hue
        {
            ret <- scale_fill_hue(
                breaks=brk, drop=0, direction=1, aesthetics=aes, labels=dct,
                name=ttl)
        }
        else # brewer
        {
            typ <- c(d="div", q="qua", s="seq")[typ]
            pal <- suppressWarnings(as.integer(pal))
            ret <- scale_colour_brewer(
                breaks=brk, drop=0, direction=1, aesthetics=aes, labels=dct,
                name=ttl, type=typ, palette=pal, na.value = "grey50")
        }
        ret
    }

    #' wrapper for ggsave
    #'
    #' Special care is taken to allow saving multi-page PDF.
    gsv <- function(g, out=NULL, dim=NULL, rsz=NULL, dpi=NULL, new=0, bg=NULL, ...)
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

    #' default theme
    .th <- function(lgp=NULL)
    {
        if(is.null(lgp)) # legend position
            lgp <- "none"
        e <- theme_bw()
        e <- e + theme(plot.title = element_text(margin=margin(0, 0, 0, 0)))
        e <- e + theme(legend.position=lgp, legend.justification=lgp)
        e <- e + theme(legend.background = element_blank())
        e <- e + theme(legend.margin = margin(1, 1, 1, 1))
        e <- e + theme(legend.box.margin = margin(0, 0, 0, 0))
        e <- e + theme(axis.text.y = element_text(angle=270, hjust=.5))
        e <- e + theme(plot.margin = margin(1, 1, 1, 1))
        e <- e + theme(strip.text=element_blank())
        e <- e + theme(panel.spacing=unit(0, "pt"))
        e <- e + theme(panel.spacing=unit(0, "pt"))
        e
    }

    #' fortified data for ploting
    .fd <- function(x, y=NULL, z=NULL, a=NULL)
    {
        N <- length(x)
        if(is.null(y)) # y: y-axis
            y <- x
        y <- rep(y, length.out = N)
        if(is.null(z)) # z: labels
            z <- "ALL"
        z <- rep(z, length.out = N) |> as.factor()
        if(is.null(a)) # a: alpha transparency
            a <- sqrt(1 / max(1, nlevels(z)))
        a <- rep(a, length.out = N)
        ## combine
        data.frame(x, y, z, a)
    }

    #' X and Y as dots on 2D painted by Z.
    xoy <- function(x, y, z=NULL, a=NULL, lgp=NULL, fxy=0, zcl="hue", c2d=0, dct=NULL, ann=NULL)
    {
        pdt <- .fd(x, y, z, a)                # fix data
        rm(x, y, z, a)                        # avoid local ambiguity
        map <- aes(x=x, y=y, color=z)         # main aesthetics
        clr <- .cs(pdt$z, zcl, "color", dct)  # color scheme
        crd <- if(fxy) coord_flip() else NULL # flip x and y
        ## draw contour 2D?
        if(c2d > 0)
        {
            gsz <- with(pdt, unsplit(lapply(split(z, z), length), z)) # group size
            ## break ties so bandwidth.nrd can "normally" guessed the bandwidth 
            c2d <- geom_density_2d(
                aes(x=jitter(x, .1, 0), y=jitter(y, .1, 0), group=z),
                data=~subset(.x, gsz > 5),
                alpha=c2d, color="gray40", contour_var="ndensity", linewidth=.5/sqrt(rsz))
        }
        else
            c2d <- NULL
        ## color schecme
        ## annotate?
        if(length(ann))
        {
            ax <- with(pdt, tapply(x, z, mean))
            ay <- with(pdt, tapply(y, z, mean))
            al <- names(ax)
            if(is.character(ann) && length(ann) >= length(al))
                al <- ann[al]
            ann <- annotate("text", x=ax, y=ay, label=al, hjust=1, vjust=.5)
            ## angle=90 * (1 - fxy), lineheight=.8
        }
        else
            ann <- NULL
        
        ## ---- make figure ----
        g <- ggplot(pdt, map)
        A <- unique(pdt$a)
        if(length(A > 1))
        {
            g <- g + geom_point(aes(alpha=a))
        }
        else
        {
            g <- g + geom_point(alpha=A)
        }
        g <- g + clr + crd + c2d + ann
        g <- g + labs(x=NULL, y=NULL, color=NULL) + guides(alpha="none")
        g <- g + .th(lgp)
        ## return
        invisible(g)
    }

    ## histogram
    hst <- function(x, y, z=NULL, a=NULL, lgp=NULL, fxy=0)
    {
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

    ## density plot
    dst <- function(x, y, z=NULL, a=NULL, lgp=NULL, fxy=0)
    {
        x <- if(fxy) y else x   # y as x?
        pdt <- .fd(x, z=z, a=a) # plot data
        map <- aes(x=x)         # aesthetics
        hue <- NULL             # hue for fill
        huc <- NULL             # hue for color
        crd <- if(fxy) coord_flip() else NULL
        z <- pdt$z
        a <- if(is.null(a)) mean(pdt$a) else mean(a)
        if(!is.null(z))
        {
            map <- c(map, aes(color=z, fill=z))
            hue <- scale_fill_hue(breaks=levels(z), drop=FALSE)
            huc <- scale_color_hue(breaks=levels(z), drop=FALSE)
            geo <- geom_density(alpha=a)
        }
        else
            geo <- geom_density(fill="gray", alpha=a)
        class(map) <- "uneval"
        ## ---- make figure ----
        g <- ggplot(pdt, map) + hue + huc + crd
        g <- g + geo
        g <- g + labs(x=NULL, y=NULL, fill=NULL, color=NULL) + guides(alpha="none")
        ## ---- theme ----
        g <- g + .th(lgp)
        ## print
        invisible(g)
    }

    #' bar chart
    #'
    #' @param txp text for percentage values.
    #' @param xcl x-color scheme (conditions)
    #' @param zcl z-color scheme (categories)
    bar <- function(x, y, z=NULL, a=NULL, lgp=NULL, fxy=0, ttl=NULL, dct=NULL, zcl="hue")
    {
        x <- if(fxy) y else x                    # y as x?
        pdt <- .fd(x, z=z, a=a)                  # plot data
        map <- aes(x=z)                          # aesthetics
        crd <- if(fxy) coord_flip() else NULL
        if(is.null(a))
            a <- mean(pdt$a) # alpha of all bars
        
        ## counts, proportions, labels, and annotations
        num <- with(pdt, table(z))
        prp <- proportions(num, margin=NULL)
        num <- as.data.frame(num, responseName="num") |> na.omit()
        prp <- as.data.frame(prp, responseName="prp") |> na.omit()
        pdt <- within(merge(num, prp),
        {
            lbl <- local(
            {
                pct <- sprintf("%.0f%%", prp * 100)
                pct[prp < 0.01] <- "<1%" # small %
                num <- format(num, trim=TRUE, big.mark=",")
                sprintf(ifelse(fxy, "%s\n%s", "%s(%s)"), num, pct)
            })
            ann <- dct[match(levels(z), names(dct), 0)] # export only
        })
        
        ## plot elements
        ymx <- with(pdt, max(num)) # y-axis top
        if(!is.null(z))
        {
            map <- c(map, aes(x=z, y=num, fill=z))
            fcl <- .cs(z, zcl, "fill") # fill by z (labels)
            geo <- geom_col(alpha=a, linewidth=1)
        }
        else
        {
            fcl <- NULL
            geo <- geom_col(alpha=a, linewidth=1, fill="gray50")
        }

        ## annotation and text
        ann <- with(pdt, dct[match(levels(z), names(dct), 0)])
        ann <- annotate("text", x=names(ann), y=ymx, label=ann, hjust=1, vjust=.5,
                        angle=90 * (1 - fxy), lineheight=.8)
        txt <- geom_text(aes(label=lbl, y=0), hjust=0, vjust=0.5,
                         angle=90 * (1 - fxy), lineheight=.8)
        class(map) <- "uneval"
        ## ---- make figure ----
        g <- ggplot(pdt, map) + fcl + crd
        g <- g + geo + ann + txt
        g <- g + labs(x=NULL, y=NULL, fill=NULL, title=ttl) + guides(alpha="none")
        g <- g + .th(lgp)
        ## print
        invisible(g)
    }

    #' box plot by group
    #'
    #' @param x values
    #' @param y values
    #' @param z group label
    bxp <- function(x, y, z=NULL, a=NULL, lgp=NULL, fxy=0, zcl="hue")
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
        amd <- annotate("text", x=rpt[, 1], y=rpt[, "MED"], label=rpt[, "MED"],
                        hjust=.5, vjust=0.5, lineheight=0.8)
        aq1 <- annotate("text", x=rpt[, 1], y=rpt[, "Q25"], label=rpt[, "Q25"],
                        hjust=.5, vjust=1.0, lineheight=0.8)
        aq3 <- annotate("text", x=rpt[, 1], y=rpt[, "Q75"], label=rpt[, "Q75"],
                        hjust=.5, vjust=0.0, lineheight=0.8)

        ## ---- make figure ----
        g <- ggplot(pdt, map) + fcs + lcs + crd
        g <- g + geo + ebr + amd + aq1 + aq3
        g <- g + scale_x_discrete(drop=FALSE)
        g <- g + labs(x=NULL, y=NULL, fill=NULL) + guides(alpha="none")
        g <- g + .th(lgp)
        ## return
        invisible(g)
    }

    #' proportions of class z given x (mosasics)
    #'
    #' Proportion of levels in {z} across levels in {x}
    #'
    #' @param xcl x-color scheme (conditions)
    #' @param zcl z-color scheme (categories)
    pzx <- function(x, y, z=NULL, a=NULL, lgp=NULL, fxy=0, ttl=NULL,
                    xcl="hue", zcl="hue", ...)
    {
        x <- if(fxy) y else x    # y as x?
        crd <- if(fxy) coord_flip() else NULL
        ## plot data
        pdt <- .fd(x, y, z, a)
        a <- if(is.null(a)) mean(pdt$a) # alpha of all bars
        
        ## counts, proportions, and labels
        num <- with(pdt, table(x, z))
        prp <- proportions(num, margin = 1) # race by label
        num <- as.data.frame(num, responseName="num") |> na.omit()
        prp <- as.data.frame(prp, responseName="prp") |> na.omit()
        pdt <- within(merge(num, prp), # prop and %
        {
            lbl <- local(
            {
                pct <- sprintf("%.0f%%", prp * 100)
                pct[prp < 0.01] <- "<1%" # small %
                num <- format(num, trim=TRUE, big.mark=",")
                sprintf(ifelse(fxy, "%s\n%s", "%s(%s)"), num, pct)
            })
        })
        
        ## figure elements
        map <- aes(x=x, y=prp, color=x, fill=z) # aesthetics
        fcs <- FIG$.cs(z, zcl, "fill")          # fill colours: z-class
        lcs <- FIG$.cs(x, xcl, "colour")        # line colours: x-class
        geo <- geom_col(aes(alpha=a), position=position_stack(reverse=1), linewidth=1)
        txt <- geom_text(aes(label=lbl), color="black",
                         position=position_stack(vjust=0.5, reverse=1), lineheight=0.8)
        class(map) <- "uneval"
        ## 
        g <- ggplot(pdt, map)
        g <- g + geo + txt + crd + fcs + lcs
        g <- g + scale_x_discrete(drop=FALSE)
        g <- g + labs(x=NULL, y=NULL, fill=NULL, title=ttl) + guides(alpha="none")
        g <- g + .th(lgp)
        ## return
        invisible(g)
    }

    #' print legend only
    #'
    #' @param txp text for percentage values.
    #' @param xcl x-color scheme (conditions)
    #' @param zcl z-color scheme (categories)
    lgz <- function(x, y, z=NULL, a=NULL, lgp=NULL, fxy=0, ttl=NULL, dct=NULL, zcl="hue")
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
        fcl <- .cs(z, zcl, "fill", dct) # fill by z (labels)
        geo <- geom_col(alpha=a, linewidth=1)
        class(map) <- "uneval"
        ## ---- make figure ----
        g <- ggplot(pdt, map) + fcl + crd
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
    
    environment()
})
