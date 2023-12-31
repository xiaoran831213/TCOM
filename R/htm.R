## TCOM Tool related helpers
HTM <- new.env()

#' write html to a local file and browse.
#'
#' The browser may not be able to open HTML under the OS temporary directory.
HTM$try_htm <- function(htm, dst="~/rtmp")
{
    dst <- normalizePath(dst)
    dir.create(dst, showWarnings=FALSE, recursive=TRUE)
    ttl <- deparse1(substitute(htm))
    htm <- c(
        '<!DOCTYPE html>',
        '<html>',
        '<head>', '<title>', ttl, '</title>', '</head>',
        '<body>',
        htm,
        '</body>',
        '</html>')
    fn <- tempfile(tmpdir=dst, fileext=".html")
    writeLines(htm, fn)
    browseURL(fn)
}

#' base64 encoder with plain R
#'
#' @param raw_vec raw vector
#' @return single character string containing base64 encoded values
#' @examples
#' encode_base64(as.raw(1:20))
#'
#' see coolbutuseless.github.io/2021/12/04/base64-encoding/decoding-in-plain-r
HTM$b64_enc <- function(raw)
{
    char_to_int <- function(vec)
    {
        unname(vapply(as.character(vec), utf8ToInt, integer(1)))
    }
    lookup_names <- c(LETTERS, letters, 0:9, '+', '/', '=')
    lookup_values <- c(
        char_to_int(LETTERS) - char_to_int('A'),
        char_to_int(letters) - char_to_int('a') + 26L,
        char_to_int(0:9)     - char_to_int('0') + 52L,
        62L, 63L, 0L)

    lookup <- setNames(lookup_values, lookup_names)
    stopifnot(is.raw(raw))

    ## work out if we need to pad the result to an 8-bit boundary
    npad <- 3L - (length(raw) %% 3L)
    if (npad %in% 1:2)
        length(raw) <- length(raw) + npad

    ## Create an 8 row matrix.  Each column is the bit-vector for an 8-bit number
    int <- as.integer(raw)
    res <- as.integer(bitwAnd(rep(int, each = 8),  c(128L, 64L, 32L, 16L, 8L, 4L, 2L, 1L)) > 0)
    mat <- matrix(res, nrow = 8)

    ## Reshape to a 6-row matrix (i.e. 6-bit numbers)
    N <- length(mat)
    stopifnot(N %% 6 == 0)
    dim(mat) <- c(6, N/6)

    ## Calcualte the 6-bit numbers
    mat <- mat * c(32L, 16L, 8L, 4L, 2L, 1L)
    values <- colSums(mat)

    ## Find the letter which is associated with each 6-bit number and paste
    ## together into a string
    chars <- lookup_names[values + 1L]
    b64 <- paste(chars, collapse = "")

    ## Replace padding bits with '=' signs
    if (npad == 1)
        b64 <- gsub(".$", "=", b64)
    else if (npad == 2)
        b64 <- gsub("..$", "==", b64)
    b64
}

#' make base64 data URL for a file.
#'
#' @param fns filename.
#' @param mda media types (def=<infer from filename surffix>)
HTM$b64_url <- function(fns, mda=NULL)
{
    ## major media types
    MDA <- c(.aac = "audio/aac", .abw = "application/x-abiword",
      .arc = "application/x-freearc", .avif = "image/avif",
      .avi = "video/x-msvideo", .azw = "application/vnd.amazon.ebook",
      .bin = "application/octet-stream", .bmp = "image/bmp",
      .bz = "application/x-bzip", .bz2 = "application/x-bzip2",
      .cda = "application/x-cdf", .csh = "application/x-csh", .css = "text/css",
      .csv = "text/csv", .doc = "application/msword",
      .docx = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
      .eot = "application/vnd.ms-fontobject", .epub = "application/epub+zip",
      .gz = "application/gzip", .gif = "image/gif", .htm = "text/html",
      .html = "text/html", .ico = "image/vnd.microsoft.icon",
      .ics = "text/calendar", .jar = "application/java-archive",
      .jpg = "image/jpeg", .jpeg = "image/jpeg", .js = "text/javascript",
      .json = "application/json", .jsonld = "application/ld+json",
      .midi = "audio/midi ", .mid = "audio/midi ", .mjs = "text/javascript",
      .mp3 = "audio/mpeg", .mp4 = "video/mp4", .mpeg = "video/mpeg",
      .mpkg = "application/vnd.apple.installer+xml",
      .odp = "application/vnd.oasis.opendocument.presentation",
      .ods = "application/vnd.oasis.opendocument.spreadsheet",
      .odt = "application/vnd.oasis.opendocument.text", .oga = "audio/ogg",
      .ogv = "video/ogg", .ogx = "application/ogg", .opus = "audio/opus",
      .otf = "font/otf", .png = "image/png", .pdf = "application/pdf",
      .php = "application/x-httpd-php", .ppt = "application/vnd.ms-powerpoint",
      .pptx = "application/vnd.openxmlformats-officedocument.presentationml.presentation",
      .rar = "application/vnd.rar", .rtf = "application/rtf",
      .sh = "application/x-sh", .svg = "image/svg+xml",
      .tar = "application/x-tar", .tiff = "image/tiff", .tif = "image/tiff",
      .ts = "video/mp2t", .ttf = "font/ttf", .txt = "text/plain",
      .vsd = "application/vnd.visio", .wav = "audio/wav", .weba = "audio/webm",
      .webm = "video/webm", .webp = "image/webp", .woff = "font/woff",
      .woff2 = "font/woff2", .xhtml = "application/xhtml+xml",
      .xls = "application/vnd.ms-excel",
      .xlsx = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      .xml = "application/xml", .xul = "application/vnd.mozilla.xul+xml",
      .zip = "application/zip", ".3gp" = "video/3gpp", ".3g2" = "video/3gpp2",
      ".7z" = "application/x-7z-compressed")
    
    ## use filename surffix extention 
    if(length(mda) < 1)
        mda <- MDA[sub("^.*([.][^.]*)$", "\\1", fns)]
    mda[is.na(mda)] <- "text/plain" # default media
    
    ## compose the url(s)
    url <- character(length(fns))
    for(i in seq_along(fns))
    {
        r <- readBin(fns[i], "raw", file.info(fns[i])$size)
        r <- b64_enc(r) # base64 encode
        url[i] <- sprintf("data:%s;base64,%s", mda[i], r)
    }
    url
}

#' make base64 hyper link from files.
#'
#' @param fns filenames to hyper-link to.
#' @param txt text to put in between <a ...> and </a> (def=filenames)
#' @param att list of additional attributes
#' @param sty list of styles
#' @param div sandwich <a href=..> between <div> & </div>? (def=1)
#'
#' {txt} can be another vector of HTML, for example, a list of images.
HTM$b64_hrf <- function(fns, txt=NULL, att=NULL, sty=NULL, div=1)
{
    url <- b64_url(fns) # base64 encoded data URL(s)
    url_hrf(url, txt=txt, att=att, sty=sty, div=1)
}

#' write hyper link from URL.
#'
#' @param url file by url(s) or name(s) to hyper-link to.
#' @param txt text to put in between <a ...> and </a> (def=filenames)
#' @param att list of additional attributes
#' @param sty list of styles
#' @param div sandwich <a href=..> between <div> & </div>? (def=1)
#' @return vector of <a href>..</a> linking each file.
#'
#' {txt} can be another vector of HTML, for example, a list of images.
HTM$url_hrf <- function(url, txt=NULL, att=NULL, sty=NULL, div=1)
{
    ## hyperlink text and download names
    if(length(txt) < 1)
        txt <- names(url)
    if(length(txt) < 1)
        txt <- url
    ## add default download name
    if(!"download" %in% names(att))
        att <- append(att, list(download=basename(url)), 0)
    att <- str_att(att)
    att <- if(length(att)) paste0(" ", att) else ""
    sty <- str_sty(sty)
    sty <- if(length(sty)) paste0(" ", sty) else ""
    ## compose html
    htm <- sprintf('<a%s%s href="%s">%s</a>', att, sty, url, txt)
    if(div)
        htm <- sprintf("<div>%s</div>", htm)
    htm
}

#' make base64 image from files.
#'
#' Base64 image allowed embeded HTML to be distribution alone.
#' @param fns image filename.
#' @param w (in px) width 
#' @param h (in px) height
#' @param att list of additional attributes
#' @param sty list of styles
#' @param div sandwich <img src=..> between <div> & </div>? (def=0)
HTM$b64_img <- function(fns, w=NULL, h=NULL, att=NULL, sty=NULL, div=0)
{
    url <- b64_url(fns) # base64 encoded data URL(s)
    url_img(url, w=w, h=h, att=att, sty=sty, div=div)
}

#' write image html based on filename.
#'
#' Named image allows tiny HTML not meant to distributed alone.
#' @param img image by url(s) or name(s).
#' @param w (in px) width
#' @param h (in px) height
#' @param att list of additional attributes
#' @param sty list of styles
#' @param div sandwich <img src=..> between <div> & </div>? (def=0)
HTM$url_img <- function(url, w=NULL, h=NULL, att=NULL, sty=NULL, div=0)
{
    ## compose html, handel attributes
    if(length(h) > 0)
        att <- append(att, list(height=h), 0)
    if(length(w) > 0)
        att <- append(att, list(width =w), 0)
    att <- str_att(att)
    att <- if(length(att)) paste0(" ", att) else ""
    sty <- str_sty(sty)
    sty <- if(length(sty)) paste0(" ", sty) else ""
    ## compose html
    htm <- sprintf('<img%s%s src="%s"/>', att, sty, url)
    if(div)
        htm <- sprintf("<div>%s</div>", htm)
    htm
}

#' make attribute string from a list.
#'
#' @param att a list of attribute assigntment.
#' @return html attribute string.
#'
#' The elements in {att} came in the form of {k*=v*}, write HTML attribute
#' string as {a1=\"v2*\", a2=\"v2*\"}.
HTM$str_att <- function(...)
{
    att <- c(...)
    if(length(att))
    {
        att <- mapply(sprintf, as.list(names(att)), att,
                      MoreArgs=list(fmt="%s=\"%s\""), SIMPLIFY=FALSE)
        att <- do.call(paste, att)
    }
    att
}

#' make style strings from a list.
#'
#' @param sty a list of style assignment.
#' @return html style string.
#'
#' The elements in {sty} came in the form of {k*=v*}, write HTML style string as
#' {style=\"e1=v1*;e2=v2*;\"}.
HTM$str_sty <- function(...)
{
    sty <- c(...)
    if(length(sty))
    {
        ## per item
        sty <- mapply(sprintf, as.list(names(sty)), sty,
                      MoreArgs=list(fmt="%s:%s;"), SIMPLIFY=FALSE)
        sty <- sprintf("style=\"%s\"", do.call(paste0, sty))
    }
    sty
}

#' capture screen output as pre-formated html
#'
#' @param ... expression to print out.
#' @param att list of additional attributes
#' @param sty list of styles
#' @param div sandwich <img src=..> between <div> & </div>? (def=0)
#'
#' Use <pre>..</pre> to preserve the output as it is.
HTM$out_pre <- function(..., att=NULL, sty=NULL, div=0)
{
    ## capture screen output
    txt <- paste0(capture.output(...), collapse="\n")
    ## compose html, handel attributes
    att <- str_att(att)
    att <- if(length(att)) paste0(" ", att) else ""
    sty <- str_sty(sty)
    sty <- if(length(sty)) paste0(" ", sty) else ""
    ## compose html
    htm <- sprintf('<pre%s%s>\n%s\n</pre>', att, sty, txt)
    if(div)
        htm <- sprintf("<div>%s</div>", htm)
    htm
}
HTM$pre_out <- HTM$out_pre

#' make tabbed buttons
#' 
#' @param lst list of items to display when buttons are pressed.
#' @param btx button text (def=names(lst)).
#' @param att list of additional attributes
#' @param sty list of styles
#' The output is always enclosed in a <div id="tab_wnd$[addr] class="tab_wnd">
#'
#' @examples
#' ## make a list of HTML content, one page per follow species
#' x <- lapply(split(iris, iris$Species), \(.)
#' {
#'     paste("<pre>",
#'           paste0(capture.output(print(head(.), row.names=FALSE)), collapse="\n"),
#'           "</pre>", sep="\n")
#' })
#' x <- tab_btn(x)
#' x <- paste0(x, collapse="\n")
#' cat(x, "\n", sep="")
HTM$tab_btn <- function(lst, btx=NULL, att=NULL, sty=NULL)
{
    ## www.brodieg.com/2019/02/18/an-unofficial-reference-for-internal-inspect
    pfx <- substring(capture.output(.Internal(inspect(lst)))[1], 2, 13)
    if(length(btx) < 1)
        btx <- names(lst)
    if(length(btx) < 1)
        btx <- sprintf("[%0d]", seq_along(lst))

    ## compose html, handel attributes
    att <- str_att(att)
    att <- if(length(att)) paste0(" ", att) else ""
    sty <- str_sty(sty)
    sty <- if(length(sty)) paste0(" ", sty) else ""
    ## compose html
    htm <- list()
    htm <- append(htm, sprintf('<div id="tab_wnd@%s" class="tab_wnd"%s%s>', pfx, att, sty))
    ## tab bar and buttons
    htm <- append(htm, sprintf('<div id="tab_bar@%s" class="tab_bar">', pfx))
    fmt <- '<button id="t%03x" class="tab_btn" onclick="tab_btn_click(event)">%s</button>'
    for(i in seq_along(lst))
        htm <- append(htm, sprintf(fmt, i, btx[i]))
    htm <- append(htm, '</div>')

    ## tab window and content
    htm <- append(htm, sprintf('<div id="tab_dsp@%s" class="tab_dsp">', pfx))
    fmt <- '<div id="t%03x" class="tab_txt" style="display:none">\n%s\n</div>'
    for(i in seq_along(lst))
    {
        if(length(lst[[i]]) < 1) # empty item have a button, but no content.
            next
        htm <- append(htm, sprintf(fmt, i, lst[[i]]))
    }
    htm <- append(htm, '</div>')

    ## javascript for tab click
    js <- '<script>
    function tab_btn_click(evt)
    {
        var i, pfx, bar, dsp, bts;
        /* get tab_btn, tab_bar and tab_dsp */
        btn = evt.currentTarget;
        bar = btn.parentElement;
        dsp = bar.parentElement.getElementsByClassName("tab_dsp")[0];

        /* activate clicked tab_btn, dectivate the rest */
        bts = bar.getElementsByClassName("active");
        for(i = 0; i < bts.length; i++)
        {
            bts[i].className = bts[i].className.replace(" active", "");
        }
        btn.className += " active"; // activate clicked tab_btn

        /* show tab_txt with the same id of clicked tab_btn, hide the rest*/ 
        for (i = 0; i < dsp.children.length; i++) 
        {
            if(dsp.children[i].id == btn.id)
            {
                dsp.children[i].style.display = "block";
            }
            else
            {
                dsp.children[i].style.display = "none";
            }
        }
    }
    </script>'
    htm <- append(htm, gsub("\n    ", "\n", js))
    htm <- append(htm, '</div>')
    htm
}

#' make dropdown buttons
#' 
#' @param key N x M data of N keys, each with M criteria
#' @param val N x 1 list of N display values corresponding to the keys.
#' @param btx M x 1 button text (def=names(key)).
#' @param att list of additional attributes
#' @param sty list of styles
#' @return the html of {M} dropdown buttons toggling lists of levels in {key},
#' and a display of content in {val}.
#'
#' Click a dropdown item displays contnet  in {val} corresponding to the current
#' combintion of factors in {key}.
#'
#' @examples
#' ## use Titanic Survivor data as an example
#' dat <- merge(
#'     as.data.frame(Titanic, responseName="NUM"),
#'     as.data.frame(proportions(Titanic, c("Class", "Sex", "Age")), responseName="PRP"))
#' dat <- within(dat, PRP[!is.na(PRP)] <- sprintf("%.1f%%", round(PRP[!is.na(PRP)] * 100)))
#' val <- with(dat, by(data.frame(Survived, NUM, PRP), list(Class, Sex, Age), \(.)
#' {
#'      paste("<pre>",
#'            paste0(capture.output(., row.names=FALSE), collapse="\n"),
#'            "</pre>", sep="\n")
#' }))
#' key <- expand.grid(dimnames(Titanic)[c("Class", "Sex", "Age")])
#' htm <- dpd_btn(key, val)
HTM$dpd_btn <- function(key, val, btx=NULL, att=NULL, sty=NULL)
{
    ## www.brodieg.com/2019/02/18/an-unofficial-reference-for-internal-inspect
    pfx <- substring(capture.output(.Internal(inspect(key)))[1], 2, 13)
    if(length(btx) < 1)
        btx <- names(key)
    if(length(btx) < 1)
        btx <- sprintf("[%0d]", seq_along(key))
    ## factor levels
    key <- as.data.frame(lapply(key, \(x) addNA(as.factor(x), ifany=TRUE)))
    lvl <- lapply(key, levels)
    
    ## compose html, handel attributes
    htm <- list()
    ## outer window
    att <- str_att(att); att <- if(length(att)) paste0(" ", att) else ""
    sty <- str_sty(sty); sty <- if(length(sty)) paste0(" ", sty) else ""
    htm <- append(htm, sprintf('<div id="dpd_wnd@%s" class="dpd_wnd"%s%s>', pfx, att, sty))
    ## outer window > top > bar
    ## htm <- append(htm, sprintf('<div id="dpd_top@%s" class="dpd_top">', pfx))
    htm <- append(htm, sprintf('<div id="dpd_bar@%s" class="dpd_bar">', pfx))
    ## outer window > top > bar > box(s)
    fmt.btn <- '<button id="%03x" class="dpd_btn" onclick="dpd_btn_click(event)">%s</button>'
    sty.box <- str_sty(`position`="relative", `display`="inline-block")
    sty.lst <- str_sty(`position`="absolute", `display`="block",
                       `border`="solid", `overflow`="auto", `z-index`=1)
    for(i in seq_along(key))
    {
        ## box > [button | item_list]
        htm <- append(htm, sprintf('<div id="%03x" class="dpd_box" %s>', i, sty.box))
        htm <- append(htm, sprintf(fmt.btn, i, btx[i]))
        htm <- append(htm, sprintf('<div id="%03x" class="dpd_lst" %s>', i, sty.lst))
        for(j in levels(key[[i]]))
        {
            htm <- append(htm, sprintf('<a id="%s" href="#">%s</a>', j, j))
        }
        htm <- append(htm, '</div>') # end of dpd_lst
        htm <- append(htm, '</div>') # end of dpd_box
    }
    htm <- append(htm, '</div>') # end of bar
    ## htm <- append(htm, '</div>') # end of top

    ## tab window and content
    ## htm <- append(htm, sprintf('<div id="dpd_dsp@%s" class="dpd_dsp">', pfx))
    ## fmt <- '<div id="%s" class="dpd_txt" style="display:none">\n%s\n</div>'
    ## for(i in seq(nrow(key)))
    ##     htm <- append(htm, sprintf(fmt, paste0("[", key[i, ], "]", collapse="."), val[[i]]))
    ## htm <- append(htm, '</div>')

    ## javascript for tab click
    js <- '<script>
    function dpd_btn_click(evt)
    {
        var i, pfx, bar, box, bts, bxs;
        /* get dpd_btn, dpd_bar and dpd_box */
        btn = evt.currentTarget;
        box = btn.parentElement;
        bar = box.parentElement;
        lst = box.getElementsByClassName("dpd_lst")[0];

        /* activate clicked dpd_btn, dectivate the rest */
        bts = bar.getElementsByClassName("active");
        for(i = 0; i < bts.length; i++)
        {
            bts[i].className = bts[i].className.replace(" active", "");
        }
        btn.className += " active"; // activate clicked dpd_btn

        /* show dpd_lst with the same id of clicked dpd_btn, hide the rest*/ 
        bxs = bar.getElementsByClassName("dpd_box");
        for (i = 0; i < bxs.length; i++) 
        {
            if(bxs[i].id == btn.id)
            {
                bxs[i].getElementsByClassName("dpd_lst")[0].style.display = "block";
            }
            else
            {
                bxs[i].getElementsByClassName("dpd_lst")[0].style.display = "none";
            }
        }
    }
    </script>'
    htm <- append(htm, gsub("\n    ", "\n", js))
    htm <- append(htm, '</div>') # end of outer window
    paste0(htm, collapse="\n")
}

if("TCOM:HTM" %in% search())
    detach("TCOM:HTM")
attach(HTM, name="TCOM:HTM", warn.conflicts = TRUE)
rm(HTM)
