library(openxlsx)
library(readODS)

#' Save Table-Like Objects To Excel Worksheets
#'
#' Write one table-like object, or a list of table-like objects, into an Excel
#' workbook using \pkg{openxlsx}. If the workbook already exists, any target
#' sheets are replaced. When `sheet_name` is omitted, the function infers a
#' default name from the object supplied as `x`.
#'
#' @param x A data.frame, an object convertable to a data.frame, or a list of
#'   such objects to write into the workbook.
#' @param path A single file path to the target `.xlsx` workbook.
#' @param sheet_name Optional worksheet name. If `NULL`, the name is inferred
#'   from the calling expression for `x`. Unnamed list elements use this base
#'   name followed by their position.
#' @param overwrite Logical scalar passed to [openxlsx::saveWorkbook()].
#'
#' @return Invisibly returns `path`.
#'
#' @examples
#' df <- data.frame(a = 1:3, b = c("x", "y", "z"))
#' saveXLS(df, tempfile(fileext = ".xlsx"))
#'
#' lst <- list(first = df, df[1:2, ])
#' saveXLS(lst, tempfile(fileext = ".xlsx"), sheet_name = "Table")
#'
#' @export
saveXLS <- function(x, path, sheet_name = NULL, overwrite = TRUE) {
    ## -------------------- Capture Calling Object Name -------------------- %%
    ## Keep the original symbol name so it can become the de-fault sheet name.
    ## That inferred name is used whenever the caller leaves it un-specified.
    object_name <- deparse(substitute(x))

    ## ------------------------- Validate File Path ------------------------ %%
    ## Require one non-empty character string before file work is at-tempted.
    ## This guards the read and write steps from a malformed workbook path.
    if (!is.character(path) || length(path) != 1L ||
        is.na(path) || !nzchar(path)) {
        stop("'path' must be a non-empty file path.", call. = FALSE)
    }

    ## ---------------------- Resolve Worksheet Name ----------------------- %%
    ## If no sheet name is supplied, reuse the caller's captured object name.
    ## Fall back to a plain default only when that cap-tured name is empty.
    if (is.null(sheet_name)) {
        sheet_name <- object_name
        if (!nzchar(sheet_name)) {
            sheet_name <- "Sheet1"
        }
    }

    ## ---------------------- Validate Worksheet Name ---------------------- %%
    ## Ensure the worksheet name is one non-empty string before use in Excel.
    ## This keeps later openxlsx work-book operations simple and predictable.
    if (!is.character(sheet_name) || length(sheet_name) != 1L ||
        is.na(sheet_name) || !nzchar(sheet_name)) {
        stop("'sheet_name' must be a non-empty character string.", call. = FALSE)
    }

    ## ----------------------- Normalize Input Tables ---------------------- %%
    ## Wrap one table-like object into a list so the write path can handle one
    ## or many sheets with the same loop and validation logic.
    if (!is.list(x) || is.data.frame(x)) {
        x <- list(x)
    }

    ## ----------------------- Prepare Worksheet Data ---------------------- %%
    ## Convert each incoming object into a data.frame and assign a worksheet
    ## name. Unnamed entries reuse the base sheet name plus list position.
    sheet_data <- vector("list", length(x))
    sheet_names <- names(x)
    if (is.null(sheet_names)) {
        sheet_names <- rep("", length(x))
    }

    for (idx in seq_along(x)) {
        sheet_data[[idx]] <- tryCatch(
            as.data.frame(x[[idx]], stringsAsFactors = FALSE),
            error = function(e) NULL
        )

        if (is.null(sheet_data[[idx]])) {
            stop("Each element of 'x' must be a data.frame or convertable to one.",
                 call. = FALSE)
        }

        if (is.null(sheet_names) || is.na(sheet_names[[idx]]) ||
            !nzchar(sheet_names[[idx]])) {
            sheet_names[[idx]] <- paste0(sheet_name, idx)
        }
    }

    ## --------------------- Check Worksheet Names ------------------------- %%
    ## Ensure the final worksheet names are unique before any workbook writes
    ## begin so the resulting file layout stays deterministic.
    if (anyDuplicated(sheet_names)) {
        stop("Worksheet names must be unique after name resolution.",
             call. = FALSE)
    }

    ## --------------------- Open Or Create Workbook ----------------------- %%
    ## Load the existing workbook when the file is already present on disk.
    ## Replace any matching sheet so the final write result stays deterministic.
    if (file.exists(path)) {
        wb <- loadWorkbook(path)
    } else {
        wb <- createWorkbook()
    }

    for (idx in seq_along(sheet_data)) {
        current_name <- sheet_names[[idx]]
        current_data <- sheet_data[[idx]]

        #' Create A Cell Style For Table Edges
        #'
        #' Build a style object for header or body cells using optional bold
        #' text and thick borders on selected outer edges.
        #'
        #' @param bold Logical scalar indicating whether text should be bold.
        #' @param top Logical scalar indicating whether to draw a top border.
        #' @param bottom Logical scalar indicating whether to draw a bottom border.
        #' @param left Logical scalar indicating whether to draw a left border.
        #' @param right Logical scalar indicating whether to draw a right border.
        #' @param halign Optional horizontal alignment passed to
        #'   [openxlsx::createStyle()].
        #'
        #' @return An `openxlsx` style object.
        make_style <- function(bold = FALSE, top = FALSE, bottom = FALSE,
                               left = FALSE, right = FALSE, halign = NULL) {
            ## --------------------- Collect Edge Flags --------------------- %%
            ## Build the border-side list ex-pected by openxlsx from edge flags.
            ## The helper turns simple booleans into the border names to apply.
            borders <- character(0)

            if (top) {
                borders <- c(borders, "top")
            }
            if (bottom) {
                borders <- c(borders, "bottom")
            }
            if (left) {
                borders <- c(borders, "left")
            }
            if (right) {
                borders <- c(borders, "right")
            }

            ## ---------------------- Build Cell Style ---------------------- %%
            ## Create one style object that can op-tionally bold text and frame.
            ## Only the requested outer edges receive the thick line treatment.
            createStyle(
                textDecoration = if (bold) "bold" else NULL,
                halign = halign,
                border = if (length(borders) > 0L) borders else NULL,
                borderStyle = if (length(borders) > 0L) "thick" else NULL
            )
        }

        ## ------------------------ Replace Old Sheet ------------------------ %%
        ## Remove any prior worksheet with the resolved name before writing the
        ## fresh table contents into the workbook.
        if (current_name %in% names(wb)) {
            removeWorksheet(wb, current_name)
        }

        ## ------------------------- Write Table Data -------------------------- %%
        ## Add the target worksheet and write the full data.frame into that sheet.
        ## Keep Excel's filter row enabled for quick man-ual exploration after save.
        addWorksheet(wb, current_name)
        writeData(wb, sheet = current_name, x = current_data, withFilter = TRUE)

        ## ------------------------ Apply Table Styling ------------------------ %%
        ## Decorate the table only when columns exist in the incoming data.frame.
        ## The header and body each re-ceive the thick outer-edge styling needed.
        if (ncol(current_data) > 0L) {
            M <- ncol(current_data)
            N <- nrow(current_data)
            ## ------------------------ Style Body Cells ------------------------ %%
            ## Apply thin borders on interior cell edges, including the header
            if (N > 1L) {
                addStyle(
                    wb,
                    sheet = current_name,
                    style = createStyle(border = "bottom", borderStyle = "thin"),
                    rows = 2L:N,
                    cols = 1L:M,
                    gridExpand = TRUE,
                    stack = TRUE
                )
            }
            if(M > 1L) {
                addStyle(
                    wb,
                    sheet = current_name,
                    style = createStyle(border = "right", borderStyle = "thin"),
                    rows = 1L:(N + 1L),
                    cols = 1L:(M - 1L),
                    gridExpand = TRUE,
                    stack = TRUE
                )
            }

            ## ----------------------- Style Header Cells ----------------------- %%
            ## Walk across the header row and apply bold text with thick borders.
            ## The first and last cells complete the out-er edge of the header box.
            header_style <- make_style(bold = TRUE, top = TRUE, bottom = TRUE,
                                       halign = "center")
            addStyle(wb, sheet = current_name, style = header_style, rows = 1L,
                     cols = 1L:M, gridExpand = FALSE, stack = TRUE)
            addStyle(wb, sheet = current_name, style = make_style(left=TRUE),
                     rows = 1L, cols = 1L, gridExpand = FALSE, stack = TRUE)
            addStyle(wb, sheet = current_name, style = make_style(right=TRUE),
                     rows = 1L, cols = M, gridExpand = FALSE, stack = TRUE)
            ## -------------------------- Style Columns ------------------------- %%
            if (N > 0L) {
                for(j in seq_len(M)) {
                    x <- current_data[[j]]
                    col_align <- if (is.factor(x)) {
                                     "center"
                                 } else if (is.numeric(x)) {
                                     "right"
                                 } else if (is.character(x)) {
                                     "left"
                                 } else {
                                     NULL
                                 }
                    addStyle(wb,
                             sheet = current_name,
                             style = make_style(halign=col_align),
                             rows = seq(2L, N + 1L),
                             cols = j,
                             gridExpand = FALSE,
                             stack = TRUE)
                }
            }

            ## ----------------------- Style Outer Border ----------------------- %%
            ## Apply thick borders only on the outer perimeter of the data body.
            ## In-terior cell edges are left unchanged so the body stays readable.
            if (N > 0L) {
                addStyle(wb, sheet = current_name,
                         style = make_style(left=TRUE), rows = 2L:(N + 1L),
                         cols = 1L, gridExpand = FALSE, stack = TRUE)
                addStyle(wb, sheet = current_name,
                         style = make_style(right=TRUE), rows = 2L:(N + 1L),
                         cols = M, gridExpand = FALSE, stack = TRUE)
                addStyle(wb, sheet = current_name,
                         style = make_style(bottom=TRUE), rows = N + 1L,
                         cols = 1L:M, gridExpand = FALSE, stack = TRUE)
            }
        }

        ## ------------------------ Freeze Worksheet ------------------------- %%
        ## Freeze the header row on each worksheet before the workbook is saved.
        ## This keeps longer tables easier to browse after they are opened.
        freezePane(wb, sheet = current_name, firstRow = TRUE)
    }

    ## ------------------------ Save Workbook ----------------------------- %%
    ## Save the workbook after all sheets are written and return the file path
    ## in-visibly so callers can chain or inspect it.
    saveWorkbook(wb, file = path, overwrite = overwrite)

    invisible(path)
}
