#' xlsx-based Data viewer on a matrix-like R object
#'
#' Invokes a xlsx-implemented viewer for a more handsome look of matrix-like R objects especially under Linux.
#'
#'@param dfr an R object which can be coerced to a data frame with non-zero numbers of rows and columns. 
#'@param name Optional: name of the temporarily saved file (without file extension, see examples). May be useful if several files are opened in Excel to differentiate between them. 
#'@param large Logical: If the data set is large, the procedure becomes very time-consuming. With \code{large = TRUE} a faster procedure (based on csv instead of Excel) is used that may be suitable for large datasets. However, the result does not look quite as nice as in Excel. 
#'@param font character of the font which should be used. Font must be installed in the operating system 
#'@param font_size Numeric value of font size for displaying the results 
#'@param headerBackground hex code for background color of the column header, see https://htmlcolorcodes.com/
#'@param stripColor hex code for striped color of the rows, see https://htmlcolorcodes.com/
#'@param digits numerical value of decimal places to round to. If \code{NULL}, the values are not rounded.
#'@param row.names logical value indicating whether the row names of \code{dfr} are to be written along with \code{dfr}. Only works for csv files, i.e., if \code{large = TRUE}. 
#'
#'@details The R-based data viewer under Linux is not very clear and descriptive. A better display can be achieved by temporarily saving the data object as a formatted Excel file and then opening it with Excel or Libre Office. For this purpose, functions of the R package openxlsx2 are used internally. Please note that the procedure can be very slow for larger data sets. In this case, use the option \code{large = TRUE} which generates only a csv file instead of a formatted Excel file, which is considerably faster for large data sets. It is recommended to use \code{large = TRUE} for data sets with more than 1000 lines. 
#'
#'@examples
#'\dontrun{
#'xview(mtcars)
#'xview(mtcars, name = "mtcars")
#'}
#'@export

xview <- function(dfr, name = NULL, large = FALSE, font = "Carlito", font_size = 11, headerBackground = "#7dcea0", stripColor = "#fadbd8", digits = 3, row.names=FALSE) {
    lapply(list(font_size), checkmate::assert_numeric, len = 1, lower = 1,  any.missing = FALSE)
    lapply(list(digits), checkmate::assert_numeric, len = 1, lower = 1, null.ok = TRUE, any.missing = FALSE)
    lapply(list(name), checkmate::assert_character, len = 1, null.ok = TRUE, any.missing = FALSE)
    if (is.null(dim(dfr))) {
        dfr <- data.frame ( dfr)
    }  else  {
        dfr <- eatTools::makeDataFrame(dfr, verbose=FALSE)
    }
    if (!is.null(digits)) {dfr <- eatTools::roundDF(dfr, digits =digits)}
    if(isFALSE(large)) {
        suff     <- ".xlsx"
        inp      <- openxlsx2::temp_xlsx()
        wb       <- openxlsx2::wb_workbook() |> openxlsx2::wb_add_worksheet(sheet = 1) |> openxlsx2::wb_add_data(x=dfr, na.strings = "") |> openxlsx2::wb_save(file = inp)
        totalCols<- list(first = LETTERS, second = expand.grid(LETTERS, LETTERS), third = expand.grid(LETTERS, LETTERS, LETTERS))
        totalCols<- c(totalCols[[1]], paste0(totalCols[[2]][, 2], totalCols[[2]][, 1]), paste0(totalCols[[3]][, 3], totalCols[[3]][,2], totalCols[[3]][, 1]))
        wb       <- openxlsx2::wb_load(inp) |> openxlsx2::wb_add_cell_style(sheet = 1, dims = paste0("A1:", totalCols[ncol(dfr)],  nrow(dfr) + 1), vertical = "center")
        dims     <- attr(openxlsx2::wb_data(wb, sheet = 1), "dims")
        TO       <- openxlsx2::int2col(ncol(dims))
        ROW      <- nrow(dims)
        headDim  <- sprintf("A1:%s1", TO)
        bodyDim  <- sprintf("A2:%s%s", TO, ROW)
        wb       <- wb |> openxlsx2::wb_add_fill(sheet = 1, dims = headDim, color = openxlsx2::wb_color(headerBackground)) |> openxlsx2::wb_add_font(sheet = 1, bold = 1, dims = headDim, color = openxlsx2::wb_color("black"), size = font_size, name = font) |>
                    openxlsx2::wb_set_row_heights(sheet = 1, rows = 1:nrow(dfr), heights = 20) |>
                    openxlsx2::wb_add_border(sheet = 1, dims = headDim, top_color = openxlsx2::wb_color("black"), bottom_color = openxlsx2::wb_color("black"),  left_color = openxlsx2::wb_color("black"), right_color = openxlsx2::wb_color("black"), inner_vcolor = openxlsx2::wb_color("white"), top_border = "medium", bottom_border = "medium", left_border = "medium", right_border = "medium", inner_vgrid = "medium" ) |>
                    openxlsx2::wb_add_font(sheet = 1, dims = bodyDim, size = font_size, name = font) |>
                    openxlsx2::wb_add_border(sheet = 1, dims = bodyDim,top_color = openxlsx2::wb_color("black"), bottom_color = openxlsx2::wb_color("black"), left_color = openxlsx2::wb_color("black"), right_color = openxlsx2::wb_color("black"),inner_vcolor = openxlsx2::wb_color("black"), top_border = "thin", bottom_border = "thin", left_border = "thin", right_border = "thin", inner_vgrid = "thin") |>
                    openxlsx2::wb_add_filter(sheet = 1, rows=1, cols=1:ncol(dfr)) |> openxlsx2::wb_freeze_pane(sheet=1, first_active_row = 2) |> openxlsx2::wb_add_fill(sheet = 1, dims = bodyDim, color = openxlsx2::wb_color(stripColor), every_nth_row = 2, every_nth_col = 1)
    } else {
        suff     <- ".csv"
    }
    if(!is.null(name)) {
        name <- eatTools::cleanifyString(name)
        file <- paste0(name, suff)
    } else {
        file <- paste0("P1", suff)
        name <- "P"
    }
    i        <- 1
    while(file.exists(file.path(tempdir(), file))) {i <- i+1; file <- paste0(name, i, suff)}
    if(isFALSE(large)) {
        openxlsx2::wb_save(wb, file.path(tempdir(),file))
    } else {
        write.csv2(dfr, file.path(tempdir(),file), na="", row.names=row.names)
    }
    sysInfo  <- Sys.info()
    if(sysInfo[["sysname"]] == "Linux") {
       #open_calc_focus(file=file.path(tempdir(),file))
       openxlsx::openXL(file = file.path(tempdir(), file))
    } else  {
       shell(file.path(tempdir(),file), wait = FALSE)
    }}

