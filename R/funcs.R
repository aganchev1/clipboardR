#' Copy data out of R
#'
#' @param obj data of type data.frame, zoo, xts or list
#' @param row.names should row names be included defaults to FALSE
#' @param size set directory size - leave default as 4KB
#'
#' @return Nothing
#' @examples
#' copy_table(CO2)
#'
#' @author Nicholas Marshall
#' @export
copy_table <- function(obj, row.names = FALSE, size = 4096) UseMethod("copy_table")

#' Copy data out of R
#'
#' @param obj data of type data.frame, zoo, xts or list
#' @param row.names should row names be included defaults to FALSE
#' @param size set directory size - leave default as 4KB
#' @export
copy_table.default <- function(obj, row.names = FALSE, size = 4096) {
  clip <- paste0('clipboard-', size)
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = row.names, sep = '\t')
  close(f)
}

copy_table.data.frame <- function(obj, row.names = FALSE, size = 4096) {
  clip <- paste0('clipboard-', size)
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = row.names, col.names = colnames(obj), sep = '\t')
  close(f)
}

#' Copy data out of R
#'
#' @param obj data of type data.frame, zoo, xts or list
#' @param row.names should row names be included defaults to FALSE
#' @param size set directory size - leave default as 4KB
#' @export
copy_table.zoo <- function(obj, row.names = FALSE, size = 4096) {
  obj.df <- data.frame(date = zoo::index(obj), zoo::coredata(obj))
  colnames(obj.df) <- c("date", colnames(obj))
  copy_table.data.frame(obj.df, row.names, size)
}


#' Paste and object from the clipboard
#'
#' Returns the clipboard as a \pkg{data.frame}.
#'
#' @return A \pkg{data.frame} of the data in the clipboard.
#'
#' @author Nicholas Marshall
#' @export
paste_table <- function() {
  f <- file(description = 'clipboard', open = 'r')
  on.exit(close(f))
  df <- read.table(f, sep = '\t', header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
  return(df)
}
