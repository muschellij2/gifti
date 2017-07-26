#' @title Test if GIFTI
#' @description Simple wrapper to determine if class is GIFTI
#'
#' @param x object to test
#'
#' @return Logical if \code{x} is GIFTI
#' @export
is.gifti = function(x) {
  inherits(x, "gifti")
}

#' @export
#' @rdname is.gifti
is_gifti = is.gifti