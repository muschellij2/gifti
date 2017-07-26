#' @title Map Values to Triangles from GIFTI
#' @description Takes values and maps them to the correct triangles in
#' space.
#'
#' @param pointset pointset from GIFTI
#' @param triangle triangles from GIFTI
#' @param values Values to map to the triangles.  Same length as indices
#' @param indices indices to place the values, must be in the range of 1 and
#' the number of rows of \code{pointset}
#' @param add_one Should \code{1} be added to the indices for the triangle?
#'
#' @return A list of coordinates (in triangles) and the corresponding value
#' mapped to those triangles
#' @export
gifti_map_value = function(
  pointset,
  triangle,
  values,
  indices = seq(nrow(pointset)),
  add_one = TRUE) {
  stopifnot(length(values) == length(indices))
  if (any(indices < 1)) {
    stop("Indices have numbers less than zero!")
  }

  dat = rep(NA, length = nrow(pointset))
  dat[ indices ] = values

  tri = as.vector(t(triangle) + add_one)
  dat = dat[tri]

  pointset = pointset[tri,]
  L = list(coords = pointset,
           values = dat)
}
