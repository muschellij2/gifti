#' @title Make Triangles from GIfTI Image
#' @description Creates Triangles for plotting in RGL from a GIfTI image
#' @param file File name of GIfTI image, usually \code{surf.gii}
#'
#' @return List of values corresponding to the \code{data} element from
#' \code{\link{readgii}}
#' @export
surf_triangles = function(file) {

  if (is.gifti(file)) {
    L = file
  } else {
    L = readgii(file)
  }
  L = L$data
  # n_l = names(L)
  L$triangle = as.vector(t(L$triangle) + 1)
  L$pointset = L$pointset[ L$triangle, ]
  L$normal = L$normal[L$triangle,]
  n = names(L)
  n = which(!n %in% c("triangle", "pointset", "normal"))
  if (length(n) > 0) {
    for (iname in n) {
      x = L[[iname]]
      if (is.vector(x)) {
        x = x[ L$triangle ]
      }
      if (is.matrix(x)) {
        x = x[ L$triangle, ]
      }
      L[[iname]] = x
    }
  }
  return(L)
}
