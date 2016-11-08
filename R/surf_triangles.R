#' @title Make Triangles from GIfTI Image
#' @description Creates Triangles for plotting in RGL from a GIfTI image
#' @param file File name of GIfTI image, usuall \code{surf.gii}
#'
#' @return List of values corresponding to the \code{data} element from
#' \code{\link{readgii}}
#' @export
surf_triangles = function(file) {
  L = readgii(file)
  L = L$data
  # n_l = names(L)
  L$faces = as.vector(t(L$faces) + 1)
  L$vertices = L$vertices[ L$faces, ]
  L$normals = L$normals[L$faces,]
  L$cdata = L$cdata[L$faces]
  L$unknown = L$unknown[L$faces]
  return(L)
}
