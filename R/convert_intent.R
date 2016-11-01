#' @title Convert Intent
#' @description Converts the \code{intent} field from a GIFTI image
#' to a more standard naming
#'
#' @param intent (character) string of intent type
#'
#' @return A character string
#' @export
convert_intent = function(intent) {
  namer = switch(
    intent,
    'NIFTI_INTENT_POINTSET' = "vertices",
    'NIFTI_INTENT_TRIANGLE' = "faces",
    'NIFTI_INTENT_NODE_INDEX' = "indices",
    'NIFTI_INTENT_VECTOR' = "normals",
    'NIFTI_INTENT_NONE' = "cdata"
  )
  if (is.null(namer)) {
    stop("intent is wrong")
  }
  return(namer)
}