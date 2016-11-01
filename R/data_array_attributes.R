#' @title Data Array Attributes
#' @description Parses a list of XML data to get the attributes
#'
#' @param darray List of \code{xml_nodes} from GIFTI data array
#'
#' @return \code{data.frame} of attributes
#' @export
#' @importFrom xml2 xml_attrs
data_array_attributes = function(darray) {
  ################################################
  # Get external header information
  ################################################
  info = lapply(darray, xml_attrs)
  n1 = names(info[[1]])
  check = sapply(n1, function(x) {
    all(names(x) == n1)
  })
  if (!all(check)) {
    stop("Different named elements for each data array!")
  }
  info = do.call("rbind", info)
  info = data.frame(info, stringsAsFactors = FALSE)
  ################################################
  # Getting dimensions
  ################################################
  n1 = colnames(info)
  dims = grep("^Dim\\d", n1, value = TRUE)
  for (icol in dims) {
    info[, icol] = as.numeric(info[, icol])
  }
  info$n = apply(info[, dims, drop = FALSE], 1, prod)
  return(info)
}