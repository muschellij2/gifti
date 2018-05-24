
#' Create Data Matrix with ordering respected
#'
#' @param data Data output from \code{\link{data_decoder}}
#' @param dims Dimensions of output
#' @param ordering Ordering of the data
#'
#' @return Matrix of Values
#' @export
create_data_matrix = function(
  data,
  dims,
  ordering =  c("RowMajorOrder",
                "ColumnMajorOrder")) {
  ordering = match.arg(ordering)

  ndims = length(dims)

  byrow = ordering %in% "RowMajorOrder"


  if (ndims == 1) {
    data = matrix(data,
                  nrow = dims[1],
                  ncol = 1)
  } else if (ndims == 2) {
    data = matrix(data,
                  nrow = dims[1],
                  ncol = dims[2], byrow = byrow)
  } else {
    stop("not implemented")
  }

  return(data)
}
