#' @title Write GIFTI from Template
#' @description Writes a GIFTI file identical to an existing file, except with
#'  certain values replaced.
#'
#' @param template_file Path to the template GIFTI file
#' @param out_file Where to write the new GIFTI file
#' @param new_data List of new data values. The length of \code{new_data}
#'  should match the number of "DataArray"s in the template GIFTI file. Default
#'  \code{NULL} will leave the data unchanged.
#' 
#' @import xml2
#' @export
write_gifti_from_template = function(template_file, out_file, new_data=NULL){
  stopifnot(file.exists(template_file))
  if (!is.null(new_data)) { stopifnot(is.list(new_data)) }

  gii = read_xml(template_file)
  gii = as_list(gii)
  stopifnot(identical(names(gii), "GIFTI"))

  DataArray_mask = names(gii$GIFTI) == "DataArray"
  n_DataArray = sum(DataArray_mask) # xml_attr(gii_xml, "NumberOfDataArrays")
  if (!is.null(new_data)) { 
    stopifnot(is.list(new_data)) 
  } else {
    new_data = vector("list", n_DataArray)
  }

  for (ii in 1:length(new_data)) {
    # Read the attributes (including dimensions) and data.
    old_data = gii$GIFTI[[which(DataArray_mask)[ii]]]
    att = attributes(old_data)
    dims = att[paste0("Dim", 0:(as.numeric(att$Dimensionality)-1))]

    # Check that the data are compatible. 
    # [TO-DO]: check data type e.g. integer.s
    # If no new data was provided, use the existing data.
    if (!is.null(new_data[[ii]])) {
      stopifnot(all.equal(dim(new_data[[ii]]), dims))
    } else {
      new_data[[ii]] = data_decoder(
        old_data$Data[[1]], 
        encoding=att$Encoding, 
        datatype=att$DataType, 
        endian=att$Endian
      )
    }

    # Encode new data.
    gii$GIFTI[[which(DataArray_mask)[ii]]]$Data[[1]] = data_encoder(
      new_data[[ii]],
      encoding=att$Encoding, 
      datatype=att$DataType, 
      endian=att$Endian
    )
  }

  #gii = add_CDATA_to_gifti(gii)
  write_xml(as_xml_document(gii), out_file, options = c("format", "as_xml"))

  invisible()
}

# add_CDATA_to_gifti = function(gii) {
#   stop("Does not work.")

#   make_CDATA = function(x){ paste0("<![CDATA[",x,"]]>") }
#   add_CDATA_to_MetaData = function(MetaData) {
#     for(ii in 1:length(MetaData)){
#       MetaData[[ii]]$Name = make_CDATA(MetaData[[ii]]$Name)
#       MetaData[[ii]]$Value = make_CDATA(MetaData[[ii]]$Value)
#     }
#     MetaData
#   }

#   # File metadata.
#   gii$GIFTI$MetaData = add_CDATA_to_MetaData(gii$GIFTI$MetaData)

#   # Metadata of each DataArray.
#   DataArray_mask = names(gii$GIFTI) == "DataArray"
#   n_DataArray = sum(DataArray_mask)
#   for(jj in 1:length(n_DataArray)){
#     for(ii in 1:length(gii$GIFTI[[which(DataArray_mask)[jj]]]$MetaData)){
#       gii$GIFTI[[which(DataArray_mask)[jj]]]$MetaData = add_CDATA_to_MetaData(
#         gii$GIFTI[[which(DataArray_mask)[jj]]]$MetaData
#       )
#     }
#   }

#   gii
# }