#' @title Write .gii xml from "gifti" object
#' @description Writes a "gifti" object to a GIFTI file (ends in *.gii).
#'
#' @param gii The "gifti" object
#' @param out_file Where to write the new GIFTI file
#' @param use_parsed_transformations Should the \code{$parsed_transformations}
#'  be written instead of the \code{transformations}? Use if the XML pointers
#'  in \code{transformations} might be invalid. Default: \code{FALSE}
#'
#' @import xml2
#' @export
writegii <- function(gii, out_file, use_parsed_transformations=FALSE){
  stopifnot(is.gifti(gii))

  # GIFTI ROOT
  root <- xml2::xml_add_child(xml2::xml_new_document(),
    "GIFTI",
    Version=gii$version,
    `xmlns:xsi`="http://www.w3.org/2001/XMLSchema-instance",
    noNamespaceSchemaLocation="http://brainvis.wustl.edu/caret6/xml_schemas/GIFTI_Caret.xsd",
    NumberOfDataArrays=length(gii$data)
  )

  # Can't get this to work.
  # So, I add the doctype declaration at the end via gsub
  #xml_add_child(root, as.character(xml_dtd("GIFTI" ,"SYSTEM", "\"http://www.nitrc.org/frs/download.php/1594/gifti.dtd\">")))

  # META DATA
  file_meta <- xml2::xml_add_child(root, "MetaData")
  if (length(gii$file_meta) > 0) {
    for (ii in 1:length(gii$file_meta)) {
      MD_ii <- xml2::xml_add_child(file_meta, "MD")
      N_ii <- xml2::xml_add_child(MD_ii, "Name")
      xml2::xml_add_child(
        N_ii,
                          xml2::xml_cdata(attributes(gii$file_meta)$names[ii]))
      V_ii <- xml2::xml_add_child(MD_ii, "Value")
      xml2::xml_add_child(
        V_ii,
                          xml2::xml_cdata(as.character(gii$file_meta)[ii]))
    }
  }

  # LABEL TABLE
  labels <- xml2::xml_add_child(root, "LabelTable")
  if (!is.null(gii$label)) {
    for (ii in 1:nrow(gii$label)) {
      label_ii <- xml2::xml_add_child(
        labels, "Label",
        Key=gii$label[ii,which(colnames(gii$label)=="Key")],
        Red=gii$label[ii,which(colnames(gii$label)=="Red")],
        Green=gii$label[ii,which(colnames(gii$label)=="Green")],
        Blue=gii$label[ii,which(colnames(gii$label)=="Blue")],
        Alpha=gii$label[ii,which(colnames(gii$label)=="Alpha")]
      )
      xml2::xml_add_child(
        label_ii, xml2::xml_cdata(rownames(gii$label)[ii]))
    }
  }

  # DATA ARRAY
  for (ii in 1:length(gii$data)) {
    D_ii <- xml2::xml_add_child(root, "DataArray")

    # DataArray Attributes
    for (jj in 1:ncol(gii$data_info)) {
      atr_jj <- colnames(gii$data_info)[jj]
      if (atr_jj %in% c("n", "name")) {next}
      xml_attr(D_ii, atr_jj) <- gii$data_info[ii, atr_jj]
    }

    # DataArray MetaData
    D_ii_meta <- xml2::xml_add_child(D_ii, "MetaData")
    if (nrow(gii$data_meta[[ii]] > 0)) {
      for (jj in 1:nrow(gii$data_meta[[ii]])) {
        MD_jj <- xml2::xml_add_child(D_ii_meta, "MD")
        N_jj <- xml2::xml_add_child(MD_jj, "Name")
        xml2::xml_add_child(
          N_jj, xml2::xml_cdata(gii$data_meta[[ii]][jj,1]))
        V_jj <- xml2::xml_add_child(MD_jj, "Value")
        xml2::xml_add_child(V_jj,
                            xml2::xml_cdata(gii$data_meta[[ii]][jj,2]))
      }
    }

    # DataArray Transformations
    ## From parsed
    if (use_parsed_transformations && !is.null(gii$transformations[[ii]])){
      for (jj in 1:(length(gii$parsed_transformations[[ii]])/3)) {
        CSTM <- gii$parsed_transformations[[ii]][(jj-1)*3 + 1:3]
        T_jj <- xml2::xml_add_child(D_ii, "CoordinateSystemTransformMatrix")
        T_D_jj <- xml2::xml_add_child(T_jj, "DataSpace")
        xml2::xml_add_child(T_D_jj,
                            xml2::xml_cdata(CSTM[[1]]))
        T_D_jj <- xml2::xml_add_child(T_jj, "TransformedSpace")
        xml2::xml_add_child(T_D_jj, xml2::xml_cdata(CSTM[[2]]))
        T_D_jj <- xml2::xml_add_child(
          T_jj, "MatrixData",
          paste(apply(CSTM[[3]], 1, paste, collapse=" "), collapse="\n")
        )
      }
    ## From not parsed
    } else {
      for (jj in 1:length(gii$transformations[[ii]])) {
        CSTM <- gii$transformations[[ii]][[jj]]
        if (is.null(CSTM)) {next}
        T_jj <- xml2::xml_add_child(D_ii, "CoordinateSystemTransformMatrix")
        xml2::xml_replace(T_jj, CSTM)
      }
    }

    # DataArray Data
    # [TO DO]: external files?
    # [TO DO]: resolve below case
    if (gii$data_info$Encoding[ii] != "ASCII" &&
        gii$data_info$DataType[ii] == "NIFTI_TYPE_INT32") {
      stop("Not working right now: NIFTI_TYPE_INT32 and non-ASCII encoding.")
    }
    dat <- gii$data[[ii]]
    if ((length(dim(dat)) > 1) &&
        gii$data_info$ArrayIndexingOrder[ii]=="RowMajorOrder") {
      dat <- aperm(dat, length(dim(dat)):1)
    }
    D_ii_data <- xml2::xml_add_child(
      D_ii,
      "Data",
      data_encoder(
        as.numeric(dat),
        encoding = gii$data_info$Encoding[ii],
        datatype = gii$data_info$DataType[ii],
        endian = gii$data_info$Endian[ii]
      )
    )
  }

  # Add doctag and write it.
  to_write <- as.character(root)
  to_write <- gsub("<GIFTI", '<!DOCTYPE GIFTI SYSTEM "http://www.nitrc.org/frs/download.php/1594/gifti.dtd">
  <GIFTI', to_write, fixed=TRUE)
  writeLines(to_write, out_file)
}

#' @rdname writegii
#' @export
writeGIfTI = function(gii, out_file, use_parsed_transformations=FALSE){
  writegii(gii, out_file, use_parsed_transformations)
}

#' @rdname writegii
#' @export
write_gifti = function(gii, out_file, use_parsed_transformations=FALSE){
  writegii(gii, out_file, use_parsed_transformations)
}
