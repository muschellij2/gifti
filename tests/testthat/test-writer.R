testthat::test_that("writing gifti files", {
  tdir = tempdir()
  download_gifti_data(tdir)
  expect_true(have_gifti_test_data(tdir))

  ######################################
  # Read files
  ######################################
  gii_files = download_gifti_data(tdir)
  gii_list = lapply(gii_files, readgii)

  ##########################################
  # Write files, read again, and check match
  ##########################################
  gii_out <- file.path(tdir, "written.gii")
  for (ii in 1:length(gii_list)) {
    gii1 <- gii_list[[ii]]

    # error #5
    gii1$data_info$Encoding <- ifelse(
      gii1$data_info$DataType == "NIFTI_TYPE_INT32",
      "ASCII",
      gii1$data_info$Encoding
    )

    writegii(gii1, gii_out)
    gii2 <- readgii(gii_out)

    # transformations is an XML pointer, which may differ
    gii1 <- gii1[names(gii1) != "transformations"]
    gii2 <- gii2[names(gii2) != "transformations"]

    expect_equal(gii1, gii2)
  }

  #################################################
  # Same as above, but write parsed transformations
  #################################################
  gii_out <- file.path(tdir, "written.gii")
  for (ii in 1:length(gii_list)) {
    gii1 <- gii_list[[ii]]

    # error #5
    gii1$data_info$Encoding <- ifelse(
      gii1$data_info$DataType == "NIFTI_TYPE_INT32",
      "ASCII",
      gii1$data_info$Encoding
    )

    writegii(gii1, gii_out, use_parsed_transformations = TRUE)
    gii2 <- readgii(gii_out)

    # transformations is an XML pointer, which may differ
    gii1 <- gii1[names(gii1) != "transformations"]
    gii2 <- gii2[names(gii2) != "transformations"]

    expect_equal(gii1, gii2)
  }
})
