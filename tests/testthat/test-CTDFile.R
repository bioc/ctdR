test_that("CTDFile() constructs a BiocFile subclass", {
    f <- CTDFile("some/path.csv")
    expect_s4_class(f, "CTDFile")
    expect_true(methods::is(f, "BiocFile"))
    expect_identical(BiocIO::resource(f), "some/path.csv")
})

test_that("CTDFile() rejects invalid resources", {
    expect_error(CTDFile(c("a", "b")), "single non-NA character")
    expect_error(CTDFile(NA_character_), "single non-NA character")
    expect_error(CTDFile(123), "single non-NA character")
})

test_that("import() on a CTDFile returns a DataFrame of human interactions", {
    sample_file <- system.file(
        "extdata", "CTD_chem_gene_ixns_sample.csv",
        package = "ctdR"
    )
    skip_if(sample_file == "", "sample CTD file not installed")

    res <- suppressMessages(BiocIO::import(CTDFile(sample_file)))

    expect_s4_class(res, "DataFrame")
    expect_true(all(
        c("ChemicalName", "ChemicalID", "GeneID", "InteractionActions",
            "OrganismID") %in% colnames(res)
    ))
    expect_gt(nrow(res), 0L)
    # .read_and_validate_ctd filters to Homo sapiens (OrganismID 9606) only
    expect_true(all(res$OrganismID == "9606"))
})

test_that("import() errors helpfully on a missing local file", {
    missing <- tempfile(fileext = ".csv")
    expect_error(
        suppressMessages(BiocIO::import(CTDFile(missing))),
        "File not found"
    )
})

test_that("import(CTDFile) and import_CTD share the same validated rows", {
    sample_file <- system.file(
        "extdata", "CTD_chem_gene_ixns_sample.csv",
        package = "ctdR"
    )
    skip_if(sample_file == "", "sample CTD file not installed")

    via_class <- suppressMessages(BiocIO::import(CTDFile(sample_file)))
    via_helper <- suppressMessages(ctdR:::.read_and_validate_ctd(sample_file))

    expect_identical(nrow(via_class), nrow(via_helper))
    expect_identical(colnames(via_class), colnames(via_helper))
})
