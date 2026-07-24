.setup_sample_cache_cache <- function() {
    sample_file <- system.file(
        "extdata", "CTD_chem_gene_ixns_sample.csv",
        package = "ctdR"
    )
    if (!nzchar(sample_file) || !file.exists(sample_file)) {
        skip("Sample CTD file not available")
    }
    suppressMessages(import_CTD(sample_file))
    invisible(NULL)
}

test_that("ctd_cache('chemicals') returns the chemical metadata table", {
    skip_on_cran()
    .setup_sample_cache_cache()

    chem <- ctd_cache("chemicals")

    expect_s3_class(chem, "data.frame")
    expect_true(all(c("ChemicalID", "ChemicalName") %in% colnames(chem)))
    expect_gt(nrow(chem), 0L)
    # one row per ChemicalID
    expect_identical(anyDuplicated(chem$ChemicalID), 0L)
})

test_that("ctd_cache('interactions') returns the long interaction table", {
    skip_on_cran()
    .setup_sample_cache_cache()

    ia <- ctd_cache("interactions")

    expect_s3_class(ia, "data.frame")
    expect_true(all(
        c("ChemicalID", "EntrezID", "InteractionActions") %in% colnames(ia)
    ))
    expect_gt(nrow(ia), 0L)
})

test_that("ctd_cache() rejects an unknown 'what'", {
    expect_error(ctd_cache("nonsense"), "should be one of")
})

test_that("ctd_cache() errors when the cache is absent", {
    empty <- tempfile("ctdR-empty-cache-")
    dir.create(empty)
    expect_error(
        ctd_cache("chemicals", cache_dir = empty),
        "not found"
    )
})
