test_that(".is_remote_url detects URLs vs local paths", {
    expect_true(ctdR:::.is_remote_url("https://ctdbase.org/x.csv.gz"))
    expect_true(ctdR:::.is_remote_url("http://example.org/y"))
    expect_true(ctdR:::.is_remote_url("ftp://example.org/y"))
    expect_false(ctdR:::.is_remote_url("/local/path.csv"))
    expect_false(ctdR:::.is_remote_url("file:///tmp/x.csv"))
    expect_false(ctdR:::.is_remote_url("relative/path.csv.gz"))
})

test_that(".ctd_license_reminder fires once per session", {
    env <- ctdR:::.ctdR_env
    env$license_shown <- FALSE
    expect_message(first <- ctdR:::.ctd_license_reminder(), "licensing terms")
    expect_true(first)
    # second call is a no-op and returns FALSE
    second <- withCallingHandlers(
        ctdR:::.ctd_license_reminder(),
        message = function(m) stop("reminder fired twice")
    )
    expect_false(second)
    # reset so the flag does not leak into other tests
    env$license_shown <- FALSE
})

test_that(".resolve_ctd_source passes an existing local path through", {
    sf <- system.file(
        "extdata", "CTD_chem_gene_ixns_sample.csv", package = "ctdR"
    )
    skip_if(sf == "", "sample CTD file not installed")
    expect_identical(ctdR:::.resolve_ctd_source(sf), sf)
})

test_that(".resolve_ctd_source strips a file:// URI to a local path", {
    sf <- system.file(
        "extdata", "CTD_chem_gene_ixns_sample.csv", package = "ctdR"
    )
    skip_if(sf == "", "sample CTD file not installed")
    expect_identical(
        ctdR:::.resolve_ctd_source(paste0("file://", sf)), sf
    )
})

test_that(".resolve_ctd_source errors on a missing local file", {
    expect_error(
        ctdR:::.resolve_ctd_source(tempfile(fileext = ".csv")),
        "File not found"
    )
})

test_that(".resolve_ctd_source rejects invalid input", {
    expect_error(ctdR:::.resolve_ctd_source(c("a", "b")), "single non-NA")
    expect_error(ctdR:::.resolve_ctd_source(NA_character_), "single non-NA")
})

test_that("import_CTD works from a file:// URL", {
    skip_on_cran()
    sf <- system.file(
        "extdata", "CTD_chem_gene_ixns_sample.csv", package = "ctdR"
    )
    skip_if(sf == "", "sample CTD file not installed")

    tmp_cache <- file.path(tempdir(), "ctdR_url_test")
    options(ctdR.cache = tmp_cache)
    on.exit({
        options(ctdR.cache = NULL)
        unlink(tmp_cache, recursive = TRUE)
    })

    expect_message(
        suppressWarnings(import_CTD(paste0("file://", sf))),
        "cached successfully"
    )
    expect_true(
        ctdR:::.ctd_cache_has(ctdR:::.ctd_bfc(tmp_cache), "chemicals")
    )
})
