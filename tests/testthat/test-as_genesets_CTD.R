.setup_sample_cache <- function() {
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

test_that("as_genesets_CTD() returns a named list of Entrez gene sets", {
    skip_on_cran()
    .setup_sample_cache()

    gs <- as_genesets_CTD("entrez")

    expect_type(gs, "list")
    expect_true(length(gs) > 0L)
    expect_false(is.null(names(gs)))
    expect_true(all(nzchar(names(gs))))
    # elements are character vectors of numeric Entrez IDs
    expect_type(gs[[1]], "character")
    expect_true(all(grepl("^[0-9]+$", gs[[1]])))
})

test_that("as_genesets_CTD('symbol') returns HGNC symbols", {
    skip_on_cran()
    .setup_sample_cache()

    gs <- as_genesets_CTD("symbol")

    expect_type(gs, "list")
    expect_true(length(gs) > 0L)
    # symbols are not purely numeric
    expect_false(all(grepl("^[0-9]+$", unlist(gs))))
})

test_that("as_genesets_CTD() matches the internal loader (parity)", {
    skip_on_cran()
    .setup_sample_cache()

    cache_dir <- ctdR:::.ctd_cache_dir()
    expect_identical(
        as_genesets_CTD("entrez"),
        ctdR:::.load_geneset_list("entrez", cache_dir)
    )
})

test_that("as_genesets_CTD() errors when the cache is absent", {
    empty <- tempfile("ctdR-empty-cache-")
    dir.create(empty)
    expect_error(
        as_genesets_CTD("entrez", cache_dir = empty),
        "CTD cache not found"
    )
})

test_that("as_genesets_CTD() honours interaction_types filtering", {
    skip_on_cran()
    .setup_sample_cache()

    all_sets <- as_genesets_CTD("entrez")
    # A type present in the CTD vocabulary; filtered result must be a subset
    filtered <- suppressMessages(
        as_genesets_CTD("entrez", interaction_types = "increases^expression")
    )
    expect_type(filtered, "list")
    expect_true(length(filtered) <= length(all_sets))
})
