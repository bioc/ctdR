test_that("enrichment_CTD errors when CTD data not imported", {
    # Temporarily override user_cache_dir to an empty temp dir
    tmp_cache <- file.path(tempdir(), "ctdR_empty_cache")
    dir.create(tmp_cache, showWarnings = FALSE)
    options(ctdR.cache = tmp_cache)
    on.exit({
        options(ctdR.cache = NULL)
        unlink(tmp_cache, recursive = TRUE)
    })

    expect_error(
        enrichment_CTD(data.frame(EntrezID = "7124", value = 0.01)),
        "CTD data not found"
    )
    expect_error(
        enrichment_CTD(data.frame(EntrezID = "7124", value = 0.01)),
        "import_CTD"
    )
    expect_error(
        enrichment_CTD(data.frame(EntrezID = "7124", value = 0.01)),
        "CTD_chem_gene_ixns.csv.gz"
    )
})

test_that("enrichment_CTD errors on invalid pAdjustMethod", {
    expect_error(
        enrichment_CTD(data.frame(EntrezID = "7124", value = 0.01),
            pAdjustMethod = "invalid"
        ),
        "pAdjustMethod"
    )
})

test_that("pAdjustMethod accepts every stats::p.adjust.methods value", {
    df <- data.frame(EntrezID = "7124", value = 0.01)
    empty <- file.path(tempdir(), "ctdR_padj_validate")
    dir.create(empty, showWarnings = FALSE)
    on.exit(unlink(empty, recursive = TRUE))

    # A valid p.adjust method passes the pAdjustMethod check and only then
    # fails on the (empty) cache -- proving the method itself was accepted.
    for (m in stats::p.adjust.methods) {
        expect_error(
            ctdR:::.validate_enrichment_args(df, "ORA", NULL, NULL, m, empty),
            "CTD data not found"
        )
    }
    expect_error(
        ctdR:::.validate_enrichment_args(df, "ORA", NULL, NULL, "nope", empty),
        "must be one of"
    )
})

test_that("enrichment_CTD error mentions download URL", {
    tmp_cache <- file.path(tempdir(), "ctdR_empty_cache2")
    dir.create(tmp_cache, showWarnings = FALSE)
    options(ctdR.cache = tmp_cache)
    on.exit({
        options(ctdR.cache = NULL)
        unlink(tmp_cache, recursive = TRUE)
    })

    expect_error(
        enrichment_CTD(data.frame(EntrezID = "7124", value = 0.01)),
        "ctdbase.org"
    )
})
