#' @title Internal BiocFileCache-backed store for processed CTD data
#'
#' @description
#' Small internal wrapper around \pkg{BiocFileCache}. \code{import_CTD()} writes
#' the four processed CTD objects (\code{chemicals},
#' \code{ChemicalName_GeneEntrezIds}, \code{ChemicalName_GeneSymbols},
#' \code{ctd_interactions}) as cache resources keyed by name; readers retrieve
#' them by the same name. Replaces the previous hand-managed \pkg{rappdirs}
#' directory of \code{.rda} files.
#'
#' @name ctdR-cache
#' @keywords internal
NULL

#' Resolve the ctdR cache directory
#'
#' Honours \code{getOption("ctdR.cache")} (used by the test suite to redirect
#' the cache to a temporary location); otherwise the standard per-user package
#' cache from \code{tools::R_user_dir()}.
#'
#' @return A single directory path (character).
#' @keywords internal
.ctd_cache_dir <- function() {
    getOption("ctdR.cache", tools::R_user_dir("ctdR", which = "cache"))
}

#' Open (creating if needed) the ctdR BiocFileCache
#'
#' @param cache_dir Directory backing the cache.
#' @return A \code{BiocFileCache} object.
#' @importFrom BiocFileCache BiocFileCache
#' @keywords internal
.ctd_bfc <- function(cache_dir = .ctd_cache_dir()) {
    if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }
    BiocFileCache::BiocFileCache(cache = cache_dir, ask = FALSE)
}

#' Is a resource with this name present in the cache?
#'
#' @param bfc A \code{BiocFileCache} object.
#' @param rname Resource name.
#' @return A logical scalar.
#' @importFrom BiocFileCache bfcquery bfccount
#' @keywords internal
.ctd_cache_has <- function(bfc, rname) {
    BiocFileCache::bfccount(
        BiocFileCache::bfcquery(bfc, rname, field = "rname", exact = TRUE)
    ) > 0L
}

#' Save (or overwrite) an object into the cache under a resource name
#'
#' @param bfc A \code{BiocFileCache} object.
#' @param rname Resource name.
#' @param value The object to serialize.
#' @return Invisibly, the resource path written.
#' @importFrom BiocFileCache bfcnew bfcrpath
#' @keywords internal
.ctd_cache_save <- function(bfc, rname, value) {
    path <- if (.ctd_cache_has(bfc, rname)) {
        BiocFileCache::bfcrpath(bfc, rnames = rname)
    } else {
        BiocFileCache::bfcnew(bfc, rname = rname, ext = ".rds")
    }
    saveRDS(value, path)
    invisible(path)
}

#' Load a cached object by resource name
#'
#' @param bfc A \code{BiocFileCache} object.
#' @param rname Resource name.
#' @return The deserialized object; \code{stop()}s if the resource is absent.
#' @importFrom BiocFileCache bfcrpath
#' @keywords internal
.ctd_cache_load <- function(bfc, rname) {
    if (!.ctd_cache_has(bfc, rname)) {
        stop("CTD cache resource '", rname, "' not found.\n",
            "Run import_CTD() on your CTD_chem_gene_ixns file first.",
            call. = FALSE
        )
    }
    readRDS(BiocFileCache::bfcrpath(bfc, rnames = rname))
}
