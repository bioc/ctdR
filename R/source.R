#' @title CTD source resolution (local path or URL)
#'
#' @description
#' Internal helpers that let \code{\link{import_CTD}} and the \code{CTDFile}
#' \code{import()} method accept either a local file path or a URL. Remote URLs
#' are downloaded and cached with \pkg{BiocFileCache}; a one-time data-licensing
#' reminder is emitted. No default CTD URL is assumed --- the caller must supply
#' the source, keeping the user the party who downloads under (and agrees to)
#' the CTD terms.
#'
#' @name ctdR-source
#' @keywords internal
NULL

# Package-local mutable state (e.g. one-time messages).
.ctdR_env <- new.env(parent = emptyenv())

#' Is a source string a remote (downloadable) URL?
#'
#' @param src Character scalar.
#' @return \code{TRUE} for http(s)/ftp URLs, otherwise \code{FALSE}.
#' @keywords internal
.is_remote_url <- function(src) {
    grepl("^(https?|ftp)://", src)
}

#' Emit the CTD data-licensing reminder once per session
#'
#' @return Invisibly \code{TRUE} the first time it prints, \code{FALSE} after.
#' @keywords internal
.ctd_license_reminder <- function() {
    if (isTRUE(.ctdR_env$license_shown)) {
        return(invisible(FALSE))
    }
    .ctdR_env$license_shown <- TRUE
    message(
        "Fetching CTD data from a remote URL. CTD data are subject to ",
        "licensing terms:\n",
        "  free for research; redistribution and commercial use require ",
        "written permission.\n",
        "  See https://ctdbase.org/about/legal.jsp\n",
        "ctdR does not redistribute CTD data; you are downloading it ",
        "directly from the source you provided."
    )
    invisible(TRUE)
}

#' Resolve a CTD source to a readable local path
#'
#' Accepts a local file path, a \code{file://} URI, or an
#' \code{http(s)}/\code{ftp} URL. Remote URLs are downloaded and cached via
#' \pkg{BiocFileCache} (emitting \code{\link{.ctd_license_reminder}} once). No
#' default URL is assumed.
#'
#' @param src Character scalar: local path or URL.
#' @return A local, readable file path.
#' @importFrom BiocFileCache bfcadd bfcrpath
#' @keywords internal
.resolve_ctd_source <- function(src) {
    if (!is.character(src) || length(src) != 1L || is.na(src)) {
        stop("CTD source must be a single non-NA character path or URL.",
            call. = FALSE
        )
    }
    if (grepl("^file://", src)) {
        src <- sub("^file://", "", src)
    }
    if (.is_remote_url(src)) {
        .ctd_license_reminder()
        bfc <- .ctd_bfc()
        if (.ctd_cache_has(bfc, src)) {
            return(unname(BiocFileCache::bfcrpath(bfc, rnames = src)))
        }
        return(unname(BiocFileCache::bfcadd(
            bfc, rname = src, fpath = src, rtype = "web", download = TRUE
        )))
    }
    if (!file.exists(src)) {
        stop("File not found: ", src, "\n",
            "Provide a local path to CTD_chem_gene_ixns.csv[.gz], or a URL to\n",
            "  https://ctdbase.org/reports/CTD_chem_gene_ixns.csv.gz",
            call. = FALSE
        )
    }
    src
}
