#' @title Retrieve Cached CTD Data
#'
#' @description
#' Returns a processed CTD table from the \pkg{ctdR} cache populated by
#' \code{\link{import_CTD}}, without reaching into the \code{.rda} cache files
#' by hand. Use this instead of \code{load(file.path(cache_dir, "...rda"))} when
#' you need the chemical metadata or the raw interaction table in your own code.
#'
#' For the chemical \emph{gene sets} (the input to enrichment engines) use
#' \code{\link{as_genesets_CTD}} instead.
#'
#' @param what Which cached table to return:
#'   \describe{
#'     \item{\code{"chemicals"}}{a data frame with one row per chemical
#'       (\code{ChemicalID}, \code{ChemicalName}).}
#'     \item{\code{"interactions"}}{the long-format interaction table
#'       (\code{ChemicalID}, \code{EntrezID}, \code{InteractionActions}).}
#'   }
#' @param cache_dir Directory holding the cached CTD files. Defaults to the
#'   \pkg{ctdR} user cache populated by \code{\link{import_CTD}}.
#'
#' @return A data frame with the requested cached table.
#'
#' @seealso \code{\link{import_CTD}} to populate the cache;
#'   \code{\link{as_genesets_CTD}} for the chemical gene sets.
#'
#' @examples
#' sample_file <- system.file(
#'     "extdata", "CTD_chem_gene_ixns_sample.csv",
#'     package = "ctdR"
#' )
#' import_CTD(sample_file)
#' chem <- ctd_cache("chemicals")
#' head(chem)
#'
#' @importFrom rappdirs user_cache_dir
#' @export
ctd_cache <- function(what = c("chemicals", "interactions"),
    cache_dir = rappdirs::user_cache_dir("ctdR")) {
    what <- match.arg(what)
    file <- switch(what,
        chemicals    = "chemicals.rda",
        interactions = "ctd_interactions.rda"
    )
    path <- file.path(cache_dir, file)
    if (!file.exists(path)) {
        stop("CTD cache file '", file, "' not found in '", cache_dir, "'.\n",
            "Run import_CTD() on your CTD_chem_gene_ixns file first.",
            call. = FALSE
        )
    }
    e <- new.env(parent = emptyenv())
    load(path, envir = e)
    switch(what,
        chemicals    = e$chemicals,
        interactions = e$ctd_interactions
    )
}
