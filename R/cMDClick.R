#' @rdname cMDclick
#'
#' @title Helper functions to interact with ClickHouse using the ODBC driver.
#'
#' @description These functions provide a simple interface to interact with a
#' ClickHouse database using the ODBC driver.
#'
#' @importFrom dplyr filter as_tibble tbl
#' @importFrom glue glue
#'
#' @return
#'   * cMDListTables() returns a character vector of table names
#'   * cMDColNames() returns a character vector of column names
#'   * cMDHead() returns a tibble of the first few rows of a table
#'   * cMDGetMarker() returns a tibble of marker data
#'   * cMDGetRelab() returns a tibble of bug relative abundance data
#'
#' @examples
#' conn <- clickhouse_connect(dsn = "ClickHouseDSN")
#' cMDListTables(conn)
#' @export
cMDListTables <- function(con) {
    con |>
        DBI::dbListTables() |>
        grep("src_cmgd_v4", x=_, value = TRUE)
}

#' @name cMDclick
#' @examples
#' cMDColNames(conn, "src_cmgd_v4__marker_abundance")
#' @export
cMDColNames <- function(con, tblName) {
    con |>
        DBI::dbListFields(tblName)
}

#' @name cMDclick
#' @examples
#' cMDHead(conn, "src_cmgd_v4__marker_abundance")
#' @export
cMDHead <- function(con, tblName, n = 6) {
    con |>
        dplyr::tbl(tblName) |>
        utils::head(n)
}

#' @name cMDclick
#' @examples
#' cMDGetMarker(conn, samples = "SAMEA103958109")
#'
#' cMDGetMarker(conn, features = "UniClust90_AGMDBMFK00910|1__15|SGB72336")
#' @export
cMDGetMarker <- function(
    con, features = NULL, samples = NULL, type = c("abundance", "presence")
) {
    type <- match.arg(type)
    tab <- glue::glue("src_cmgd_v4__marker_{type}")

    dplyr::tbl(con, tab) |>
        dplyr::filter(
            if (!is.null(features)) {
                "marker_id" %in% features
            } else { TRUE },
            if (!is.null(samples)) {
                "sample_id" %in% samples
            } else { TRUE }
        ) |>
        tibble::as_tibble()
}

#' @name cMDclick
#' @importFrom rlang `!!`
#' @examples
#' cMDGetRelab(
#'     conn, features = "k__Bacteria", filter_by = "marker_concatenated"
#' )
#' @export
cMDGetRelab <- function(
    con, features = NULL, samples = NULL,
    filter_by = c("ncbi_tax_id", "marker_concatenated")
) {
    filter_by <- match.arg(filter_by)
    filter_by <- rlang::sym(filter_by)
    dplyr::tbl(con, "src_cmgd_v4__bugs_list") |>
        dplyr::filter(
            if (!is.null(features)) {
                !!filter_by %in% features
            } else { TRUE },
            if (!is.null(samples)) {
                "sample_id" %in% samples
            } else { TRUE }
        ) |>
        tibble::as_tibble()
}
