.DBDIR_MEMORY <- duckdb:::DBDIR_MEMORY
.STORE_CANCERDATASCI_URL <- "https://store.cancerdatasci.org/cmgd/cMDv4"

#' @title Setup a connection to a DuckDB database with Parquet data
#'
#' @description This function creates a connection to a DuckDB database and
#'   imports Parquet data from <https://store.cancerdatasci.org>.
#'
#' @details The `parquet_setup` function provides a connection object for use
#'   with either `parquet_import` or `parquet_tbl` functions. The
#'   `parquet_import` downloads the entirety of the data and stores it in the
#'   user's local cache (see `cMDClickCache()`). The `parquet_tbl` function uses
#'   the connection object to connect to the storage location and allow remote
#'   querying of the data. Note that `dplyr::collect()` must be used to obtain
#'   the full data from the server.
#'
#' @param db `character(1)` The name of the database file (default:
#'   "cMDClick.duckdb").
#'
#' @param dbdir `character(1)` The directory where the database file is stored
#'   (default: `cMDClickCache()`).
#'
#' @param verbose `logical(1)` Show informative messages in the console
#'   (default: `FALSE`).
#'
#' @importFrom BiocBaseUtils isScalarCharacter isScalarLogical
#'
#' @examples
#' if (interactive()) {
#'     con <- parquet_setup()
#'     parquet_import(con, dataType = "metaphlan_bugs", verbose = TRUE)
#'     ## check the local cache for downloaded tables
#'     DBI::dbListTables(con)
#'     ## query the local cache and filter
#'     dplyr::tbl(con, "metaphlan_bugs") |>
#'         dplyr::filter(tax_id_string == "2|1239||||") |>
#'         dplyr::collect()
#'     ## query the parquet file remotely
#'     parquet_tbl(con, "metaphlan_bugs") |>
#'         dplyr::filter(tax_id_string == "2|1239||||") |>
#'         dplyr::collect()
#'     DBI::dbDisconnect(con)
#' }
#' @export
parquet_setup <-
    function(
        db = "cMDClick.duckdb",
        dbdir = cMDClickCache(verbose = verbose),
        verbose = FALSE
    )
{
    stopifnot(isScalarCharacter(db), isScalarCharacter(dbdir))

    if (!identical(dbdir, .DBDIR_MEMORY))
        dbdir <- file.path(dbdir, db)
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbdir)

    DBI::dbExecute(con, "install httpfs; load httpfs;")

    con
}

#' @rdname parquet_setup
#'
#' @param con `duckdb_connection` A connection object to a DuckDB database.
#'
#' @param dataType `character()` The type of data to import. Can be one or more
#'   of "metaphlan_bugs", "marker_abundances", or "marker_presences". By
#'   default, all three are imported.
#'
#' @export
parquet_import <-
    function(
        con,
        dataType = c("metaphlan_bugs", "marker_abundances", "marker_presences"),
        verbose = FALSE
    )
{
    stopifnot(isScalarLogical(verbose))
    dataType <- match.arg(dataType, several.ok = TRUE)

    for (dt in dataType) {
        file <- file.path(.STORE_CANCERDATASCI_URL, paste0(dt, ".parquet"))
        pq_cmd <- glue::glue(
            "CREATE TABLE {dt} AS SELECT * FROM read_parquet('{file}');"
        )
        if (!dt %in% DBI::dbListTables(con))
            DBI::dbExecute(con, pq_cmd)
        else if (verbose)
            message("Table '", dt, "' already exists in database.")
    }
    con
}

#' @rdname parquet_setup
#'
#' @importFrom dplyr tbl
#' @importFrom dbplyr sql
#'
#' @export
parquet_tbl <-
    function(
        con,
        dataType = c("metaphlan_bugs", "marker_abundances", "marker_presences"),
        verbose = FALSE
    )
{
    stopifnot(isScalarLogical(verbose))
    dataType <- match.arg(dataType, several.ok = FALSE)

    file <- file.path(.STORE_CANCERDATASCI_URL, paste0(dataType, ".parquet"))
    pq_cmd <- glue::glue(
        "SELECT * FROM read_parquet('{file}')"
    )
    tbl(con, sql(pq_cmd))
}

#' @rdname parquet_setup
#'
#' @param cache_dir `character(1)` The directory where the cache is stored.
#'
#' @param ask `logical(1)` Ask the user to create the cache directory if it does
#'   not exist (default: `interactive()`).
#'
#' @importFrom BiocBaseUtils askUserYesNo
#'
#' @export
cMDClickCache <-
    function(
        cache_dir = getOption(
            "cMDClickCache",
            tools::R_user_dir("cMDClick", "cache")
        ),
        verbose = TRUE,
        ask = interactive()
    )
{
    stopifnot(isScalarCharacter(cache_dir))

    if (!dir.exists(cache_dir)) {
        if (ask) {
            qtxt <- sprintf(
                "Create cMDClick cache at\n  %s",
                cache_dir
            )
            answer <- askUserYesNo(qtxt)
            if (!answer)
                stop("'cMDClickCache' cache_dir not created. Use 'setCache'")
        }
        dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }
    options(cMDClickCache = cache_dir)

    if (verbose)
        message("cMDClick 'cache_dir' set to:\n    ", cache_dir)

    invisible(cache_dir)
}
