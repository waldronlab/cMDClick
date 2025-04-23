#' @name config
#'
#' @title Check and validate the user configuration file
#'
#' @description These are utilities to check whether the user has an ODBC
#'   configuration file or a general configuration file. These files are used to
#'   connect via ODBC driver either with an `.odbc.ini` file or a general
#'   configuration file `config.yml` as used by `config` R package. For details,
#'   see [clickhouse_connect()]. `valid_config` checks if the general
#'   configuration file has the required fields for connecting to the Clickhouse
#'   server.
#'
#' @return * `has_config`: a logical named vector indicating if the user has an
#'   ODBC configuration (`.odbc.ini`) or a general configuration (`config.yml`)
#'   file.
#' * `valid_config`: a logical value indicating if the user has a valid
#'   `config.yml` file.
#'
#' @examples
#' has_config()
#' valid_config()
#' @export
has_config <- function() {
    home <- Sys.getenv("HOME")
    c(
        .odbc.ini = file.exists(file.path(home, ".odbc.ini")),
        config.yml = file.exists(file.path(home, "config.yml"))
    )
}

#' @rdname config
#'
#' @export
valid_config <- function() {
    config <- file.path(Sys.getenv("HOME"), "config.yml")
    if (!file.exists(config))
        stop("No configuration file found in '~/config.yml'")
    BiocBaseUtils::checkInstalled("yaml")

    clist <- yaml::read_yaml(config)

    if (!identical("default", names(clist)))
        stop("No 'default' configuration found in '~/config.yml'")

    cfields <- c("host", "user", "password", "port", "db")
    found <- cfields %in% names(clist[["default"]])
    notfound <- paste0(cfields[!found], collapse = ", ")

    if (!all(found))
        stop("Missing fields in config.yml: ", notfound)

    TRUE
}
