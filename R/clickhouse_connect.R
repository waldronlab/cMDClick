#' Establish a connection to ClickHouse
#'
#' This function establishes a connection to a ClickHouse database using the
#' ODBC driver.
#'
#' @param dsn character(1) (optional) The Data Source Name (DSN) configured for
#'   the ClickHouse ODBC driver. Created via the `/home/user/.odbc.ini` file.
#'
#' @param host The hostname or IP address of the ClickHouse server (optional if
#'   DSN is used).
#'
#' @param port The port of the ClickHouse server (default: 8443).
#'
#' @param database The name of the ClickHouse database (default: "default").
#'
#' @param user The username for authentication (default: "default").
#'
#' @param password The password for authentication (default: "").
#'
#' @return A database connection object.
#'
#' @details
#' The `.odbc.ini` file is a configuration file that stores the connection
#' details for the ClickHouse ODBC driver. The file is located in the user's
#' home directory (`/home/user/.odbc.ini`). The file should have the following
#' format:
#'
#'     [ClickHouseDSN]
#'     Driver=ClickHouse
#'     Description=ClickHouse DSN
#'     Server=localhost
#'     Port=1234
#'     Database=default
#'     UID=default
#'     PWD=foo
#'
#' The input `dsn` should be the name of the DSN in the `.odbc.ini` file
#' which is `ClickHouseDSN` in the example above.
#'
#' Note that a configuration file may also be used instead of an `.odbc.ini`
#' file. The configuration file should be located in the user's home directory
#' (`/home/user/config.yml`). The file should have the following format:
#'
#'     default:
#'       host: "localhost"
#'       user: "default"
#'       password: "foo"
#'       db: "default"
#'
#'
#' @examples
#' clickhouse_connect(
#'     host = "localhost", port = 8123, database = "default", user = "default"
#' )
#'
#' @export
clickhouse_connect <- function(
    dsn = NULL,
    host = config::get()$host,
    port = 8443,
    database = config::get()$db,
    user = config::get()$user,
    password = config::get()$password
) {
    if (!is.null(dsn)) {
        conn <- DBI::dbConnect(
            odbc::odbc(), .connection_string = paste0("DSN=", dsn)
        )
    } else {
        conn <- DBI::dbConnect(
            odbc::odbc(),
            Driver = "ClickHouse",
            Server = host,
            Port = port,
            Database = database,
            UID = user,
            PWD = password,
        )
    }
    conn
}
