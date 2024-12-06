#' @examples
#' has_config()
#'
#' @export
has_config <- function() {
    home <- Sys.getenv("HOME")
    c(
        ODBC = file.exists(file.path(home, ".odbc.ini")),
        config = file.exists(file.path(home, ".config.yml"))
    )
}
