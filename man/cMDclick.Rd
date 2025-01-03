% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cMDClick.R
\name{cMDListTables}
\alias{cMDListTables}
\alias{cMDclick}
\alias{cMDColNames}
\alias{cMDHead}
\alias{cMDGetMarker}
\alias{cMDGetRelab}
\title{Helper functions to interact with ClickHouse using the ODBC driver.}
\usage{
cMDListTables(con)

cMDColNames(con, tblName)

cMDHead(con, tblName, n = 6)

cMDGetMarker(
  con,
  features = NULL,
  samples = NULL,
  type = c("abundance", "presence")
)

cMDGetRelab(
  con,
  features = NULL,
  samples = NULL,
  filter_by = c("ncbi_tax_id", "marker_concatenated")
)
}
\arguments{
\item{con}{A connection created with DBI.}

\item{features}{A \code{character} vector with marker ids.}

\item{samples}{A \code{character} vector with sample names.}

\item{type}{A \code{character(1)} vector. One of two options: "\code{abundance}" or
"\code{presence}".}
}
\value{
\itemize{
\item cMDListTables() returns a character vector of table names
\item cMDColNames() returns a character vector of column names
\item cMDHead() returns a tibble of the first few rows of a table
\item cMDGetMarker() returns a tibble of marker data
\item cMDGetRelab() returns a tibble of bug relative abundance data
}
}
\description{
These functions provide a simple interface to interact with a
ClickHouse database using the ODBC driver.
}
\examples{
conn <- clickhouse_connect(dsn = "ClickHouseDSN")
cMDListTables(conn)
cMDColNames(conn, "src_cmgd_v4__marker_abundance")
cMDHead(conn, "src_cmgd_v4__marker_abundance")
cMDGetMarker(conn, samples = "SAMEA103958109")

cMDGetMarker(conn, features = "UniClust90_AGMDBMFK00910|1__15|SGB72336")
cMDGetRelab(
    conn, features = "k__Bacteria", filter_by = "marker_concatenated"
)
}
