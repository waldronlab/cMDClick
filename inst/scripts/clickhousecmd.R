
# library(DBI)
# library(dplyr)
# library(dbplyr)
# library(purrr)

samp <- c("SAMEA103958109", "SAMEA103958110", "SAMEA103958111")
feat <- c("UniClust90_GMEPEMOD01001|1__4|SGB72336", "UniClust90_MABEGJKF00110|1__5|SGB72336", "UniClust90_HPNJJKHE00621|1__5|SGB72336")
ncbi_ids <- c("562", "573", "1301", "1392", "470", "213", "243230", "1352", "282", "83333")

con <- function() {
    DBI::dbConnect(
        drv = ClickHouseHTTP::ClickHouseHTTP(),
        port = 8443,
        https = TRUE,
        host = config::get()$host,
        user = config::get()$user,
        password = config::get()$password,
        dbname = config::get()$db
    )
}

con()

## List tables in the cMD database
tblNames <- cMDListTables(con())

## Get marker data (presence, abundance)
mAb <- cMDGetMarker(con(), features = feat)
mPr <- cMDGetMarker(con(), features = feat, type = "p")

## Get bug relative abundance data
relab <- cMDGetRelab(con(), features = ncbi_ids)

## TODO convert to matrix

# -------------------------------------------------------------------------

# x <- con() |>
#     dplyr::tbl("src_cmgd_v4__bugs_list") |>
#     head(100)
#     dplyr::as_tibble()



# gg <- con() |> 
#         dbGetQuery(
#     "
#         SELECT
#             run_name, toString(run_id) as run_id, utc_time, trace, metadata_,
#             toString(id) as id, event
#         FROM src_cmgd__telemetry
#         LIMIT 100
#     "
#     ) |> 
#     as_tibble()



# colnames <- dbListFields(con(), "src_cmgd_v4__marker_abundance")
# colnames
# 
# 
# 
# 
# ## Number of bugs
# bugs <- con() |> 
#     dbGetQuery(
#     "
#         SELECT COUNT(*) AS bugs, presence, sample_id
#         FROM src_cmgd_v4__marker_presence
#         WHERE presence = true
#         GROUP BY ALL
#         ORDER BY bugs DESC
#         LIMIT 100
#     "
#     ) |> 
#     as_tibble()
# bugs






## Bioconductor query
# bioc <- con() |> 
#     dbGetQuery(
#     "
#         SELECT COUNT(*) AS n
#         FROM src_pubmed
#         WHERE (abstract ILIKE '%bioconductor%') OR (title ILIKE '%bioconductor%')
#     "
#     ) |> 
#     as_tibble()
# bioc

## Split by year
# by_year <- con() |> 
#     dbGetQuery(
#     "
#         SELECT
#             splitByChar('-', coalesce(pubdate, ''))[1] AS year,
#             COUNT(*) AS n
#         FROM src_pubmed
#         WHERE (abstract ILIKE '%bioconductor%') OR (title ILIKE '%bioconductor%')
#         GROUP BY year
#         ORDER BY year DESC
#     "
# ) |> 
#     as_tibble()

## Relative abundance data
# con() |> 
#     dbListFields("src_cmgd_v4__marker_abundance")
# relab <- con() |> 
#     dbGetQuery(
#     "
#         SELECT sample_id, marker_id, abundance
#         FROM src_cmgd_v4__marker_abundance
#         ORDER BY abundance DESC
#         LIMIT 100
#     "
# ) |> 
#     as_tibble()
# relab

## Studies?
# con() |> 
#     dbListFields("src_pubmed")

# studies <- con() |> 
#     dbGetQuery(
#     "
#         SELECT COUNT(*) AS n, pmid
#         FROM src_pubmed
#         GROUP BY pmid
#         ORDER BY n DESC
#         LIMIT 100
#     "
#     ) |> 
#     as_tibble()
# studies

## Disconnect
# dbDisconnect(con())

## Check DBplyr
## Add messages to direct to the next function in a workflow.


# library(curatedMetagenomicData)
# cMD_data <- curatedMetagenomicData("AsnicarF_2017", dryrun = FALSE)
# names(cMD_data)
# 
# 
# cMD_data$`2021-10-14.AsnicarF_2017.marker_abundance` |> 
#     rowData()
# 
# cMD_data$`2021-10-14.AsnicarF_2017.marker_presence` |> 
#     rowData()
# 
# cMD_data$`2021-10-14.AsnicarF_2017.relative_abundance` |>
#     rowData()
# 
# 
