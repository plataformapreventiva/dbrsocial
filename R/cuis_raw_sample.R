#' Sample CUIS raw table
#'
#' Random Sample of CUIS table on the raw schema.
#' @param con a DBIConnection object to Postgres predictiva data base, as
#'   returned by \code{\link[DBI]{dbConnect}}.
#' @param p Sample size as proportion of the total number of households
#'   in CUIS, defaults to 0.01.
#' @param seed numeric value to be used to generate a seeding for the PRNG
#'   random generator in Postgres backend.
#' @importFrom magrittr %>%
#' @export
cuis_raw_sample <- function(con, p = 0.01, seed = 1234){
    domicilios_sample_query <- DBI::dbSendQuery(con, "select * from
        raw.cuis_historico_domicilios tablesample bernoulli($1) repeatable($2)")
    DBI::dbBind(domicilios_sample_query, list(p, seed))
    domicilios_sample <- DBI::dbFetch(domicilios_sample_query)
    DBI::dbClearResult(domicilios_sample_query)
    cuis_table <- dplyr::tbl(con, dbplyr::in_schema("raw", "cuis_39_9"))
    cuis_sample <- cuis_table %>%
        dplyr::select(-actualizacion_sedesol, -data_date) %>%
        dplyr::filter(llave_hogar_h %in% domicilios_sample$llave_hogar_h) %>%
        dplyr::collect()
    return(cuis_sample)
}
