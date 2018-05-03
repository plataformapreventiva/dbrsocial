#!/usr/bin/env Rscript

#' @title sample_table
#'
#' @description Random Sample of any given table and schema.
#' @param connection DBIConnection object to Postgres predictiva data base, as
#'   returned by \code{\link[DBI]{dbConnect}}.
#' @param p Sample size as proportion of the total number of rows in a
#' table, defaults to 0.01.
#' @param seed numeric value to be used to generate a seeding for the PRNG
#'   random generator in Postgres backend.
#' @importFrom magrittr %>%
#'
#' @param lim int. A limit for SQL lines to return.
#'
#' @examples sample_table(con,0.01,1234,raw,cuis_historico_domicilios)
#' @export
sample_table <- function(connection, p = 0.01, seed = 1234, schema, the_table, lim=0){
    the_table <- deparse(substitute(the_table))
    schema <-  deparse(substitute(schema))
    if (lim==0){
    query  <-  "SELECT * FROM %s.%s tablesample bernoulli($1) repeatable($2)"
    the_query <- sprintf(query,schema,the_table)
    sample_query <- RPostgreSQL::postgresqlExecStatement(connection,the_query,c(p,seed))
    the_sample <- DBI::dbFetch(sample_query)
    DBI::dbClearResult(sample_query)
    return(the_sample)
    }
    else{
    query  <-  "SELECT * FROM %s.%s tablesample bernoulli($1) repeatable($2) limit ($3)"
    the_query <- sprintf(query,schema,the_table)
    sample_query <- RPostgreSQL::postgresqlExecStatement(connection,the_query,c(p,seed,lim))
    the_sample <- DBI::dbFetch(sample_query)
    DBI::dbClearResult(sample_query)
    return(the_sample)
    }
}


# cuis_raw_sample <- function(con, p = 0.01, seed = 1234){
#     domicilios_sample_query <- DBI::dbSendQuery(con, "select * from
#         raw.cuis_historico_domicilios tablesample bernoulli($1) repeatable($2)")
#     DBI::dbBind(domicilios_sample_query, list(p, seed))
#     domicilios_sample <- DBI::dbFetch(domicilios_sample_query)
#     DBI::dbClearResult(domicilios_sample_query)
#     cuis_table <- dplyr::tbl(con, dbplyr::in_schema("raw", "cuis_39_9"))
#     cuis_sample <- cuis_table %>%
#         dplyr::select(-actualizacion_sedesol, -data_date) %>%
#         dplyr::filter(llave_hogar_h %in% domicilios_sample$llave_hogar_h) %>%
#         dplyr::collect()
#     return(cuis_sample)
# }

# cuist_table %>% dplyr::select(llave_hogar_h) %>% dplyr::tbl_df() %in% domicilios_sample$llave_hogar_h

cross_tables <- function(table_1, table_2, key_1, key_2){
    key_1 <- deparse(substitute(key_1))
    key_2 <- (substitute(key_2))
    where <- table_1[,key_1]
    in_tables <- table_2 %>%
        dplyr::tbl_df() %>%
        dplyr::select(key_2 %in% table_1[[key_1]] ) %>%
        dplyr::collect()
    return(in_tables)
}

#' @title retrive_result
#'
#' @description Return the fetch results of a query
#' @param query An exec unfetched query
#'
#' @examples sample_table(load_table(prev_connect(),raw,sifode))
#' @export
retrive_result <- function(query){
    the_table <- DBI::dbFetch(query)
    return(the_table)
}
