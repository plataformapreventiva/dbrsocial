#' @title prev_connect
#'
#' @description This function takes the Postgres environment variables
#' to automatically connect to the preventivadb database.
#' The connection must be saved into a variable in order to
#' use future functions.
#'
#' @examples con <- prev_connect()
#'
#' @param No parameter is needed.
prev_connect <- function(){
  DBI::dbConnect(RPostgreSQL::PostgreSQL(),
  host     =  Sys.getenv("PGHOST"),
  user     =  Sys.getenv("POSTGRES_USER"),
  password =  Sys.getenv("POSTGRES_PASSWORD"),
  port     =  Sys.getenv("PGPORT"),
  dbname   =  Sys.getenv("PGDATABASE"))
}

#' @title load_table
#'
#' @description This function loads a connection to
#' a given table from a particular schema in a database
#' connection.
#'
#' @param connection DBI connection. A connection to a database
#' must be open and given.
#' @param schema variable. A valid schema from a database on the
#' connected database.
#' @param the_table. An existing table in the given schema.
#'
#' @examples the_dic<-load_table(con,raw,sifode_dic)
load_table <- function(connection,schema,the_table){
    the_query <- "SELECT * FROM %s.%s"
    schema    <- deparse(substitute(schema))
    the_table <- deparse(substitute(the_table))
    initial <- RPostgreSQL::dbSendQuery(connection,
                             sprintf(the_query,schema,the_table))
}

#' @title discon_db
#'
#' @description This function disconnects a PostgreSQL
#' connection.
#'
#' @param connection DBI connection. A connection to a database must be open and given.
#'
#' @examples con <- prev_connect()
discon_db <- function(connection){
    RPostgreSQL::dbDisconnect(connection)
}

#' @title sample_table
#'
#' @description Random Sample of any given table.
#' @param connection DBIConnection object to Postgres predictiva data base, as
#'   returned by \code{\link[DBI]{dbConnect}}.
#' @param p Sample size as proportion of the total number of households
#'   in CUIS, defaults to 0.01.
#' @param seed numeric value to be used to generate a seeding for the PRNG
#'   random generator in Postgres backend.
#' @importFrom magrittr %>%
#' @export
cuis_raw_sample <- function(connection, p = 0.01, seed = 1234){
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


#' @title sample_table
#'
#' @description Random Sample of any given table.
#' @param connection DBIConnection object to Postgres predictiva data base, as
#'   returned by \code{\link[DBI]{dbConnect}}.
#' @param p Sample size as proportion of the total number of households
#'   in CUIS, defaults to 0.01.
#' @param seed numeric value to be used to generate a seeding for the PRNG
#'   random generator in Postgres backend.
#' @importFrom magrittr %>%
#' @export
cuis_raw_sample <- function(con, p = 0.01, seed = 1234){
    domicilios_sample_query <- DBI::dbSendQuery(con, "select * from raw.cuis_historico_domicilios tablesample bernoulli($1) repeatable($2)")
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

