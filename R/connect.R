#!/usr/bin/env Rscript

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
#' @export
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
#' @export
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
#' @examples discon_db(con)
#' @export
discon_db <- function(connection){
    RPostgreSQL::dbDisconnect(connection)
}

#' @title clear_results
#'
#' @description This function clears the results from a previous executed
#' query.
#'
#' @param connection DBI connection. A connection to a database must be open and given.
#'
#' @examples clear_results(con)
#' @export
clear_results <- function(connection){
    DBI::dbClearResult(DBI::dbListResults(connection)[[1]])
}

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
#' @examples sample_table(con,0.01,1234,raw,cuis_historico_domicilios)
#' @export
sample_table <- function(connection, p = 0.01, seed = 1234, schema, the_table,lim=0){
    the_table <- deparse(substitute(the_table))
    schema <-  deparse(substitute(schema))
    if (lim==0){
    query  <-  "SELECT * FROM %s.%s tablesample bernoulli($1) repeatable($2)"
    the_query <- sprintf(query,schema,the_table)
    sample_query <- RPostgreSQL::postgresqlExecStatement(con,the_query,c(p,seed))
    the_sample <- DBI::dbFetch(sample_query)
    DBI::dbClearResult(sample_query)
    return(the_sample)
    }
    else{
    query  <-  "SELECT * FROM %s.%s tablesample bernoulli($1) repeatable($2) limit ($3)"
    the_query <- sprintf(query,schema,the_table)
    sample_query <- RPostgreSQL::postgresqlExecStatement(con,the_query,c(p,seed,lim))
    the_sample <- DBI::dbFetch(sample_query)
    DBI::dbClearResult(sample_query)
    return(the_sample)
    }
}

