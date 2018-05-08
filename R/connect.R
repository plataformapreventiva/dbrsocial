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

#' @title pub_connect
#'
#' @description This function connects to an Athena schema and sets the saving
#' directory in AWS' S3.
#' to automatically connect to the preventivadb database.
#' The connection must be saved into a variable in order to
#' use future functions.
#'
#' @examples con <- pub_connect()
#' @param s3dir The S3 route to save the query results
#' @param schema The existing schema in Athena
#' @export
pub_connect <- function(s3dir,schema){
    # schema <- deparse(substitute(schema))
    # s3dir  <- deparse(substitute(s3dir))
    DBI::dbConnect(AWR.Athena::Athena(), region='us-west-2', s3_staging_dir=s3dir,
          schema_name=schema)
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

#' @title large_table
#'
#' @description This function loads a connection to a large table without
#' loading it to memory.
#'
#' @param connection DBI connection. A connection to a database
#' must be open and given.
#' @param schema variable. A valid schema from a database on the
#' connected database.
#' @param the_table. An existing table in the given schema.
#'
#' @examples cuis_table <- large_table(con,raw,cuis_39_9)
#' @export
large_table <- function(connection,schema,the_table){
    schema    <- deparse(substitute(schema))
    the_table <- deparse(substitute(the_table))
    retrieved <- dplyr::tbl(connection,dbplyr::in_schema(schema,the_table))
    return(retrieved)
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
