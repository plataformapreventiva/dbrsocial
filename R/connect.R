prev_connect <- function(){
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
  DBI::dbConnect(RPostgreSQL::PostgreSQL(),
  host     =  Sys.getenv("PGHOST"),
  user     =  Sys.getenv("POSTGRES_USER"),
  password =  Sys.getenv("POSTGRES_PASSWORD"),
  port     =  Sys.getenv("PGPORT"),
  dbname   =  Sys.getenv("PGDATABASE"))
}

load_table <- function(connection,schema,the_table){
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
    #' @param the_dic<-load_table(con,raw,sifode_dic)
    the_query <- "SELECT * FROM %s.%s"
    schema    <- deparse(substitute(schema))
    the_table <- deparse(substitute(the_table))
    initial <- RPostgreSQL::dbSendQuery(connection,
                             sprintf(the_query,schema,the_table))
}
