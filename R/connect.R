prev_connect <- function(){
    #' This function takes the Postgres environment variables
    #' to automatically connect to the preventivadb database.
    #' The connection must be saved into a variable in order to
    #' use future functions.
    #' Example:
    #' con <- prev_connect()
  DBI::dbConnect(RPostgreSQL::PostgreSQL(),
  host     =  Sys.getenv("PGHOST"),
  user     =  Sys.getenv("POSTGRES_USER"),
  password =  Sys.getenv("POSTGRES_PASSWORD"),
  port     =  Sys.getenv("PGPORT"),
  dbname   =  Sys.getenv("PGDATABASE"))
}
