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

#' @title query_dic
#'
#' @description Returns a data fram of the Athena queries that has been already runned
#' @examples queries <- query_dic()
#'
#' @export
query_dic <- function(){
    objects <- aws.s3::get_bucket_df(gsub("s3://","",Sys.getenv("S3_DIR")))
    if (!("dict/fun_dict.csv" %in% objects$Key)){
        the_dic <- tibble(the_query=character(), s3_name=character())
        write_s3(dataf=the_dic, name="dict/fun_dict.csv", s3bucket=Sys.getenv("S3_DIR"))
    }
	the_dic <- csv_s3(paste0(Sys.getenv("S3_DIR"),"/dict/fun_dict.csv"))
    return(the_dic)
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
pub_connect <- function(s3dir=Sys.getenv("S3_DIR"),schema=Sys.getenv("SCHEMA")){
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

#' @title load_query
#'
#' @description Gives a "ready to go" data frame for geometry plotting
#'
#' @param connection DBI connection. A connection to a database
#' @param schema variable. A valid schema from a database on the
#' @param the_table.  An existing table in the given schema.
#' @param colums string. The columns in the database we want to retrieve
#' information
#' @param options string. Part of the SQL query with containing WHERE, ORDER,
#' LIMIT and so statements
#'
#' @examples geom_muni <-
#' load_query(con,raw,sifode,columns="entidadfederativa",options="WHERE
#' mes='Abril'")
#' @export
load_query <- function(connection,schema,the_table,columns="*",options=""){
    the_query <- "SELECT %s FROM %s.%s"
    complete <- paste0(the_query," ",options)
    schema    <- deparse(substitute(schema))
    the_table <- deparse(substitute(the_table))
    initial <- RPostgreSQL::dbSendQuery(connection,
                             sprintf(complete,columns,schema,the_table))
    return(initial)
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

#' @title csv_s3
#'
#' @description Brings a CSV stored in S3 into a dataframe. By default the
#' "catalogo de beneficios"
#'
#' @param route Bucket object. String of the bucket object in S3.
#'
#' @examples catalogo <- csv_s3()
#' @export
csv_s3 <- function(object="s3://pub-raw/diccionarios/catalogo_beneficio.csv"){
    the_file <- aws.s3::s3read_using(read_csv, object = object)
    return(the_file)
}

#' @title load_geom
#'
#' @description Gives a "ready to go" data frame for geometry plotting
#'
#' @param connection DBI connection. A connection to a database
#' @param schema variable. A valid schema from a database on the
#' @param the_table.  An existing table in the given schema.
#' @param colums string. The columns in the database we want to retrieve
#' @param geom_col . The name of the column in the database that contains a geometry
#' @param col_shape. The name of the column that we want to use to join
#' information
#' @param options string. Part of the SQL query with containing WHERE, ORDER,
#' LIMIT and so statements
#'
#' @examples geom_muni <- load_geom(con1,raw,geom_municipios,geom_col=geom,col_shape=cve_muni,options=options)
#' @export
load_geom <- function(connection,schema,the_table,columns="cve_mun, cve_ent, cve_muni, ", geom_col, col_shape, options=""){
    geom_col <- deparse(substitute(geom_col))
    schema    <- deparse(substitute(schema))
    the_table <- deparse(substitute(the_table))
    col_shape <- deparse(substitute(col_shape))

    the_query <- "SELECT %s FROM %s.%s"
    geom_col_as <- sprintf("ST_AsText(%s) as geom",geom_col)
    columns <- paste0(columns,geom_col_as)
    complete <- paste0(the_query," ",options)

    initial <- RPostgreSQL::dbSendQuery(connection,
                             sprintf(complete,columns,schema,the_table)) %>%
    retrieve_result()

    mun_shp = WKT2SpatialPolygonsDataFrame(initial, geom = geom_col, id = col_shape)
    mun_df <- fortify(mun_shp, region = col_shape)
    names(mun_df)[names(mun_df)=="id"] <- col_shape

    return(mun_df)
}
