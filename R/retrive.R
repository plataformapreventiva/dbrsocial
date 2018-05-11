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
    if (connection@class[1]=="PostgreSQLConnection"){
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
    else if (connection@class[1]=="AthenaConnection"){
        the_table <- deparse(substitute(the_table))
        schema <-  deparse(substitute(schema))
        if (lim==0){
        query  <-  "SELECT * FROM %s.%s tablesample bernoulli(%s)"
        the_query <- sprintf(query,schema,the_table,p)
        the_sample <- DBI::dbGetQuery(connection,the_query)
        return(the_sample)
        }
        else{
        query  <-  "SELECT * FROM %s.%s tablesample bernoulli(%s) limit (%s)"
        the_query <- sprintf(query,schema,the_table,p,lim)
        the_sample <- DBI::dbGetQuery(connection,the_query)
        return(the_sample)
        }
    }
}


#' @title join_tables
#'
#' @description Returns a match between two tbl by defined key
#' @param left_table a tbl-like object
#' @param right_table a tbl-like object
#' @param left_key the column name from left_table to compare
#' @param right_key the column name from right_table to compare
#'
#' @examples cross_tables(domicilios_sample_query,cuis_sample,llave_hogar_h,llave_hogar_h)
#' @export
join_tables <- function(left_table, right_table, left_key, right_key){
    left_key <- deparse(substitute(left_key))
    right_key <- (substitute(right_key))
    where <- left_table[,left_key]
    in_tables <- right_table %>%
        dplyr::tbl_df() %>%
        dplyr::select(right_key %in% left_table[[left_key]] ) %>%
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
retrive_result <- function(query,n){
    the_table <- DBI::dbFetch(query,n)
    return(the_table)
}


#' @title write_s3
#'
#' @description Writes a dataframe as a CSV in a S3 bucket
#' @param dataf The data.frame objet to write as a CSV
#' @param name The name of the file in the S3 bucket
#' @param s3bucket The name of the S3 bucket
#'
#' @examples write_s3(the_dic, "dict/fun_dict.csv", Sys.getenv("S3_DIR"))
#' @export
write_s3 <- function(dataf, name, s3bucket=Sys.getenv("S3_DIR")){
    name <- deparse(substitute(name))
    s3bucket <- deparse(substitute(s3bucket))
    utils::write.csv(dataf,file="tmp")
    aws.s3::put_object("tmp", object=name, bucket=s3bucket,region="us-west-2")
}


#' @title load_or_run
#'
#' @description Checks if a query has already been runned in Athena and if it has then
#' loads the previous results and if it hasn't then it runs it and saves the
#' query for further usage
#' @param connection An Athena connection
#' @param query charachter. A query string
#' @param the_dic The previously loaded query dictionary
#'
#' @export
load_or_run <- function(connection,query,the_dic){
    if (connection@class[1]=="AthenaConnection"){
        query <- gsub("^ *|(?<= ) | *$", "", query, perl = TRUE) %>% str_replace_all("[\r\n]" , "")
        if (query %in% the_dic$the_query){
            the_table <- csv_s3(paste0(Sys.getenv("S3_DIR"),"/",the_dic$s3_name))
            return(the_table)
        }
        else {
            the_table <- DBI::dbGetQuery(connection, query)
            objects <- aws.s3::get_bucket_df(gsub("s3://","",Sys.getenv("S3_DIR")))
            where_stored <- objects[order(objects$LastModified, decreasing = TRUE),]$Key[1]
            where_stored <- gsub("^ *|(?<= ) | *$", "", where_stored, perl = TRUE) %>% str_replace_all("[\r\n]" , "")
            the_dic <- rbind(the_dic,c(deparse(substitute(dbGetQuery)),query,where_stored))
            write_s3(the_dic, "dict/fun_dict.csv", Sys.getenv("S3_DIR"))
            return(the_table)
        }
    }
}
