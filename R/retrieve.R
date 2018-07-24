#!/usr/bin/env Rscript

#' @title multireturn
#' @description Allows to return and save multiple variables within a function
#' @examples c(programas_persona,los_queries) := load_or_run(con,query,los_queries)
#' @export
':=' <- function(lhs, rhs) {
  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs <- lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL))
  }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  return(invisible(NULL))
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
#' @param colums string. The columns in the database we want to retrieve
#' information defaults to all
#' @param options string. Part of the SQL query with containing WHERE, ORDER,
#' LIMIT and so statements
#' @importFrom magrittr %>%
#'
#' @param lim int. A limit for SQL lines to return.
#'
#' @examples sample_table(con,0.01,1234,raw,cuis_historico_domicilios)
#' @export
sample_table <- function(connection, p = 0.01, seed = 1234, schema, the_table, columns="*",options="", lim=0){
    if (connection@class[1]=="PostgreSQLConnection"){
        the_table <- deparse(substitute(the_table))
        schema <-  deparse(substitute(schema))
        if (lim==0){
        query  <-  "SELECT %s FROM %s.%s tablesample bernoulli($1) repeatable($2)"
        the_query <- sprintf(query,columns,schema,the_table)
        complete <- paste0(the_query," ",options)
        sample_query <- RPostgreSQL::postgresqlExecStatement(connection,columns,c(p,seed))
        the_sample <- DBI::dbFetch(sample_query)
        DBI::dbClearResult(sample_query)
        return(the_sample)
        }
        else{
        query  <-  "SELECT %s FROM %s.%s tablesample bernoulli($1) repeatable($2) limit ($3)"
        the_query <- sprintf(query,columns,schema,the_table)
        complete <- paste0(the_query," ",options)
        sample_query <- RPostgreSQL::postgresqlExecStatement(connection,complete,c(p,seed,lim))
        the_sample <- DBI::dbFetch(sample_query)
        DBI::dbClearResult(sample_query)
        return(the_sample)
        }
    }
    else if (connection@class[1]=="AthenaConnection"){
        the_table <- deparse(substitute(the_table))
        schema <-  deparse(substitute(schema))
        if (lim==0){
        query  <-  "SELECT %s FROM %s.%s tablesample bernoulli(%s)"
        the_query <- sprintf(query,columns,schema,the_table,p)
        complete <- paste0(the_query," ",options)
        the_sample <- DBI::dbGetQuery(connection,complete)
        return(the_sample)
        }
        else{
        query  <-  "SELECT %s FROM %s.%s tablesample bernoulli(%s) limit (%s)"
        the_query <- sprintf(query,columns,schema,the_table,p,lim)
        complete <- paste0(the_query," ",options)
        the_sample <- DBI::dbGetQuery(connection,complete)
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
#' @examples join_tables(cuis_sample,llave_hogar_h,domicilios_sample_query,llave_hogar_h)
#' @export
join_tables <- function(left_table, left_key, right_table, right_key){
    left_key <- substitute(left_key)
    right_key <- deparse(substitute(right_key))
    in_tables <- left_table %>%
        dplyr::filter(left_key %in% right_table[[right_key]])
    return(in_tables)
}

#' @title retrieve_result
#'
#' @description Return the fetch results of a query
#' @param query An exec unfetched query
#'
#' @examples sample_table(load_table(prev_connect(),raw,sifode))
#' @export
retrieve_result <- function(query,n=-1,number=Inf){
    if (class(query)[1] == "tbl_dbi"){
        the_table <- dplyr::collect(query,n=number)
        return(the_table)
    }
    else{
    the_table <- DBI::dbFetch(query,n)
    return(the_table)
    }
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
    # name <- deparse(substitute(name))
    # s3bucket <- deparse(substitute(s3bucket))
    utils::write.csv(dataf,file="tmp",row.names=FALSE)
    instr <- "aws s3 cp tmp %s/%s"
    instr <- sprintf(instr,s3bucket,name)
    system(instr)
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
            rown <- which(the_dic$the_query == query)
            the_table <- csv_s3(object=paste0(Sys.getenv("S3_DIR"),"/",the_dic$s3_name[rown]))
            return(list(the_table,the_dic))
        }
        else {
            the_table <- DBI::dbGetQuery(connection, query)
            objects <- aws.s3::get_bucket_df(gsub("s3://","",Sys.getenv("S3_DIR")))
            where_stored <- objects[order(objects$LastModified, decreasing = TRUE),]$Key[1]
            where_stored <- gsub("^ *|(?<= ) | *$", "", where_stored, perl = TRUE) %>%
                str_replace_all("[\r\n]" , "") %>%
                str_replace_all(".metadata" , "")
            the_dic <- bind_rows(the_dic,tibble(the_query=query,s3_name=where_stored))
            write_s3(dataf=the_dic, name="dict/fun_dict.csv", s3bucket=Sys.getenv("S3_DIR"))
            rown <- which(the_dic$the_query == query)
            the_table <- csv_s3(object=paste0(Sys.getenv("S3_DIR"),"/",the_dic$s3_name[rown]))
            return(list(the_table,the_dic))
        }
    }
}
