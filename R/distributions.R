#' @title box_payment
#'
#' @description Ease the query execution to get payments distribution
#'
#' @param connection DBI connection. A connection to a database
#' @param dict DataFrame. The queries dictionary for load_or_run
#' @param options string. Part of the SQL query with containing WHERE, ORDER,
#' LIMIT and so statements
#'
#' @examples options <- "WHERE anio=2017 AND cast(newid as integer) < 500000000
#' AND cveent='29' AND cddependencia='20' GROUP BY cdbeneficio, newid"
#' c(dinero_programa, all_values) <- box_payment(con, the_queries, options=options)
#' @export
box_payment <- function(connection,dict,options=""){
    the_query1 <- "WITH los_montos AS (SELECT numespago, cdbeneficio, newid, sum(nuimpmonetario) as monto
              FROM athena_pub.pub_public "
    the_query2 <- "),los_valores AS (SELECT cdbeneficio, avg(monto) as media, approx_percentile(monto,0.25) as q1,
                      approx_percentile(monto,0.5) as q2, approx_percentile(monto,0.75) as q3,
                      stddev(monto) as std, array_agg(monto) as valores
              FROM los_montos
              GROUP BY cdbeneficio),

              los_rangos AS (SELECT cdbeneficio, media, q1, q2, q3,  std, abs(q3)-abs(q1) as IQR, filter(valores, x -> x IS NOT NULL) as nonull
              FROM los_valores)

              SELECT cdbeneficio, media, q1, q2, q3, std,
              array_union(filter(nonull, x -> x > q3+1.5*IQR),filter(nonull, x -> x < q1-1.5*IQR)) as outliers,
              array_min(array_intersect(filter(nonull,x -> x > q3),filter(nonull,x -> x < q3+1.5*IQR))) as max,
              array_max(array_intersect(filter(nonull,x -> x < q1),filter(nonull,x -> x > q1-1.5*IQR))) as min
              FROM los_rangos"
    query <- paste0(the_query1,options,the_query2)
    c(the_df,dict) := load_or_run(connection,query,dict)
    the_df <- the_df %>%
    left_join(catalogo_beneficios)
    the_df$outliers <- gsub('\\[|\\]','',the_df$outliers) %>%
    strsplit(., split=", ")
    the_df$outliers <- lapply(the_df$outliers,as.integer)
    the_df$max[is.na(the_df$max)] <- the_df$q3[is.na(the_df$max)]
    the_df$min[is.na(the_df$min)] <- the_df$q1[is.na(the_df$min)]
    the_df$nbbeneficio[is.na(the_df$nbbeneficio)] <- as.character(the_df$cdbeneficio[is.na(the_df$nbbeneficio)])
    the_df <- the_df[!is.na(the_df$media),]

    all_values <-unnest(the_df,outliers)
    all_values$max[is.na(all_values$max)] <- all_values$q3[is.na(all_values$max)]
    all_values$min[is.na(all_values$min)] <- all_values$q3[is.na(all_values$min)]

    return(list(the_df,all_values))
}
