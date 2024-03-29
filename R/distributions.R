#' @title box_payment
#'
#' @description Ease the query execution to get payments distribution
#' @importFrom magrittr %>%
#' @param connection DBI connection. A connection to a database
#' @param dict DataFrame. The queries dictionary for load_or_run
#' @param options string. Part of the SQL query with containing WHERE, ORDER,
#' LIMIT and so statements
#'
#' @examples options <- "WHERE anio=2017 AND cast(newid as integer) < 500000000 AND cveent='29' AND cddependencia='20' GROUP BY cdbeneficio, newid"
#' @examples c(dinero_programa, all_values) <- box_payment(con, the_queries, options=options)
#' @export
box_payment <- function(connection,dict,columns="numespago, cdbeneficio, newid, sum(nuimpmonetario) as monto",options="",to_join="estados"){
    the_query1 <- "WITH los_montos AS (SELECT "
    the_from <-  "FROM athena_pub.pub_public"
    the_query2 <- "),los_valores AS (SELECT %s, avg(monto) as media, approx_percentile(monto,0.25) as q1, approx_percentile(monto,0.5) as q2, approx_percentile(monto,0.75) as q3,stddev(monto) as std, array_agg(monto) as valores FROM los_montos GROUP BY %s),

              los_rangos AS (SELECT %s, media, q1, q2, q3,  std, abs(q3)-abs(q1) as IQR, filter(valores, x -> x IS NOT NULL) as nonull FROM los_valores)

              SELECT %s, media, q1, q2, q3, std,
              array_union(filter(nonull, x -> x > q3+1.5*IQR),filter(nonull, x -> x < q1-1.5*IQR)) as outliers,
              array_min(array_intersect(filter(nonull,x -> x > q3),filter(nonull,x -> x < q3+1.5*IQR))) as max,
              array_max(array_intersect(filter(nonull,x -> x < q1),filter(nonull,x -> x > q1-1.5*IQR))) as min
              FROM los_rangos"

	if (to_join=="estados"){
        the_query2 <- sprintf(the_query2,"cveent","cveent","cveent","cveent")
		joinner <- csv_s3("s3://pub-raw/diccionarios/estados.csv")
        colnames(joinner) <- c("cveent","name","pagos","distintos")
    } else if (to_join=="beneficios"){
        the_query2 <- sprintf(the_query2,"cdbeneficio","cdbeneficio","cdbeneficio","cdbeneficio")
        joinner <- csv_s3()
        colnames(joinner) <- c("cdbeneficio","nbbeneficio")
    } else if (to_join=="programa"){
        the_query2 <- sprintf(the_query2,"cdprograma","cdprograma","cdprograma","cdprograma")
    }

    query <- paste0(the_query1,columns," ",the_from," ",options,the_query2)
    c(the_df,dict) := load_or_run(connection,query,dict)

    if (to_join=="estados"){
        the_df$cveent <- as.integer(the_df$cveent)
    } else if (to_join=="beneficios"){
    the_df$nbbeneficio[is.na(the_df$nbbeneficio)] <- as.character(the_df$cdbeneficio[is.na(the_df$nbbeneficio)])
}

    if (to_join != "programa"){
        the_df <- the_df %>%
        dplyr::left_join(joinner)
    }
    the_df$outliers <- gsub('\\[|\\]','',the_df$outliers) %>%
    strsplit(., split=", ")
    the_df$outliers <- lapply(the_df$outliers,as.integer)
    the_df$max[is.na(the_df$max)] <- the_df$q3[is.na(the_df$max)]
    the_df$min[is.na(the_df$min)] <- the_df$q1[is.na(the_df$min)]

    the_df <- the_df[!is.na(the_df$media),]
    all_values <- tidyr::unnest(the_df,outliers)
    all_values$max[is.na(all_values$max)] <- all_values$q3[is.na(all_values$max)]
    all_values$min[is.na(all_values$min)] <- all_values$q3[is.na(all_values$min)]

    return(list(the_df,all_values))
}
