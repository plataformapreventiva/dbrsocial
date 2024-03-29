% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distributions.R
\name{box_payment}
\alias{box_payment}
\title{box_payment}
\usage{
box_payment(connection, dict,
  columns = "numespago, cdbeneficio, newid, sum(nuimpmonetario) as monto FROM athena_pub.pub_public",
  options = "")
}
\arguments{
\item{connection}{DBI connection. A connection to a database}

\item{dict}{DataFrame. The queries dictionary for load_or_run}

\item{options}{string. Part of the SQL query with containing WHERE, ORDER,
LIMIT and so statements}
}
\description{
Ease the query execution to get payments distribution
}
\examples{
options <- "WHERE anio=2017 AND cast(newid as integer) < 500000000 AND cveent='29' AND cddependencia='20' GROUP BY cdbeneficio, newid"
c(dinero_programa, all_values) <- box_payment(con, the_queries, options=options)
}
