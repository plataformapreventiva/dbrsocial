context("Connection to the database")
library(dbconnection)

test_that("sample_table gives a random sample from a given table",{
              con <- prev_connect()
              table <- sample_table(con,0.01,1234,raw,cuis_historico_domicilios)
              expect_equal(sum(dim(table)==c(2272,6)),2)
})
