context("Connection to the database")
library(dbconnection)

test_that("sample_table gives a random sample from a given table",{
              expect_equal(sum(dim(sample_table(prev_connect(),0.01,1234,raw,cuis_historico_domicilios))==c(2272,6)),2)
})
