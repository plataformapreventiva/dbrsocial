context("Connection to the database")
library(dbconnection)

test_that("prev_connect connects to the database",{
              expect_equal(typeof(prev_connect()),"S4")
})

test_that("load_table makes a query to load a table",{
              expect_equal(typeof(load_table(prev_connect(),raw,sifode_dic)),"S4")
})
