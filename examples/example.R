#!/usr/bin/env Rscript

library(dbrsocial)
library(magrittr)

con <- prev_connect()

domicilios_sample_query <- sample_table(con,0.01,1234,raw,cuis_historico_domicilios)

cuis_sample <- large_table(con,raw,cuis_39_9) %>%
    join_tables(llave_hogar_h, domicilios_sample_query,llave_hogar_h) %>%
    retrive_result()


e_viv <- large_table(con,raw,enigh_viviendas) %>%
    dplyr::collect() %>%
    dplyr::rowwise()%>%
    dplyr::mutate(alberca=mean(c(pileta,renta)))
