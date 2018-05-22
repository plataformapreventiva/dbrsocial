#!/usr/bin/env Rscript

library(dbrsocial)
library(magrittr)

con <- prev_connect()

domicilios_sample_query <- sample_table(con,0.01,1234,raw,cuis_historico_domicilios)
cuis_sample <- large_table(con,raw,cuis_39_9) %>%
    dplyr::filter(llave_hogar_h %in% domicilios_sample_query$llave_hogar_h) %>%
    dplyr::collect()

e_viv <- large_table(con,raw,enigh_viviendas) %>%
    dplyr::collect() %>%
    dplyr::rowwise()%>%
    dplyr::mutate(hola=mean(c(pileta,renta)))
