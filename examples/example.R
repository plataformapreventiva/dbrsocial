#!/usr/bin/env Rscript

library(dbrsocial)
library(magrittr)
library(tidyverse)

dotenv::load_dot_env("../.env")

con <- prev_connect()

domicilios_sample_query <- sample_table(con,0.01,1234,raw,cuis_historico_domicilios)

cuis_sample <- large_table(con,raw,cuis_39_9) %>%
    join_tables(llave_hogar_h, domicilios_sample_query,llave_hogar_h) %>%
    retrieve_result()

clear_results(con)

discon_db(con)

e_viv <- large_table(con,raw,enigh_viviendas) %>%
    dplyr::collect() %>%
    dplyr::rowwise()%>%
    dplyr::mutate(alberca=mean(c(pileta,renta)))



#############################################
#############################################
#############################################
#############################################
#############################################

con <- pub_connect(s3dir = Sys.getenv("S3_DIR"), schema = Sys.getenv("SCHEMA"))
los_queries <- query_dic()
query <- "SELECT *
              FROM athena_pub.pub_nominal
              LIMIT 6;"
c(examp,los_queries) := load_or_run(con,query,los_queries)
examp


#############################################
#############################################
#############################################
#############################################
#############################################
### Ejemplo de geometrías


library(rangeMapper)
library(ggmap)

con1 <- prev_connect()
options <- "WHERE cve_ent = '29'"
geom_muni <- load_geom(con1,raw,geom_municipios,geom_col=geom,col_shape=cve_muni,options=options)
