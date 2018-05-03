#### Este archivo sólo es para guardar la forma en la que sebe instalar cierta
###rama.

devtools::install_github("plataformapreventiva/dbconnection", ref = "feature/testing/ES-17",auth_token=Sys.getenv("GITHUB_PATH"), build_vignettes=TRUE)
