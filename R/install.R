#### Este archivo sólo es para guardar la forma en la que sebe instalar cierta
###rama.

dotenv::load_dot_env("../.env")
devtools::install_github("plataformapreventiva/dbrsocial", ref = "develop",auth_token=Sys.getenv("GITHUB_PATH"), build_vignettes=TRUE)
