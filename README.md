# dbrsocial
R package to run queries over the predictivadb and athena_pub databases.

In order to get this package running you have to set the following environmental variables:
* PGHOST
* POSTGRES_USER
* POSTGRES_PASSWORD
* PGPORT
* PGDATABASE

## Token
As this is a private repo, in order to install it via devtools you have to
ask your administrator  for the token and add it to the environmental variables
as "GITHUB_PATH".
For more information about access tokens [follow this](https://github.com/settings/tokens/).

## Installation
devtools::install_github("plataformapreventiva/dbrsocial", ref = "develop", auth_token=Sys.getenv("GITHUB_PATH"), build_vignettes=TRUE)
